module Lib
    ( resolveFromStdin
    ) where

import           Control.Arrow                (Arrow ((&&&), (***)), (>>>))
import           Control.Concurrent           (MVar, newMVar, withMVar)
import           Control.Monad.Reader         (ReaderT, ask, liftIO, runReaderT,
                                               (<=<))
import           Data.ByteString.Char8        (pack)
import           Data.Either                  (fromRight)
import           Data.Foldable                (foldlM)
import           Data.List                    (intercalate)
import           Data.Maybe                   (mapMaybe)
import           Network.DNS                  (DNSError, RData (RD_CNAME),
                                               Resolver)
import           Network.DNS.Lookup           (lookupA, lookupAAAA, lookupMX,
                                               lookupSRV, lookupTXT)
import           Network.DNS.LookupRaw        (lookup)
import           Network.DNS.Resolver         (FileOrNumericHost (RCHostNames),
                                               ResolvConf (resolvInfo, resolvRetry, resolvTimeout),
                                               ResolvSeed, defaultResolvConf,
                                               makeResolvSeed, withResolver)
import           Network.DNS.Types            (Domain, TYPE (CNAME))
import           Streamly.Data.Fold           as Fold (drain)
import           Streamly.Data.Stream.Prelude as Stream (fold, fromList,
                                                         maxThreads, parMapM)
import           System.Console.GetOpt        (ArgDescr (NoArg, ReqArg),
                                               ArgOrder (Permute),
                                               OptDescr (..), getOpt, usageInfo)
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure)
import           Text.Read                    (readMaybe)

data Flag = Resolvers String | Type String | Parallel String | Help
    deriving (Show, Eq)

type LookupFunction a = Resolver -> Domain -> IO (Either DNSError a)

data Config = Config
    { resolvers :: Maybe String
    , type_     :: String
    , parallel  :: Int
    }

defaultConfig :: Config
defaultConfig = Config Nothing "A" 50

type Nameserver = String
type DomainName = String
type PrettyDNSResult = IO (Either DNSError String)

defaultResolver :: Nameserver
defaultResolver = "9.9.9.9"

resolvTimeoutMicros :: Int
resolvTimeoutMicros = 2000000

resolvRetryCount :: Int
resolvRetryCount = 3

options :: [OptDescr Flag]
options = [
    Option ['r'] ["resolvers"] (ReqArg Resolvers "FILE") "path to a file containing a list of DNS resolvers",
    Option ['t'] ["type"] (ReqArg Type "type") "type of record to look up, default 'A'",
    Option ['p'] ["parallel"] (ReqArg Parallel "N") "number of parallel threads",
    Option ['h'] ["help"] (NoArg Help) "display this help message"]

resolveSeedForResolver :: [Nameserver] -> IO ResolvSeed
resolveSeedForResolver rs =
    makeResolvSeed defaultResolvConf {
        resolvInfo = RCHostNames rs,
        resolvTimeout = resolvTimeoutMicros,
        resolvRetry = resolvRetryCount
    }

concatShowableListToString :: Functor a => Functor b => Show c => a (b [c]) -> a (b String)
concatShowableListToString = fmap (fmap concatInner)
    where
        concatInner = Data.List.intercalate "," . (show <$>)

recordTypeHandlers :: String -> Resolver -> Domain -> PrettyDNSResult
recordTypeHandlers "A"     = (concatShowableListToString .) . lookupA
recordTypeHandlers "AAAA"  = (concatShowableListToString .) . lookupAAAA
recordTypeHandlers "MX"    = (concatShowableListToString .) . lookupMX
recordTypeHandlers "TXT"   = (concatShowableListToString .) . lookupTXT
recordTypeHandlers "SRV"   = (concatShowableListToString .) . lookupSRV
recordTypeHandlers "CNAME" = (concatShowableListToString .) . lookupCNAME
recordTypeHandlers _       = undefined -- this happens only if argument parsing was wrong

lookupCNAME :: Resolver -> Domain -> IO (Either DNSError [Domain])
lookupCNAME r n = (mapMaybe maybeShowCNAME <$>) <$> lookupCNAMERaw r n
    where
        lookupCNAMERaw = curry $ flip (uncurry Network.DNS.LookupRaw.lookup) CNAME
        maybeShowCNAME (RD_CNAME d) = Just d
        maybeShowCNAME _            = Nothing

-- | All combinations of given length 'n' from elements of a given list 'as'
combinationsOfLengthN :: Int -> [a] -> [[a]]
combinationsOfLengthN n as = if n <= 0 then [[]] else [a : b | a <- as, b <- combinationsOfLengthN  (n - 1) as]

-- | Central function which does all the resolveing and printing
asyncBulkLookup :: ReaderT Config IO ()
asyncBulkLookup = do
    config <- ask
    (lock, input, nameserverList) <- liftIO $ do l <- newMVar (); i <- getContents; nss <- fetchNameservers $ resolvers config; return (l, i, nss)

    liftIO $ Stream.fold Fold.drain $ Stream.parMapM (maxThreads $ parallel config) (queryAndPrintResult config lock) $ Stream.fromList
        $ uncurry zip $ nameserverListDomainNamePairs nameserverList input
    where
        fetchNameservers :: Maybe FilePath -> IO [Nameserver]
        fetchNameservers = maybe (return [defaultResolver]) $ (lines <$>) . readFile

        queryAndPrintResult :: Config -> MVar () -> ([Nameserver], DomainName) -> IO ()
        queryAndPrintResult = (uncurry (>>>) .) . curry ((***) lookupFunReturnArg printAndLockIOMaybeResult)
            where
                resolveWithNameserver :: LookupFunction a -> DomainName -> [Nameserver] -> IO (Either DNSError a)
                resolveWithNameserver = (((<=< resolveSeedForResolver) . flip withResolver) .) . (. pack) . flip

                lookupFunfromConfig :: Config -> [Nameserver] -> DomainName -> PrettyDNSResult
                lookupFunfromConfig = flip . resolveWithNameserver . recordTypeHandlers . type_

                lookupFunReturnArg :: Config -> ([Nameserver], DomainName) -> (String, PrettyDNSResult)
                lookupFunReturnArg = (&&&) (show . snd) . uncurry . lookupFunfromConfig

                printAndLockIOMaybeResult :: MVar () -> (DomainName, PrettyDNSResult) -> IO ()
                printAndLockIOMaybeResult lock (arg, ioMabeList) = do
                    list <- ioMabeList
                    withMVar lock $ const (putStrLn . (++) (arg ++ ":") $ fromRight "" list)

        nameserverListDomainNamePairs :: [Nameserver] -> String -> ([[Nameserver]], [String])
        nameserverListDomainNamePairs = curry $ (***) (repeatedCombinationsOfLength 3) lines
            where
                repeatedCombinationsOfLength :: Int -> [a] -> [[a]]
                repeatedCombinationsOfLength = curry $ cycle . (Prelude.reverse <$>) . uncurry combinationsOfLengthN

-- | Build Config based on a list of flags
parseConfig :: [Flag] -> Either String Config
parseConfig = foldlM updateConfig defaultConfig
    where
        updateConfig conf (Resolvers s) = Right conf {resolvers = Just s}
        updateConfig conf (Type s) = if s `elem` ["A", "AAAA", "SRV", "CNAME", "MX", "TXT"] then Right conf {type_ = s} else Left "invalid type"
        updateConfig conf (Parallel s) = maybe (Left "invalid number of parallel threads") (\r -> Right conf {parallel = r}) $ readMaybe s
        updateConfig _ Help = Left $ usageInfo "dnsplay" options

-- | Gets command line options, creates config and either runs the central function asyncBulkLookup or prints errors/help message
resolveFromStdin :: IO ()
resolveFromStdin = do
    (opts, _, _) <- getOpt Permute options <$> getArgs

    either (const exitFailure <=< putStrLn) (runReaderT asyncBulkLookup) $ parseConfig opts
