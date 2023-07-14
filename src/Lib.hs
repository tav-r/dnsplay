module Lib
    ( resolveFromStdin
    ) where

import           Control.Arrow                (Arrow ((&&&), (***)), (>>>))
import           Control.Concurrent           (MVar, newMVar, withMVar)
import           Control.Monad.Reader         (ReaderT, ask, liftIO, runReaderT,
                                               (<=<))
import           Data.ByteString.Char8        (pack, unpack)
import           Data.Either                  (fromRight)
import           Data.Foldable                (foldlM)
import           Data.List                    (intercalate)
import           Data.Maybe                   (mapMaybe)
import           Network.DNS                  (DNSError, RData (RD_CNAME),
                                               Resolver)
import           Network.DNS.Lookup           (lookupA, lookupAAAA, lookupMX,
                                               lookupSRV, lookupTXT, lookupPTR)
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

data Flag = Resolvers String | Type String | Parallel String | Retries String | Timeout String | Help
    deriving (Show, Eq)

data Config = Config
    { resolvers :: Maybe String
    , type_     :: String
    , parallel  :: Int
    , retries :: Int
    , timeout :: Int
    }

defaultConfig :: Config
defaultConfig = Config Nothing "A" 50 3 2000000

type Nameserver = String
type DomainName = String
type PrettyDNSResult = IO (Either DNSError String)

defaultResolvers :: [Nameserver]
defaultResolvers = ["9.9.9.9", "9.9.9.10", "9.9.9.11"]

options :: [OptDescr Flag]
options = [
    Option ['e'] ["retries"] (ReqArg Retries "N") "Specify number of retries on lookup failure",
    Option ['h'] ["help"] (NoArg Help) "display this help message",
    Option ['i'] ["timeout"] (ReqArg Timeout "μs") "Specify timeout in microseconds",
    Option ['p'] ["parallel"] (ReqArg Parallel "N") "number of parallel threads",
    Option ['r'] ["resolvers"] (ReqArg Resolvers "FILE") "path to a file containing a list of DNS resolvers",
    Option ['t'] ["type"] (ReqArg Type "type") "type of record to look up, default 'A'"]

resolveSeedForResolver :: Int -> Int -> [Nameserver] -> IO ResolvSeed
resolveSeedForResolver rTimeout rRetry rs =
    makeResolvSeed defaultResolvConf {
        resolvInfo = RCHostNames rs,
        resolvTimeout = rTimeout,
        resolvRetry = rRetry
    }

intercalateFunctorialList :: Functor a => Functor b => (c -> String) -> a (b [c]) -> a (b String)
intercalateFunctorialList f = (fmap . fmap) $ concatInner f
    where
        concatInner = (Data.List.intercalate "," .) .(<$>)

recordTypeHandlers :: String -> Resolver -> Domain -> PrettyDNSResult
recordTypeHandlers "A"     = (intercalateFunctorialList show .) . lookupA
recordTypeHandlers "AAAA"  = (intercalateFunctorialList show .) . lookupAAAA
recordTypeHandlers "MX"    = (intercalateFunctorialList (unpack . fst) .) . lookupMX
recordTypeHandlers "TXT"   = (intercalateFunctorialList unpack .) . lookupTXT
recordTypeHandlers "SRV"   = (intercalateFunctorialList (\(_, _, _, d) -> unpack d).) . lookupSRV
recordTypeHandlers "CNAME" = (intercalateFunctorialList unpack .) . lookupCNAME
recordTypeHandlers "PTR" = (intercalateFunctorialList unpack .) . lookupPTR
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

-- | Central function which does all the resolving and printing
asyncBulkLookup :: ReaderT Config IO ()
asyncBulkLookup = do
    config <- ask
    (lock, input, nameserverList) <- liftIO $ do l <- newMVar (); i <- getContents; nss <- fetchNameservers $ resolvers config; return (l, i, nss)

    liftIO $ Stream.fold Fold.drain $ Stream.parMapM (maxThreads $ parallel config) (queryAndPrintResult config lock) $ Stream.fromList
        $ uncurry zip $ nameserverListDomainNamePairs nameserverList input
    where
        fetchNameservers :: Maybe FilePath -> IO [Nameserver]
        fetchNameservers = maybe (return defaultResolvers) $ (lines <$>) . readFile

        queryAndPrintResult :: Config -> MVar () -> ([Nameserver], DomainName) -> IO ()
        queryAndPrintResult = (uncurry (>>>) .) . curry ((***) lookupFunReturnArg printAndLockIOMaybeResult)
            where
                lookupFunFromConfig :: Config -> [Nameserver] -> DomainName -> PrettyDNSResult
                lookupFunFromConfig config nss name = do
                    seed <-resolveSeedForResolver (timeout config) (retries config) nss

                    withResolver seed ((flip . recordTypeHandlers $ type_ config) (pack name))

                lookupFunReturnArg :: Config -> ([Nameserver], DomainName) -> (String, PrettyDNSResult)
                lookupFunReturnArg = (&&&) snd . uncurry . lookupFunFromConfig

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
        updateConfig conf (Type s) = if s `elem` ["A", "AAAA", "SRV", "CNAME", "MX", "TXT", "PTR"] then Right conf {type_ = s} else Left "invalid type"
        updateConfig conf (Parallel s) = maybe (Left "invalid number of parallel threads") (\r -> Right conf {parallel = r}) $ readMaybe s
        updateConfig conf (Retries r) = maybe (Left "invalid number retries") (\x -> Right conf {retries = x}) $ readMaybe r
        updateConfig conf (Timeout t) = maybe (Left "invalid value for microseconds") (\x -> Right conf {timeout = x}) $ readMaybe t
        updateConfig _ Help = Left $ usageInfo "dnsplay" options

-- | Gets command line options, creates config and either runs the central function asyncBulkLookup or prints errors/help message
resolveFromStdin :: IO ()
resolveFromStdin = do
    (opts, _, _) <- getOpt Permute options <$> getArgs

    either (const exitFailure <=< putStrLn) (runReaderT asyncBulkLookup) $ parseConfig opts
