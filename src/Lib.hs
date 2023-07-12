module Lib
    ( resolveFromStdin
    ) where

import           Control.Arrow                (Arrow ((&&&), (***)))
import           Control.Concurrent           (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad.Reader         (ReaderT, ask, liftIO, runReaderT)
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
    Option ['h'] ["help"] (NoArg Help) "display this help message"
    ]

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
recordTypeHandlers = recordTypeHandlersR
    where
        recordTypeHandlersR "A"     r =  concatShowableListToString . lookupA r
        recordTypeHandlersR "AAAA"     r = concatShowableListToString . lookupAAAA r
        recordTypeHandlersR "MX"     r = concatShowableListToString. lookupMX r
        recordTypeHandlersR "TXT"     r = concatShowableListToString . lookupTXT r
        recordTypeHandlersR "SRV"     r = concatShowableListToString . lookupSRV r
        recordTypeHandlersR "CNAME"     r = concatShowableListToString . lookupCNAME r
        recordTypeHandlersR _ _       = undefined -- this happens only if argument parsing was wrong

lookupCNAME :: Resolver -> Domain -> IO (Either DNSError [Domain])
lookupCNAME r n = (mapMaybe maybeShowCNAME <$>) <$> lookupCNAMERaw r n
    where
        lookupCNAMERaw = curry $ flip (uncurry Network.DNS.LookupRaw.lookup) CNAME
        maybeShowCNAME (RD_CNAME d) = Just d
        maybeShowCNAME _            = Nothing

resolveWithNameserver :: LookupFunction a -> [Nameserver] -> DomainName -> IO (Either DNSError a)
resolveWithNameserver f nameservers name =
    resolveSeedForResolver nameservers >>= resolve
    where
        resolve = (`withResolver` (`f` pack name))

permutationsN :: Int -> [a] -> [[a]]
permutationsN n l
    | n <= 0 = [[]]
    | otherwise = [a : b | a <- l, b <- permutationsN  (n - 1) l]

printAndLockIOMaybeResult :: MVar () -> (DomainName, PrettyDNSResult) -> IO ()
printAndLockIOMaybeResult lock (arg, ioMabeList) = do
    ips <- ioMabeList
    takeMVar lock
    putStr $ arg ++ ":"
    putStrLn $ fromRight "" ips
    putMVar lock ()

asyncBulkLookup :: ReaderT Config IO ()
asyncBulkLookup = do
    config <- ask
    lock <- liftIO $ newMVar ()
    input <- liftIO getContents
    nameserverList <- liftIO $ fetchNameservers $ resolvers config

    liftIO $ Stream.fold Fold.drain $ Stream.parMapM (maxThreads $ parallel config) (queryAndPrintResult config lock) $ Stream.fromList
        $ uncurry zip $ nameserverListDomainNamePairs nameserverList input
    where
        fetchNameservers :: Maybe FilePath -> IO [Nameserver]
        fetchNameservers = maybe (return [defaultResolver]) linesFromFile
            where
                linesFromFile :: FilePath -> IO [String]
                linesFromFile = (lines <$>) . readFile

        queryAndPrintResult :: Config -> MVar () -> ([Nameserver], DomainName) -> IO ()
        queryAndPrintResult = (uncurry (flip (.)) .) . curry ((***) lookupFunReturnArg printAndLockIOMaybeResult)
            where
                lookupFunfromConfig :: Config -> [Nameserver] -> DomainName -> PrettyDNSResult
                lookupFunfromConfig = resolveWithNameserver . recordTypeHandlers . type_

                applyAndReturnArg :: Show b => ((a, b) -> c) -> ((a, b) -> (String, c))
                applyAndReturnArg = (&&&) (show . snd)

                lookupFunReturnArg :: Config -> ([Nameserver], DomainName) -> (String, PrettyDNSResult)
                lookupFunReturnArg = applyAndReturnArg . uncurry . lookupFunfromConfig

        nameserverListDomainNamePairs :: [Nameserver] -> String -> ([[Nameserver]], [String])
        nameserverListDomainNamePairs = curry $ (***) (cycledPermutationsOfSize 3) lines
            where
                cycledPermutationsOfSize :: Int -> [a] -> [[a]]
                cycledPermutationsOfSize = curry $ cycle . (Prelude.reverse <$>) . uncurry permutationsN

parseConfig :: [Flag] -> Either String Config
parseConfig = foldlM updateConfig defaultConfig
    where
        updateConfig conf (Resolvers s) = Right conf {resolvers = Just s}
        updateConfig conf (Type s) = if s `elem` ["A", "AAAA", "SRV", "CNAME", "MX", "TXT"] then Right conf {type_ = s} else Left "invalid type"
        updateConfig conf (Parallel s) = maybe (Left "invalid number of parallel threads") (\r -> Right conf {parallel = r}) $ readMaybe s
        updateConfig _ Help = Left $ usageInfo "dnsplay" options

resolveFromStdin :: IO ()
resolveFromStdin = do
    (opts, _, _) <- getOpt Permute options <$> getArgs

    either displayErrorAndExit (runReaderT asyncBulkLookup) $ parseConfig opts
        where
            displayErrorAndExit s = do
                putStrLn s
                exitFailure
