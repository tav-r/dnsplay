module Lib
    ( resolveFromStdin
    ) where

import           Control.Arrow                (Arrow ((&&&)))
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

resolveSeedForResolver :: [String] -> IO ResolvSeed
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

resolveWithNameserver :: LookupFunction a -> [String] -> String -> IO (Either DNSError a)
resolveWithNameserver f nameservers name =
    resolveSeedForResolver nameservers >>= resolve
    where
        resolve = flip withResolver (flip f $ pack name)

permutationsN :: Int -> [a] -> [[a]]
permutationsN n l
    | n <= 0 = [[]]
    | otherwise = [a : b | a <- l, b <- permutationsN  (n - 1) l]

printAndLockIOMaybeResult :: MVar () -> (String, PrettyDNSResult) -> IO ()
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
    nameserverList <- liftIO $ maybe (return [defaultResolver]) readLinesFromFile $ resolvers config

    liftIO $ Stream.fold Fold.drain $
        Stream.parMapM (maxThreads $ parallel config)
        (lookupAndPrint lock config)
        $ Stream.fromList $ zip (cycledPermutationsOfSize 3 nameserverList) $ lines input
    where
        readLinesFromFile :: FilePath -> IO [String]
        readLinesFromFile = (lines <$>) . readFile

        applyAndReturnArg :: Show b => ((a, b) -> c) -> ((a, b) -> (String, c))
        applyAndReturnArg = (&&&) (show . snd)

        lookupFunfromConfig :: Config -> [String] -> String -> PrettyDNSResult
        lookupFunfromConfig = resolveWithNameserver . recordTypeHandlers . type_

        cycledPermutationsOfSize :: Int -> [a] -> [[a]]
        cycledPermutationsOfSize = curry $ cycle . (Prelude.reverse <$>) . uncurry permutationsN

        lookupAndPrint :: MVar () -> Config -> ([String], String) -> IO ()
        lookupAndPrint lock = (printAndLockIOMaybeResult lock .) . applyAndReturnArg . (uncurry . lookupFunfromConfig)

parseConfig :: [Flag] -> Either String Config
parseConfig = foldlM updateConf defaultConfig
    where
        updateConf conf (Resolvers s) = Right conf {resolvers = Just s}
        updateConf conf (Type s) = if s `elem` ["A", "AAAA", "SRV", "CNAME", "MX", "TXT"] then Right conf {type_ = s} else Left "invalid type"
        updateConf conf (Parallel s) = maybe (Left "invalid number of parallel threads") (\r -> Right conf {parallel = r}) $ readMaybe s
        updateConf _ Help = Left $ usageInfo "dnsplay" options

resolveFromStdin :: IO ()
resolveFromStdin = do
    (opts, _, _) <- getOpt Permute options <$> getArgs

    either printAndExit (runReaderT asyncBulkLookup) $ parseConfig opts
        where
            printAndExit s = do
                putStrLn s
                exitFailure
