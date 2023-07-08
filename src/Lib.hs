{-# LANGUAGE LambdaCase #-}

module Lib
    ( resolveFromStdin
    ) where

import           Control.Arrow                (Arrow ((&&&)))
import           Control.Concurrent           (MVar, newMVar, putMVar, takeMVar)
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
import           Text.Read                    (readMaybe)
import System.Exit (exitFailure)

data Flag = Resolvers String | Type String | Parallel String | Help
    deriving (Show, Eq)

type LookupFunction a = Resolver -> Domain -> IO (Either DNSError [a])

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

recordTypeHandlers :: String -> (Int -> [String] -> [String] -> IO ())
recordTypeHandlers "A"     = asyncBulkLookup lookupA
recordTypeHandlers "AAAA"  = asyncBulkLookup lookupAAAA
recordTypeHandlers "MX"    = asyncBulkLookup lookupMX
recordTypeHandlers "TXT"   = asyncBulkLookup lookupTXT
recordTypeHandlers "SRV"   = asyncBulkLookup lookupSRV
recordTypeHandlers "CNAME" = asyncBulkLookup lookupCNAME
recordTypeHandlers _       = undefined -- this happens only if argument parsing was wrong

lookupCNAME :: LookupFunction Domain
lookupCNAME r n = (mapMaybe maybeShowCNAME <$>) <$> lookupCNAMERaw r n
    where
        lookupCNAMERaw = curry $ flip (uncurry Network.DNS.LookupRaw.lookup) CNAME
        maybeShowCNAME = \case RD_CNAME d -> Just d; _ -> Nothing

resolveWithNameserver :: LookupFunction a -> [String] -> String -> IO (Either DNSError [a])
resolveWithNameserver f nameservers name =
    resolveSeedForResolver nameservers >>= resolve
    where
        resolve = flip withResolver (flip f $ pack name)

resolveWithNameserverPretty :: Show a => LookupFunction a -> [String] -> String -> PrettyDNSResult
resolveWithNameserverPretty f nameservers name =
     (convertIPv4ListToString <$>) <$> resolveWithNameserver f nameservers name
        where
            convertIPv4ListToString = Data.List.intercalate "," . (show <$>)

permutationsN :: Int -> [a] -> [[a]]
permutationsN n l
    | n <= 0 = [[]]
    | otherwise = [a : b | a <- l, b <- permutationsN  (n - 1) l]

printAndLockDNSResult :: MVar () -> (PrettyDNSResult, String) -> IO ()
printAndLockDNSResult lock (ioMabeIps, domain) = do
    ips <- ioMabeIps
    takeMVar lock
    putStr $ domain ++ ":"
    putStrLn $ fromRight "" ips
    putMVar lock ()

asyncBulkLookup :: Show a => LookupFunction a -> Int -> [String] -> [String] -> IO ()
asyncBulkLookup lookupFun npar nameservers ds = do
    lock <- newMVar ()
    Stream.fold Fold.drain $
        Stream.parMapM (maxThreads npar)
        (\(s, n) -> (printAndLockDNSResult lock . (resolveWithNameserverPretty lookupFun s &&& id)) n)
        $ Stream.fromList $ zip (cycle $ Prelude.reverse <$> permutationsN 3 nameservers) ds

parseConfig :: [Flag] -> Either String Config
parseConfig = foldlM updateConf defaultConfig
    where
        updateConf conf (Resolvers s) = Right conf {resolvers = Just s}
        updateConf conf (Type s) = if s `elem` ["A", "AAAA", "SRV", "CNAME", "MX", "TXT"] then Right conf {type_ = s} else Left "invalid type"
        updateConf conf (Parallel s) = maybe (Left "invalid number of parallel threads") (\r -> Right conf {parallel = r}) $ readMaybe s
        updateConf _ Help = Left $ usageInfo "dnsplay" options

resolveFromStdin :: IO ()
resolveFromStdin = do
    input <- getContents

    (opts, _, _) <- getOpt Permute options <$> getArgs

    either printAndExit (\conf -> nameserversIO conf >>= runResolve conf input) $ parseConfig opts
        where
            printAndExit s = do
                putStrLn s
                exitFailure
            runResolve conf input nameservers = recordTypeHandlers (type_ conf) (parallel conf) nameservers (lines input)
            nameserversIO config = maybe (return [defaultResolver]) ((lines <$>) . readFile) (resolvers config)
