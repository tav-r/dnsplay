{-# LANGUAGE LambdaCase #-}

module Lib
    ( resolveFromStdin
    ) where

import           Control.Arrow                (Arrow ((&&&)))
import           Control.Concurrent           (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad                (when)
import           Data.ByteString.Char8        (pack)
import           Data.Either                  (fromRight)
import           Data.List                    (find, intercalate)
import qualified Data.Map                     as Map
import           Data.Maybe                   (mapMaybe)
import           Network.DNS                  (DNSError, RData (RD_CNAME),
                                               Resolver)
import           Network.DNS.Lookup           (lookupA, lookupAAAA, lookupMX,
                                               lookupSOA, lookupSRV, lookupTXT)
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
import           System.Environment           (getArgs, getProgName)
import           System.Exit                  (exitSuccess)
import           Text.Read                    (readMaybe)

data Flag = Resolvers String | Type String | Parallel String | Help
    deriving (Show, Eq)

type LookupFunction a = Resolver -> Domain -> IO (Either DNSError [a])

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
resolveSeedForResolver resolvers =
    makeResolvSeed defaultResolvConf {
        resolvInfo = RCHostNames resolvers,
        resolvTimeout = resolvTimeoutMicros,
        resolvRetry = resolvRetryCount
    }

recordTypeHandlers :: Map.Map String (Int -> [String] -> [String] -> IO ())
recordTypeHandlers = Map.fromList
    [ ("A", asyncBulkLookup lookupA)
    , ("AAAA", asyncBulkLookup lookupAAAA)
    , ("TXT", asyncBulkLookup lookupTXT)
    , ("MX", asyncBulkLookup lookupMX)
    , ("SRV", asyncBulkLookup lookupSRV)
    , ("SOA", asyncBulkLookup lookupSOA)
    , ("CNAME", asyncBulkLookup lookupCNAME)
    ]

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
asyncBulkLookup lookupFun parallel nameservers ds = do
    lock <- newMVar ()
    Stream.fold Fold.drain $
        Stream.parMapM (maxThreads parallel)
        (\(s, n) -> (printAndLockDNSResult lock . (resolveWithNameserverPretty lookupFun s &&& id)) n)
        $ Stream.fromList $ zip (cycle $ Prelude.reverse <$> permutationsN 3 nameservers) ds

resolveFromStdin :: IO ()
resolveFromStdin = do
    input <- getContents

    (opts, _, _) <- getOpt Permute options <$> getArgs

    when (Help `elem` opts) $ do
        getProgName >>= putStr . flip usageInfo options
        exitSuccess

    nameservers <-
        maybe (return [defaultResolver])
        ((lines <$>) . readFile) $
        (\case Just (Resolvers r) -> Just r; _ -> Nothing)
        (find (\case Resolvers _ -> True; _ -> False) opts)

    let recordType =
            (\case (Just (Type t)) -> t; _ -> "A")
            (find (\case Type _ -> True; _ -> False) opts)
    let parallel = readMaybe $
            (\case (Just (Parallel s)) -> s; _ -> "50")
            (find (\case Parallel _ -> True; _ -> False) opts) :: Maybe Int

    maybe (putStrLn "invalid number specified for 'parallel'")
        (\n ->
            maybe (putStrLn "Unknown record type")
                (\handler -> handler n nameservers $ lines input)
                (Map.lookup recordType recordTypeHandlers)
        ) parallel
