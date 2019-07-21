{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{- |
Module      : Configuration
Description : Configuration-related data types and functions
-}
module Configuration where

import System.Environment (getProgName, getArgs)
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import Data.List (intercalate)
import Data.Either (either)
import Data.Text (Text, unpack)
import Data.Ini (Ini, readIniFile, lookupValue)
import Text.Read (readMaybe)


-- configuration data structures

-- | HTTP listener configuration
data Listen = Listen
    { host :: Maybe Text
    , port :: Int
    , tlsCertificateFile :: Text
    , tlsCertificateKeyFile :: Text } deriving (Show)

-- | Application configuration
data Configuration = Configuration
    { listen :: Listen } deriving (Show)


fromRight :: Either a b -> b
fromRight (Left _) = error "Bad pattern match"
fromRight (Right b) = b


buildListen :: Ini -> Either [String] Listen
buildListen ini =
    if null allErrors
    then Right $ Listen
        { host = either (const Nothing) Just host'
        , port = fromRight port'
        , tlsCertificateFile = fromRight tlsCertificateFile'
        , tlsCertificateKeyFile = fromRight tlsCertificateKeyFile' }
    else Left allErrors
    where
        allErrors = concat [ singletonError port'
                           , singletonError tlsCertificateFile'
                           , singletonError tlsCertificateKeyFile' ]
        host' = lookupValue "server" "host" ini
        port' = case lookupValue "server" "port" ini of
            Left e -> Left e
            Right p -> case readMaybe $ unpack p of
                Nothing -> Left "Invalid server port"
                Just p -> Right p
        tlsCertificateFile' = lookupValue "server" "tls_certificate" ini
        tlsCertificateKeyFile' = lookupValue "server" "tls_certificate_key" ini
        singletonError = either (\ e -> [e]) (const [])

buildConfiguration :: Ini -> Either [String] Configuration
buildConfiguration ini =
    if null allErrors
    then Right $ Configuration { listen = fromRight $ buildListen ini }
    else Left allErrors
    where
        allErrors = errors $ buildListen ini
        errors = either id (const [])


-- | Loads configuration from given file
--
-- If there's an error (bad configuration, etc.),
-- prints error to stderr and returns Nothing
configuration' :: FilePath -> IO (Maybe Configuration)
configuration' fileName = do
    ini <- (readIniFile fileName) :: IO (Either String Ini)
    case ini of
        Left err -> do
            printError $ "Failed to parse configuration file "
                ++ fileName ++ ": " ++ err
            return Nothing
        Right ini -> do
            case buildConfiguration ini of
                Left errors -> do
                    printError $ "Found configuration errors: "
                        ++ intercalate ", " errors
                    return Nothing
                Right configuration -> return $ Just configuration
    where
        printError error = getProgName >>=
                           \ p -> hPutStrLn stderr $ p ++ ": " ++ error


-- | Loads confiugration from file indicated by
-- first application argument.
--
-- If there's an error (bad argument, bad configuration, etc.),
-- prints error to stderr and exits with non-zero exit code
configuration :: IO Configuration
configuration = do
    progName <- getProgName
    args <- getArgs
    fileName <- case args of
        [a] -> return a
        [] -> do hPutStrLn stderr $ "Usage: " ++ progName ++ " configuration_file"
                 exitFailure
        _  -> do hPutStrLn stderr $ progName ++ ": Too many arguments"
                 exitFailure

    maybeConfiguration <- configuration' fileName

    case maybeConfiguration of
        Nothing -> exitFailure
        Just c -> return c
