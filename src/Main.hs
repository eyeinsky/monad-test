{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Concurrent (forkIOWithUnmask)
import System.IO (stdout, BufferMode(LineBuffering), hSetBuffering)
import Data.Default.Class (def)
import Data.Maybe (maybe)
import Data.String (fromString)
import Data.Text (unpack)
import Data.X509 (CertificateChain(..))
import Network.TLS (ServerHooks(onClientCertificate),
                    CertificateUsage(CertificateUsageAccept),
                    CertificateUsage(CertificateUsageReject),
                    CertificateRejectReason(CertificateRejectUnknownCA))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort,  setFork)
import Network.Wai.Handler.WarpTLS (TLSSettings(tlsWantClientCert),
                                    TLSSettings(tlsServerHooks),
                                    tlsSettings,
                                    runTLS)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Wai.Routes (waiApp, middleware)
import Configuration (configuration,
                      Configuration(listen),
                      Listen(host, port),
                      Listen(tlsCertificateFile, tlsCertificateKeyFile))


verifyCertificate :: CertificateChain -> IO CertificateUsage
verifyCertificate (CertificateChain certificates) = do
    -- Here we have access to client certificate
    return $ if null certificates
             then CertificateUsageReject CertificateRejectUnknownCA
             else CertificateUsageAccept


-- Default fork function
fork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ()
fork io = forkIOWithUnmask io >> return ()
--fork io = forkIOWithUnmask io' >> return ()
--    where
--        io' :: (forall a. IO a -> IO a) -> IO ()
--        io' unmask = newIORef (CertificateChain []) >>= runReaderT (liftIO $ io unmask)


application :: Configuration -> Application
application _ = waiApp $ do
    middleware logStdout
    -- Here I'd like to access the client certificate


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    c <- configuration

    let listener = listen c
    let settings = setHost (fromString $ maybe "*" unpack $ host listener)
                 $ setPort (port listener)
                 $ setFork fork
                 $ defaultSettings

    let warpTlsSettings = tlsSettings (unpack $ tlsCertificateFile listener)
                                      (unpack $ tlsCertificateKeyFile listener)

    let serverHooks = def { onClientCertificate = verifyCertificate }

    let settingsWithHooks = warpTlsSettings
                          { tlsWantClientCert = True
                          , tlsServerHooks = serverHooks }

    runTLS settingsWithHooks settings $ application c

