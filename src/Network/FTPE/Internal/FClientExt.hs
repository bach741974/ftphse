{-# LANGUAGE FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}
module Network.FTPE.Internal.FClientExt where

import Network.FTPE.Internal.FClient 
import Data.Global
import Data.IORef
import Data.Typeable 
import Network.Socket(PortNumber, HostName)
import Network.FTP.Client.Parser (FTPResult)

instance Typeable FConn where
        typeOf _ = typeOf ""

getVarFTP :: IORef (Maybe FConn)
getVarFTP = declareIORef "get-ftp" Nothing 


easyConnectFTPE :: HostName -> IO FConn
easyConnectFTPE h = do 
                      con <-  easyConnectFTP h
                      writeIORef getVarFTP $ Just con
                      return con
                      

connectFTPE :: HostName -> PortNumber  -> IO (FConn,FTPResult)
connectFTPE h p = do 
                     (con, res) <-  connectFTP h p 
                     writeIORef getVarFTP $ Just con
                     return (con, res)                  

maker :: (FConn -> IO b) -> IO b
maker f = do 
            con <- readIORef getVarFTP
            case con of
               (Just con') -> f con'
               Nothing -> error "There is no any ftp connection; Please make connection"
               
quitE :: IO FTPResult
quitE = maker quit

isPassiveE :: IO Bool
isPassiveE = maker isPassive 

loginAnonE :: Maybe Timeout -> IO (Maybe FTPResult, Maybe ThreadId)
loginAnonE t = maker (`loginAnon` t)

loginE :: Maybe Timeout
                -> String
                -> Maybe String
                -> Maybe String
                -> IO (Maybe FTPResult, Maybe ThreadId)
loginE t name p acc = maker (\con -> login con t name p acc)

setPassiveE :: Bool -> IO ()
setPassiveE b = maker (`setPassive` b)

getbinaryE :: String -> IO (String, FTPResult)
getbinaryE s = maker (`getbinary` s)  

getlinesE, retrlinesE :: String -> IO ([String], FTPResult)
getlinesE s = maker (`getlines` s)

retrlinesE s = maker (`retrlines` s)                                