{-# LANGUAGE FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}
module Network.FTPE.Internal.FClientExt 
( -- * FTP commands 
     easyConnectFTPE, connectFTPE, loginE
   , dirE, quitE, sendcmdE, cwdE, nlstE, loginAnonE 
   , setPassiveE, isPassiveE, getlinesE, getbinaryE
   , downloadbinaryE, deleteE, sizeE, rmdirE, mkdirE, pwdE, renameE, putlinesE
   , putbinaryE)
where

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

d' :: (FConn -> t -> IO b) -> t -> IO b
d' f par = maker (`f` par) 

setPassiveE :: Bool -> IO ()
setPassiveE = d' setPassive

getbinaryE :: String -> IO (String, FTPResult)
getbinaryE = d' getbinary 

getlinesE, retrlinesE :: String -> IO ([String], FTPResult)
getlinesE  = d' getlines
retrlinesE  = d' retrlines

nlstE, dirE :: Maybe String -> IO [String]
nlstE = d' nlst
dirE = d' dir

sendcmdE, cwdE, downloadbinaryE, rmdirE,deleteE, uploadbinaryE:: String -> IO FTPResult
sendcmdE   = d' sendcmd
cwdE = d' cwd
downloadbinaryE = d' downloadbinary
rmdirE = d' rmdir
deleteE = d' delete
uploadbinaryE = d' uploadbinary 

sizeE :: (Num a, Read a) => String -> IO a
sizeE = d' size

mkdirE :: String -> IO (Maybe String, FTPResult)
mkdirE = d' mkdir

pwdE :: IO (Maybe String, FTPResult)
pwdE = maker pwd  


d'' :: (FConn -> t -> t1 -> IO b) -> t -> t1 -> IO b
d'' f par1 par2  = maker (\ftp -> f ftp par1 par2)

renameE, putbinaryE :: String -> String -> IO FTPResult
renameE = d'' rename
putbinaryE = d'' putbinary

putlinesE, storlinesE :: String -> [String] -> IO FTPResult
putlinesE  = d'' putlines
storlinesE = d'' storlines 

                           