
module Network.FTPE.Internal.FClient 
(
 -- * FTP commands 
  
   setLogLevel, setLevel, Priority(..), easyConnectFTP, getPassword, connectFTP, login,
   Timeout (Time), dir, quit, sendcmd, cwd, nlst, loginAnon, FConnection(FTP), FConn 
   ,  block', setPassive, isPassive, N.enableFTPDebugging, getlines, getbinary
   , ThreadId, downloadbinary, delete, size, rmdir, mkdir, pwd, rename, putlines
   , putbinary, uploadbinary, retrlines, storlines 
   
 ) 
           
where
import System.Log.Logger (Priority(..), Priority, updateGlobalLogger, setLevel)
import qualified Network.FTP.Client  as N 

import Network.FTP.Client.Parser (FTPResult)
import Network.Socket(PortNumber, HostName)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Concurrent (forkFinally, threadDelay)
import GHC.Conc.Sync (ThreadId)
import Control.Monad.State.Strict (forever)
import Control.Exception.Base (finally, onException)
import System.IO (stdin, hGetEcho, hSetEcho, stdout, hFlush)
import Control.Exception (bracket_)
import Control.Monad (void)

newtype FConnection = FTP (N.FTPConnection, Bool)
newtype Timeout = Time Int
type FConn = TMVar FConnection
 
         
setLogLevel :: Priority -> IO ()
setLogLevel lev = do
                        updateGlobalLogger "Network.FTP.Client.Parser" level
                        updateGlobalLogger "Network.FTP.Client" level
                    where  level = setLevel lev 
                    

             
                        
easyConnectFTP :: HostName -> IO FConn
easyConnectFTP h = do 
                        con <-  N.easyConnectFTP h
                        atomically $ newTMVar $ FTP (con, False)
                


connectFTP :: HostName
                -> PortNumber -> IO (FConn, FTPResult)
connectFTP h p = do 
                     (ftp, res) <-  N.connectFTP h p 
                     v <- atomically $ newTMVar $ FTP (ftp, False)
                     return (v, res)                  
                                                              

loginAnon :: FConn
                   -> Maybe Timeout -> IO (Maybe FTPResult, Maybe ThreadId)
loginAnon = l' N.loginAnon

login :: FConn
            -> Maybe Timeout
            -> String
            -> Maybe String
            -> Maybe String
            -> IO (Maybe FTPResult, Maybe ThreadId)
login var t name p acc = l' (\f ->  N.login f name p acc) var t

l' :: (N.FTPConnection -> IO t)
        -> FConn -> Maybe Timeout -> IO (Maybe t, Maybe ThreadId)
l' fun var t' = do 
                 ftp@(FTP (f, b)) <- atomically $ takeTMVar var
                 if b  
                    then 
                       atomically $ putTMVar var ftp >>
                       return (Nothing, Nothing)
                    else                                              
                        do res <-  fun f
                           idT <-  case t' of 
                                     (Just (Time t)) -> fmap Just $ forkFinally (forever $ threadDelay t >> noop) 
                                        $ \_ -> atomically $ void (putTMVar var ftp)                                                               
                                     Nothing -> return Nothing 
                           atomically $ putTMVar var $ FTP (f, True)
                           return (Just res, idT)  
                            
                      where noop = do 
                                       v <- atomically $ tryTakeTMVar var                        
                                       case v  of
                                           Nothing -> return ()
                                           (Just ftp@(FTP (f, _))) ->  do _ <- N.sendcmd f "NOOP"
                                                                          atomically $ putTMVar var ftp  
                                                                     


setPassive :: FConn -> Bool -> IO ()
setPassive var b = do 
                ftp@(FTP (f, b1)) <- atomically $ takeTMVar var
                onException  
                   (return (N.setPassive f b) >>= \f1 -> atomically $ putTMVar var $ FTP (f1, b1))                      
                   $ atomically  (putTMVar var ftp)  
                   
getbinary :: FConn -> String -> IO (String, FTPResult)
getbinary var s = block' var $ \f -> do (l, res) <- N.getbinary f s                 
                                        fmap (\l1 -> (l1,res)) $ mapM return l 
               
getlines :: FConn -> String -> IO ([String], FTPResult)
getlines var s = block' var $ \f -> do (l, res) <- N.getlines f s                 
                                       fmap (\l1 -> (l1,res)) $ mapM return l 
                                       
retrlines :: FConn
                   -> String -> IO ([String], FTPResult)
retrlines var s = block' var $ \f -> do (l, res) <- N.retrlines f s                 
                                        fmap (\l1 -> (l1,res)) $ mapM return l                                                          
                   

nlst, dir :: FConn -> Maybe String -> IO [String]
nlst = d' N.nlst
dir = d' N.dir



d' :: (N.FTPConnection -> t -> IO [b])
          -> FConn -> t -> IO [b]
d' fun var d  = block' var $ \f -> do l <- fun f d                 
                                      mapM return l
                                      
quit :: FConn -> IO FTPResult
quit = s' N.quit

isPassive :: FConn -> IO Bool
isPassive = s' $ return . N.isPassive

s' :: (N.FTPConnection -> IO b) -> FConn -> IO b
s' fun var = block' var fun 

sendcmd, cwd, downloadbinary, delete,rmdir, uploadbinary :: FConn -> String -> IO FTPResult
sendcmd   = s'' N.sendcmd
cwd = s'' N.cwd
downloadbinary = s'' N.downloadbinary
rmdir = s'' N.rmdir
delete = s'' N.delete
uploadbinary = s'' N.uploadbinary

size :: (Num a, Read a) => FConn -> String -> IO a
size = s'' N.size

mkdir :: FConn
               -> String -> IO (Maybe String, FTPResult)
mkdir = s'' N.mkdir

s'' :: (N.FTPConnection -> b1 -> IO b) -> FConn -> b1 -> IO b
s'' fun var str = block' var $ flip fun str

pwd :: FConn -> IO (Maybe String, FTPResult)
pwd  = flip block' N.pwd

rename :: FConn -> String -> String -> IO FTPResult
rename = aux N.rename 

putlines, storlines :: FConn -> String -> [String] -> IO FTPResult
putlines  = aux N.putlines
storlines = aux N.storlines

putbinary :: FConn -> String -> String -> IO FTPResult
putbinary = aux N.putbinary 

aux :: (N.FTPConnection -> t -> t1 -> IO b)
         -> FConn -> t -> t1 -> IO b
aux fun var s1 s2 = block' var (\f -> fun f s1 s2)
                 
block' :: FConn -> (N.FTPConnection -> IO b) -> IO b
block' var action = do 
                ftp@(FTP (f, _)) <- atomically $ takeTMVar var
                finally  
                   (action f)
                   $ atomically $ putTMVar var ftp
                            
-- | For using in getPassword function.          
withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action  

-- | Getting password for more safe login.
  
getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass 
