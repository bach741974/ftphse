
module Network.FTPE.Internal.FClient 
(
 -- * FTP commands 
  
   setLogLevel, Priority(..), easyConnectFTP, getPassword, connectFTP, login,
   Timeout (Time), dir, quit, sendcmd, cwd, nlst, loginAnon, FConnection(FTP) 
   ,  block', setPassive, isPassive, N.enableFTPDebugging, getlines, getbinary
   , ThreadId, downloadbinary, delete, size, rmdir, mkdir, pwd, rename, putlines
   , putbinary 
   
 ) 
           
where
import System.Log.Logger (Priority(..), updateGlobalLogger, setLevel)
import qualified Network.FTP.Client  as N 

import Network.FTP.Client.Parser (FTPResult)
import Network.Socket(PortNumber, HostName)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Concurrent (forkFinally, threadDelay)
import GHC.Conc.Sync (ThreadId)
--import Control.Monad (forever)
import Control.Monad.State.Strict (forever)
import Control.Exception.Base (finally, onException)
import System.IO (stdin, hGetEcho, hSetEcho, stdout, hFlush)
import Control.Exception (bracket_)
import Control.Monad (void)

newtype FConnection = FTP (N.FTPConnection, Bool)
newtype Timeout = Time Int

 
         
setLogLevel :: Priority -> IO ()
setLogLevel lev = do
                        updateGlobalLogger "Network.FTP.Client.Parser" level
                        updateGlobalLogger "Network.FTP.Client" level
                    where  level = setLevel lev 
                    

             
                        
easyConnectFTP :: HostName -> IO (TMVar FConnection)
easyConnectFTP h = do 
                        con <-  N.easyConnectFTP h
                        atomically $ newTMVar $ FTP (con, False)
                


connectFTP :: HostName
                -> PortNumber -> IO (TMVar FConnection, FTPResult)
connectFTP h p = do 
                     (ftp, res) <-  N.connectFTP h p 
                     v <- atomically $ newTMVar $ FTP (ftp, False)
                     return (v, res)                  
                                                              
loginAnon :: TMVar FConnection
                -> Maybe Timeout -> IO (Maybe FTPResult, Maybe ThreadId)
loginAnon = l' N.loginAnon

login :: TMVar FConnection
            -> Maybe Timeout
            -> String
            -> Maybe String
            -> Maybe String
            -> IO (Maybe FTPResult, Maybe ThreadId)
login var t name p acc = l' (\f ->  N.login f name p acc) var t

l' :: (N.FTPConnection -> IO t)
        -> TMVar FConnection -> Maybe Timeout -> IO (Maybe t, Maybe ThreadId)
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
                                                                     


setPassive :: TMVar FConnection -> Bool -> IO ()
setPassive var b = do 
                ftp@(FTP (f, b1)) <- atomically $ takeTMVar var
                onException  
                   (return (N.setPassive f b) >>= \f1 -> atomically $ putTMVar var $ FTP (f1, b1))                      
                   $ atomically  (putTMVar var ftp)  
                   
getbinary :: TMVar FConnection -> String -> IO (String, FTPResult)
getbinary var s = block' var $ \f -> do (l, res) <- N.getbinary f s                 
                                        fmap (\l1 -> (l1,res)) $ mapM return l 
               
getlines :: TMVar FConnection -> String -> IO ([String], FTPResult)
getlines var s = block' var $ \f -> do (l, res) <- N.getlines f s                 
                                       fmap (\l1 -> (l1,res)) $ mapM return l                    
                   

nlst, dir :: TMVar FConnection -> Maybe String -> IO [String]
nlst = d' N.nlst
dir = d' N.dir



d' :: (N.FTPConnection -> t -> IO [b])
          -> TMVar FConnection -> t -> IO [b]
d' fun var d  = block' var $ \f -> do l <- fun f d                 
                                      mapM return l
                                      
quit :: TMVar FConnection -> IO FTPResult
quit = s' N.quit

isPassive :: TMVar FConnection -> IO Bool
isPassive = s' $ return . N.isPassive

s' :: (N.FTPConnection -> IO b) -> TMVar FConnection -> IO b
s' fun var = block' var fun 

sendcmd, cwd, downloadbinary, delete,rmdir :: TMVar FConnection -> String -> IO FTPResult
sendcmd   = s'' N.sendcmd
cwd = s'' N.cwd
downloadbinary = s'' N.downloadbinary
rmdir = s'' N.rmdir
delete = s'' N.delete

size :: (Num a, Read a) => TMVar FConnection -> String -> IO a
size = s'' N.size

mkdir :: TMVar FConnection
               -> String -> IO (Maybe String, FTPResult)
mkdir = s'' N.mkdir

s'' :: (N.FTPConnection -> b1 -> IO b) -> TMVar FConnection -> b1 -> IO b
s'' fun var str = block' var $ flip fun str

pwd :: TMVar FConnection -> IO (Maybe String, FTPResult)
pwd  = flip block' N.pwd

rename :: TMVar FConnection -> String -> String -> IO FTPResult
rename = aux N.rename 

putlines :: TMVar FConnection -> String -> [String] -> IO FTPResult
putlines  = aux N.putlines

putbinary :: TMVar FConnection -> String -> String -> IO FTPResult
putbinary = aux N.putbinary 

aux :: (N.FTPConnection -> t -> t1 -> IO b)
         -> TMVar FConnection -> t -> t1 -> IO b
aux fun var s1 s2 = block' var (\f -> fun f s1 s2)
                 
block' :: TMVar FConnection -> (N.FTPConnection -> IO b) -> IO b
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
