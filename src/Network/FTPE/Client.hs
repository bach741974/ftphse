{-# LANGUAGE FlexibleContexts #-}
module Network.FTPE.Client 
(
 -- * FTP commands 
  
   setLogLevel, Priority(..), easyConnectFTP, getPassword, connectFTP, FConnection, login,
   Timeout (Time), dir, quit, sendcmd, cwd, nlst, loginAnon
   , module Control.Monad.State.Strict
   , sendcmdM, cwdM
 )
           
where
import System.Log.Logger (Priority(..), updateGlobalLogger, setLevel)
import qualified Network.FTP.Client  as N 
import Network.FTP.Client (getPassword)
import Network.FTP.Client.Parser (FTPResult)
import Network.Socket(PortNumber, HostName)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Concurrent (forkFinally, threadDelay)
import GHC.Conc.Sync (ThreadId)
--import Control.Monad (forever)
import Control.Monad.State.Strict
import Control.Exception.Base (finally)

newtype FConnection = FTP (N.FTPConnection, Bool)
newtype Timeout = Time Int

type FTPM   = StateT (TMVar FConnection) IO  
         
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
                                     (Just (Time t)) -> fmap Just $ forkFinally (forever $ threadDelay t >> noop) $ \_ -> return ()                                                               
                                     Nothing -> return Nothing 
                           atomically $ putTMVar var $ FTP (f, True)
                           return (Just res, idT)  
                            
                      where noop = do 
                                       v <- atomically $ tryTakeTMVar var                        
                                       case v  of
                                           Nothing -> return ()
                                           (Just ftp@(FTP (f, _))) ->  do _ <- N.sendcmd f "NOOP"
                                                                          atomically $ putTMVar var ftp  
                                                                     

nlst, dir :: TMVar FConnection -> Maybe String -> IO [String]
nlst = d' N.nlst
dir = d' N.dir

d' :: (N.FTPConnection -> t -> IO [b])
          -> TMVar FConnection -> t -> IO [b]
d' fun var d  = block' var $ \f -> do l <- fun f d                 
                                      mapM return l
                                      
quit :: TMVar FConnection -> IO FTPResult
quit = s' N.quit

s' :: (N.FTPConnection -> IO b) -> TMVar FConnection -> IO b
s' fun var = block' var fun 

sendcmd, cwd :: TMVar FConnection -> String -> IO FTPResult
sendcmd   = s'' N.sendcmd
cwd = s'' N.cwd

s'' :: (N.FTPConnection -> b1 -> IO b) -> TMVar FConnection -> b1 -> IO b
s'' fun var str = block' var $ flip fun str


sendcmdM, cwdM:: String -> FTPM FTPResult                  
sendcmdM = sM'' N.sendcmd
cwdM = sM'' N.cwd
 
sM'' :: (MonadIO m, MonadState (TMVar FConnection) m) =>
          (N.FTPConnection -> b1 -> IO b) -> b1 -> m b
sM'' fun str = monblock' (\s ->  block' s  $ flip fun str)

monblock' :: (MonadIO m, MonadState t m) =>
               (t -> IO b) -> m b
monblock' fun = do s <- get
                   liftIO $ fun s
                 
block' :: TMVar FConnection -> (N.FTPConnection -> IO b) -> IO b
block' var action = do 
                ftp@(FTP (f, _)) <- atomically $ takeTMVar var
                finally  
                   (action f)
                   $ atomically $ putTMVar var ftp
                           

