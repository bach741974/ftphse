module Network.FTPE.Internal.MClient 
(
   sendcmdM, cwdM, quitM, runStateT, StateT, liftIO   
 )
           
where
import Network.FTPE.Internal.FClient

import qualified Network.FTP.Client  as N 

import Network.FTP.Client.Parser (FTPResult)

--import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TMVar
--import Control.Concurrent (forkFinally, threadDelay)
-- import GHC.Conc.Sync (ThreadId)
--import Control.Monad (forever)
import Control.Monad.State.Strict (runStateT, StateT, liftIO, get)


type FTPM   = StateT (TMVar FConnection) IO  
         



quitM :: FTPM FTPResult
quitM = monblock' $ flip block' N.quit

sendcmdM, cwdM:: String -> FTPM FTPResult                  
sendcmdM = sM'' N.sendcmd
cwdM = sM'' N.cwd
 
--sM'' :: (MonadIO m, MonadState (TMVar FConnection) m) =>
--          (N.FTPConnection -> b1 -> IO b) -> b1 -> m b
sM'' ::(N.FTPConnection -> b1 -> IO b) -> b1 -> FTPM b
sM'' fun str = monblock' (\s ->  block' s  $ flip fun str)

monblock' :: 
               (TMVar FConnection -> IO b) -> FTPM b
monblock' fun = do s <- get
                   liftIO $ fun s




