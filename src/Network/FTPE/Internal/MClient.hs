module Network.FTPE.Internal.MClient 
(
   sendcmdM, cwdM, quitM, runStateT, StateT, liftIO   
 )
           
where
import Network.FTPE.Internal.FClient

import qualified Network.FTP.Client  as N 

import Network.FTP.Client.Parser (FTPResult)


import Control.Concurrent.STM.TMVar

import Control.Monad.State.Strict (runStateT, StateT, liftIO, get)


type FTPM   = StateT (TMVar FConnection) IO  
         



quitM :: FTPM FTPResult
quitM = monblock' $ flip block' N.quit

sendcmdM, cwdM:: String -> FTPM FTPResult                  
sendcmdM s = monblock' $ flip sendcmd s
cwdM s = monblock' $ flip cwd s


monblock' :: 
               (TMVar FConnection -> IO b) -> FTPM b
monblock' fun = do s <- get
                   liftIO $ fun s




