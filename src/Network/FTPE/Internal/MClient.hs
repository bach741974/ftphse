module Network.FTPE.Internal.MClient 
(
   sendcmdM, cwdM, quitM, runStateT, StateT, liftIO
   , isPassiveM, dirM, nlstM, getbinaryM, getlinesM
   , setPassiveM, loginM  
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
sendcmdM  = aux sendcmd
cwdM  = aux cwd 

dirM, nlstM :: Maybe String -> FTPM [String]
dirM = aux dir
nlstM= aux nlst

getlinesM :: String -> FTPM ([String], FTPResult)
getlinesM= aux getlines

getbinaryM :: String -> FTPM (String, FTPResult)
getbinaryM = aux getbinary

setPassiveM :: Bool -> FTPM ()
setPassiveM = aux setPassive

loginM :: Maybe Timeout
                -> String
                -> Maybe String
                -> Maybe String
                -> FTPM (Maybe FTPResult, Maybe ThreadId)
loginM t name p acc = do s <- get
                         liftIO $ login s t name p acc

aux :: (TMVar FConnection -> b1 -> IO b) -> b1 -> FTPM b
aux f t = monblock' $ flip f t

isPassiveM :: FTPM Bool
isPassiveM= monblock' isPassive

monblock' :: 
               (TMVar FConnection -> IO b) -> FTPM b
monblock' fun = do s <- get
                   liftIO $ fun s




