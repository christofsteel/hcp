module Network.HCPIH.Tainted (
send,
getCommunication,
getInitialCommunication,
Communication (Message, Error, Register, Login)
) where

import System.IO
import Control.Concurrent

data Communication = Message String | Error String | Register String String | Login String String

sendStr :: Communication -> String
sendStr (Message s)  = "\1" ++ s ++ "\0"
sendStr (Error s)  = "\0" ++ s ++ "\0"
sendStr (Register u p)  = "\3" ++ u ++ "\0" ++ p ++ "\0"
sendStr (Login u p)  = "\2" ++ u ++ "\0" ++ p ++ "\0"

send :: Handle -> Communication -> IO ThreadId
send h m = forkIO $ do
  hPutStrLn h $ sendStr m
  hFlush h

getCommunication :: Handle -> IO Communication
getCommunication h = do
  string <- hGetToNull h
  case string of
    '\1':message -> return $ Message message
    '\0':message -> return $ Error message

getInitialCommunication :: Handle -> (String -> String -> IO (Maybe a)) -> (String -> String -> IO (Maybe a)) -> IO (Maybe a)
getInitialCommunication h flogin fregister = do
  lusername <- hGetToNull h
  password <- hGetToNull h
  case lusername of
    '\3':username -> fregister username password
    '\2':username -> flogin username password

hGetToNull :: Handle -> IO String
hGetToNull h = do
  char <- hGetChar h
  string <- repeatedGet ""
  return $ char:string
  where 
    repeatedGet xs = do
      char <- hGetChar h
      case char of
        '\0' -> return xs
        a -> repeatedGet $ xs++[a]

