module Network.HCPIH.Tainted (
send,
getCommunication,
Communication (Message, Error, Register, Login, Logout, DM, Emote, Undefined)
) where

import System.IO
import Control.Concurrent
import Unsafe.Coerce

data Communication = Message String | Error String | Register String String | Login String String | Logout String | DM String String | Emote String | Undefined deriving Show

sendStr :: Communication -> String
sendStr (Error s)  = "\0" ++ s ++ "\0"
sendStr (Message s)  = "\1" ++ s ++ "\0"
sendStr (Login u p)  = "\2" ++ u ++ "\0" ++ p ++ "\0"
sendStr (Logout r)  = "\3" ++ r ++ "\0"
sendStr (Register u p)  = "\4" ++ u ++ "\0" ++ p ++ "\0"
sendStr (DM u s)  = "\5" ++ u ++ "\0" ++ s ++ "\0"
sendStr (Emote s) = "\6" ++ s ++ "\0"

send :: Handle -> Communication -> IO ThreadId
send h m = forkIO $ do
  hPutStr h $ sendStr m
  hFlush h

getCommunication :: Handle -> IO Communication
getCommunication h = do
  string <- hGetToNull h
  case string of
    '\0':message -> return $ Error message
    '\1':message -> return $ Message message
    '\2':username -> do
      password <- hGetToNull_ h
      return $ Login username password
    '\3':reason -> return $ Logout reason
    '\4':username -> do
      password <- hGetToNull_ h
      return $ Register username password
    '\5':username -> do
      message <- hGetToNull_ h
      return $ DM username message
    '\6':emote -> return $ Emote emote
    _ -> return Undefined

hGetToNull_ :: Handle -> IO String
hGetToNull_ h = do
--  char <- hGetCihar h
  string <- repeatedGet ""
  return $ string
  where 
    repeatedGet xs = do
      char <- hGetChar h
      case char of
        '\0' -> return xs
        a -> repeatedGet $ xs++[a]

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

