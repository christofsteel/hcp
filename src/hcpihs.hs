import Network
import Control.Concurrent
import Data.Maybe
import Data.Digest.SHA2
import Data.IORef
import System.IO
import System.IO.Error
import qualified Data.Map as Map
import Data.List
import Network.HCPIH.Tainted
import Network.HCPIH.Conf

type Connections = Map.Map String Handle -- Zuordnung Username -> Handles

noConnections :: Connections
noConnections = Map.empty

doDebug = True 

debug string = case doDebug of   -- nur Ausgabe, falls doDebug = True
  True -> putStrLn string
  False -> return ()

sendToAll :: Connections -> Communication -> IO [ThreadId]
sendToAll c m = mapM (\(user, handle) -> send handle m) $ Map.toList c -- iteriert über alle Aktiven Connections und sendet die Communication m an jeden einzelnen
	
register :: String -> String -> IO (Maybe String)
register u p = do
  currUsers <- loadUsers
  let user = Map.lookup u currUsers -- guck, ob User vorhanden ist
  case user of
    Nothing -> do 
      saveUsers $ Map.insert u (show $ sha256Ascii $ u++p) currUsers -- Falls nein, füge ihn hinzu mit gesalzenem Passwort
      return $ Just u
    Just a -> return Nothing -- Falls ja, gib nichts zurück

login :: String -> String -> IO (Maybe String)
login u p = do
  currUsers <- loadUsers
  let password = Map.lookup u currUsers -- guck, ob user existiert
  return $ case password of
    Just aPassword -> if aPassword == (show $ sha256Ascii $ u++p) then Just u else Nothing -- falls Ja, überprüfe das Passwort und gib den Usernamen zurück
    Nothing -> Nothing -- ansonsten nicht

chatWith :: Handle -> IORef Connections -> IO ()
chatWith h c = do
  initial <- tryIOError $ getCommunication h  -- versuche login oder register zu bekommen
  maybeUsername <- case initial of
    Left a -> return Nothing  -- bei IO Fehler beende Thread
    Right(Login u p) -> login u p
    Right(Register u p) -> register u p
    otherwise -> return Nothing -- Falls nicht login, oder register, beende Thread
  case maybeUsername of
    Just username -> do  -- Falls login/register erfolgreich
      send h $ Message "Willkommen"
      atomicModifyIORef c (\con -> (Map.insert username h con, ())) -- Update die globale connections
      connections <- readIORef c
      sendToAll connections $ Login ( username ++ " logged in") "" -- Sende an alle "username logged in"
      debug $ username ++ " logged in"
      chat username -- starte chat loop
    Nothing -> do -- falls nicht erfolgreich
      case initial of -- gib entsprechende Fehlermeldung
        Right(Login u p) -> send h $ Error "Wrong username/password"
        Right(Register u p) -> send h $ Error $ "User " ++ u ++ " already exists"
        otherwise -> send h $ Error "Not logged in"
      hClose h -- und beende die Verbindunf
  where
    chat username = do -- chat loop
      message <- tryIOError $ getCommunication h 
      connections <- readIORef c
      case message of
        Left a -> do
          atomicModifyIORef c (\con -> (Map.delete username con, ()))
          hClose h
          newConnections <- readIORef c
          sendToAll newConnections $ Logout $ "User " ++ username ++ " disconnected"
          debug $ "User " ++ username ++" disconnected"
        Right (Message msg) -> do
          sendToAll connections $ Message $ username ++": " ++ msg
          debug $ username ++": " ++ msg
          chat username
        Right (Logout reason) -> do
          atomicModifyIORef c (\con -> (Map.delete username con, ()))
          hClose h
          newConnections <- readIORef c
          sendToAll newConnections $ Logout $ "User " ++ username ++ " logged out" ++ if reason == "" then " for unknown reason" else ", "++ reason
          debug $ "User " ++ username ++ " logged out" ++ if reason == "" then " for unknown reason" else ", "++ reason
        Right (DM user message) -> do
          let target = Map.lookup user connections
          curruser <- loadUsers
          let exists = isJust $ Map.lookup user curruser
          case target of
            Just targetuser -> send targetuser $ DM username message
            Nothing -> send h $ Error $ if exists 
              then "User " ++ user ++ " is currently offline. Try again later!"
              else "User " ++ user ++ " not Found"
          chat username
        Right (Emote emote) -> do
          sendToAll connections $ Emote $ username ++ " " ++ emote
          debug $ username ++ " " ++ emote
          chat username	
	Right (Undefined) -> do
	  send h $ Error "Unknown Command"
	  debug $ "(" ++ username ++ ") Unknown Command"
	  chat username

main = withSocketsDo $ do
  connections <- newIORef noConnections
  socket <- listenOn $ PortNumber 7331
  forkIO $ repeat $ do
    (handle,_,_) <- accept socket
    debug "Client Connect"
    threadId <- forkIO $ chatWith handle connections		
    return ()
  repeat $ do
    servermessage <- getLine
    connections' <- readIORef connections
    sendToAll connections' $ Message $ "SERVER: " ++ servermessage
  where
    repeat f = f >>= \_ -> repeat f
