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


type Connections = Map.Map String Handle

noConnections :: Connections
noConnections = Map.empty

doDebug = True

debug string = case doDebug of
	True -> putStrLn string
	False -> return ()

sendToAll :: Connections -> Communication -> IO [ThreadId]
sendToAll c m = mapM (\(user, handle) -> send handle m) $ Map.toList c
	
register :: String -> String -> IO (Maybe String)
register u p = do
	currUsers <- loadUsers
        let user = Map.lookup u currUsers
        case user of
            Nothing -> do
              saveUsers $ Map.insert u (show $ sha256Ascii $ u++p) currUsers
	      return $ Just u
            Just a -> return Nothing

login :: String -> String -> IO (Maybe String)
login u p = do
	currUsers <- loadUsers
	let password = Map.lookup u currUsers
	return $ case password of
		Just aPassword -> if aPassword == (show $ sha256Ascii $ u++p) then Just u else Nothing
		Nothing -> Nothing

chatWith :: Handle -> IORef Connections -> IO ()
chatWith h c = do
        initial <- getCommunication h
        maybeUsername <- case initial of
          Login u p -> login u p
          Register u p -> register u p
	case maybeUsername of
		Just username -> do
                        send h (Message "Willkommen") 
		        atomicModifyIORef c (\con -> (Map.insert username h con, ()))
                        connections <- readIORef c
                        sendToAll connections $ Login ( username ++ " logged in") "" 
                        debug $ username ++ " logged in"
                        chat username
		Nothing -> do
                        case initial of
                            Login u p -> send h $ Error "Wrong username/password"
                            Register u p -> send h $ Error $ "User " ++ u ++ " already exists"
                        hClose h
	where
		chat username = do 
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
