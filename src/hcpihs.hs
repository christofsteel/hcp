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


type Connections = [Handle]

noConnections :: Connections
noConnections = []

doDebug = True

debug string = case doDebug of
	True -> putStrLn string
	False -> return ()

sendToAll :: Connections -> String -> IO [ThreadId]
sendToAll c m = mapM ((flip send) (Message m)) c
	
register :: String -> String -> IO (Maybe String)
register u p = do
	currUsers <- loadUsers
	saveUsers $ Map.insert u (show $ sha256Ascii $ u++p) currUsers
	return $ Just u

login :: String -> String -> IO (Maybe String)
login u p = do
	currUsers <- loadUsers
	let password = Map.lookup u currUsers
	return $ case password of
		Just aPassword -> if aPassword == (show $ sha256Ascii $ u++p) then Just u else Nothing
		Nothing -> Nothing

chatWith h c = do
	maybeUsername <- getInitialCommunication h login register	
	case maybeUsername of
		Just username -> send h (Message "Willkommen") >>= \_ -> chat username ""
		Nothing -> do
			send h $ Error "Wrong username/password"
			hClose h
	where
		chat username quitreason = do 
			message <- tryIOError $ getCommunication h	
			case message of
				Left a -> do
					atomicModifyIORef c (\con -> (delete h con, ()))
					hClose h
                                        connections <- readIORef c
                                        sendToAll connections $ username ++ " hat den Chat verlassen (" ++ quitreason ++ ")"
					debug "Client Disconnect"
				Right (Message msg) -> do
					connections <- readIORef c
                                        reason <- case msg of
                                           '/':'m':'e':' ':emote -> do
                                             sendToAll connections $ username ++ " " ++ emote 
                                             return ""
                                           "/quit" -> return "Unknown Reason"
                                           '/':'q':'u':'i':'t':' ':qreason -> return qreason
					   otherwise -> do
                                             sendToAll connections $ username ++": " ++ msg
                                             return ""
					debug msg
					chat username reason
				
main = withSocketsDo $ do
	connections <- newIORef noConnections
	socket <- listenOn $ PortNumber 7331
	forkIO $ repeat $ do
		(handle,_,_) <- accept socket
		atomicModifyIORef connections (\con -> (handle:con, ()))
		debug "Client Connect"
		threadId <- forkIO $ chatWith handle connections		
		return ()
	repeat $ do
		servermessage <- getLine
		connections' <- readIORef connections
		sendToAll connections' $ "SERVER: " ++ servermessage
	where
		repeat f = f >>= \_ -> repeat f
