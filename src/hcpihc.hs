import System.IO
import Network
import Control.Concurrent
import Control.Exception
import Network.HCPIH.Tainted
import System.Environment
import System.IO.Error

port = 7331

getHostName = do
  putStr "Server: "
  getLine

main = withSocketsDo $ do
   hSetBuffering stdout NoBuffering
   args <- getArgs 
   server <- if length args > 0
     then return $ args !! 0
     else getHostName
   tryHandle <- tryIOError $ connectTo server $ PortNumber port
   case tryHandle of
     Left a -> putStrLn "Error on connect"
     Right handle -> do
       putStrLn "Welcome to the Hauer Chat Protocol In Haskell Client"
       putStrLn "/login <username> to login with username"
       putStrLn "/register <username> to register with the Server"
       forkIO $ printChats handle
       command <- getLine
       case break (==' ') command of
         ("/login",x:username) -> do
           putStr "Password: "
           password <- withEcho False getLine
           send handle $ Login username password
	   chat handle
         ("/register",x:username) -> do
	   putStr "Password: "
	   password1 <- withEcho False getLine
	   putStrLn " "
	   putStr "Repeat password: "
	   password2 <- withEcho False getLine
	   putStrLn " "
	   case password1 == password2 of
	     True -> do
	       send handle $ Register username password1
	       chat handle
	     False -> putStrLn "Passwords don't match"
	 _ -> putStrLn "Command not recognized"

printChats h = do
  communication <- tryIOError $ getCommunication h
  putStrLn $ show communication
  case communication of
    Left a -> putStrLn "Disconnected"
    Right (Error error) -> do
      putStrLn $ "! " ++ error
      printChats h
    Right (Message msg) -> do
      putStrLn msg
      printChats h
    Right (Login msg _) -> do
      putStrLn $ "> " ++ msg
      printChats h
    Right (Logout msg) -> do
      putStrLn $ "< " ++ msg
      printChats h
    Right (DM user msg) -> do
      putStrLn $ "[" ++ user ++ "] " ++ msg
      printChats h
    Right (Emote msg) -> do
      putStrLn $ "* " ++ msg
      printChats h
    _ -> do
      putStrLn "Seltsam"
      printChats h


chat :: Handle -> IO ()
chat handle = do
  repeat $ do
    line <- getLine
    case break (==' ') line of
      ("/err",x:errormsg) -> send handle $ Error errormsg
      ("/dm",x:usernamemessage) -> let (username, message) = break (==' ') usernamemessage in send handle $ DM username message
      ("/me",x:emote) -> send handle $ Emote emote
      ("/logout",x:reason) -> send handle $ Logout reason
      _ -> send handle $ Message line
  where
    repeat f = f >>= \_ -> repeat f

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
