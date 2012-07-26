import System.IO
import Network
import Control.Concurrent
import Control.Exception
import Network.HCPIH.Tainted
import System.Environment
import System.IO.Error

port = 7331

getLineWithPromptIfEmpty :: String -> Bool -> String -> IO String
getLineWithPromptIfEmpty prompt nl string = if string == ""
  then getLineWithPrompt prompt nl
  else return string

getLineWithPrompt :: String -> Bool -> IO String
getLineWithPrompt prompt nl = do
  putStr prompt
  line <- getLine
  if nl
    then putStrLn ""
    else return ()
  return line

safetail :: String -> String
safetail (x:xs) = xs
safetail xs = xs

main = withSocketsDo $ do
   hSetBuffering stdout NoBuffering
   args <- getArgs 
   server <- if length args > 0
     then return $ args !! 0
     else getLineWithPrompt "Server: " False
   tryHandle <- tryIOError $ connectTo server $ PortNumber port
   case tryHandle of
     Left a -> putStrLn "Error on connect"
     Right handle -> do
       putStrLn "Welcome to the Hauer Chat Protocol In Haskell Client"
       putStrLn "/login <username> to login with username"
       putStrLn "/register <username> to register with the Server"
       command <- getLine
       case break (==' ') command of
         ("/login",username) -> do
           username_ <- getLineWithPromptIfEmpty "Username: " False $ safetail username
           password <- withEcho False $ getLineWithPrompt "Password: " True
           send handle $ Login username_ password
           forkIO $ printChats handle
	   chat handle
         ("/register",username) -> do
           username_ <- getLineWithPromptIfEmpty "Username: " False $ safetail username
	   password1 <- withEcho False $ getLineWithPrompt "Password: " True
	   password2 <- withEcho False $ getLineWithPrompt "Repeat Password: " True
	   case password1 == password2 of
	     True -> do
	       send handle $ Register username_ password1
               forkIO $ printChats handle
	       chat handle
	     False -> putStrLn "Passwords don't match"
	 _ -> putStrLn "Command not recognized"

printChats h = do
  hFlush stdout
  communication <- tryIOError $ getCommunication h
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
      printChats h

chat :: Handle -> IO ()
chat handle = do
  line <- getLine
  case break (==' ') line of
    ("/err",x:errormsg) -> do
      send handle $ Error $ safetail errormsg 
      chat handle
    ("/dm",x:usernamemessage) -> do
      let (username, message) = break (==' ') usernamemessage
      send handle $ DM username $ safetail message
      chat handle
    ("/me",emote) -> do
      send handle $ Emote $ safetail emote
      chat handle
    ("/logout",reason) -> do
      send handle $ Logout $ safetail reason
      return ()
    _ -> do 
      send handle $ Message line
      chat handle

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
