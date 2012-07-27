{-# LANGUAGE DeriveDataTypeable #-}
import System.IO
import Network
import Control.Concurrent
import Control.Exception
import Network.HCPIH.Tainted
import System.Environment
import System.IO.Error
import System.Console.CmdArgs.Implicit

data Options = Options
   {port :: Integer
   ,user :: String
   ,register :: Bool
   ,host :: String} deriving (Show, Data, Typeable)

getOptions = Options
  { port = 7331 &= help "Port" &= typ "PORT"
  , user = def &= help "Username" &= typ "STRING"
  , register = def &= help "Register a new user"
  , host = def &= typ "Server" &= args} &= program "hcpihc" &= summary "Hauer Chat Protocol In Haskell Client v0.0.1"


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

main = withSocketsDo $ do
   hSetBuffering stdout NoBuffering
   args <- cmdArgs getOptions
   server <- if host args /= ""
     then return $ host args
     else getLineWithPrompt "Server: " False
   tryHandle <- tryIOError $ connectTo server $ PortNumber $ fromInteger $ port args
   case tryHandle of
     Left a -> putStrLn "Error on connect"
     Right handle -> do
       putStrLn "Welcome to the Hauer Chat Protocol In Haskell Client"
       command <- if register args
         then return ("/register",user args)
         else if user args /= ""
           then return ("/login", user args)
           else do           
             putStrLn "/login <username> to login with username"
             putStrLn "/register <username> to register with the Server"
             line <- getLine
             return $ break (==' ') line
       case command of
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
    Right (QueryOnline users) -> do
      putStrLn $ ": " ++ (init $ init $ concatMap (\user -> user ++ ", ") users)
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
    ("/who",_) -> do
      send handle $ QueryOnline []
      chat handle
    _ -> do 
      send handle $ Message line
      chat handle

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
