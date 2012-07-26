import System.IO
import Network
import Network.HCPIH.Tainted
import System.Environment

printStream handle = do
  char <- hGetChar handle
  putChar char
  hFlush stdout
  printStream handle

main = do
  args <- getArgs
  if length args >= 3
    then do
      handle <- connectTo (args !! 0) $ PortNumber 7331
      send handle $ Login (args !! 1) (args !! 2)
      printStream handle
    else putStrLn "Usage: hcpraw <server> <username>"
