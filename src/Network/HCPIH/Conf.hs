module Network.HCPIH.Conf (
  writeConf,
  readConf,
  Users
) where

import System.IO
import System.IO.Error
import qualified Data.Map as Map
import Data.Maybe

type Users = Map.Map String String -- Username -> SHA1(Username ++ Password)


writeConf :: String -> Users -> IO ()
writeConf filename conf = do
                error <- tryIOError $ writeFile filename $ show conf
                case error of
                    Left _ -> putStrLn "Irgendwas ist beim schreiben schief gegangen.... egal"
                    Right _ -> return ()

readConf :: String -> IO Users
readConf filename = do
    let readThisFile s h = do
        eof <- hIsEOF h
        if eof then return s else do
            content <- hGetLine h
            return $ s ++ "\n" ++ content
    content <- tryIOError $ withFile filename ReadMode $ readThisFile ""
    return $ case content of
        Left _ -> Map.empty
        Right conf -> fst $ head $reads conf

