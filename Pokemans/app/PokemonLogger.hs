module PokemonLogger where

import System.IO
import Data.Time.Clock
import Data.Time.Format


data LogType = INFO | WARNING | ERROR
    deriving (Show)


logFileName :: FilePath
logFileName = "pokemonLog.txt"


clearLogs :: IO ()
clearLogs = writeFile logFileName ""


log :: Show s => LogType -> s -> IO ()
log logType content = do
    logString logType $ show content


logString :: LogType -> String -> IO ()
logString logType content = do
    time <- prettyTime
    let details = "[" ++ show logType ++ "] " ++ time ++ " " ++ content
    withFile logFileName AppendMode (`hPutStrLn` details)


test :: IO String -> IO String
test iostr = (++ "hi") <$> iostr


prettyTime :: IO String
prettyTime = formatTime defaultTimeLocale "%m/%d/%Y %I:%M %p" <$> getCurrentTime
