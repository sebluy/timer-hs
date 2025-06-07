import System.IO
import Data.IORef (IORef, writeIORef, newIORef, readIORef)
import Data.Time.Clock
import Data.Function
import Control.Concurrent.Timer
import Control.Concurrent.Suspend

data Command =
    Start String
    | Stop
    | Unknown

data Status =
    Running String UTCTime TimerIO
    | Stopped

data AppState = AppState { status :: Status }

main :: IO ()
main = do
    stateRef <- newIORef AppState {status=Stopped}
    runLoop stateRef

runLoop :: IORef AppState -> IO ()
runLoop stateRef = do
    command <- prompt ">>> "
    let cmd = parseCommand command
    processCommand cmd stateRef
    print cmd
    readIORef stateRef >>= print
    runLoop stateRef

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

parseCommand :: String -> Command
parseCommand cmd = case words cmd of
    ["start", task] -> Start task
    ["stop"] -> Stop
    _ -> Unknown

instance Show Command where
    show (Start task) = "starting " ++ task
    show Stop = "stopping"
    show Unknown = "unknown command"

instance Show Status where
    show (Running task time _) = "running " ++ task ++ " " ++ show time
    show Stopped = "stopped"

instance Show AppState where
    show = show . status

processCommand :: Command -> IORef AppState -> IO ()
processCommand (Start task) stateRef = do
    time <- getCurrentTime
    timer <- repeatedTimer (putStrLn "time") (sDelay 1)
    writeIORef stateRef AppState {status=Running task time timer}
processCommand Stop stateRef = do
    state <- readIORef stateRef
    case status state of
        Running _ time timer -> do
            stopTimer timer
            writeIORef stateRef AppState {status=Stopped}
            currentTime <- getCurrentTime
            putStrLn ("spent: " ++ show (diffTimeInMinutes currentTime time) ++ " minutes")
        Stopped -> return ()
processCommand Unknown _ = return ()


diffTimeInMinutes :: UTCTime -> UTCTime -> Int
diffTimeInMinutes t1 t2 =
   diffUTCTime t1 t2
   & nominalDiffTimeToSeconds
   & (/ 60)
   & round
