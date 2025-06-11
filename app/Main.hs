import System.Posix.Signals
import System.Environment
import Data.Time.Clock
import Data.Function
import Control.Concurrent.Timer
import Control.Concurrent.Suspend
import Control.Concurrent
import Text.Printf
import Text.Read
import Data.Maybe
import Control.Exception
import Data.Time.Clock.POSIX
import Data.Map (elems, fromListWith, foldrWithKey)

data Command =
    Start String
    | Summary
    | Unknown

data LogEntry =
    CreateLog { logId :: Int
           , logTask :: String
           , logStart :: UTCTime
           , logEnd :: UTCTime
           } deriving Show

data Entry = Entry { entryId :: Int
                   , entryTask :: String
                   , entryStart :: UTCTime
                   , entryEnd :: UTCTime
                   }

main :: IO ()
main = do
    args <- getArgs
    processCommand $ parseCommand args

parseCommand :: [String] -> Command
parseCommand cmd = case cmd of
    ["start", task] -> Start task
    ["summary"] -> Summary
    _ -> Unknown

writeCreateLog :: Int -> String -> UTCTime -> UTCTime -> IO ()
writeCreateLog eid task start end =
    appendFile "time.log"
               (printf "Create %d %s %d %d\n" eid task start' end')
    where start' = round $ utcTimeToPOSIXSeconds start :: Int
          end' = round $ utcTimeToPOSIXSeconds end :: Int

minuteHandler :: UTCTime -> IO ()
minuteHandler start = do
    current <- getCurrentTime
    putStrLn $ show (diffTimeInMinutes current start) ++ ":00"

readLog :: IO [Entry]
readLog = do
    contents <- readFile "time.log"
    let logEntries = fromMaybe [] $ sequence $ map parseLogEntry $ lines contents
        ids = map logId logEntries
    return $ map entryFromLog $ elems $ fromListWith (\e1 _ -> e1) (zip ids logEntries)

entryFromLog :: LogEntry -> Entry
entryFromLog e = Entry {
    entryId = logId e,
    entryTask = logTask e,
    entryStart = logStart e,
    entryEnd = logEnd e
}

parseLogEntry :: String -> Maybe LogEntry
parseLogEntry line = case words line of
    ["Create", id', task, start, end] -> do
        id'' <- readMaybe id'
        start' <- readMaybe start
        end' <- readMaybe end
        return CreateLog {
            logId = id'',
            logTask = task,
            logStart = posixSecondsToUTCTime $ fromIntegral (start' :: Int),
            logEnd = posixSecondsToUTCTime $ fromIntegral (end' :: Int)
        }
    _ -> Nothing


getNextID :: IO Int
getNextID = do
    entries <- readLog
    let maxId = max0 $ map entryId $ entries
    _ <- evaluate maxId
    return (maxId + 1)
    where max0 [] = 0
          max0 xs = maximum xs

processCommand :: Command -> IO ()
processCommand (Start task) = do
    stopChan <- newChan
    start <- getCurrentTime
    _ <- repeatedTimer (minuteHandler start) (mDelay 1)
    _ <- installHandler sigINT (Catch $ writeChan stopChan ()) Nothing
    _ <- readChan stopChan
    end <- getCurrentTime
    putStrLn ("\nspent: " ++ show (diffTimeInMinutes end start) ++ " minutes")
    nextId <- getNextID
    writeCreateLog nextId task start end

processCommand Summary = do
    return ()
    entries <- readLog
    let zipped = zip (map entryTask entries) (map duration entries)
    let entryMap = fromListWith (+) zipped
    _ <- sequence $ foldrWithKey (\task dur l -> (putStrLn $ printf "%s: %d" task dur) : l) [] entryMap
    return ()

processCommand Unknown = putStrLn "Usage:\n\n\
      \  timer-hs start <task>\
      \  timer-hs summary"

diffTimeInMinutes :: UTCTime -> UTCTime -> Int
diffTimeInMinutes t1 t2 =
   diffUTCTime t1 t2
   & nominalDiffTimeToSeconds
   & (/ 60)
   & round

duration :: Entry -> Int
duration e = diffUTCTime (entryEnd e) (entryStart e)
    & nominalDiffTimeToSeconds
    & round