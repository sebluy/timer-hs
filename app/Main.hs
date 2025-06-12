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
import Data.Map (elems, fromListWith, mapWithKey, empty, insert, delete)
import Data.Time.Format
import Data.Time.LocalTime

data Command =
    Start String
    | Delete Int
    | List
    | Summary
    | Unknown

data LogEntry =
    CreateLog { logId :: Int
              , logTask :: String
              , logStart :: LocalTime
              , logEnd :: LocalTime
              }
    | DeleteLog { logId :: Int } deriving Show

data Entry = Entry { entryId :: Int
                   , entryTask :: String
                   , entryStart :: LocalTime
                   , entryEnd :: LocalTime
                   }

main :: IO ()
main = do
    args <- getArgs
    processCommand $ parseCommand args

parseCommand :: [String] -> Command
parseCommand cmd = case cmd of
    ["start", task] -> Start task
    ["summary"] -> Summary
    ["list"] -> List
    ["delete", eid] -> maybe Unknown Delete $ readMaybe eid
    _ -> Unknown

writeCreateLog :: Int -> String -> LocalTime -> LocalTime -> IO ()
writeCreateLog eid task start end = do
    tz <- getCurrentTimeZone
    let start' = localToTimestamp start tz
        end' = localToTimestamp end tz
    appendFile "time.log" $ printf "Create %d %s %d %d\n" eid task start' end'

writeDeleteLog :: Int -> IO ()
writeDeleteLog eid = appendFile "time.log" $ printf "Delete %d\n" eid

minuteHandler :: LocalTime -> IO ()
minuteHandler start = do
    current <- getLocalTime
    putStrLn $ show (diffTimeInMinutes current start) ++ ":00"

getLocalTime :: IO LocalTime
getLocalTime = do
    zoned <- getZonedTime
    return $ zonedTimeToLocalTime zoned

readLog :: IO [Entry]
readLog = do
    contents <- readFile "time.log"
    tz <- getCurrentTimeZone
    let logEntries = fromMaybe [] $ sequence $ map (parseLogEntry tz) $ lines contents
    return $ replayLog logEntries

replayLog :: [LogEntry] -> [Entry]
replayLog entryLog = elems $ foldr combine empty $ reverse entryLog
    where combine logItem@(CreateLog eid _ _ _) = insert eid (entryFromLog logItem)
          combine (DeleteLog eid) = delete eid

entryFromLog :: LogEntry -> Entry
entryFromLog e = Entry {
    entryId = logId e,
    entryTask = logTask e,
    entryStart = logStart e,
    entryEnd = logEnd e
}

parseLogEntry :: TimeZone -> String -> Maybe LogEntry
parseLogEntry tz line = case words line of
    ["Create", id', task, start, end] -> do
        id'' <- readMaybe id'
        start' <- readMaybe start
        end' <- readMaybe end
        return CreateLog {
            logId = id'',
            logTask = task,
            logStart = timestampToLocal start' tz,
            logEnd = timestampToLocal end' tz
        }
    ["Delete", id'] -> do
        id'' <- readMaybe id'
        return DeleteLog {
            logId = id''
        }
    _ -> Nothing


timestampToLocal :: Int -> TimeZone -> LocalTime
timestampToLocal ts tz =
     utcToLocalTime tz $ posixSecondsToUTCTime $ fromIntegral (ts :: Int)

localToTimestamp :: LocalTime -> TimeZone -> Int
localToTimestamp time tz =
       round $ utcTimeToPOSIXSeconds $ localTimeToUTC tz time

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
    start <- getLocalTime
    _ <- repeatedTimer (minuteHandler start) (mDelay 1)
    _ <- installHandler sigINT (Catch $ writeChan stopChan ()) Nothing
    _ <- readChan stopChan
    end <- getLocalTime
    putStrLn ("\nspent: " ++ show (diffTimeInMinutes end start) ++ " minutes")
    nextId <- getNextID
    writeCreateLog nextId task start end

processCommand Summary = do
    entries <- readLog
    let zipped = zip (map entryTask entries) (map duration entries)
    let entryMap = fromListWith (+) zipped
    _ <- sequence $ elems $ mapWithKey printSummary entryMap
    return ()
    where printSummary task dur = putStrLn $ printf "%s: %d" task dur

processCommand List = do
    entries <- readLog
    _ <- sequence $ map (putStrLn . show) entries
    return ()

processCommand (Delete eid) = do
    writeDeleteLog eid
    return ()

processCommand Unknown = putStrLn "Usage:\n\n\
      \  timer-hs start <task>\n\
      \  timer-hs list\n\
      \  timer-hs summary"

diffTimeInMinutes :: LocalTime -> LocalTime -> Int
diffTimeInMinutes t1 t2 =
   diffLocalTime t1 t2
   & nominalDiffTimeToSeconds
   & (/ 60)
   & round

duration :: Entry -> Int
duration e = diffLocalTime (entryEnd e) (entryStart e)
    & nominalDiffTimeToSeconds
    & round

formatLocalTime :: LocalTime -> String
formatLocalTime date =  formatTime defaultTimeLocale "%F %I:%M:%S %p" date

instance Show Entry where
    show e = printf "%5d: %10s %24s %24s"
        (entryId e)
        (entryTask e)
        (formatLocalTime $ entryStart e)
        (formatLocalTime $ entryEnd e)
