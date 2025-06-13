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
import Data.Map (Map, elems, fromListWith, mapWithKey, empty, insert, delete)
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time
import Data.List (find)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import Control.Monad (void)

data Command =
    Start String
    | Delete Int
    | Update Int String TimeOfDay TimeOfDay
    | List
    | SummaryDaily
    | SummaryWeekly
    | SummaryTotal
    | Unknown deriving Show

data LogEntry =
    CreateLog { logId :: Int
              , logTask :: String
              , logStart :: LocalTime
              , logEnd :: LocalTime
              }
    | UpdateLog { logId :: Int
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
    ["summary"] -> SummaryDaily
    ["summary", "daily"] -> SummaryDaily
    ["summary", "weekly"] -> SummaryWeekly
    ["summary", "total"] -> SummaryTotal
    ["list"] -> List
    ["delete", eid] -> maybe Unknown Delete $ readMaybe eid
    ["update", eid, task, start, end] -> fromMaybe Unknown $ do
        eid' <- readMaybe eid
        start' <- readLocalTime start
        end' <- readLocalTime end
        return $ Update eid' task start' end'
    _ -> Unknown

writeLog :: LogEntry -> IO ()
writeLog (CreateLog eid task start end) = do
    tz <- getCurrentTimeZone
    let start' = localToTimestamp start tz
        end' = localToTimestamp end tz
    appendFile "time.log" $ printf "Create %d %s %d %d\n" eid task start' end'

writeLog (UpdateLog eid task start end) = do
    tz <- getCurrentTimeZone
    let start' = localToTimestamp start tz
        end' = localToTimestamp end tz
    appendFile "time.log" $ printf "Update %d %s %d %d\n" eid task start' end'

writeLog (DeleteLog eid) = appendFile "time.log" $ printf "Delete %d\n" eid

minuteHandler :: LocalTime -> IO ()
minuteHandler start = do
    current <- getLocalTime
    putStrLn $ showDuration (diffTimeInSeconds current start)

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
    where combine (CreateLog eid task start end) = insert eid (Entry eid task start end)
          combine (UpdateLog eid task start end) = insert eid (Entry eid task start end)
          combine (DeleteLog eid) = delete eid

parseLogEntry :: TimeZone -> String -> Maybe LogEntry
parseLogEntry tz line = case words line of
    ["Create", id', task, start, end] -> do
        id'' <- readMaybe id'
        start' <- readMaybe start
        end' <- readMaybe end
        return $ CreateLog id'' task (timestampToLocal start' tz) (timestampToLocal end' tz)
    ["Update", id', task, start, end] -> do
        id'' <- readMaybe id'
        start' <- readMaybe start
        end' <- readMaybe end
        return $ UpdateLog id'' task (timestampToLocal start' tz) (timestampToLocal end' tz)
    ["Delete", id'] -> do
        id'' <- readMaybe id'
        return $ DeleteLog id''
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
    putStrLn ("\nspent: " ++ showDuration (diffTimeInSeconds end start))
    nextId <- getNextID
    writeLog $ CreateLog nextId task start end

processCommand SummaryDaily = do
    entries <- readLog
    now <- getLocalTime
    putStrLn "Today:"
    printTaskDurations $ filter (entryBetween (startOfDay now) (endOfDay now)) entries


processCommand SummaryWeekly = do
    entries <- readLog
    now <- getLocalTime
    putStrLn "This week:"
    printTaskDurations $ filter (entryBetween (startOfWeek now) (endOfWeek now)) entries

processCommand SummaryTotal = do
    entries <- readLog
    putStrLn "Total:"
    printTaskDurations entries
    return ()

processCommand List = do
    entries <- readLog
    _ <- sequence $ map (putStrLn . show) entries
    return ()

processCommand (Delete eid) = do
    writeLog $ DeleteLog eid
    return ()

processCommand (Update eid task startTime endTime) = do
    entries <- readLog
    let entry = find (\e -> eid == entryId e) entries
    case entry of
        Just (Entry _ _ start end) -> do
            writeLog $ UpdateLog eid task (setTime start startTime) (setTime end endTime)
            return ()
        Nothing -> return ()

processCommand Unknown = putStrLn "Usage:\n\n\
      \  timer-hs start <task>\n\
      \  timer-hs list\n\
      \  timer-hs summary"

diffTimeInSeconds :: LocalTime -> LocalTime -> Int
diffTimeInSeconds t1 t2 =
   diffLocalTime t1 t2
   & nominalDiffTimeToSeconds
   & round

duration :: Entry -> Int
duration (Entry _ _ start end) = diffTimeInSeconds end start

entryBetween :: LocalTime -> LocalTime -> Entry -> Bool
entryBetween t1 t2 (Entry _ _ start _) = t1 < start && start < t2

showDuration :: Int -> String
showDuration d = printf "%d:%02d:%02d" hours minutes seconds
    where hours = d `div` 60 `div` 60
          minutes = (d `div` 60) `rem` 60
          seconds = d `rem` 60

formatLocalTime :: LocalTime -> String
formatLocalTime date =  formatTime defaultTimeLocale "%F %I:%M:%S %p" date

startOfDay :: LocalTime -> LocalTime
startOfDay (LocalTime day _) = LocalTime day midnight

endOfDay :: LocalTime -> LocalTime
endOfDay (LocalTime day _) = LocalTime (addDays 1 day) midnight

startOfWeek :: LocalTime -> LocalTime
startOfWeek (LocalTime day _) = LocalTime (fromWeekDate year week 1) midnight
    where (year, week, _) = toWeekDate day

endOfWeek :: LocalTime -> LocalTime
endOfWeek datetime = LocalTime (addDays 7 day) midnight
    where (LocalTime day _) = startOfWeek datetime

setTime :: LocalTime -> TimeOfDay -> LocalTime
setTime (LocalTime day _) time = LocalTime day time

readLocalTime :: String -> Maybe TimeOfDay
readLocalTime = parseTimeM True defaultTimeLocale "%I:%M%p"

durationByTask :: [Entry] -> Map String Int
durationByTask entries = fromListWith (+) zipped
    where zipped = zip (map entryTask entries) (map duration entries)

printTaskDurations :: [Entry] -> IO ()
printTaskDurations entries = void $ sequence $ elems $ mapWithKey printSummary (durationByTask entries)
    where printSummary task dur = putStrLn $ printf "%10s %s" task (showDuration dur)

instance Show Entry where
    show (Entry eid task start end) = printf "%5d: %10s %24s %24s"
        eid
        task
        (formatLocalTime start)
        (formatLocalTime end)


