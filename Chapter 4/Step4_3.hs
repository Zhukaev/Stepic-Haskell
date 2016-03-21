--4.3.3

import Data.Time.Clock
import Data.Time.Format
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String} 

logLevelToString :: LogLevel -> String 
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString (LogEntry timestamp logLevel message) = (timeToString timestamp) ++ ": " ++ (logLevelToString logLevel) ++ ": " ++ (message)

--4.3.5

data Person = Person {firstName :: String, lastName :: String, age :: Int} deriving (Eq,Show)

updateLastName :: Person -> Person -> Person
updateLastName pers1 pers2 = pers2 {lastName = (lastName pers1)}

--4.3.8

data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName p = p {firstName = helper (firstName p)}

helper :: String -> String
helper name = if length name < 2 then name else [head name] ++ "."