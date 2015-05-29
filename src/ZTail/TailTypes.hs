{-# LANGUAGE CPP, Rank2Types, DeriveGeneric, StandaloneDeriving #-}
module ZTail.TailTypes (
    Interval,
    fromInterval,
    TailTarget(..),
    TailRuntime(..),
    TailMatch(..), TailAction(..),
    TailMatches,
    Tail(..),
    TailPacket(..),
    tailName, tailErrMsg,
    tail2packet
  ) where

import Data.Aeson
import GHC.Generics
import qualified System.Posix.Types
import qualified System.Posix.IO
import qualified Text.Regex
#ifdef INOTIFY
import qualified System.INotify as INotify
#endif

import qualified Data.Time.Clock
import qualified Data.Time.LocalTime

import ZTail.Display

newtype Interval = Interval { fromInterval :: Int } deriving (Eq, Ord)
instance Show Interval where
  show (Interval x) 
    | x `mod` 1000000 == 0 = show (x `div` 1000000)
    | otherwise = show (fromIntegral x / 1000000.0)
instance Read Interval where
  readsPrec n s = map (\(x,s) -> (Interval $ floor (1000000.0 * x), s)) $ readsPrec n s
instance Num Interval where
  Interval x + Interval y = Interval (x + y)
  Interval x * Interval y = Interval (x * y)
  Interval x - Interval y = Interval (x - y)
  negate (Interval x) = Interval (negate x)
  abs (Interval x) = Interval (abs x)
  signum (Interval x) = Interval (signum x)
  fromInteger n = Interval (fromInteger $ 1000000 * n)

data TailTarget = TailPath !FilePath | TailFd !System.Posix.Types.Fd
instance Show TailTarget where
  show (TailFd 0) = "-"
  show (TailFd x) = '&':(show x)
  show (TailPath path) = path
instance Read TailTarget where
  readsPrec _ "-" = [(TailFd System.Posix.IO.stdInput, "")]
  readsPrec n ('&':s) = map (\(f,s) -> (TailFd f, s)) $ readsPrec n s
  readsPrec _ s = [(TailPath s, "")]

data TailMatch =
    MatchAll
  | MatchRegex !Text.Regex.Regex
  | MatchNotRegex !Text.Regex.Regex
data TailAction =
    ActionNone
  | ActionHide
  | ActionColor !TermColor
  | ActionSubst !String
  | ActionExecute !String
type TailMatches = [(TailMatch, TailAction)]

data Tail = Tail
  { tailTarg :: TailTarget
  , tailPollInterval :: !Interval
  , tailReopenInterval :: !Interval
#ifdef INOTIFY
  , tailPollINotify :: !Bool
  , tailReopenINotify :: !Bool
#endif
  , tailBegin :: !Bool -- start at beginning of file
  , tailFileTail :: !Bool -- enable this tail for non-directories (almost always True)
  , tailDirTail :: !Bool -- tail immediate children
  , tailDirList :: !Bool -- enable this tail for directory content
  , tailDirRecursive :: !Bool -- tail children recursively
  , tailTimeFmt :: !String
  , tailMatches :: !TailMatches
  , ioAct :: TermColor -> TailPacket -> IO ()
  }

data TailPacket = TailPacket
  { path :: String
  , buf :: String
  , clock :: Data.Time.Clock.UTCTime
  , tz :: Data.Time.LocalTime.TimeZone
} deriving (Read, Show, Generic)

instance FromJSON TailPacket
instance ToJSON TailPacket

deriving instance Generic Data.Time.Clock.UTCTime
deriving instance Generic Data.Time.LocalTime.TimeZone

--instance FromJSON Data.Time.Clock.UTCTime
--instance ToJSON Data.Time.Clock.UTCTime

instance FromJSON Data.Time.LocalTime.TimeZone
instance ToJSON Data.Time.LocalTime.TimeZone

tailName :: Tail -> String
tailName = show . tailTarg

tailErrMsg :: Tail -> String -> IO ()
tailErrMsg t msg =
  errMsg ("ztail " ++ tailName t ++ ": " ++ msg)

data TailRuntime = TailRuntime
  { trUnlock :: forall a. IO a -> IO a
  , trAddTail :: Tail -> IO ()
  , trText :: Tail -> String -> IO ()
#ifdef INOTIFY
  , trINotify :: Maybe INotify.INotify
#endif
}

tail2packet :: Tail -> String -> Data.Time.Clock.UTCTime -> Data.Time.LocalTime.TimeZone -> TailPacket
tail2packet tail buf t z = TailPacket
 { path = show $ tailTarg tail
 , buf = buf
 , clock = t
 , tz = z
 }
