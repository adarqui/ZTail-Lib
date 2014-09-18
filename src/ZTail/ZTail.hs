{-# LANGUAGE CPP #-}
module ZTail.ZTail (
 run,
 run_main,
 defaultTail,
 parse_args
) where

import System.Exit
import Control.Monad
--import qualified System.Environment
import qualified System.Console.GetOpt as GetOpt
import System.Posix.Signals (installHandler, sigINT, Handler(..))
import qualified GHC.IO.Exception
import qualified System.IO.Error
import qualified Control.Concurrent
import qualified Control.Exception
import Data.IORef
import qualified Text.Regex
#ifdef INOTIFY
import qualified System.INotify as INotify
#endif

import ZTail.Util
import ZTail.Display
import ZTail.TailTypes
import ZTail.Tail
import ZTail.TailHandle

data Options = Options
  { optionTails :: [Tail]
  , optionTail :: Tail
  , optionMatch :: TailMatch
  }

defaultTail = Tail
  { tailTarg = undefined -- read "-"
  , tailPollInterval = 5
  , tailReopenInterval = 0
#ifdef INOTIFY
  , tailPollINotify = True
  , tailReopenINotify = False
#endif
  , tailBegin = False
  , tailFileTail = True
  , tailDirTail = False
  , tailDirList = False
  , tailDirRecursive = False
  , tailTimeFmt = "%c"
  , tailMatches = []
  }

defaultOptions = Options
  { optionTails = []
  , optionTail = defaultTail
  , optionMatch = MatchAll
  }

set_opt :: (Tail -> Tail) -> Options -> Options
set_opt p o = o{ optionTail = p $ optionTail o }

set_match :: TailMatch -> Options -> Options
set_match m o = o{ optionMatch = m }

add_action :: TailAction -> Options -> Options
add_action a o = set_opt add o where
  add t = t{ tailMatches = (optionMatch o, a) : (tailMatches t) }

prog_header = "Usage: ztail [OPTIONS] FILE ...\n\
Follow the specified files (ala tail -f).  FILE may be a path, '-' for stdin,\n\
or '&N' for file descriptor N.  OPTIONS apply only to the following FILE\n\
except those marked *, and match options (-amn) apply to all following actions\n\
(-hcdse).  Actions involving TEXT can contain the following \\-escapes:\n\
    \\0 current file        \\@ current time (from -t)\n\
    \\_ current line        \\` \\' pre- and post-matching text\n\
    \\& matching text       \\N (1-9) group in match\n\
\&" --"
prog_usage = GetOpt.usageInfo prog_header prog_options

prog_options :: [GetOpt.OptDescr (Options -> Options)]
prog_options = 
  [ GetOpt.Option "i" ["interval"]
      (GetOpt.ReqArg (\i -> set_opt $ \p -> p
        { tailPollInterval = read i
#ifdef INOTIFY
        , tailPollINotify = False
#endif
        }) "INT")
      ("*poll for data every INT seconds [" ++ show (tailPollInterval defaultTail) ++ "]")
  , GetOpt.Option "r" ["reopen"]
      (GetOpt.OptArg (\i -> set_opt $ \p -> p
        { tailReopenInterval = maybe (tailPollInterval p) read i
#ifdef INOTIFY
        , tailReopenINotify = False
#endif
        }) "INT")
      ("*check file name (like tail -F) every INT seconds or every poll [" ++ show (tailReopenInterval defaultTail) ++ "]")
#ifdef INOTIFY
  , GetOpt.Option "I" ["inotify"]
      (GetOpt.OptArg (\i -> set_opt $ \p -> p
        { tailPollINotify = True
        , tailPollInterval = maybe 0 read i }) "INT")
      ("*use inotify to poll for new data (and also poll every INT)")
  , GetOpt.Option "R" ["ireopen"]
      (GetOpt.NoArg (set_opt $ \p -> p
        { tailReopenINotify = True }))
      ("*use inotify to monitor file renames (only for preexisting, leaf files)")
#endif
  , GetOpt.Option "b" ["begin"]
      (GetOpt.NoArg (set_opt $ \p -> p
        { tailBegin = True }))
      (" start reading at the beginning of the file (rather than only new lines at the end)")
  , GetOpt.Option "l" ["dirlist"]
      (GetOpt.NoArg (set_opt $ \p -> p
        { tailDirList = True }))
      (" watch the contents of a directory, reporting when files are added or removed")
  , GetOpt.Option "D" ["dirtail"]
      (GetOpt.NoArg (set_opt $ \p -> p
        { tailDirTail = True }))
      (" tail all the files in a directory")
  , GetOpt.Option "A" ["recursive"]
      (GetOpt.NoArg (set_opt $ \p -> p
        { tailDirRecursive = True }))
      (" apply the above directory modifiers recursively")

  , GetOpt.Option "t" ["timefmt"]
      (GetOpt.ReqArg (\t -> set_opt $ \p -> p
        { tailTimeFmt = t }) "FMT")
      ("*set time format for \\@ substitution (in strftime(3)) [" ++ tailTimeFmt defaultTail ++ "]")
  , GetOpt.Option "T" ["timestamp"]
      (GetOpt.OptArg (maybe id $ \t -> add_action (ActionSubst "\\@ \\_") . set_opt (\p -> p
        { tailTimeFmt = t })) "FMT")
      (" timestamp with FMT; equivalent to: [-t FMT] -h '\\@ '")

  , GetOpt.Option "a" ["all"]
      (GetOpt.NoArg (set_match MatchAll))
      (" perform following action for every line from this FILE (default)")
  , GetOpt.Option "m" ["match"]
      (GetOpt.ReqArg (\m -> set_match $ MatchRegex $ Text.Regex.mkRegexWithOpts m False True) "REGEX")
      (" perform following action for each line matching REGEX")
  , GetOpt.Option "M" ["imatch"]
      (GetOpt.ReqArg (\m -> set_match $ MatchRegex $ Text.Regex.mkRegexWithOpts m False False) "REGEX")
      (" perform following action for each line matching REGEX (case-insensitive)")
  , GetOpt.Option "n" ["no-match"]
      (GetOpt.ReqArg (\m -> set_match $ MatchNotRegex $ Text.Regex.mkRegexWithOpts m False True) "REGEX")
      (" perform following action for each line not matching REGEX")
  , GetOpt.Option "N" ["no-imatch"]
      (GetOpt.ReqArg (\m -> set_match $ MatchNotRegex $ Text.Regex.mkRegexWithOpts m False False) "REGEX")
      (" perform following action for each line not matching REGEX (case-insensitive)")

  , GetOpt.Option "h" ["header"]
      (GetOpt.ReqArg (\h -> add_action $ ActionSubst (h ++ "\\_")) "TEXT")
      (" display TEXT header before (matching) lines (same as -s 'TEXT\\_')")
  , GetOpt.Option "c" ["color"]
      (GetOpt.ReqArg (\c -> add_action $ ActionColor $ parseColor c) "COLOR")
      (" display (matching) lines in COLOR (valid colors are: normal, bo,ul,bl,rev, nobo,noul..., black,red,green,yellow,blue,magenta,cyan,white, /black,/red,...)")
  , GetOpt.Option "d" ["hide"]
      (GetOpt.NoArg (add_action ActionHide))
      (" hide (matching) lines")
  , GetOpt.Option "s" ["substitute"]
      (GetOpt.ReqArg (\s -> add_action $ ActionSubst s) "TEXT")
      (" substitute (matching) lines with TEXT")
  , GetOpt.Option "e" ["execute"]
      (GetOpt.ReqArg (\e -> add_action $ ActionExecute e) "PROG")
      (" execute PROG for every (matching) line")
  ]
prog_arg a Options{ optionTails = l, optionTail = t } = Options
  { optionTails = t
    { tailTarg = read a
    , tailMatches = reverse (tailMatches t)
    } : l
    , optionTail = t
      { tailBegin = False
      , tailMatches = []
      , tailDirList = False
      , tailDirTail = False
      , tailDirRecursive = False
      }
    , optionMatch = MatchAll
  }

run :: (Tail -> String -> IO ()) -> [Tail] ->  IO (Control.Concurrent.MVar ExitCode)
run tcb tails = do
  emv <- Control.Concurrent.newEmptyMVar
  let exit = Control.Concurrent.putMVar emv
      exit0 = exit ExitSuccess
      exit1 = exit (ExitFailure 1)
  installHandler sigINT (CatchOnce exit0) Nothing

  lockv <- Control.Concurrent.newQSem 0
  let lock = Control.Exception.bracket_ (Control.Concurrent.waitQSem lockv) (Control.Concurrent.signalQSem lockv)
      unlock = Control.Exception.bracket_ (Control.Concurrent.signalQSem lockv) (Control.Concurrent.waitQSem lockv)
      incr r = atomicModifyIORef r (\i -> (succ i, ()))

  count <- newIORef (length tails)
  errors <- newIORef 0
#ifdef INOTIFY
  inotify <- 
    catchWhen ((GHC.IO.Exception.UnsupportedOperation ==) . System.IO.Error.ioeGetErrorType)
      (Just =.< INotify.initINotify) 
      (return Nothing)
#endif

  let error t e = case Control.Exception.fromException e of
	Just Control.Exception.UserInterrupt -> exit0
	_ -> tailErrMsg t (show e) >> incr errors
      tr = TailRuntime
	{ trText = tcb
        , trAddTail = (incr count >>) . runt
	, trUnlock = unlock
#ifdef INOTIFY
	, trINotify = inotify
#endif
	}
      runt t = void $ Control.Concurrent.forkIO $ lock $ do
        Control.Exception.handle (error t) $ runTail tr t
        i <- atomicModifyIORef count (\i -> (pred i, i))
        when (i == 1) $ do
          e <- readIORef errors
          if e > 0
            then exit1
            else exit0
  mapM_ runt tails
  Control.Concurrent.signalQSem lockv
  return emv

parse_args argv = do
  tails <- case (GetOpt.getOpt (GetOpt.ReturnInOrder prog_arg) prog_options argv) of
    (s, [], []) -> case optionTails $ foldl (\s t -> t s) defaultOptions s of
      [] -> do
	putStrLn prog_usage
	exitWith ExitSuccess
      t -> return t
    (_, _, err) -> do
      mapM_ putStrLn err
      putStrLn prog_usage
      exitFailure
  return tails

run_main argv tcb tails = do
  e <- run tcb tails >>= Control.Concurrent.takeMVar
  when (e == ExitSuccess) $
    errMsg "ztail: done"
  exitWith e
