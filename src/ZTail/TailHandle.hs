{-# LANGUAGE CPP, DeriveDataTypeable, ForeignFunctionInterface #-}
module ZTail.TailHandle (
  runTail
  ) where

import Control.Monad
import System.Posix.Types
import System.Posix.Files
import System.Posix.Directory
import System.Posix.IO
import System.IO.Error
import Data.List (genericTake, genericLength)
import Data.Maybe
import qualified Data.Set as Set
import GHC.IO.Handle (SeekMode(AbsoluteSeek))
import Control.Concurrent
import qualified Control.Exception
import qualified Data.Typeable
#ifdef INOTIFY
import qualified System.INotify as INotify
#endif
import Data.IORef
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt(..))
import Foreign.C.Error (throwErrnoIfNull)
import qualified Unsafe.Coerce
import System.FilePath ((</>))

import ZTail.Util
import ZTail.TailTypes

data TailHandle = TailHandle
  { thTail :: Tail
  , thRuntime :: TailRuntime
  , thPoll :: ThreadId
  , thReopen :: Maybe ThreadId
  , thFd :: Maybe Fd
  , thPos :: FileOffset -- or -1 for blocking fd
  , thIno :: Maybe FileID
  , thBuf :: String
  , thDirStream :: Maybe DirStream
  , thDirList :: Set.Set FilePath
  , thAgain :: Bool
#ifdef INOTIFY
  , thPollWatch :: Maybe INotify.WatchDescriptor
  , thReopenWatch :: Maybe INotify.WatchDescriptor
#endif
  }

data TailSignal 
  = SignalPoll 
  | SignalReopen 
  | SignalInsert String Bool
  | SignalDelete String
  deriving (Show, Data.Typeable.Typeable, Eq, Ord)
instance Control.Exception.Exception TailSignal

thErrMsg = tailErrMsg . thTail

catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist f = catchWhen isDoesNotExistError (liftM Just f) (return Nothing)

bad = ioError . userError

closeTail :: TailHandle -> IO TailHandle
closeTail th@TailHandle{ thFd = Nothing } = return th
closeTail th@TailHandle{ thFd = Just fd } = do
#ifdef INOTIFY
  whenJust rm_watch (thPollWatch th)
  whenJust rm_watch (thReopenWatch th)
#endif
  closeFd fd
  return th{
    thFd = Nothing
  , thPos = 0
  , thIno = Nothing
#ifdef INOTIFY
  , thPollWatch = Nothing
  , thReopenWatch = Nothing 
#endif
  }
#ifdef INOTIFY
  where
    rm_watch = INotify.removeWatch
#endif

seekTail :: FileOffset -> TailHandle -> IO TailHandle
seekTail _ TailHandle{ thFd = Nothing } = bad "seek on closed fd"
seekTail c th@TailHandle{ thFd = Just fd } =
  fdSeek fd AbsoluteSeek c >. th{ thPos = c }

inotifyTail :: TailHandle -> IO TailHandle
#ifdef INOTIFY
inotifyTail th@TailHandle
    { thRuntime = TailRuntime{ trINotify = Just inotify }
    , thPoll = tid
    , thPos = pos
    , thTail = Tail
      { tailTarg = TailPath path
      , tailPollINotify = ipoll
      , tailReopenINotify = ireopen
    } } = do
  let sig (INotify.Modified {}) = Just SignalPoll
      sig (INotify.MovedSelf {}) = Just SignalReopen
      sig (INotify.MovedOut { INotify.filePath = f }) | notdot f = Just $ SignalDelete f
      sig (INotify.MovedIn { INotify.filePath = f }) | notdot f = Just $ SignalInsert f False
      sig (INotify.Created { INotify.filePath = f }) | notdot f = Just $ SignalInsert f True
      sig (INotify.Deleted { INotify.filePath = f }) | notdot f = Just $ SignalDelete f
      sig _ = Nothing
      notdot "" = False
      notdot ('.':_) = False
      notdot _ = True
      add l = INotify.addWatch inotify l path (whenJust (Control.Exception.throwTo tid) . sig)
  poll <- justWhen (ipoll && pos >= 0) $
    if isJust (thDirStream th)
      then add [INotify.OnlyDir, INotify.Move, INotify.Create, INotify.Delete]
      else add [INotify.Modify]
  reopen <- justWhen ireopen $ add [INotify.MoveSelf, INotify.DeleteSelf]
  return th{
    thPollWatch = poll,
    thReopenWatch = reopen
  }
#endif
inotifyTail th = return th

foreign import ccall unsafe fdopendir :: CInt -> IO (Ptr ())
fdOpenDirStream :: Fd -> IO DirStream
fdOpenDirStream (Fd d) = (Unsafe.Coerce.unsafeCoerce :: Ptr () -> DirStream) =.< throwErrnoIfNull "fdOpenDirStream" (fdopendir d)

readDirStreamAll :: DirStream -> IO [FilePath]
readDirStreamAll d = readDirStream d >>= c where
  c [] = return []
  c ('.':_) = readDirStreamAll d
  c f = (f :) =.< readDirStreamAll d

subTail :: TailHandle -> Bool -> FilePath -> IO ()
subTail th@TailHandle{ thRuntime = tr, thTail = t@Tail{ tailTarg = TailPath p } } new f | tailDirTail t || tailDirList t =
  trAddTail tr t
    { tailTarg = TailPath (p </> f)
    , tailFileTail = tailDirTail t
    , tailDirList = tailDirList t && tailDirRecursive t
    , tailDirTail = tailDirTail t && tailDirRecursive t
    , tailBegin = tailBegin t || new
    }
subTail _ _ _ = nop

openTail :: TailHandle -> IO TailHandle
openTail th@TailHandle{ thFd = Nothing } = get (tailTarg (thTail th)) where
  get (TailFd fd) = got (Just fd)
  get (TailPath path) = got =<<
    catchDoesNotExist (
      openFd path ReadOnly Nothing OpenFileFlags{
	append = False, exclusive = False, noctty = False, nonBlock = True, trunc = False
      })
  got Nothing = thErrMsg th "No such file or directory" >. th{ thPos = 0 }
  got (Just fd) = do
    setFdOption fd NonBlockingRead True
    inotifyTail =<< go fd =<< getFdStatus fd
  go fd stat
    | isRegularFile stat =
      seekTail (if pos < 0 then max 0 $ sz + 1 + pos else min sz pos) th'
    | isNamedPipe stat || isSocket stat || isCharacterDevice stat =
      return th'{ thPos = -1 }
    | isDirectory stat && (tailDirList (thTail th) || tailDirTail (thTail th)) = do
      ds <- fdOpenDirStream fd
      dl <- readDirStreamAll ds
      let nl = genericTake (if pos < 0 then genericLength dl + 1 + pos else pos) dl
          th'' = th'{ thDirStream = Just ds, thDirList = Set.fromList nl }
      mapM_ (subTail th'' False) nl
      return th''
    | otherwise =
      closeFd fd >> thErrMsg th "Unsupported file type" >. th
    where
      th' = th{ thFd = Just fd, thIno = Just (fileID stat), thAgain = True, thPos = 0 }
      sz = fileSize stat
      pos = thPos th
openTail _ = bad "open on opened tail"

reopenTail :: TailHandle -> IO TailHandle
reopenTail th@TailHandle{ thTail = Tail{ tailTarg = TailPath path }, thIno = ino } = do
  stat <- catchDoesNotExist $ getFileStatus path
  case stat of
    Nothing -> return th
    Just _ | ino == Nothing -> openTail th
    Just stat | ino == Just (fileID stat) -> return th
    Just stat | fileSize stat == 0 -> return th
    _ -> do
      thErrMsg th "Following new file"
      closeTail th >>= openTail
reopenTail th = return th

noRead :: TailHandle -> (TailHandle, [String])
noRead th = (th{ thAgain = False }, [])

bufsiz :: ByteCount
bufsiz = 8192

insertMsg :: String -> String
insertMsg = ('+':)

deleteMsg :: String -> String
deleteMsg = ('-':)

readTail :: TailHandle -> IO (TailHandle, [String])
readTail th@TailHandle{ thFd = Nothing } = return (noRead th)
readTail th@TailHandle{ thDirStream = Just ds } = do
  dl <- rewindDirStream ds >> readDirStreamAll ds
  let df f (n, o, s) 
        | Set.member f o = (n, Set.delete f o, s)
        | otherwise = (f : n, o, Set.insert f s)
      (nl, os, ds) = foldr df ([], thDirList th, thDirList th) dl
  mapM_ (subTail th True) nl
  return (th{ thDirList = Set.difference ds os, thAgain = False },
    guard (tailDirList (thTail th)) >> map insertMsg nl ++ map deleteMsg (Set.toList os))
readTail th@TailHandle{ thFd = Just fd, thPos = pos } =
  if pos == -1
    then catchWhen isEOFError readsock $ do
      checkbuf th >.= noRead
      -- thErrMsg th "closed?"
      -- closeTail th >.= noRead th
    else getFdStatus fd >.= fileSize >>= gotlen
  where
    checkbuf th@TailHandle{ thBuf = buf } = do
      when (buf /= "") $
	thErrMsg th ("Unterminated line: " ++ buf)
      return th{ thBuf = "" }
    gotlen len
      | len < pos = do
          thErrMsg th ("File truncated to " ++ show len)
	  seekTail 0 th >>= readTail
      | len == pos = do
	  checkbuf th >.= noRead
      | otherwise = do
	  let count = min (fromIntegral (len - pos)) bufsiz
	  (buf, buflen) <- readit (fromIntegral count)
	  when (buflen /= count) $
	    thErrMsg th ("Short read (" ++ show buflen ++ "/" ++ show count ++ ")")
	  return $ gotbuf th{ thPos = pos + fromIntegral buflen, thAgain = buflen == bufsiz } buf
    readsock =
      readit bufsiz >.= fst >.= gotbuf th{ thAgain = True }
    gotbuf th "" = noRead th
    gotbuf th@TailHandle{ thBuf = oldbuf } buf =
      case initlast $ split (== '\n') buf of
	([], r) ->
	  (th{ thBuf = oldbuf ++ r }, [])
	(l1 : l, r) ->
	  (th{ thBuf = r }, (oldbuf ++ l1) : l)
    readit len = catchWhen isFullError
      (fdRead fd len)
      (return ("", 0))

pause :: IO ()
pause = threadDelay (30*60*1000000) -- fixme how? (-1) doesn't work

waitTail :: TailHandle -> IO ()
waitTail TailHandle{ thFd = Nothing } = pause
waitTail TailHandle{ thFd = Just fd, thPos = -1 } = threadWaitRead fd
waitTail TailHandle{ thAgain = True } = yield
waitTail TailHandle{ thTail = Tail{ tailPollInterval = i } }
  | i == 0 = pause
  | otherwise = threadDelay (fromInterval i)

reopenThread :: Int -> ThreadId -> IO ()
reopenThread ri tid = forever $ do
  threadDelay ri
  Control.Exception.throwTo tid SignalReopen

newTail :: TailRuntime -> Tail -> IO TailHandle
newTail tr tail = do
  tid <- myThreadId
  rid <- justWhen (ri /= 0) $ forkIO (reopenThread ri tid)
  return TailHandle
    { thTail = tail
    , thRuntime = tr
    , thPoll = tid
    , thReopen = rid
    , thFd = Nothing
    , thPos = if tailBegin tail then 0 else -1
    , thIno = Nothing
    , thBuf = []
    , thDirStream = Nothing
    , thDirList = Set.empty
    , thAgain = True
#ifdef INOTIFY
    , thPollWatch = Nothing
    , thReopenWatch = Nothing
#endif
    }
  where 
    ri = fromInterval (tailReopenInterval tail)

runTail :: TailRuntime -> Tail -> IO ()
runTail tr tail = Control.Exception.mask $ \unmask ->
  let
    catch th SignalReopen = reopenTail th -- >>= wait
--    catch th SignalPoll = reopenTail th
-- might be a 'bug'
    catch th SignalPoll = return th{ thAgain = True }
    catch th@TailHandle{ thDirStream = Just _, thDirList = l, thTail = t } (SignalInsert f new) = do
      subTail th new f
      if tailDirList t
        then proc th' [insertMsg f]
        else return th'
      where th' = th{ thDirList = Set.insert f l }
    catch th@TailHandle{ thDirStream = Just _, thDirList = l, thTail = t } (SignalDelete f) = 
      if tailDirList t 
        then proc th' [deleteMsg f]
        else return th'
      where th' = th{ thDirList = Set.delete f l }
    wait th = Control.Exception.catch
      (trUnlock tr $ unmask $ waitTail th >. th) 
      (catch th >=> wait)
    poll th
      | thReopen th == Nothing
	&& (thFd th == Nothing
	  || (thAgain th == False && thPos th /= -1
	    && fromInterval (tailPollInterval (thTail th)) == 0
#ifdef INOTIFY
	    && thPollWatch th == Nothing 
	    && thReopenWatch th == Nothing
#endif
	    ))
	= return th
      | otherwise = wait th >>= go
    go th = readTail th >>= uncurry proc >>= poll
    proc th s = mapM_ fun s >. th
    fun = (trText tr) tail
  in
  newTail tr tail >>=
  openTail >>= go >>= 
  \TailHandle{ thReopen = wid } -> whenJust killThread wid
