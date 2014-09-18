module ZTail.Tail (
    tailText
  ) where

import Data.Array (Array, array, (//), (!))
import Text.Regex
import qualified Data.Char
import qualified System.Exit
import qualified System.Cmd
import qualified Data.Time.Clock
import qualified Data.Time.LocalTime
import qualified Data.Time.Format
import qualified System.Locale

import ZTail.Util
import ZTail.Display
import ZTail.TailTypes

type Substitutions = Array Char String
default_subst :: Substitutions
default_subst = array ('\0','\127') [('\\',"\\")]

timeFmt :: String -> Data.Time.Clock.UTCTime -> Data.Time.LocalTime.TimeZone -> IO String
timeFmt f t z = do
  return $ Data.Time.Format.formatTime System.Locale.defaultTimeLocale f (Data.Time.LocalTime.utcToLocalTime z t)

shellEscape :: String -> String
shellEscape [] = []
shellEscape (c:l)
  | Data.Char.isAlphaNum c = c: shellEscape l
  | otherwise = '\\':c: shellEscape l

substText :: Substitutions -> (String -> String) -> String -> String
substText sub f str = go str
  where
    go ('\\':x:l) = f (sub!x) ++ go l
    go (x:l) = x : go l
    go [] = []

matchText :: Substitutions -> TailMatch -> String -> Maybe Substitutions
matchText sub MatchAll t = Just (sub // [('_',t)])
matchText sub (MatchRegex m) t =
  case matchRegexAll m t of
    Nothing -> Nothing
    Just (pre, mat, post, exps) ->
      Just (sub
	// [('_',t), ('`',pre), ('&',mat), ('\'',post)]
	// zip ['1'..'9'] exps)
matchText sub (MatchNotRegex m) t =
  case matchRegex m t of
    Nothing -> Just (sub // [('_',t)])
    Just _ -> Nothing

execute :: Tail -> String -> IO System.Exit.ExitCode
execute th e =
  tailErrMsg th ("execute: " ++ e) >>
  System.Cmd.system e

processText :: Tail -> String -> IO ()
processText tail x = do
  t <- Data.Time.Clock.getCurrentTime
  z <- Data.Time.LocalTime.getCurrentTimeZone
  now <- timeFmt (tailTimeFmt tail) t z
  let init_sub = default_subst // [('0',tailName tail),('@',now)] 
  case foldl mact (Just x, init_sub, [], []) (tailMatches tail) of
    (Nothing, _, _, _) -> nop
    (Just out, _, color, exec) ->
        (ioAct tail) color (tail2packet tail out t z) >>
      mapM_ (execute tail) exec
  where
    mact r@(Nothing, _, _, _) _ = r
    mact r@(Just s, sub, cl, el) (m, a) =
      case matchText sub m s of
	Nothing -> r
	Just sub -> case a of
	  ActionNone -> (Just s, sub, cl, el)
	  ActionHide -> (Nothing, sub, cl, el)
	  ActionColor c -> (Just s, sub, c ++ cl, el)
	  ActionSubst s' -> (Just (substText sub id s'), sub, cl, el)
	  ActionExecute e -> (Just s, sub, cl, (substText sub shellEscape e) : el)

tailText = processText
