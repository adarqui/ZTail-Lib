{-# LANGUAGE PatternGuards #-}
module ZTail.Display (
    TermColor,
    displayColor,
    displayResetColor,
    parseColor,
    errMsg,
    output,
    reset
  ) where

import System.IO
import Data.Char
import ZTail.Util

type TermColor = [Int]

displayColor :: TermColor -> String
displayColor [] = ""
displayColor c =
  "\ESC[" ++ (join ';' (map show c)) ++ "m"

displayResetColor = displayColor colorReset

isInteger :: String -> Bool
isInteger [] = False
isInteger x = all isDigit x

colorReset = colorValue "reset"
colorValue :: String -> TermColor
colorValue "reset"	= [0]
colorValue "bo" 	= [1]
--colorValue "dark"	= [2]
colorValue "so"		= [3]
colorValue "hl"		= [3]
colorValue "ul"		= [4]
colorValue "bl"		= [5]
colorValue "rev"	= [7]
colorValue "hidden"	= [8]
colorValue "nobo"	= [22]--[21]
--colorValue "nodark"	= [22]
colorValue "noso"	= [23]
colorValue "nohl"	= [23]
colorValue "noul"	= [24]
colorValue "nobl"	= [25]
colorValue "norev"	= [27]
colorValue "nohidden"	= [28]
colorValue "black"	= [30]
colorValue "red"	= [31]
colorValue "green"	= [32]
colorValue "yellow"	= [33]
colorValue "blue"	= [34]
colorValue "magenta"	= [35]
colorValue "cyan"	= [36]
colorValue "white"	= [37]
colorValue "default"	= [39]
colorValue ('c':'o':'l':'o':'r':n) | isInteger n = [38,5,read n]
colorValue ('m':'o':'d':'e':n) | isInteger n = [read n]
colorValue ('b':'r':'i':'g':'h':'t':c) | [n] <- colorValue c, n >= 30 && n < 40 = [60 + n]
colorValue ('b':'r':c) | [n] <- colorValue c, n >= 30 && n < 40 = [60 + n]
colorValue ('/':c) | (n:r) <- colorValue c = 10+n:r

colorValue "normal"	= colorValue "reset"
colorValue "bold"	= colorValue "bo"
colorValue "nobold"	= colorValue "nobo"
colorValue "dim"	= colorValue "dark"
colorValue "nodim"	= colorValue "nodark"
colorValue "standout"	= colorValue "so"
colorValue "nostandout"	= colorValue "noso"
colorValue "hilite"	= colorValue "hl"
colorValue "nohilite"	= colorValue "nohl"
colorValue "underline"  = colorValue "ul"
colorValue "nounderline"= colorValue "noul"
colorValue "blink"	= colorValue "bl"
colorValue "noblink"	= colorValue "nobl"
colorValue "reverse"	= colorValue "rev"
colorValue "noreverse"	= colorValue "norev"

colorValue x = error ("unknown color name: " ++ x)

colorSep = not . isAlphaNum

parseColor :: String -> TermColor
parseColor [] = []
parseColor s@(c:s')
  | colorSep c = parseColor s'
  | otherwise = colorValue x ++ parseColor r
      where
	(x, r) = break colorSep s

errMsg :: String -> IO ()
errMsg m = hPutStrLn stderr (displayResetColor ++ m)

output :: TermColor -> String -> IO ()
output c m = putStrLn (displayColor (colorReset ++ c) ++ m)

reset :: IO ()
reset = putStr displayResetColor
