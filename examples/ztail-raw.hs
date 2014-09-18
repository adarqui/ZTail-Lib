import System.Environment
import ZTail

simplePrint :: TermColor -> TailPacket -> IO ()
simplePrint _ tp = do
 putStrLn $ show tp

main :: IO ()
main = do
 argv <- getArgs
 tails <- parse_args argv
 run_main argv tailText $ map (\t -> t { ioAct = simplePrint }) tails
