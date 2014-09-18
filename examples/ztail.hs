import System.Environment
import ZTail

simplePrint :: TermColor -> TailPacket -> IO ()
simplePrint color tp = do
 output color $ buf tp
-- putStrLn $ show tp

main :: IO ()
main = do
 argv <- getArgs
 tails <- parse_args argv
 run_main argv tailText $ map (\t -> t { ioAct = simplePrint }) tails
