import System.IO
import Control.Monad

doubleMe x = x + x
doubleUs x y = x*2 + y*2

main = do
  putStrLn "Please enter your name:"
  name <- getLine
  putStrLn( "Hello, " ++name++ " how are you?")
  doGuessing 20
  rFile "./sample_1.txt"

doGuessing num = do
   putStrLn "Enter your guess:"
   guess <- getLine
   if (read guess) < num
     then do putStrLn "Too low!"
             doGuessing num
     else if (read guess) > num
            then do putStrLn "Too high!"
                    doGuessing num
            else putStrLn "You Win!"


rFile fname = do
  contents <- readFile fname
  print . map readInt . words $ contents

readInt :: String -> Int
readInt = read
  
