module Main where
import System.Environment (getArgs)
import Playfair

main :: IO ()
main = do
    args <- getArgs

    if length args < 3
      then putStrLn "Using: playfair <encrypt|decrypt> <KEY> <MESSAGE>"
    else do
      let modeStr = args !! 0
      let key = args !! 1
      let msg = args !! 2

      let mode = if modeStr == "encrypt" then Encrypt else Decrypt

      let result = cryptMessage mode key msg

      putStrLn result


