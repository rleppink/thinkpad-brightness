#!/usr/bin/env runhaskell


import System.Environment
import System.Exit


main :: IO ()
main = getArgs >>= parse >> putStrLn "yo?"


parse :: [String] -> IO String
parse []        = usage                              >> exitSuccess
parse ["-h"]    = usage                              >> exitSuccess
parse ["-v"]    = version                            >> exitSuccess
parse ["-i"]    = specify "-i"                       >> exitSuccess
parse ["-d"]    = specify "-d"                       >> exitSuccess
parse ["-i", x] = changeBrightness (read x :: Int) >> exitSuccess
parse ["-d", x] = changeBrightness (negate $ read x :: Int) >> exitSuccess


backlightFilePath :: String -> String
backlightFilePath filename  = "/sys/class/backlight/intel_backlight/" ++ filename


changeBrightness :: Int -> IO ()
changeBrightness x = do
    brightnessFileContents <- readFile $ backlightFilePath "brightness"
    seq (length brightnessFileContents) (return())
    maxBrightnessFileContents <- readFile $ backlightFilePath "max_brightness"

    let currentBrightness = read brightnessFileContents :: Int
    let maxBrightness = read maxBrightnessFileContents :: Int

    let newBrightness = calcDeltaBrightness currentBrightness 10 maxBrightness x

    writeFile (backlightFilePath "brightness") $ show newBrightness


calcDeltaBrightness :: Int -> Int-> Int -> Int -> Int
calcDeltaBrightness current min max delta
  | (newValue == min) || (newValue == max) = newValue
  | (min < newValue) && (newValue < max) = newValue
  | newValue > max = max
  | newValue < min = min
  where newValue = current + delta


specify :: String -> IO ()
specify x = putStrLn $ x ++ " needs an argument"


usage :: IO ()
usage = putStrLn "Usage: tbr [-id] [amount]"


version :: IO ()
version = putStrLn "Thinkpad Brightness v0.1"

