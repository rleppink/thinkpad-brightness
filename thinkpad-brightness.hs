#!/usr/bin/env runhaskell

import System.Environment
import System.Exit


main :: IO ()
main = getArgs >>= parse >>= putStrLn


parse :: [String] -> IO String
parse ["-v"]     = version                                       >> exitSuccess

parse []         = usage                                         >> exitSuccess
parse ["-h"]     = usage                                         >> exitSuccess

parse ["-i"]     = specify "-i"                                  >> exitSuccess
parse ["-d"]     = specify "-d"                                  >> exitSuccess
parse ["-s"]     = specify "-s"                                  >> exitSuccess
parse ["-si"]    = specify "-si"                                 >> exitSuccess
parse ["-sd"]    = specify "-sd"                                 >> exitSuccess

parse ["-i", x]  = changeBrightness (read x :: Int)              >> exitSuccess
parse ["-d", x]  = changeBrightness (negate $ read x :: Int)     >> exitSuccess

parse ["-s", x]  = setBrightness (read x :: Int)                 >> exitSuccess
 
parse ["-si", x] = stepChangeBrightness (read x :: Int)          >> exitSuccess
parse ["-sd", x] = stepChangeBrightness (negate $ read x :: Int) >> exitSuccess

parse _          = usage                                         >> exitSuccess


backlightFilePath :: String -> String
backlightFilePath filename  = "/sys/class/backlight/intel_backlight/" ++ filename


brightnessFilePath :: String
brightnessFilePath = backlightFilePath "brightness"


maxBrightnessFilePath :: String
maxBrightnessFilePath = backlightFilePath "max_brightness"


changeBrightness :: Int -> IO ()
changeBrightness x = do
    brightnessFileContents    <- readFile brightnessFilePath
    seq (length brightnessFileContents) (return())  -- Stupid hack

    maxBrightnessFileContents <- readFile maxBrightnessFilePath

    let currentBrightness = read brightnessFileContents    :: Int
    let maxBrightness     = read maxBrightnessFileContents :: Int

    let newBrightness = calcBrightnessDelta currentBrightness 1 maxBrightness x

    setBrightness newBrightness


calcBrightnessDelta :: Int -> Int-> Int -> Int -> Int
calcBrightnessDelta current min max delta
    | (newValue == min) || (newValue == max) = newValue
    | (min < newValue)  && (newValue < max)  = newValue
    | newValue > max                         = max
    | newValue < min                         = min
    where newValue = current + delta


calcStep :: Int -> Int
calcStep 0 = 0
calcStep 1 = 1
calcStep 3000 = 15
calcStep x = round $ 1.8688 * (1.6357 ^ x)


closestStep :: Int -> Int
closestStep 0 = 0
closestStep 1 = 1
closestStep 3000 = 15
closestStep y = round $ logBase 1.6357 (fromIntegral y / 1.8688)


setBrightness :: Int -> IO ()
setBrightness x = writeFile (backlightFilePath "brightness") $ show x


specify :: String -> IO ()
specify x = putStrLn $ x ++ " needs an argument"


stepChangeBrightness :: Int -> IO ()
stepChangeBrightness x = do
    brightnessFileContents    <- readFile brightnessFilePath
    seq (length brightnessFileContents) (return())  -- Stupid hack

    maxBrightnessFileContents <- readFile maxBrightnessFilePath

    let currentBrightness = read brightnessFileContents    :: Int
    let maxBrightness     = read maxBrightnessFileContents :: Int

    let newBrightness = calcStepDelta currentBrightness x

    setBrightness newBrightness


calcStepDelta :: Int -> Int -> Int
calcStepDelta currentBrightness stepDelta = 
    calcStep $ closestStep currentBrightness + stepDelta

usage :: IO ()
usage = putStrLn "Usage: tbr [-id] [amount]"


version :: IO ()
version = putStrLn "Thinkpad Brightness v0.1"

