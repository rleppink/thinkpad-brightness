#!/usr/bin/env stack
-- stack --resolver lts-9.4 script

import System.Environment
import System.Exit


main :: IO ()
main = getArgs >>= parse >>= putStrLn


parse :: [String] -> IO String
parse ["-v"]     = version                                       >> exitSuccess

parse ["-i"]     = needsArgument "-i"                            >> exitSuccess
parse ["-d"]     = needsArgument "-d"                            >> exitSuccess
parse ["-s"]     = needsArgument "-s"                            >> exitSuccess
parse ["-si"]    = needsArgument "-si"                           >> exitSuccess
parse ["-sd"]    = needsArgument "-sd"                           >> exitSuccess

parse ["-s", x]  = writeBrightness (read x :: Int)               >> exitSuccess
parse ["-g"]     = readBrightness                                >> exitSuccess

-- parse ["-i", x]  = changeBrightness (read x :: Int)              >> exitSuccess
-- parse ["-d", x]  = changeBrightness (negate $ read x :: Int)     >> exitSuccess

-- parse ["-si", x] = stepChangeBrightness (read x :: Int)          >> exitSuccess
-- parse ["-sd", x] = stepChangeBrightness (negate $ read x :: Int) >> exitSuccess

parse []         = usage                                         >> exitSuccess
parse ["-h"]     = usage                                         >> exitSuccess
parse _          = usage                                         >> exitSuccess


backlightFilePath :: String -> String
backlightFilePath filename  = "/sys/class/backlight/intel_backlight/" ++ filename

brightnessFilePath :: String
brightnessFilePath = backlightFilePath "brightness"

maxBrightnessFilePath :: String
maxBrightnessFilePath = backlightFilePath "max_brightness"

readBrightness :: IO ()
readBrightness = putStrLn =<< readFile brightnessFilePath

writeBrightness :: Int -> IO ()
writeBrightness x = writeFile brightnessFilePath $ show x

needsArgument :: String -> IO ()
needsArgument x = putStrLn $ x ++ " needs an argument"

usage :: IO ()
usage = putStrLn "Usage: tbr [-id] [amount]"

version :: IO ()
version = putStrLn "Thinkpad Brightness v0.1"

changeBrightness :: (Int -> Int -> Int -> Int) -> IO ()
changeBrightness f = do
    maxBrightnessFileContents <- readFile maxBrightnessFilePath
    brightnessFileContents    <- readFile brightnessFilePath
    -- Hacky solution to force readFile to read the entire file, before trying to write to it
    seq (length brightnessFileContents) (return())

    let maxBrightness     = read maxBrightnessFileContents :: Int
    let currentBrightness = read brightnessFileContents    :: Int

    -- let newBrightness     = f currentBrightness maxBrightness args

    -- writeBrightness newBrightness
    putStrLn "some"


--
-- Flat change brightness calculator
--
-- flatChange :: Int -> (Int -> Int -> Int -> Int)
-- flatChange args =
    -- flatBrightnessDelta currentBrightness maxBrightness x

-- flatBrightnessDelta :: Int -> Int -> Int -> Int
-- flatBrightnessDelta current max delta
    -- | (newValue == 0) || (newValue == max) = newValue
    -- | (0 < newValue)  && (newValue < max)  = newValue
    -- | newValue > max                       = max
    -- | newValue < 0                         = 0
    -- where newValue = current + delta


-- 
-- Step change brightness calculator, using a gradual increase appropriate for Thinkpad screen's
-- 0-3000 brightness in 15 steps.
--
gradualStepChange :: Int -> Int -> Int -> Int
gradualStepChange current max delta =
    calcStep $ closestStep current + delta

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

