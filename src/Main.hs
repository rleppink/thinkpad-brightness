module Main where


import System.Environment
import System.Exit


main :: IO ()
main = getArgs >>= parse >>= putStrLn


parse :: [String] -> IO String
parse ["-v"]     = version                                              >> exitSuccess

parse ["-i"]     = needsArgument "-i"                                   >> exitSuccess
parse ["-d"]     = needsArgument "-d"                                   >> exitSuccess
parse ["-w"]     = needsArgument "-w"                                   >> exitSuccess

parse ["-g"]     = printBrightness                                      >> exitSuccess
parse ["-w", x]  = writeBrightness (read x :: Int)                      >> exitSuccess

parse ["-t"]     = changeBrightness toggleStep                          >> exitSuccess

parse ["-i", x]  = changeBrightness (flatStep (read x :: Int))          >> exitSuccess
parse ["-d", x]  = changeBrightness (flatStep (negate $ read x :: Int)) >> exitSuccess

parse ["-i", "--gradual", x] =
    changeBrightness (gradualStep (read x :: Int))                      >> exitSuccess
parse ["-d", "--gradual", x] =
    changeBrightness (gradualStep (negate $ read x :: Int))             >> exitSuccess

parse ["-i", "--binary", x] =
    changeBrightness (binaryStep (read x :: Int))                       >> exitSuccess
parse ["-d", "--binary", x] =
    changeBrightness (binaryStep (negate $ read x :: Int))              >> exitSuccess

parse []         = usage                                                >> exitSuccess
parse ["-h"]     = usage                                                >> exitSuccess
parse _          = usage                                                >> exitSuccess


backlightFilePath :: String -> String
backlightFilePath filename  = "/sys/class/backlight/intel_backlight/" ++ filename

brightnessFilePath :: String
brightnessFilePath = backlightFilePath "brightness"

maxBrightnessFilePath :: String
maxBrightnessFilePath = backlightFilePath "max_brightness"

printBrightness :: IO ()
printBrightness = putStrLn =<< readFile brightnessFilePath

writeBrightness :: Int -> IO ()
writeBrightness x = writeFile brightnessFilePath $ show x

needsArgument :: String -> IO ()
needsArgument x = putStrLn $ x ++ " needs an argument"

usage :: IO ()
usage = putStrLn "Usage: tpb [-idw] [--calculator] [amount]"

version :: IO ()
version = putStrLn "Thinkpad Brightness v0.2"

changeBrightness :: (Int -> Int) -> IO ()
changeBrightness f = do
    maxBrightnessFileContents <- readFile maxBrightnessFilePath
    brightnessFileContents    <- readFile brightnessFilePath

    -- Hacky solution to Haskell's lazy evaluation
    -- Force readFile to read the entire file, before trying to write to it later
    seq (length brightnessFileContents) (return())

    let maxBrightness     = read maxBrightnessFileContents :: Int
    let currentBrightness = read brightnessFileContents    :: Int

    let newBrightness = sanitizeBrightness maxBrightness $ f currentBrightness

    writeBrightness newBrightness

sanitizeBrightness :: Int -> Int -> Int
sanitizeBrightness maxB new
    | (new == 0) || (new == maxB) = new
    | (new >  0) && (new <  maxB) = new
    | new > maxB                  = maxB
    | new < 0                     = 0
    | otherwise                   = 0


-- Toggle brightness calculator
toggleStep :: Int -> Int
toggleStep current
  | current == 0 = 3000
  | otherwise    = 0


-- Flat step brightness calculator
-- Just add the delta to the current brightness
flatStep :: Int -> Int -> Int
flatStep delta current = current + delta


-- Gradual step brightness calculator
-- Use a gradual increase appropriate for Thinkpad screen's 0-3000 brightness in 16 steps
gradualStep :: Int -> Int -> Int
gradualStep delta current
  | step <= 0 = 0
  | otherwise = calcGradualBrightness step
      where step = closestGradualStep current + delta

calcGradualBrightness :: Int -> Int
calcGradualBrightness 0 = 0
calcGradualBrightness 1 = 1
calcGradualBrightness x = round $ (1.8688 :: Double)  * (1.6357 ^ x)

closestGradualStep :: Int -> Int
closestGradualStep 0 = 0
closestGradualStep 1 = 1
closestGradualStep 2 = 1
closestGradualStep x = round $ logBase (1.6357 :: Double) (fromIntegral x / 1.8688)


-- Binary step brightness calculator.
-- 1, 2, 4, 8 etc...
binaryStep :: Int -> Int -> Int
binaryStep delta current
  | current == 0 && delta ==  1 = 1
  | current == 2 && delta == -1 = 1
  | nextStep + delta < 0        = 0
  | otherwise                   = 2 ^ nextStep
  where nextStep = closestBinaryStep current + delta

closestBinaryStep :: Int -> Int
closestBinaryStep x
  | x == 0    = 0
  | otherwise = round (logBase (2 :: Double) (fromIntegral x))
