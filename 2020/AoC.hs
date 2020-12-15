module AoC
  ( testAndRun
  , testAndRun_
  , startTimer
  , printTimer
  , printTimerLn
  ) where

import System.Console.ANSI
import Control.Monad (void)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Control.DeepSeq (deepseq, NFData)

putBoldColor :: Color -> String -> IO ()
putBoldColor color s = do
  setSGR  [ SetConsoleIntensity BoldIntensity
          , SetColor Foreground Vivid color
          ]
  putStr s
  setSGR  [ Reset ]

putNormalColor :: Color -> String -> IO ()
putNormalColor color s = do
  setSGR  [ SetConsoleIntensity NormalIntensity
          , SetColor Foreground Vivid color
          ]
  putStr s
  setSGR  [ Reset ]

putGreen  = putBoldColor Green
putWhite  = putBoldColor White
putRed    = putBoldColor Red
putYellow = putBoldColor Yellow
putNormal = putNormalColor White

putSuccess :: IO ()
putSuccess = do
  putWhite "[ "
  putGreen "Success"
  putWhite " ] "

putFailure :: IO ()
putFailure = do
  putWhite "[ "
  putRed   "Failure"
  putWhite " ] "

prettyTime :: Integer -> String
prettyTime t
  | ms < 1    = printf "%.3f microseconds" micro
  | sec < 1   = printf "%.3f ms" ms
  | otherwise = printf "%.3f s" sec
  where sec   = pico / (10^12)
        ms    = pico / (10^9)
        pico  = fromIntegral t :: Double
        micro = pico / (10^6)

putResultAndTime result time = do
  putWhite $ show result
  putNormal " ("
  putYellow $ prettyTime time
  putNormal ")"

performTest :: (Eq b, Show b, NFData b) => (a->b) -> (a, b) -> IO Bool
performTest func (input, output) = do
  putYellow " * "

  start <- getCPUTime
  let result = func input
  end <- result `deepseq` getCPUTime

  if result == output then
    putSuccess
  else
    putFailure

  putResultAndTime result $ end - start

  putStrLn ""
  return $ result == output

-- only run actual calculation if all tests pass
testAndRun :: (Eq b, Show b, NFData b) => (a->b) -> [(a, b)] -> a -> IO (Maybe b)
testAndRun func tests actuali
  = do
    if not $ null tests then
      putStrLn "Running tests..."
    else
      putStrLn "No tests"

    results <- mapM (performTest func) tests
    putStrLn ""

    if and results then do
      putStrLn "Calculating..."
      start <- getCPUTime
      let result = func actuali
      end <- result `deepseq` getCPUTime

      putStrLn ""
      putResultAndTime result $ end - start
      putStrLn ""
      return $ Just result
    else do
      putRed "Some tests failed"
      putStrLn ""
      return Nothing

-- don't care about return value
testAndRun_ func tests actuali =
  void $ testAndRun func tests actuali

startTimer = getCPUTime

printTimer start = do
  end <- getCPUTime
  putStr $ prettyTime $ end - start

printTimerLn start = printTimer start >> putStrLn ""
