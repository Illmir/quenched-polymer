{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( runIsingSimulation
  ) where

import Control.Monad (forM_)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler.Strict (sampleIO)
import Control.Monad.Bayes.Inference.MCMC (mh)
import Numeric.Log (Log)
import Data.List (intercalate)
import System.IO (writeFile)

-- Interpret Bool as +/- 1 spin
boolToSpin :: Bool -> Int
boolToSpin b = if b then 1 else -1

-- 2D Ising energy, n x n spins, row-major
energy :: Int -> [Bool] -> Double
energy n spins =
  let spinAt r c = boolToSpin $ spins !! (r*n + c)
      pairs =
        [ spinAt r c * spinAt r ((c+1) `mod` n)
        | r <- [0..n-1], c <- [0..n-1] ]
        ++
        [ spinAt r c * spinAt ((r+1) `mod` n) c
        | r <- [0..n-1], c <- [0..n-1] ]
      eSum = fromIntegral (foldl' (+) 0 pairs)
  in negate eSum  -- E = - sum_{<i,j>} s_i s_j

-- monad-bayes model: each spin ~ Bernoulli(0.5), factor by exp(-E/T)
isingModel
  :: (MonadSample m, MonadFactor m)
  => Int    -- lattice size n
  -> Double -- temperature T
  -> m [Bool]
isingModel n t = do
  spins <- mapM (\_ -> bernoulli 0.5) [1..(n*n)]
  let e = energy n spins
  score (realToFrac $ negate e / t) -- factor = exp(-E/T)
  return spins

-- Helper: Write simulation steps to a CSV
writeSimulationToCSV :: FilePath -> [(Int, [Bool])] -> IO ()
writeSimulationToCSV path dataRows = do
  let csvRows = map (\(step, spins) ->
                      show step ++ "," ++ intercalate "," (map (show . boolToSpin) spins)
                    ) dataRows
      header = "Step," ++ intercalate "," [ "Spin_" ++ show i | i <- [1..length (snd (head dataRows))] ]
      content = unlines (header : csvRows)
  writeFile path content

-- Run Ising simulation with given N and temperature
runIsingSimulation :: Int -> Double -> IO ()
runIsingSimulation n temp = do
  let steps   = 10    -- number of MCMC samples
      csvPath = "ising_simulation.csv"
  putStrLn $ "Ising Model n=" ++ show n ++ ", T=" ++ show temp ++ ", steps=" ++ show steps
  chain <- sampleIO $ do
    fullChain <- mh (isingModel n temp)
    return (take steps fullChain)
  -- Pair each sample with its step number
  let dataRows = zip [1..] chain
  -- Write data to CSV
  writeSimulationToCSV csvPath dataRows
  putStrLn $ "Simulation data written to " ++ csvPath
