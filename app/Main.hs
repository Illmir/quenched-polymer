{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment (getArgs)
import Control.Monad (forM_)
import Lib (runIsingSimulation)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [nStr, tempStr] -> do
      let n = read nStr :: Int
          temp = read tempStr :: Double
      putStrLn $ "Running Ising simulation with N=" ++ show n ++ ", T=" ++ show temp
      runIsingSimulation n temp
    _ -> do
      putStrLn "Usage: ising-simulation <N> <temperature>"
      putStrLn "  N           - Lattice size (NxN grid)"
      putStrLn "  temperature - Simulation temperature"
