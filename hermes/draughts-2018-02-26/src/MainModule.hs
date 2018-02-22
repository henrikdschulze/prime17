{-
    This module contains 'main' function.
-}
module MainModule(m) where

import Draughts

-- | The 'main' function asks the user what size of 'Draughts' to play.
m :: IO()
m = do
  playDraughts
--   putStrLn $ "Enter 8 to play Draughts 8x8"
--   putStrLn $ "Enter 0 to Exit"
--   x <- getLine
--   let i = read x
--   if i == 8 then playDraughts else
--     if i == 0 then putStrLn ("Thank you for playing! :-)") else do
--     putStrLn $ "Invalid Input"
--     main
