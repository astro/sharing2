module Main where

import Application


main :: IO ()
main = run Settings 
       { settingsPort = 8000
       }

  