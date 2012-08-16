module Main where

import Application


main = run Settings 
       { settingsPort = 8000
       }

  