#!/usr/bin/runhaskell

{-
dotfiles - Personal configuration files and scripts
Copyright (C) 2025  Zach Podbielniak

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}


import Data.Time (getCurrentTime, utctDay)
import Data.Time.Format (formatTime, defaultTimeLocale)


main :: IO () 
main = do 
    currentDate <- getCurrentDate
    putStrLn $ journalTemplate currentDate



getCurrentDate :: IO String 
getCurrentDate = do 
    currentTime <- getCurrentTime
    let currentDay = utctDay currentTime 
    return $ formatTime defaultTimeLocale "%Y-%m-%d" currentDay 


journalTemplate :: String -> String 
journalTemplate date = 
    unlines 
        [ "# daily journal (" ++ date ++ ")"
        , "## what were today's metrics?"
        , "- wakeup time: "
        , "- weight: "
        , "- food: "
        , "- drink: "
        , "- excercise: "
        , "- feeling: /10"
        , "- energy: /10"
        , ""
        , "## what did i do today?"
        , ""
        , "## what am i grateful for?"
        , ""
        , "## what am i doing really well with right now?"
        , ""
        , "## what is one thing i am not doing the best with?"
        , ""
        , "## what would be one thing i would change today if i could?"
        , ""
        , "## what would i like my life to look like:"
        , "### in 1 month"
        , ""
        , "### in 3 months"
        , ""
        , "### in 1 year"
        , ""
        , "## other notable things"
        ]
