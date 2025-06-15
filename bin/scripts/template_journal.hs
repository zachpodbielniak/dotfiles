#!/usr/bin/runhaskell

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
