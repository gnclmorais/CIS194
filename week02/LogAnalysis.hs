{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

msgType :: String -> String -> Maybe MessageType
msgType t o
 | t == "I" = Just Info
 | t == "W" = Just Warning
 | t == "E" = Just $ Error (read o)
 | otherwise = Nothing

timeStamp :: (Read timeStamp) => String -> timeStamp
timeStamp ts = read ts

parseMessage :: String -> LogMessage
parseMessage msg = 
    let tokens = words msg
        mt = msgType (tokens !! 0) (tokens !! 1)
        rest = unwords ((tail . tail) tokens)
    in
        case mt of
            Just t -> LogMessage t (timeStamp (tokens !! 1)) rest
            _ -> Unknown msg

main :: IO ()
main = do
    print (msgType "I" "8")
    print (parseMessage "I 29 la la la")
    print (parseMessage "This is not in the right format")
    -- LogMessage Info 29 "la la la"
