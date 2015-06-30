{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
-- import Control.Exception

parseMessage :: String -> LogMessage
parseMessage msg = case (words msg) of
    ("I" : timestamp : xs) -> LogMessage Info (read timestamp) (unwords xs)
    ("W" : timestamp : xs) -> LogMessage Warning (read timestamp) (unwords xs)
    ("E" : severity : timestamp : xs) -> LogMessage  (Error $ read severity) (read timestamp) (unwords xs)
    message -> Unknown $ unwords message

parse :: String -> [LogMessage]
parse logFile = parseMessage `fmap` lines logFile

-- insert based on timestamp.
insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(LogMessage _ _ _) Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts1 _) (Node mtreeL nodeMsg@(LogMessage _ ts2 _) mtreeR) 
    | ts1 < ts2 = Node (insert msg mtreeL) nodeMsg mtreeR
    | otherwise = Node mtreeL nodeMsg (insert msg mtreeR)
insert _ tree = tree

m1 = (parseMessage "I 29 la la la") : (parseMessage "I 39 la la la") : (parseMessage "I 19 la la la")  : []

-- build up a tree
build :: [LogMessage] -> MessageTree
-- build msgs = foldl (\tree msg -> insert msg tree) Leaf msgs
build [] = Leaf
build (x:xs) = insert x (build xs)

-- inorder traversal of the tree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node mTreeL msg mTreeR) = inOrder(mTreeL) ++ [msg] ++ inOrder(mTreeR)

-- returns errors with a severity of at least 50” sorted by timestamp
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = 
    let severeMsgs = filter (isHighSeverityError 50) msgs
        timeSortedMsgs = inOrder (build severeMsgs)
    in fmap getMessage timeSortedMsgs


-- true is severity is greater than severity and is an Error Message
isHighSeverityError :: Int -> LogMessage -> Bool
isHighSeverityError baseSeverity (LogMessage  (Error severity) _ _)
    | severity > baseSeverity = True
    | otherwise = False
isHighSeverityError _ _ = False 

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown msg) = msg