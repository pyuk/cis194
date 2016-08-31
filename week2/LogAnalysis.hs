{- OPTIONS _GHC -Wall #-}
module LogAnalysis where

import Log

--exercise 1

parseHelp :: String -> Int
parseHelp = read . (!! 1) . words

parseHelp' :: String -> String
parseHelp' = unwords . drop 2 . words

parseMessage :: String -> LogMessage
parseMessage message@('I':_) = LogMessage Info (parseHelp message) (parseHelp' message)
parseMessage message@('W':_) = LogMessage Warning  (parseHelp message) (parseHelp' message)
parseMessage message@('E':m) = LogMessage (Error $ parseHelp message) (parseHelp m) (parseHelp' m)
parseMessage (message) = Unknown message

{-parseMessage (message) = case message of
                        ('I':m) -> LogMessage Info (parseHelp message) (parseHelp' message)
                        ('W':m) -> LogMessage Warning (parseHelp message) (parseHelp' message)
                        ('E':m) -> LogMessage (Error $ parseHelp message) (parseHelp m) (parseHelp' m) -}

parse :: String -> [LogMessage]
--parse logs = [parseMessage x | x <- (lines logs)]
parse logs = parse' $ lines logs
    where parse' [] = []
          parse' (x:l) = parseMessage x : parse' l
--parse logs = map parseMessage (lines logs)

--exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) Leaf = Leaf
insert log Leaf = Node Leaf log Leaf
insert log@(LogMessage _ y _) (Node a log1@(LogMessage _ y1 _) c)
    | y >= y1 = Node a log1 (insert log c)
    | y <  y1 = Node (insert log a) log1 c 

--exercise 3

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:logs) = insert x (build logs)

--exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node a log c) = inOrder a ++ [log] ++ inOrder c

--exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error x) _ z):rest)
    | x > 50    = z : whatWentWrong rest
    | otherwise = whatWentWrong rest
whatWentWrong ((LogMessage _ _ _):rest) = whatWentWrong rest
