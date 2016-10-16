module LogAnalysis where
import Log 
-- ex1
parseMessage :: String -> LogMessage
parseMessage str = let wordList = words str in 
    case wordList of
        ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
        ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
        ("E":val:ts:msg) -> LogMessage (Error (read val)) (read ts) (unwords msg)
        _ -> Unknown (unwords wordList)


parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- ex2
insert :: LogMessage -> MessageTree -> MessageTree
insert log@LogMessage{} Leaf = Node Leaf log Leaf
insert log1@(LogMessage _ val1 _) (Node left log2@(LogMessage _ val2 _ ) right)
    | val1 > val2 = Node left log2 (insert log1 right)
    | otherwise = Node (insert log1 left) log2 right
insert _ tree = tree 

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf    

inorder :: MessageTree -> [LogMessage]
inorder Leaf = []
inorder (Node left lmsg right) = inorder left ++ [lmsg] ++ inorder right


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = extractMessage . inorder . build . filter (severe 50)

severe :: Int -> LogMessage -> Bool
severe minLv1 (LogMessage (Error lvl) _ _ )
    | lvl > minLv1 = True
    | otherwise = False
severe _ _ = False

extractMessage :: [LogMessage] -> [String]
extractMessage (LogMessage _ _ msg : msgs) = msg :extractMessage msgs    
extractMessage _  = []

-- ex3


