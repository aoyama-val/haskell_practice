-- 1. 役のランクで決める
-- 2. 役を構成するカードの数字で決める
-- 3. 役を構成しないカードの数字で決める

import Data.List
import qualified Data.Char as Char

type CardNum = Int
type Mark    = Char
type Pairs   = [(CardNum, Int)]

data Card = Card { num :: CardNum, mark :: Mark } deriving (Show, Eq)

instance Ord Card where
    (Card n m) `compare` (Card n2 m2) = n `compare` n2

data Rank = HighCard | OnePair | TwoPairs | ThreeCard | Straight | Flush | FullHouse | FourCard | StraightFlush | RoyalFlush deriving (Show)

data Hand = Hand { rank :: Rank, pairs :: Pairs } deriving (Show)

rankToInt :: Rank -> Int
rankToInt HighCard      = 0
rankToInt OnePair       = 1
rankToInt TwoPairs      = 2
rankToInt ThreeCard     = 3
rankToInt Straight      = 4
rankToInt Flush         = 5
rankToInt FullHouse     = 6
rankToInt FourCard      = 7
rankToInt StraightFlush = 8
rankToInt RoyalFlush    = 9

parseLine :: String -> ([Card], [Card])
parseLine line =
    let splited = words line
        p1 = parseCards (take 5 splited)
        p2 = parseCards (drop 5 splited)
    in  (p1, p2)

parseCard :: String -> Card
parseCard (n:m:xs) =
    let num = case n of
                'T' -> 10
                'J' -> 11
                'Q' -> 12
                'K' -> 13
                'A' -> 14
                n   -> (Char.ord n) - 48
    in Card num m

parseCards :: [String] -> [Card]
parseCards cards = map parseCard cards

parseOnePlayer :: String -> [Card]
parseOnePlayer line = parseCards $ words line

pairOnePlayer :: String -> Pairs
pairOnePlayer s = pair $ fst $ parseLine s

pair :: [Card] -> Pairs
pair cards = reverse $ sortBy (\p1 -> \p2 -> pairRank p1 `compare` pairRank p2) $ map (\xs -> (head xs, length xs)) (group $ sort $ map num cards)

isOnePair :: Pairs -> Bool
isOnePair pairs = any (== 2) (map snd pairs)

isTwoPair :: Pairs -> Bool
isTwoPair pairs = length (filter (== 2) (map snd pairs)) == 2

isThreeCard :: Pairs -> Bool
isThreeCard pairs = length (filter (== 3) (map snd pairs)) == 1

isFourCard :: Pairs -> Bool
isFourCard pairs = length (filter (== 4) (map snd pairs)) == 1

isFlush :: [Card] -> Bool
isFlush cards = all (== (mark (head cards))) (map mark cards)

isStraight :: Pairs -> Bool
isStraight pairs = (length pairs == 5) && (all (\i -> (fst (pairs !! 0) - fst (pairs !! i) == i)) [0..4])

isRoyalFlush cards =
    let pairs = pair cards
    in  (isFlush cards) && (isStraight pairs) && (fst $ head pairs) == 14

isStraightFlush cards =
    let pairs = pair cards
    in  (isFlush cards) && (isStraight pairs)

isFullHouse pairs = isThreeCard pairs && isOnePair pairs

pairRank :: (CardNum, Int) -> Int
pairRank p = (snd p * 1000 + fst p)
 
getRank :: [Card] -> Hand
getRank cards =
    let p = (pair cards)
    in  case p of
            _ | isRoyalFlush cards  -> Hand RoyalFlush p
            _ | isStraightFlush cards    -> Hand StraightFlush p
            _ | isFourCard p        -> Hand FourCard p
            _ | isFullHouse p       -> Hand FullHouse p
            _ | isFlush cards       -> Hand Flush p
            _ | isStraight p        -> Hand Straight p
            _ | isThreeCard p       -> Hand ThreeCard p
            _ | isTwoPair p         -> Hand TwoPairs p
            _ | isOnePair p         -> Hand OnePair p
              | otherwise           -> Hand HighCard p

assert :: Int -> Bool -> String
assert n b =
    if b
        then (show n) ++ " OK"
        else (show n) ++ " NG"

test = do
    putStrLn $ assert  1 ((isOnePair  $ pairOnePlayer "5H 5C 6S 7S KD") == True)
    putStrLn $ assert  2 ((isOnePair  $ pairOnePlayer "2C 3S 8S 8D TD") == True)
    putStrLn $ assert  3 ((isOnePair  $ pairOnePlayer "5D 8C 9S JS AC") == False)
    putStrLn $ assert  4 ((isOnePair  $ pairOnePlayer "2C 5C 7D 8S QH") == False)
    putStrLn $ assert  5 ((isTwoPair  $ pairOnePlayer "QD 6S 6H QH 3C") == True)
    putStrLn $ assert  6 ((isTwoPair  $ pairOnePlayer "4D 6S 9H QH QC") == False)
    putStrLn $ assert  7 ((isThreeCard$ pairOnePlayer "4D 4S 9H 4H QC") == True)
    putStrLn $ assert  8 ((isThreeCard$ pairOnePlayer "4D 6S 9H QH QC") == False)
    putStrLn $ assert  9 ((isFourCard $ pairOnePlayer "QD 6S QH QH QC") == True)
    putStrLn $ assert 10 ((isFourCard $ pairOnePlayer "4D 6S 9H QH QC") == False)
    putStrLn $ assert 11 (((pairOnePlayer "5H 5C 6S 7S KD") < (pairOnePlayer "2C 3S 8S 8D TD")))    -- hand 1
    putStrLn $ assert 12 (((pairOnePlayer "5D 8C 9S JS AC") > (pairOnePlayer "2C 5C 7D 8S QH")))    -- hand 2
    putStrLn $ assert 13 (((pairOnePlayer "4D 6S 9H QH QC") > (pairOnePlayer "3D 6D 7H QD QS")))    -- hand 4
    putStrLn $ assert 13 (((pairOnePlayer "2H 2D 4C 4D 4S") > (pairOnePlayer "3C 3D 3S 9S 9D")))    -- hand 5
    putStrLn $ assert 14 ((isFlush $ parseOnePlayer "3D 6D 7D TD QD") == True)
    putStrLn $ assert 15 ((isFlush $ parseOnePlayer "3S 6D 7D TD QD") == False)
    putStrLn $ assert 16 ((isFlush $ parseOnePlayer "3D 6D 7D TD QS") == False)
    putStrLn $ assert 17 ((isStraight $ pairOnePlayer "3D 4D 5S 6C 7D") == True)
    putStrLn $ assert 18 ((isStraight $ pairOnePlayer "3D 4D 5S 6C 8D") == False)
    putStrLn $ assert 19 ((isStraight $ pairOnePlayer "2D 4D 5S 6C 8D") == False)
    putStrLn $ assert 20 ((isRoyalFlush $ parseOnePlayer "TD JD QD KD AD") == True)
    putStrLn $ assert 21 ((isRoyalFlush $ parseOnePlayer "9D TD JD QD KD") == False)
    putStrLn $ assert 22 ((isRoyalFlush $ parseOnePlayer "TC JD QD KD AD") == False)
    putStrLn $ assert 23 ((isStraightFlush $ parseOnePlayer "TD JD QD KD AD") == True)
    putStrLn $ assert 24 ((isStraightFlush $ parseOnePlayer "9D TD JD QD KD") == True)
    putStrLn $ assert 25 ((isFullHouse $ pairOnePlayer "2H 2D 4C 4D 4S") == True)
    putStrLn $ assert 26 ((isFullHouse $ pairOnePlayer "3C 3D 3S 9S 9D") == True)

isWinner1 :: ([Card], [Card]) -> Bool
isWinner1 (cards1, cards2) =
    let r1 = getRank cards1
        r2 = getRank cards2
        p1 = pairs r1
        p2 = pairs r2
    in  case 1 of
            _ | rankToInt (rank r1) > rankToInt (rank r2) -> True
              | rankToInt (rank r1) == rankToInt (rank r2) -> (p1 > p2)
              | otherwise -> False

main = do
    test
    c <- readFile "p054_poker.txt"
    let hands = map parseLine (lines $ c)
    --putStrLn $ unlines $ map (\(cards1, cards2) -> show (rank $ getRank cards1, rank $ getRank cards2)) hands
    print $ length (filter isWinner1 hands)
    

