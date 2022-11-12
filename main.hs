{-# LANGUAGE InstanceSigs #-}
import Data.Tree ( Tree(Node), drawTree )
import Data.Maybe ()
import Data.List ( find )
import qualified Data.Set as Set
import           Data.Set (Set)

data Formula = Atom !Char !(Maybe Bool)
             | Not !Formula
             | And !Formula !Formula
             | Or !Formula !Formula
             | Imply !Formula !Formula
             | Equiv !Formula !Formula
             deriving (Eq, Ord)

data TabRes = TabRes !Bool !Char deriving (Eq, Ord)

data TreeF = Empty | NodeF {
    formula :: !Formula,
    left :: !TreeF,
    right :: !TreeF
} deriving (Eq, Ord)

getRightValueToPrinta :: Maybe Bool -> String
getRightValueToPrinta (Just True) = "=T"
getRightValueToPrinta (Just False) = "=F"
getRightValueToPrinta Nothing = ""

instance Show Formula where
    show :: Formula -> String
    show (Atom c v) = c : getRightValueToPrinta v
    show (Not f) = "(!" ++ show f ++ ")"
    show (And f g) = "(" ++ show f ++ " /\\ " ++ show g ++ ")"
    show (Or f g) = "(" ++ show f ++ " \\/ " ++ show g ++ ")"
    show (Imply f g) = "(" ++ show f ++ " -> " ++ show g ++ ")"
    show (Equiv f g) = "(" ++ show f ++ " <-> " ++ show g ++ ")"

makeTreeF :: Formula -> TreeF
makeTreeF (Atom c v) = NodeF (Atom c v) Empty Empty
makeTreeF (Not f) = NodeF (Not f) (makeTreeF f) Empty
makeTreeF (And f g) = NodeF (And f g) (makeTreeF f) (makeTreeF g)
makeTreeF (Or f g) = NodeF (Or f g) (makeTreeF f) (makeTreeF g)
makeTreeF (Imply f g) = NodeF (Imply f g) (makeTreeF f) (makeTreeF g)
makeTreeF (Equiv f g) = NodeF (Equiv f g) (makeTreeF f) (makeTreeF g)

findCharInTabRes :: Set TabRes -> Char -> Maybe TabRes
findCharInTabRes s c = find (\(TabRes _ v) -> v == c) (Set.toList s)

getTabResValue :: Maybe TabRes -> Maybe Bool
getTabResValue (Just (TabRes v _)) = Just v
getTabResValue Nothing = Nothing

readCharInMaybeTabRes :: Set TabRes -> Char -> Maybe Bool
readCharInMaybeTabRes s c = getTabResValue(findCharInTabRes s c)

makeTreeFFixed :: Formula -> Set TabRes -> TreeF
makeTreeFFixed (Atom c v) st = NodeF (Atom c (readCharInMaybeTabRes st c)) Empty Empty
makeTreeFFixed (Not f) st = NodeF (Not f) (makeTreeFFixed f st) Empty
makeTreeFFixed (And f g) st = NodeF (And f g) (makeTreeFFixed f st) (makeTreeFFixed g st)
makeTreeFFixed (Or f g) st = NodeF (Or f g) (makeTreeFFixed f st) (makeTreeFFixed g st)
makeTreeFFixed (Imply f g) st = NodeF (Imply f g) (makeTreeFFixed f st) (makeTreeFFixed g st)
makeTreeFFixed (Equiv f g) st = NodeF (Equiv f g) (makeTreeFFixed f st) (makeTreeFFixed g st)

convertTreeFToTree :: TreeF -> Tree String
convertTreeFToTree Empty = Node "" []
convertTreeFToTree (NodeF f Empty Empty) = Node (show f) []
convertTreeFToTree (NodeF f l Empty) = Node (show f) [convertTreeFToTree l]
convertTreeFToTree (NodeF f Empty r) = Node (show f) [convertTreeFToTree r]
convertTreeFToTree (NodeF f l r) = Node (show f) [convertTreeFToTree l, convertTreeFToTree r]

isTrue :: TabRes -> TabRes -> Bool
isTrue (TabRes b1 n1) (TabRes b2 n2) = b1 == b2 || n1 /= n2

areAllTrue :: [TabRes] -> Bool
areAllTrue [] = False
areAllTrue [_] = True
areAllTrue (x:xs) = all (isTrue x) xs && areAllTrue xs

setFlatMap :: Ord a => (a -> Set a) -> Set a -> Set a
setFlatMap f = foldl (\accum e -> Set.union accum (f e)) Set.empty

solve :: Formula -> Set (Set TabRes)
solve (Atom name _) = Set.singleton(Set.singleton (TabRes True name))
solve (Not (Atom name _)) = Set.singleton(Set.singleton (TabRes False name))
solve (Not (Not e)) = solve e
solve (Not (And e1 e2)) = solve $ Or  (Not e1) (Not e2)
solve (Not (Or  e1 e2)) = solve $ And (Not e1) (Not e2)
solve (Not (Imply f1 f2)) = solve $ And f1 (Not f2)
solve (Not (Equiv f1 f2)) = solve $ Or (And f1 (Not f2)) (And (Not f1) f2)
solve (Or  e1 e2) = Set.union (solve e1) (solve e2)
solve (And f1 f2) = let
  s1 = solve f1
  s2 = solve f2
  in setFlatMap (\r1 ->
    setFlatMap(\r2 ->
      let union = Set.union r1 r2
      in if areAllTrue (Set.toList union)
        then Set.singleton union
        else Set.empty
    ) s2
  ) s1
solve (Imply f1 f2) = solve $ Or (Not f1) f2
solve (Equiv f1 f2) = solve $ And (Imply f1 f2) (Imply f2 f1)

findLongestSet :: Set (Set TabRes) -> Set TabRes
findLongestSet = foldl (\accum e -> if Set.size e > Set.size accum then e else accum) Set.empty

printTree :: Set (Set TabRes) -> Formula -> Tree String -> TreeF -> IO ()
printTree setSetado formula tree treeF = do
  if not (null setSetado)
    then do
      let solved = findLongestSet setSetado
      let solvedTree = makeTreeFFixed formula solved
      let solvedBaseTree = convertTreeFToTree solvedTree
      putStrLn $ drawTree solvedBaseTree
      putStrLn "Formula resolvida com sucesso"
    else do
      let unSolvedTree = makeTreeF formula
      let unSolvedBaseTree = convertTreeFToTree unSolvedTree
      putStrLn $ drawTree unSolvedBaseTree
      putStrLn "Não é possível resolver a fórmula"

main :: IO ()
main = do
    let formula = (Atom 'p' Nothing `Imply` (Atom 'q' Nothing `Or` (Atom 'p' Nothing `Equiv` Not (Atom 'r' Nothing)))) `Imply` (Atom 'p' Nothing `And` Not (Atom 'q' Nothing `And` Atom 'r' Nothing))
    let semiSolved = solve formula

    printTree semiSolved formula (convertTreeFToTree (makeTreeF formula)) (makeTreeF formula)
