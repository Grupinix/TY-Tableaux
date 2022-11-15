module TableauxTree where

import Data.Tree ( Tree(Node) )
import Data.List ( find )
import Data.Set ( Set, toList )
import BaseData ( TreeF(NodeF, Empty), Formula(..), BranchResult (..) ) 


instance Show Formula where
    show (Atom c v) = c : getRightValueToPrinta v
    show (Not f) = "(!" ++ show f ++ ")"
    show (And f g) = "(" ++ show f ++ " /\\ " ++ show g ++ ")"
    show (Or f g) = "(" ++ show f ++ " \\/ " ++ show g ++ ")"
    show (Imply f g) = "(" ++ show f ++ " -> " ++ show g ++ ")"
    show (Equiv f g) = "(" ++ show f ++ " <-> " ++ show g ++ ")"

getRightValueToPrinta :: Maybe Bool -> String
getRightValueToPrinta (Just True) = "=T"
getRightValueToPrinta (Just False) = "=F"
getRightValueToPrinta Nothing = ""

makeTreeF :: Formula -> TreeF
makeTreeF (Atom c v) = NodeF (Atom c v) Empty Empty
makeTreeF (Not f) = NodeF (Not f) (makeTreeF f) Empty
makeTreeF (And f g) = NodeF (And f g) (makeTreeF f) (makeTreeF g)
makeTreeF (Or f g) = NodeF (Or f g) (makeTreeF f) (makeTreeF g)
makeTreeF (Imply f g) = NodeF (Imply f g) (makeTreeF f) (makeTreeF g)
makeTreeF (Equiv f g) = NodeF (Equiv f g) (makeTreeF f) (makeTreeF g)

findCharInBranchResult :: Set BranchResult -> Char -> Maybe BranchResult
findCharInBranchResult s c = find (\(BranchResult _ v) -> v == c) (toList s)

getBranchResultValue :: Maybe BranchResult -> Maybe Bool
getBranchResultValue (Just (BranchResult v _)) = Just v
getBranchResultValue Nothing = Nothing

readCharInMaybeBranchResult :: Set BranchResult -> Char -> Maybe Bool
readCharInMaybeBranchResult s c = getBranchResultValue(findCharInBranchResult s c)

makeTreeFFixed :: Formula -> Set BranchResult -> TreeF
makeTreeFFixed (Atom c v) st = NodeF (Atom c (readCharInMaybeBranchResult st c)) Empty Empty
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
