module TableauxTree where

import SolveFormula ( solve, getStatus, isAtom, isNodeClosed )
import BaseData ( Formula(Atom, Not, And, Or, Imply, Equiv) , Tableaux, NodeType, SetStatus(IsFormula, Open, Closed) )

import Data.Set ( toList, Set, partition, findMin, delete, union )
import Data.List ( intercalate )
import Data.Tree ( Tree(Node), drawTree )
import Data.ByteString (intercalate)


instance Show Formula where
    show (Atom c) = c : ""
    show (Not f) = "(!" ++ show f ++ ")"
    show (And f g) = "(" ++ show f ++ " /\\ " ++ show g ++ ")"
    show (Or f g) = "(" ++ show f ++ " \\/ " ++ show g ++ ")"
    show (Imply f g) = "(" ++ show f ++ " -> " ++ show g ++ ")"
    show (Equiv f g) = "(" ++ show f ++ " <-> " ++ show g ++ ")"

formulaListToStringList :: [Formula] -> [String]
formulaListToStringList = map show

listToString :: [Formula] -> String
listToString f = Data.List.intercalate ", " (formulaListToStringList f)

showNode :: NodeType -> String
showNode (sp, res) = listToString (toList sp) ++ if res then " {x}" else " {0}"

showTableaux :: Tableaux -> String
showTableaux = drawTree . fmap showNode

makeTree :: Set Formula -> Tableaux
makeTree sp = do
    let (nonAtoms, _) = partition (not . isAtom) sp
    let min = findMin nonAtoms
    let setAux = delete min sp
    let res = map (makeTree . union setAux) (solve min)
    let allClosed = all isNodeClosed res
    
    case getStatus sp of
      Open -> Node (sp, False) []
      Closed -> Node (sp, True) []
      IsFormula -> Node (sp, allClosed) res
