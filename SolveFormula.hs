module SolveFormula where

import Data.Tree ( Tree(Node) )
import Data.Set ( Set, filter, null, member, findMin, deleteMin, size, singleton, fromList )
import BaseData ( Formula(Atom, Not, And, Or, Imply, Equiv), SetStatus(IsFormula, Open, Closed), Tableaux )

isAtom :: Formula -> Bool
isAtom (Atom _) = True
isAtom (Not (Atom _)) = True
isAtom _ = False

allAtoms :: Set Formula -> Bool
allAtoms sp = Data.Set.null (Data.Set.filter (not . isAtom) sp)

oposite :: Formula -> Formula
oposite (Not p) = p
oposite p = Not p

hasContradiction :: Set Formula -> Bool
hasContradiction sp
    | size sp <= 1 = False
    | otherwise = do
      let min = findMin sp
      let setWithoutMin = deleteMin sp
      member (oposite min) setWithoutMin || hasContradiction setWithoutMin

getStatus :: Set Formula -> SetStatus
getStatus sp
    | hasContradiction sp = Closed
    | (not . allAtoms) sp = IsFormula
    | otherwise = Open

isNodeClosed :: Tableaux -> Bool
isNodeClosed (Node (f, b) children) = b

solve :: Formula -> [Set Formula]
solve (And p q) = [fromList [p, q]]
solve (Not (Or p q)) = [fromList [Not p, Not q]]
solve (Not (Imply p q)) = [fromList [p, Not q]]
solve (Not (Not p)) = [fromList [p]]
solve (Not (And p q)) = map singleton [Not p, Not q]
solve (Not (Equiv p q)) = map singleton [Not (Imply p q), Not (Imply q p)]
solve (Equiv p q) = [fromList [Imply p q, Imply q p]]
solve (Or p q) = map singleton [p, q]
solve (Imply p q) = map singleton [Not p, q]
solve (Not p) = map singleton [Not p]
solve p = map singleton [p]
