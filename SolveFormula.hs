module SolveFormula where
    
import Data.Set ( Set, toList, union, empty, singleton, size )
import BaseData ( Formula(Atom, Not, And, Or, Imply, Equiv) )


isConsistent :: Formula -> Formula -> Bool
isConsistent (Atom n1 b1) (Atom n2 b2 ) = b1 == b2 || n1 /= n2
isConsistent f g = False

isListConsistent :: [Formula] -> Bool
isListConsistent [] = False
isListConsistent [_] = True
isListConsistent (x:xs) = all (isConsistent x) xs && isListConsistent xs

setFlatMap :: Ord a => (a -> Set a) -> Set a -> Set a
setFlatMap f = foldr (union . f) empty

solve :: Formula -> Set (Set Formula)
solve (Atom name _) = singleton(singleton (Atom name (Just True)))
solve (Not (Atom name _)) = singleton(singleton (Atom name (Just False)))
solve (Not (Not f)) = solve f
solve (Not (And f g)) = solve (Or (Not f) (Not g))
solve (Not (Or f g)) = solve (And (Not f) (Not g))
solve (Not (Imply f g)) = solve (And f (Not g))
solve (Not (Equiv f g)) = solve (Or (And f (Not g)) (And (Not f) g))
solve (Imply f g) = solve (Or (Not f) g)
solve (Equiv f g) = solve (And (Imply f g) (Imply f g))
solve (Or f g) = solve f `union` solve g
solve (And f g) = let
  f1 = solve f
  g1 = solve g
  in setFlatMap (\r1 ->
    setFlatMap(\r2 ->
      let joined = r1 `union` r2
      in if isListConsistent (toList joined)
        then singleton joined
        else empty
    ) g1
  ) f1

findLongestSet :: Set (Set Formula) -> Set Formula
findLongestSet = foldr (\accum e -> if size e > size accum then e else accum) empty

canBeSolved :: Formula -> Bool
canBeSolved f = not (null (solve f))
