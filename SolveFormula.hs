module SolveFormula where
    
import Data.Set ( Set, toList, union, empty, singleton, size )
import BaseData ( Formula(Atom, Not, And, Or, Imply, Equiv), TabRes(TabRes) )


isTrue :: TabRes -> TabRes -> Bool
isTrue (TabRes b1 n1) (TabRes b2 n2) = b1 == b2 || n1 /= n2

areAllTrue :: [TabRes] -> Bool
areAllTrue [] = False
areAllTrue [_] = True
areAllTrue (x:xs) = all (isTrue x) xs && areAllTrue xs

setFlatMap :: Ord a => (a -> Set a) -> Set a -> Set a
setFlatMap f = foldr (union . f) empty

solve :: Formula -> Set (Set TabRes)
solve (Atom name _) = singleton(singleton (TabRes True name))
solve (Not (Atom name _)) = singleton(singleton (TabRes False name))
solve (Not (Not e)) = solve e
solve (Not (And e1 e2)) = solve $ Or (Not e1) (Not e2)
solve (Not (Or  e1 e2)) = solve $ And (Not e1) (Not e2)
solve (Not (Imply f1 f2)) = solve $ And f1 (Not f2)
solve (Not (Equiv f1 f2)) = solve $ Or (And f1 (Not f2)) (And (Not f1) f2)
solve (Or  e1 e2) = solve e1 `union` solve e2
solve (And f1 f2) = let
  s1 = solve f1
  s2 = solve f2
  in setFlatMap (\r1 ->
    setFlatMap(\r2 ->
      let joined = r1 `union` r2
      in if areAllTrue (toList joined)
        then singleton joined
        else empty
    ) s2
  ) s1
solve (Imply f1 f2) = solve $ Or (Not f1) f2
solve (Equiv f1 f2) = solve $ And (Imply f1 f2) (Imply f2 f1)

findLongestSet :: Set (Set TabRes) -> Set TabRes
findLongestSet = foldr (\accum e -> if size e > size accum then e else accum) empty

canBeSolved :: Formula -> Bool
canBeSolved f = not (null (solve f))
