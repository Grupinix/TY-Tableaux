import Data.Tree ( drawTree )

import BaseData ( Formula(Atom, Not, And, Or, Imply, Equiv) )
import SolveFormula ( solve, findLongestSet, canBeSolved )
import TableauxTree ( makeTreeF, makeTreeFFixed, convertTreeFToTree )


getSeccondParametter :: String -> Int -> Int -> Int -> Int
getSeccondParametter [] _ _ virgula = virgula
getSeccondParametter string indice quantParenteses virgula = case string !! indice of
  '(' -> getSeccondParametter string (indice + 1) (quantParenteses + 1) virgula
  ',' -> if quantParenteses == 1 then getSeccondParametter string (indice + 1) quantParenteses indice else getSeccondParametter string (indice + 1) quantParenteses virgula
  ')' -> if quantParenteses == 1 then virgula else getSeccondParametter string (indice + 1) (quantParenteses - 1) virgula
  _ -> getSeccondParametter string (indice + 1) quantParenteses virgula


parseStringToFormula :: String -> Int -> Formula
parseStringToFormula string indice = case string !! indice of
  '>' -> Imply (parseStringToFormula string (indice + 2)) (parseStringToFormula string ((getSeccondParametter string (indice + 1) 0 0)))
  '-' -> Equiv (parseStringToFormula string (indice + 2)) (parseStringToFormula string ((getSeccondParametter string (indice + 1) 0 0)))
  'v' -> Or (parseStringToFormula string (indice + 2)) (parseStringToFormula string ((getSeccondParametter string (indice + 1) 0 0)))
  '^' -> And (parseStringToFormula string (indice + 2)) (parseStringToFormula string ((getSeccondParametter string (indice + 1) 0 0)))
  '!' -> Not (parseStringToFormula string (indice + 1))
  ',' -> parseStringToFormula string (indice + 1)
  c -> Atom c Nothing


main :: IO ()
main = do
    stringa <- getLine
    let formula = parseStringToFormula stringa 0

    if canBeSolved formula
      then do
        let solved = findLongestSet (solve formula)
        let solvedTree = makeTreeFFixed formula solved
        let solvedBaseTree = convertTreeFToTree solvedTree
        putStrLn $ drawTree solvedBaseTree
        putStrLn "Foi encontrado um ramo sem contradição"
      else do
        let unSolvedTree = makeTreeF formula
        let unSolvedBaseTree = convertTreeFToTree unSolvedTree
        putStrLn $ drawTree unSolvedBaseTree
        putStrLn "Há contradição em todos os ramos"
