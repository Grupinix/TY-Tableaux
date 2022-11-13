import Data.Tree ( drawTree )

import BaseData ( Formula(Atom, Not, And, Or, Imply, Equiv) )
import SolveFormula ( solve, findLongestSet, canBeSolved )
import TableauxTree ( makeTreeF, makeTreeFFixed, convertTreeFToTree )


main :: IO ()
main = do
    let formula = (Atom 'p' Nothing `Imply` (Atom 'q' Nothing `Or` (Atom 'p' Nothing `Equiv` Not (Atom 'r' Nothing)))) `And` (Atom 'p' Nothing `And` (Atom 'q' Nothing `And` Atom 'r' Nothing))

    if canBeSolved formula
      then do
        let solved = findLongestSet (solve formula)
        let solvedTree = makeTreeFFixed formula solved
        let solvedBaseTree = convertTreeFToTree solvedTree
        putStrLn $ drawTree solvedBaseTree
        putStrLn "Formula resolvida com sucesso"
      else do
        let unSolvedTree = makeTreeF formula
        let unSolvedBaseTree = convertTreeFToTree unSolvedTree
        putStrLn $ drawTree unSolvedBaseTree
        putStrLn "Não é possível resolver a fórmula"
