import System.IO ( hFlush, stdout )

import BaseData ( Formula(Atom, Not, And, Or, Imply, Equiv) )
import TableauxTree ( showTableaux, makeTree )
import Data.Set ( singleton )


getSeccondParametter :: String -> Int -> Int -> Int -> Int
getSeccondParametter [] _ _ virgula = virgula
getSeccondParametter string indice quantParenteses virgula = case string !! indice of
  '(' -> getSeccondParametter string (indice + 1) (quantParenteses + 1) virgula
  ',' -> if quantParenteses == 1 then getSeccondParametter string (indice + 1) quantParenteses indice else getSeccondParametter string (indice + 1) quantParenteses virgula
  ')' -> if quantParenteses == 1 then virgula else getSeccondParametter string (indice + 1) (quantParenteses - 1) virgula
  _ -> getSeccondParametter string (indice + 1) quantParenteses virgula


parseStringToFormula :: String -> Int -> Formula
parseStringToFormula string indice = case string !! indice of
  '>' -> Imply (parseStringToFormula string (indice + 2)) (parseStringToFormula string (getSeccondParametter string (indice + 1) 0 0))
  '-' -> Equiv (parseStringToFormula string (indice + 2)) (parseStringToFormula string (getSeccondParametter string (indice + 1) 0 0))
  'v' -> Or (parseStringToFormula string (indice + 2)) (parseStringToFormula string (getSeccondParametter string (indice + 1) 0 0))
  '^' -> And (parseStringToFormula string (indice + 2)) (parseStringToFormula string (getSeccondParametter string (indice + 1) 0 0))
  '!' -> Not (parseStringToFormula string (indice + 1))
  ',' -> parseStringToFormula string (indice + 1)
  c -> Atom c


applyRules :: Formula -> Formula
applyRules (Imply f g) = And f (Not g)
applyRules (Equiv f g) = Or (And f (Not g)) (And (Not f) g)
applyRules (And f g) = Or (Not f) (Not g)
applyRules (Or f g) = And (Not f) (Not g)
applyRules (Not f) = f
applyRules f = Not f


main :: IO ()
main = do
    putStr "Insira a formula que você deseja válidar: "
    hFlush stdout
    stringa <- getLine
    let formula = parseStringToFormula stringa 0

    putStrLn ""
    putStr "A formula inserida foi: "
    print formula
    putStrLn ""

    putStrLn "Montando a arvore de solução..."
    putStrLn ""
    let tableaux = (makeTree . singleton) (applyRules formula)

    putStrLn (showTableaux tableaux)
