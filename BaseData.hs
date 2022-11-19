module BaseData where

import Data.Set (Set)
import Data.Tree (Tree)


data Formula = Atom !Char
             | Not !Formula
             | And !Formula !Formula
             | Or !Formula !Formula
             | Imply !Formula !Formula
             | Equiv !Formula !Formula
             deriving (Eq, Ord)

data TreeF = Empty | NodeF {
    formula :: !Formula,
    left :: !TreeF,
    right :: !TreeF
} deriving (Eq, Ord)

data SetStatus = IsFormula | Open | Closed deriving (Eq, Show)

type NodeType = (Set Formula, Bool)
type Tableaux = Tree NodeType
