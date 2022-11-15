module BaseData where


data Formula = Atom !Char !(Maybe Bool)
             | Not !Formula
             | And !Formula !Formula
             | Or !Formula !Formula
             | Imply !Formula !Formula
             | Equiv !Formula !Formula
             deriving (Eq, Ord)

data BranchResult = BranchResult !Bool !Char deriving (Eq, Ord)

data TreeF = Empty | NodeF {
    formula :: !Formula,
    left :: !TreeF,
    right :: !TreeF
} deriving (Eq, Ord)
