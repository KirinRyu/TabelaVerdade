module Exercicio31 where

data Pergunta = Sim | Nao deriving Show

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

listPergs :: [Pergunta] -> [Int]
listPergs xs = [ pergNum x | x <- xs]

and' :: Pergunta -> Pergunta -> [Bool]
and' x y = [pergNum x == 1 && pergNum y == 1, pergNum x == 1 && pergNum y == 0, pergNum x == 0 && pergNum y == 1, pergNum x == 0 && pergNum y == 0]

or' :: Pergunta -> Pergunta -> [Bool]
or' x y = [pergNum x == 1 || pergNum y == 1, pergNum x == 1 || pergNum y == 0, pergNum x == 0 || pergNum y == 1, pergNum x == 0 || pergNum y == 0]

not' :: Pergunta -> Pergunta -> [Bool]
not' x y = [not and' x y]
