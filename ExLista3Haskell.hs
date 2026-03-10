module Cap3Exercicios where

-- Exercício 3.1
data Pergunta = Sim | Nao
  deriving (Show, Eq)

pergNum :: Pergunta -> Int
pergNum Nao = 0
pergNum Sim  = 1

listPergs :: [Pergunta] -> [Int]
listPergs ps = [ pergNum p | p <- ps ]

and' :: Pergunta -> Pergunta -> Pergunta
and' Sim Sim = Sim
and' _   _   = Naos

or' :: Pergunta -> Pergunta -> Perguntas
or' Nao Nao = Nao
or' _   _   = Sim

not' :: Pergunta -> Pergunta
not' Sim = Nao
not' Nao = Sim

-- Exercício 3.3
data Jogada = Pedra | Papel | Tesoura
  deriving (Show, Eq)

data Resultado = Empate | GanhaJog1 | GanhaJog2
  deriving (Show, Eq)

vencedor :: Jogada -> Jogada -> Resultado
vencedor Pedra Pedra     = Empate
vencedor Papel Papel     = Empate
vencedor Tesoura Tesoura = Empate
vencedor Pedra Tesoura   = GanhaJog1
vencedor Tesoura Papel   = GanhaJog1
vencedor Papel Pedra     = GanhaJog1
vencedor Tesoura Pedra   = GanhaJog2
vencedor Papel Tesoura   = GanhaJog2
vencedor Pedra Papel     = GanhaJog2

-- Exercício 3.5
data UnidadeImperial = Inch | Yard | Foot
  deriving (Show, Eq)

converterMetros :: UnidadeImperial -> Double -> Double
converterMetros Inch x = x * 0.0254
converterMetros Yard x = x * 0.9144
converterMetros Foot x = x * 0.3048

converterImperial :: Double -> UnidadeImperial -> Double
converterImperial m Inch = m / 0.0254
converterImperial m Yard = m / 0.9144
converterImperial m Foot = m / 0.3048

-- Exercício 3.7
isPalindromo :: String -> Bool
isPalindromo s = s == reverse s

-- Exercício 3.9
revStrings :: String -> String -> String -> (String, String, String)
revStrings x y z = (reverse x, reverse y, reverse z)