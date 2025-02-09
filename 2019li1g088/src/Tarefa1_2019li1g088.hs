-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g088 where

import LI11920
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- | Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
-- | testesT1 = [(a,b,c) | a<-[2,23,100],b<-[1,33,1200,5000,10000,564,32,11,2313], c<-[7]]
testesT1 = [(a,b,c) | a<-[1..10], b<-[1..10],c<-[0..9]]

-- * Funções pré-definidas da Tarefa 1
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1
gera :: Int -> Int -> Int -> Mapa
-- gera 0 0 _ =[[]]
-- | No caso, do 'Mapa' seja constituído apenas por comprimento = 1.
gera a 1 _ = auxgera a 1
       where
          auxgera 0 _ = []
          auxgera a 1=[[Recta Terra 0]]++(auxgera (a-1) 1)
-- | Vai implementar a função 'toPeca' na matriz de números (pares), sem o primeiro elemento de cada linha ('npistas'), tendo em conta que já se encontram definidos.
gera npistas comprimento semente = map toPeca (lol (comprimento-1) (pares lista))
  where         
     lista = geraAleatorios n semente
     n = (2*((npistas*comprimento)-npistas))

-- | Função que constrói a matriz.
lol:: Int -> [a]->[[a]] 
lol _ [] = []
lol n lista = (take n lista):(lol n (drop n lista))                               

-- | Função que transforma uma lista em uma lista de pares.
pares::[Int]->[(Int,Int)]
pares (a:b:c)= (a,b):pares c 
pares _ = []

-- | Função que transforma uma lista de pares em uma lista de 'Peca's.
toPeca:: [(Int,Int)]->[Peca]
--toPeca [] = []
toPeca l=(Recta Terra 0): aux1   0 Terra l
-- | Função auxiliar que vai gerar uma 'Peca'.
aux1 :: Int -> Piso -> [(Int,Int)]->[Peca]
aux1 _ _ [] = []
aux1 altura piso ((a,b):t) = (f b):aux1 alt2 (g a) t
    where
        -- | Função que verifica se é 'Rampa' ou 'Recta'
        f x | (elem x [6..9] || (alt == altura) || (alt<0 && altura==0))= Recta (g a) altura
            | (alt<0 && elem x [2..5]) = Rampa (g a) altura 0
            | otherwise = Rampa (g a) altura alt
        -- | Função que verifica o 'piso'.
        g x | elem x [0,1] = Terra
            | elem x [2,3] = Relva
            | elem x [6..9] = piso
        g 4 = Lama
        g 5 = Boost
        -- | Função que devolve a, suposta, altura final. 
        alt | elem b [6..9] = altura
            | b<2 = altura+b+1
            | otherwise = altura-b+1
        -- Função que utiliza a aultura final, da 'Peca' obtida, e insere-a na expressão aux1.
        alt2| (alt<=0 && altura==0) = 0
            | (alt<=0 && elem b [2..5]) = 0
            | otherwise = alt           