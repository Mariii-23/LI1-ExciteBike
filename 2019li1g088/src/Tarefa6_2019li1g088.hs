-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g088 where

import LI11920
import Tarefa4_2019li1g088
import Tarefa3_2019li1g088
import Tarefa2_2019li1g088
import Tarefa1_2019li1g088
import Tarefa0_2019li1g088

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot n (Estado mapa j) | ePosicaoMatrizValida posicao' mapa == False || ePosicaoMatrizValida posicao'' mapa == False = Just Desacelera
                      | w' j' == 1 = Just Acelera
                      | w' j'== 2 = Nothing
                      | w' j' == 3 = corrigirInclinacao' i i'
     where 
      j'= (!!) j n   
      w' (Jogador _ _ _ _ (Chao _)) = 1
      w' (Jogador _ _ _ _ (Morto _)) = 2
      w' (Jogador _ _ _ _ (Ar _ _ _)) = 3

      (a,b)=posicao'
      posicao' = (x,(fromIntegral (truncate y)))
      (x,y)=posicao
      posicao = jogadoratual n j
      jogadoratual::Int->[Jogador]->(Int,Double)
      jogadoratual numero ((Jogador a b c d e):r) | numero == 0 = (a,b)
                                                  | otherwise = jogadoratual (numero-1) r
      i = altt (estadoJogador j') 
      
      i'=incli peca'      
      peca'::Peca
      peca' = encontraPosicaoMatriz posicao'' mapa    
      posicao''= (a,b)

-- | Funcao que corrige a inclinacao
corrigirInclinacao'::Double->Double->Maybe Jogada
corrigirInclinacao' p p2 | (diferenca p p2)<=10 && (diferenca p p2)>=(-10)  = Nothing
                         | (diferenca p p2)>10 = Just (Movimenta D) 

-- | Funcao que calcula a diferenca entre dois numeros
diferenca::Double->Double->Double
diferenca p p1 | p1>=0 && p>=0 = abs (p-p1)
               | p>=0 && p1<=0 = (p-p1)
               | p1<=0 && p<=0 = -(abs (p-p1))
               | p<=0 && p1>=0 = abs (-p+p1)
