-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g088 where

  import LI11920
  import Tarefa0_2019li1g088
  import Tarefa1_2019li1g088
  -- * Testes
  
  -- | Testes unitários da Tarefa 2.
  --
  -- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
  testesT2 :: [(Int,Jogada,Estado)]
  testesT2 = testesTarefa2  
  -- * Funções principais da Tarefa 2.
  
  -- | Efetua uma jogada.
  jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
         -> Jogada -- ^ A 'Jogada' a efetuar.
         -> Estado -- ^ O 'Estado' anterior.
         -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
  jogada numero comando (Estado matrizmapa jogadoresEstado) | ((length m))>(truncate y) =  estadoFinal comando posicao matrizmapa jogadoresEstado numero
                                                            | otherwise = (Estado matrizmapa jogadoresEstado)
     where
              -- | Funcao q vai buscar à lista de jogadoresEstado o jogador q pretendemos alterar dando as suas coordenadas.
              (m:n)=matrizmapa
              (x,y)=posicao
              posicao = jogadoratual numero jogadoresEstado
              jogadoratual::Int->[Jogador]->(Int,Double)
              jogadoratual numero ((Jogador a b c d e):r) | numero == 0 = (a,b)
                                                          | otherwise = jogadoratual (numero-1) r
  -- * Funções auxiliares
  -- | Funcao estadoFinal que dá o nosso estado atualizado
  estadoFinal::Jogada->(Int,Double)->Mapa->[Jogador]->Int->Estado
  estadoFinal comando posicao matrizmapa jogadoresEstado numero = (Estado l1 l2)
       where
          l1 | (auxcola jogador1 == True && comando==Dispara) = matrizCola posicao matrizmapa
             | otherwise = matrizmapa
                  where jogador1=(encontraIndiceLista numero jogadoresEstado)

          l2 | (length jogadoresEstado)>1 = (take (numero) jogadoresEstado)++[f]++(drop (numero+1) jogadoresEstado)
             | otherwise = [f]
             where
                f |(comando==Dispara && auxcola jogador1 == False) = jogador1
                  |(comando==Dispara && auxcola jogador1 == True) = ncola jogador1
                  |(comando==Desacelera || comando==Acelera)= verificaCondicoes jogador1
                  |(comando==Movimenta C) || (comando==Movimenta B) = verificaCimaBaixo jogador1 
                  |(comando==Movimenta E) || (comando==Movimenta D) = verificaar jogador1 
                   where jogador1=(encontraIndiceLista numero jogadoresEstado)
          --
          -- | FUNCOES RELATIVAS A COLA
          -- Auxcola : verifica se o jogador tem cola para ser usada.
          auxcola::Jogador->Bool
          auxcola (Jogador num dist velo cola estado) | cola==0 = False
                                                      | (lol posicao )==0 = False
                                                      | (veri estado)== 1 = True
                                                      | otherwise = False
                                                        where 
                                                          veri (Chao _) = 1
                                                          veri _ = 0
                                                          lol (a,b) = (truncate b)

          --ncola : funcao que altera a cola do jogador caso ele dispare.
          ncola::Jogador->Jogador
          ncola (Jogador num dist velo cola estado) = (Jogador num dist velo (cola-1) estado)                                              

          --matrizcola : Dado a posicao do jogador e o nosso mapa, esta funçao altera o piso anterior do jogador para cola
          matrizCola::(Int,Double)->Mapa->Mapa
          matrizCola (a,b) matrizmapa | ePosicaoMatrizValida' p' matrizmapa = updateMatriz p' meteCola matrizmapa
                          --          | otherwise = matrizmapa
                                  where
                                  p' = (a,(b-1))
          -- meteCola : Funcao que altera o piso de uma peca para cola.
          meteCola :: Peca -> Peca
          meteCola (Rampa _ a1 a2) = Rampa Cola a1 a2
          meteCola (Recta _ a3) = Recta Cola a3

          -- | FUNCOES RELATIVAS AO DESACELERA E ACELERA
          --verificaCondicoes : Funcao que apos verifica o estado do nosso jogador acelera ou desacelera o nosso jogador
          verificaCondicoes l@(Jogador num dist velo cola estado) | verii estado == 1 && comando==Acelera && (length a)>(((truncate dist)) +1) = (Jogador num dist velo cola (Chao True))
                                                                  | verii estado == 1 && comando==Acelera = (Jogador num dist 0 cola (Chao False))
                                                                  | verii estado == 1 && comando==Desacelera = (Jogador num dist velo cola (Chao False))
                                                                  | otherwise = l
                                                                             where   
                                                                              verii::EstadoJogador->Int                                                                          
                                                                              verii (Chao _) = 1
                                                                              verii _ =0
                                                                              (a:b) = matrizmapa
          -- | FUNCOES RELATIVAS AO CIMA OU BAIXO
          --verificaCimaBaixo : Função principal que faz o nosso jogador se mover caso seja possível
          verificaCimaBaixo l@(Jogador num dist velo cola estado) | variaveis estado ==1 && verifica posicao matrizmapa = movimenta l posicao 
                                                                  | otherwise = l
                      where 
                          verifica (a,b) matrizmapa | a==0 && comando==(Movimenta C) = False
                                                    | a==((length matrizmapa)-1) && comando== (Movimenta B) = False
                                                    |otherwise = True
                          variaveis (Chao _)=1      
                          variaveis _ = 0   
          -- movimenta : Função  secundária que faz o jogador se mover
          movimenta::Jogador->(Int,Double)->Jogador
          movimenta l@(Jogador num dist velo cola estado) (m,n) = aux a b  
                                                                
                                                                  where
                                                                        -- a = Peca onde o jogador se encontra
                                                                        a = encontraPosicaoMatriz' (m,n) matrizmapa
                                                                        -- b = Peca para onde o jogador pretende efetuar a jogada
                                                                        b | comando==(Movimenta C) = encontraPosicaoMatriz' ((m-1), n) matrizmapa
                                                                          | otherwise = encontraPosicaoMatriz' ((m+1),n) matrizmapa
                                                                        -- verifica se é rampa ou recta
                                                                        veriii (Rampa piso1 hi1 hf1) = 0
                                                                        veriii (Recta piso2 h2) = 1
                                                                        -- ver melhor a funcao aux
                                                                        aux::Peca->Peca->Jogador
                                                                        aux a b | ((altu a) -(altu b))<0 = Jogador num dist 0 cola (Morto 1)
                                                                                | (abs ((altu a) -(altu b)))>0.2 && comando==(Movimenta C) = Jogador (num-1) dist velo cola (Ar (altu a) (incli a) 0)
                                                                                | (abs ((altu a) -(altu b)))>0.2 &&  comando==(Movimenta B) = Jogador (num+1) dist velo cola (Ar (altu a) (incli a) 0)
                                                                                | (abs ((altu a) -(altu b)))<=0.2 && comando==(Movimenta C) = Jogador (num-1) dist velo cola estado
                                                                                | (abs ((altu a) -(altu b)))<=0.2 &&  comando==(Movimenta B) = Jogador (num+1) dist velo cola estado
                                                                                             
                                                                        altu::Peca->Double
                                                                        altu (Recta _ h2) = (fromIntegral h2)
                                                                        altu (Rampa _ hi hf) = alt dist hi hf 

                                                                        incli (Recta _ _) = 0
                                                                        incli (Rampa _ hi hf) =(inclinacao hi hf)
                                                                        --Da a inclinacao de uma rampa
                                                                        inclinacao::Int->Int->Double
                                                                        inclinacao hi hf | hf>hi = (atan (fromIntegral (hf-hi)))*180/pi
                                                                                         | hi>hf = -((atan (fromIntegral (hi-hf)))*180/pi)
           
          -- | FUNCOES RELATIVAS AO ESQUERDA E DIREITA
          -- Verifica se o jogador se encontra no ar e caso isso acontece altera a sua rotação.
          verificaar::Jogador->Jogador
          verificaar l@(Jogador num dist velo cola estado) | verii estado == 1 = rodar l 
                                                           | otherwise = l
                                                                      where
                                                                        verii::EstadoJogador->Int                                                                          
                                                                        verii (Ar _ _ _) = 1
                                                                        verii _ =0
          -- Funcao que faz o jogador alterar a sua inclinação
          rodar l@(Jogador num dist velo cola (Ar a b c)) | comando==(Movimenta D) = (Jogador num dist velo cola (Ar a (corrigeRodar incli' 0) c))
                                                          | comando==(Movimenta E) = (Jogador num dist velo cola (Ar a (corrigeRodar incli' 1) c))
                                                             where
                                                              incli' = sIncli (b)
            
          -- Função que corrige a rotação do jogador caso ele tenha rodado demais.
          corrigeRodar x l | x'>=90 = 90
                           | x'<=(-90) = -90
                           |otherwise = x'
                               where x' |l==0 = x-15
                                        |l==1 = x+15 



