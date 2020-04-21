-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g088 where

import LI11920
import Tarefa3_2019li1g088
import Tarefa2_2019li1g088
import Tarefa1_2019li1g088
import Tarefa0_2019li1g088

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(a,b,c) | a<-[0.1,0.2,1.1,5], b<-[gera 2 10 3], c<-[ (Jogador 0 2 2 3 (Chao True)), (Jogador 0 2 0 3 (Morto 1)), (Jogador 0 2 2 3 (Ar 4 (-30) 5)) ] ]

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j | ((fromIntegral comp))<(distanciaJogador j) = corrigeJ j comp
            | otherwise = move t m (acelera t m j)
   where  (a:b) = m
          comp =(length a)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.   
acelera t mapa j | ePosicaoMatrizValida posicao mapa == False = j
                 | ver (estadoJogador j )== 0 = aceleraChao j atrito t
                 | ver (estadoJogador j) == 1 = aceleraAr j t
                 | otherwise = j
               where
                 ver (Chao _) = 0
                 ver (Ar _ _ _) = 1
                 ver _ = 2
                 (a:b) = mapa 

                 -- | Posicao do jogador
                 posicao::(Int,Int)   
                 posicao = (pistaJogador j, truncate (distanciaJogador j))
                 (x,y)=posicao
                 posicao' = (x,y-1)
               
                 -- | Peca aonde o jogador se encontra
                 peca::Peca
                 peca | (fromIntegral (length a))>(distanciaJogador j) =  encontraPosicaoMatriz posicao mapa
                      | otherwise = encontraPosicaoMatriz posicao' mapa
                 -- | Piso da peca aonde se encontra o jogador
                 piso::Piso
                 piso = h peca
                 h (Rampa piso _ _) = piso
                 h (Recta piso _) = piso
                 -- | Atrito
                 atrito::Double
                 atrito = case piso of
                       Terra->0.25
                       Relva->0.75
                       Lama->1.50
                       Boost->(-0.50)
                       Cola->3.00
                 
-- | Atualizar a velocidade do jogador no chao
aceleraChao::Jogador->Double->Double->Jogador
aceleraChao j atrito t = j {velocidadeJogador = corrige velocidadeFinal} 
 where
     velocidadeFinal = velocidade + (accelMota-atrito*velocidade)*t 
     accelJogador::Jogador->Bool
     accelJogador (Jogador _ _ _ _ (Chao a)) = a

     velocidade = velocidadeJogador j
     accelMota = if (velocidade<2 && (accelJogador j)) then 1 else 0

-- | corrige velocidade
corrige::Double->Double
corrige x = if x>=0 then x else 0

-- | Atualizar a velocidade do jogador no ar
aceleraAr::Jogador->Double->Jogador
aceleraAr j@(Jogador a b velo cola (Ar h incli grav)) t= (Jogador a b velocidadeFinal cola (Ar h incli gfinal))

  where     
     velocidadeFinal = velocidade -(resistenciaAR*velocidade*t)
     velocidade = velocidadeJogador j
     resistenciaAR = 0.125
     gfinal = grav + accelGravidade*t
     accelGravidade = 1

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t mapa j@(Jogador npista dist vel c e) | (ePosicaoMatrizValida posicao mapa == False || ePosicaoMatrizValida posicao' mapa == False) = (Jogador npista dist' 0 c (Chao False))
                                            | ver1 (estadoJogador j)== 0 = auxMorto j t
                                            | ver1 (estadoJogador j)== 1 = auxChao j t peca peca' comprimento
                                            | ver1 (estadoJogador j)== 2 = auxAr j t peca peca' comprimento
                                  
       where 
          dist' = (fromIntegral ((length mx)))

          ver1::EstadoJogador->Int
          ver1 (Morto _) = 0
          ver1 (Chao _) = 1
          ver1 (Ar _ _ _) = 2

          (mx:my)=mapa
          
          -- | Posicao do jogador
          posicao::(Int,Int)   
          posicao = (pistaJogador j, truncate (distanciaJogador j))
          (a,b)= posicao
          posicao'=(a,b+1)
          comprimento = fromIntegral (length mx)
          
          -- | Peca aonde o jogador se encontra
          peca::Peca
          peca =  encontraPosicaoMatriz posicao mapa
          
          -- | Peca a seguir a 'Peca' do 'Jogador'
          peca'::Peca
          peca' = encontraPosicaoMatriz posicao' mapa

-- | auxiliar move quando jogador se encontra morto
auxMorto::Jogador->Double->Jogador
auxMorto (Jogador a b v c (Morto m)) t | (m-t)>0 = (Jogador a b v c (Morto (m-t)))      
                                       | otherwise = (Jogador a b v c (Chao False))    

-- | auxiliar move quando jogador se encontra no chao
auxChao::Jogador->Double->Peca->Peca->Double->Jogador
auxChao j@(Jogador a dist vel c (Chao m)) t pecaInicial pecaFinal cc | (distPercorrida+dist)<(fromIntegral ((truncate dist)+1)) = corrigeJ (j {distanciaJogador = distPercorrida+dist}) (truncate cc)
                                                                     |(abs incliFinal)>=(incliInicial) = j {distanciaJogador = (fromIntegral (truncate dist))+1 }
                                                                     | otherwise = j {estadoJogador= (Ar hI incliInicial 0)}
       where     
         hI = fromIntegral (altFinal pecaInicial)
         distPercorrida = vel*t
         incliInicial = incli pecaInicial
         incliFinal = incli pecaFinal

-- | auxiliar move quando jogador se encontra no ar
auxAr::Jogador->Double->Peca->Peca->Double->Jogador
auxAr  j@(Jogador a dist vel c (Ar h incli' gravidade)) t peca peca2 _ | (abs (incli'-incliFinal'))>=45  && (intersetam (p1,p2) (p3,p4)) = Jogador a ix 0 c (Morto 1)
                                                                       | intersetam (p1,p2) (p3,p4) = Jogador a ix vel c (Chao False)
                                                                       | x'<(fromIntegral ((truncate x)+1)) = Jogador a x' vel c (Ar y' incli' gravidade)
                                                                       | otherwise = Jogador a (fromIntegral ((truncate dist)+1)) vel c (Ar hhh incli' gravidade)                                                     
       where
          -- | reta da peca atual
          (p3,p4) = auxPontos peca
          (Cartesiano x3 y3)=p3
          (Cartesiano x4 y4)=p4

          auxPontos (Recta _ h) = ((Cartesiano (fromIntegral (truncate dist)) (fromIntegral h)),(Cartesiano (fromIntegral ((truncate dist) +1)) (fromIntegral h)))
          auxPontos (Rampa _ hi hf) = ((Cartesiano (fromIntegral (truncate dist)) (fromIntegral hi)),(Cartesiano (fromIntegral ((truncate dist) +1)) (fromIntegral hf)))    

          -- | reta do movimento do jogador
          (x,y) = (dist,h) 
          p1 = (Cartesiano x y)
          p2 = (Cartesiano x' y')
    
          x' = vx*t + x 
          y' = vy*t + y 

          (Cartesiano vx vy) = vetor
          vetor = somaVetores vGrav vVel
          vVel = (Polar vel incli')
          vGrav = (Polar gravidade (-90))

          (Cartesiano ix iy)=intersecao (p1,p2) (p3,p4)

          (Cartesiano ax ay) = intersecao (p1,p2) ((Cartesiano ((fromIntegral (truncate dist)) + 1) 0),(Cartesiano ((fromIntegral (truncate dist)) +1) 1000))
          
          incliFinal' = incli peca2

          calcularx = dist + (cos incli')*vel*t
  
          calculary = h + gravidade*t

          hhh |(sIncli incli')>0 = y + altura
              |(sIncli incli')==0 = y
              | otherwise = y - altura
                 where 
                    altura = (abs (x4-x))*(abs (y-y'))/(abs (x'-x)) 
