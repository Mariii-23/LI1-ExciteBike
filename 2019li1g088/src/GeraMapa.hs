-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where

  import LI11920
  import Tarefa4_2019li1g088
  import Tarefa3_2019li1g088
  import Tarefa2_2019li1g088
  import Tarefa1_2019li1g088
  import Tarefa0_2019li1g088
  import Graphics.Gloss
  import Graphics.Gloss.Juicy
  import Graphics.Gloss.Interface.Pure.Game
  --import Data.Matrix
  import Graphics.Gloss.Interface.IO.Animate
  --import GeraMapa.hs
  --import Graphics.Gloss.Interface.Pure.Game.Polygon
  
  
  -- * Tipos de dados auxiliares
  type Pictures = [Picture]
  type EstadoGloss =(Estado,Pictures,Int)
  
  estadoGlossInicial::Pictures->EstadoGloss
  estadoGlossInicial li = (estadoTeste,li,0)
  
  -- | Display full screen
  ecra::Display
  ecra = FullScreen
  
  -- | frame
  frame::Int
  frame = 120 --- modificar valor-- talvez 60?!
  
  --x inicial
  linha::Float
  linha = (-330)
  
  --y inicial
  coluna::Float
  coluna = (-200) --defenir melhor
  -- -200 carro
  
  espaco::Float
  espaco = 20 -- defenir depois
  
  comp::Float
  comp=75
  -- | vetor translate do gera mapa
  -- | x
  p::Float
  p=0
  -- | y
  p'::Float
  p'=75
  
  
  estadoTeste::Estado
  estadoTeste = (Estado (gera 4 20 0) [(Jogador 0 0 0 3 (Ar 0 20 0)),(Jogador 1 0 0 3 (Ar 2 0 0)),(Jogador 2 2 0 3 (Chao False)),(Jogador 3 9 0 3 (Chao False))])
  
  -- | Função principal da Tarefa 5.
  --
  -- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
  main :: IO ()
  main = do 
       fundo <- loadBMP "IMAGENS/fundo.bmp"
       ceu <- loadBMP "IMAGENS/ceu.bmp"     
       rel <- loadBMP "IMAGENS/relva_2.bmp"
       j1 <- loadBMP "IMAGENS/jipeAzul.bmp"
       j1m <- loadBMP "IMAGENS/jipeAzulM.bmp"
       j2 <- loadBMP "IMAGENS/jipeRosa.bmp"
       j2m <- loadBMP "IMAGENS/jipeRosaM.bmp"
       j3 <- loadBMP "IMAGENS/jipeVerde.bmp"
       j3m <- loadBMP "IMAGENS/jipeVerdeM.bmp"
       j4 <- loadBMP "IMAGENS/jipeVermelho.bmp"
       j4m <- loadBMP "IMAGENS/jipeVermelhoM.bmp"     
       f1 <- loadBMP "IMAGENS/galaxia.bmp"
       f2 <- loadBMP "IMAGENS/chao.bmp"
       st <- loadBMP "IMAGENS/start.bmp"
       j <- loadBMP "IMAGENS/jipei.bmp"
       re <- loadBMP "IMAGENS/regras2.bmp"
       play ecra                              -- minha janela do jogo
            white                            -- cor do fundo do ecra
            frame                              -- frame rate
            (estadoGlossInicial [fundo,ceu,j1,j1m,j2,j2m,j3,j3m,j4,j4m,f1,f2,st,j,re,rel])   --estado inicial
            desenhaGLossInicial
            reageAoMOvimento                   --reage ao movimento
            reageAoTempo                       -- reage ao tempo
  
  
  
  desenhaGLossInicial::EstadoGloss->Picture
  desenhaGLossInicial e@((Estado mapa j),li,p) = jogar e
  
  
  
  reageAoTempo::Float->EstadoGloss->EstadoGloss
  --reageAoTempo t ((Estado mapa ((Jogador a b c d (Morto e)):r)),li,p) = ((Estado mapa ((Jogador a (b) c d (Morto e)):r)),li,p)
  --reageAoTempo t ((Estado mapa ((Jogador a b c d l@(Chao False)):r)),li,p) = ((Estado mapa ((Jogador a (b) c d l):r)),li,p)
  --reageAoTempo t ((Estado mapa ((Jogador a b c d l@(Chao True)):r)),li,p) = ((Estado mapa ((Jogador a (b-1) c d l):r)),li,p)
  --reageAoTempo t ((Estado mapa ((Jogador a b c d l@(Ar _ _ _)):r)),li,p) = ((Estado mapa ((Jogador a (b-1) c d l):r)),li,p)
  reageAoTempo t ((Estado mapa j),li,p) = ((Estado mapa auxj),li,p)
              where 
                auxj = map (passo (realToFrac t) mapa) j
                
  
  reageAoMOvimento::Event->EstadoGloss->EstadoGloss
  -- | MENU
  reageAoMOvimento (EventKey (SpecialKey KeyHome) Down _ _) (a,li,p) = (a,li,0)
  reageAoMOvimento (EventKey (Char '1') Down _ _) (a,li,0) = (a,li,4)
  reageAoMOvimento (EventKey (Char '2') Down _ _) (a,li,0) = (a,li,1)
  reageAoMOvimento (EventKey (Char '3') Down _ _) (a,li,1) = (a,li,0)
  
  -- | Controles dos jogadores
  -- | Player 1
  reageAoMOvimento (EventKey (Char 'n') Down _ _) (a,li,4) = (jogada 0 Desacelera a,li,4)
  reageAoMOvimento (EventKey (Char 'm') Down _ _) (a,li,4) = (jogada 0 Acelera a,li,4)
  reageAoMOvimento (EventKey (SpecialKey KeyDown) Down _ _) (a,li,4) = (jogada 0 (Movimenta C) a,li,4)
  reageAoMOvimento (EventKey (SpecialKey KeyUp) Down _ _) (a,li,4) = (jogada 0 (Movimenta B) a,li,4)
  reageAoMOvimento (EventKey (SpecialKey KeyLeft) Down _ _) (a,li,4) = (jogada 0 (Movimenta E) a,li,4)
  reageAoMOvimento (EventKey (SpecialKey KeyRight) Down _ _) (a,li,4) = (jogada 0 (Movimenta D) a,li,4)      
  reageAoMOvimento (EventKey (Char ',') Down _ _) (a,li,4) = (jogada 0 Dispara a,li,4)
  -- | Player 2
  reageAoMOvimento (EventKey (Char '1') Down _ _) (a,li,4) = (jogada 1 Desacelera a,li,4)
  reageAoMOvimento (EventKey (Char '2') Down _ _) (a,li,4) = (jogada 1 Acelera a,li,4)
  reageAoMOvimento (EventKey (Char 's') Down _ _) (a,li,4) = (jogada 1 (Movimenta C) a,li,4)
  reageAoMOvimento (EventKey (Char 'w') Down _ _) (a,li,4) = (jogada 1 (Movimenta B) a,li,4)
  reageAoMOvimento (EventKey (Char 'a') Down _ _) (a,li,4) = (jogada 1 (Movimenta E) a,li,4)
  reageAoMOvimento (EventKey (Char 'd') Down _ _) (a,li,4) = (jogada 1 (Movimenta D) a,li,4)      
  reageAoMOvimento (EventKey (Char '3') Down _ _) (a,li,4) = (jogada 1 Dispara a,li,4) 
  -- | Player 3
  reageAoMOvimento (EventKey (Char '6') Down _ _) (a,li,4) = (jogada 2 Desacelera a,li,4)
  reageAoMOvimento (EventKey (Char '7') Down _ _) (a,li,4) = (jogada 2 Acelera a,li,4)
  reageAoMOvimento (EventKey (Char 'k') Down _ _) (a,li,4) = (jogada 2 (Movimenta C) a,li,4)
  reageAoMOvimento (EventKey (Char 'i') Down _ _) (a,li,4) = (jogada 2 (Movimenta B) a,li,4)
  reageAoMOvimento (EventKey (Char 'j') Down _ _) (a,li,4) = (jogada 2 (Movimenta E) a,li,4)
  reageAoMOvimento (EventKey (Char 'l') Down _ _) (a,li,4) = (jogada 2 (Movimenta D) a,li,4)      
  reageAoMOvimento (EventKey (Char '8') Down _ _) (a,li,4) = (jogada 2 Dispara a,li,4) 
           
           
  reageAoMOvimento _ q=q
    --  where 
          -- updateLinha 4 (Translate 50 0) li4
  
          
  
  -- | Desenhar o nosso jogo
  jogar::EstadoGloss->Picture
  jogar e@((Estado mapa j),li,0) = desenhaMenu li
  jogar e@((Estado mapa j),li,1) = menuRegras li
  jogar e@((Estado mapa j),li,4) =Pictures ([fundo',ceu,(movimentaMapa mapa' j)])
    --Pictures ([(desenhaMapa e )]  ++ (desenhaJogadores j lj 0 mapa a)) 
                where lj = listaEntres 2 9 li
                      (a:b) = j
                      fundo' = (scale 1.5 1.7 ((translate (0) (0) (li !! 0))))
                      ceu = (scale 1.5 1.3 (translate (0) (150) (li !! 1)))
                      relva = (scale 3 3 (translate (0) (-170) (li !! 15)))
                      mapa' =Pictures (geraMapaJogadores' mapa linha coluna j 0 0 lj)
  
  
  -- | desenha o menu inicial do jogo
  desenhaMenu::Pictures->Picture
  desenhaMenu li = Pictures [jipe,start,regras]
                  where 
                    li' = listaEntres 10 14 li 
                    [f1,f2,st,j,re] = li'
                    start = (scale 1 1 (translate (0) (-90) st))
                    jipe = (scale 1 1 (translate (-300) (100) j))
                    regras = (scale 0.5 0.5 (translate (0) (-370) re))
  
  
  -- | Desenha o menu das regras
  menuRegras::Pictures->Picture
  menuRegras li = Pictures [regras]
                 where
                   re = (!!) li 14
                   regras = (scale 1 1 (translate (0) (0) re))
  
  
                
  -- | Desenha o nosso mapa com o fundo e as pistas
  desenhaMapa::EstadoGloss->Picture
  desenhaMapa ((Estado mapa j),li,p) = Pictures [fundo',ceu,(movimentaMapa mapa' j)]
                          where
                            mapa' = Pictures (geraMapa mapa linha coluna)
                            fundo' = (scale 1.5 1.7 ((translate (0) (0) (li !! 0))))
                            ceu = (scale 1.5 1.3 (translate (0) (150) (li !! 1)))
                            relva = (scale 3 3 (translate (0) (-170) (li !! 15)))
  
  movimentaMapa::Picture->[Jogador]->Picture
  movimentaMapa l [] = l 
  movimentaMapa l ((Jogador o d _ _ _):r) =Translate m n l 
                                 where                    
                                  -- m=linha+150
                            --       m = (-(realToFrac d)*60)+linha+150
                                   m = (-(fromIntegral (truncate d))*comp/2)+linha+150
                                   n=0
  --movimentaMapa l _ = l
                                
  
  -- | Funcao que desenha os jogadores no mapa
  desenhaJogadores::[Jogador]->Pictures->Int->Mapa->Jogador->Pictures
  --desenhaJogadores [] _ _ = error
  desenhaJogadores [] _ _  _ _= []
  --desenhaJogadores [a] li num mapa= desenhaJogador a li num mapa
  desenhaJogadores (a:b) li num mapa j1= (desenhaJogadores b li (num+1) mapa j1) ++ (desenhaJogador a li num mapa j1)
  
  
  desenhaJogador::Jogador->Pictures->Int->Mapa->Jogador->Pictures
  
  desenhaJogador j@(Jogador xj yj _ _ (Morto _)) [j1,j1m,j2,j2m,j3,j3m,j4,j4m] num mapa j1' | num == 0 = colocaJogador [j1m] xj yj altJ' iJ'
                                                                                            | num == 1 = colocaJogador [j2m] xj yj altJ' iJ'
                                                                                            | num == 2 = colocaJogador [j3m] xj yj altJ' iJ'
                                                                                            | num == 3 = colocaJogador [j4m] xj yj altJ' iJ'
                                                                            where 
                                                                              peca = pecaJ j mapa
                                                                              altJ' = altura peca yj
                                                                              iJ' = incli peca
                                                                            
  desenhaJogador j@(Jogador xj yj _ _ w) [j1,j1m,j2,j2m,j3,j3m,j4,j4m] num mapa j1' | num == 0 = colocaJogador [j1] xj yj altJ' iJ'
                                                                                    | num == 1 = colocaJogador [j2] xj yj altJ' iJ'
                                                                                    | num == 2 = colocaJogador [j3] xj yj altJ' iJ' 
                                                                                    | num == 3 = colocaJogador [j4] xj yj altJ' iJ'
                                                                                     where 
                                                                                            peca = pecaJ j mapa
                                                                                            aux (Chao _) = 0
                                                                                            aux _ = 1
                                                                                            altJ' | aux w == 0 = altura peca yj
                                                                                                  | otherwise = altt w
                                                                                            iJ' | aux w == 0 = incli peca
                                                                                                | otherwise = incliJ w
  desenhaJogador _ _ _ _ _ = []
             
  -- verificar rotacao ..n é o rotate
  colocaJogador::[Picture]->Int->Double->Double->Double->[Picture]
  colocaJogador [j] np dist h i | i == 0 = [Rotate (0) (Translate x' y' j')]
                                | otherwise = [Rotate (0) (Translate x' y' j')]
         where   
           x' | dist == 0 = linha
              | otherwise = linha
             -- | otherwise = linha + ((realToFrac dist)*comp/2)
           y' = coluna + ((realToFrac h)*(realToFrac comp)) + ((realToFrac p')*(realToFrac np))
           j' = Translate (-200) 40 (scale 0.3 0.3 j)
           ii =(realToFrac i)
  
  
  --colocarJ1::[Picture]->Int->Double->Double->Double->[Picture]
  --colocaJogador [j] np dist h i | i == 0 = [Rotate (0) (Translate x' y' j')]
    --                            | | otherwise = [Rotate (0) (Translate x' y' j'
      --     where   
        --      x' = linha
          --    y' = coluna + ((realToFrac h)*(realToFrac comp)) + ((realToFrac p')*(realToFrac np))
            --  j' = Translate (-200) 40 (scale 0.3 0.3 j)
              --ii =(realToFrac i)
  
  -- | Funcao que desenha o nosso mapa 
  
  --verificaOpacidade
  opacidade = withAlpha
  verificaOpacidade::Pictures->Double->Double->Pictures
  verificaOpacidade = undefined
  
  
  -- | Funcao que desenha um mapa
  geraMapa::Mapa->Float->Float->Pictures
  geraMapa [] _ _ = []
  geraMapa (a:b) x y =  (geraMapa b (x+p) (y+p')) ++ (umaPista a x y)
  
  -- | Funcao que desenha uma pista
  umaPista::Pista->Float->Float->Pictures
  umaPista [] _ _ = []
  umaPista (a:b) x y = (umaPeca a x y) ++ umaPista b (x+comp) y
  
  -- | Funcao que desenha uma Recta
  umaPeca::Peca->Float->Float->Pictures
  umaPeca (Recta piso h) x y = [m,m',l]
            where 
              m = (color f' ( Polygon [(x,y),(x,y+lp),(comp+x,lp+y),(x+comp,y)]))
              m' = Translate p p' m
              l =color f (Polygon [(x,lp+y),(p+x,y+lp+p'),(x+comp+p,y+lp+p'),(x+comp,y+lp)])
              lp = ((fromIntegral h)*comp)
              
              boost'' = color red (Polygon [(x+(comp/4),y),(x+(comp/4)+p,y+p'),(x+(comp/2)+(p/4),((y+p')/2))]) 
              boost' = translate (-(comp/4)) ((fromIntegral h)*comp) boost''
  
              f = dim (bright a)
              a | piso==Relva = makeColorI 92 647 51 300
                | piso==Terra = makeColorI 92 51 23 300
                | piso==Lama = makeColorI 184 134 11 300
                | piso==Boost = makeColorI 0 200 200 300
                | piso==Cola = red
                  --makeColorI 217 135 25 300
              --poe cor nas paredes
              f' = dark f
  
  -- | Funcao que desenha uma Rampa
  umaPeca (Rampa piso hi' hf') x y = [parede',base,parede]
        where
              parede= color f'  (Polygon [(x,y),(x,y+comp*hi),(x+comp,y+comp*hf),(x+comp,y)])
              parede' = Translate p p' parede
              base = color f (Polygon [(x,y+comp*hi),(x+p,y+comp*hi+p'),(x+p+comp,y+comp*hf+p'),(x+comp,hf*comp+y)])
              hf = fromIntegral hf'
              hi = fromIntegral hi'
              dif' = hf'-hi' 
           
              f = dim (bright a)
              a | piso==Relva = makeColorI 92 647 51 300
                | piso==Terra = makeColorI 92 51 23 300
                | piso==Lama = makeColorI 184 134 11 300
                | piso==Boost = makeColorI 0 200 200 300
                | piso==Cola =  red
                  --makeColorI 217 135 25 300      
              f' = dark f
   
        
              
  
  
  
            
                
  geraMapaJogadores'::Mapa->Float->Float->[Jogador]->Int->Int->Pictures->Pictures
  geraMapaJogadores' [] _ _ _ _ _ _= []
  geraMapaJogadores' (a:b) x y lj npista nPeca li = (geraMapaJogadores' b (x+p) (y+p') lj (npista+1) (nPeca) li) ++ (umaPista' a x y lj' (nPeca) li) 
                  where 
                    lj' = buscarJ lj npista 0
  
                                        
  -- | Funcao que desenha uma pista
  umaPista'::Pista->Float->Float->[(Jogador,Int)]->Int->Pictures->Pictures
  umaPista' [] _ _ _ _ _= []
  umaPista' (a:b) x y lj nPeca li = (umaPeca' a x y lj' li) ++ (umaPista' b (x+comp) y lj (nPeca+1) li)
                where
                  lj'= naPecaj lj nPeca 
                            
  umaPeca'::Peca->Float->Float->[(Jogador,Int)]->Pictures->Pictures    
  umaPeca' (Recta piso h) x y lj li | (length lj)>0 = [m,m',l] ++ [(desenharJs x y (realToFrac h) lj li)]
                                    | otherwise = [m,m',l]
            where 
              m = (color f' ( Polygon [(x,y),(x,y+lp),(comp+x,lp+y),(x+comp,y)]))
              m' = Translate p p' m
              l =color f (Polygon [(x,lp+y),(p+x,y+lp+p'),(x+comp+p,y+lp+p'),(x+comp,y+lp)])
              lp = ((fromIntegral h)*comp)
              
              boost'' = color red (Polygon [(x+(comp/4),y),(x+(comp/4)+p,y+p'),(x+(comp/2)+(p/4),((y+p')/2))]) 
              boost' = translate (-(comp/4)) ((fromIntegral h)*comp) boost''
  
              f = dim (bright a)
              a | piso==Relva = makeColorI 92 647 51 300
                | piso==Terra = makeColorI 92 51 23 300
                | piso==Lama = makeColorI 184 134 11 300
                | piso==Boost = makeColorI 0 200 200 300
                | piso==Cola = red
                  --makeColorI 217 135 25 300
              --poe cor nas paredes
              f' = dark f
  
  -- | Funcao que desenha uma Rampa
  umaPeca' (Rampa piso hi' hf') x y lj li | (length lj)>0 = [parede',base,parede] ++ [(desenharJs x y (realToFrac h') lj li)]
                                          | otherwise = [parede',base,parede]
        where
              parede= color f'  (Polygon [(x,y),(x,y+comp*hi),(x+comp,y+comp*hf),(x+comp,y)])
              parede' = Translate p p' parede
              base = color f (Polygon [(x,y+comp*hi),(x+p,y+comp*hi+p'),(x+p+comp,y+comp*hf+p'),(x+comp,hf*comp+y)])
              hf = fromIntegral hf'
              hi = fromIntegral hi'
              dif' = hf'-hi' 
              h' = altura (Rampa piso hi' hf') (distanciaJ j)
              (j,num):r = lj
           
              f = dim (bright a)
              a | piso==Relva = makeColorI 92 647 51 300
                | piso==Terra = makeColorI 92 51 23 300
                | piso==Lama = makeColorI 184 134 11 300
                | piso==Boost = makeColorI 0 200 200 300
                | piso==Cola =  red
                  --makeColorI 217 135 25 300      
              f' = dark f
    
  desenharJs::Float->Float->Float->[(Jogador,Int)]->Pictures->Picture
  --desenharJs _ _ _ [] _ = []
  desenharJs x y h [j] li = Pictures [umJ x y h j li]
  desenharJs x y h (j:b) li = Pictures ([(desenharJs x y h b li)] ++ [(umJ x y h j li)])
      
  umJ::Float->Float->Float->(Jogador,Int)->Pictures->Picture
  umJ x y h (j,num) li = (Translate x' y' j')
          where 
                x' = x + (realToFrac ((distanciaJ j) - (fromIntegral (truncate (distanciaJ j)))))*comp
                y' =  y -150 + ((realToFrac h)*(realToFrac comp)) + 200
                j' = scale 0.3 0.3 (desenhaJogador' j li num)
                
  
  
  desenhaJogador'::Jogador->Pictures->Int->Picture
  
  desenhaJogador' j@(Jogador xj yj _ _ (Morto _)) [j1,j1m,j2,j2m,j3,j3m,j4,j4m] num | num == 0 =  j1m
                                                                                    | num == 1 =  j2m 
                                                                                    | num == 2 =  j3m 
                                                                                    | num == 3 =  j4m                                                                   
                                                                            
  desenhaJogador' j@(Jogador xj yj _ _ w) [j1,j1m,j2,j2m,j3,j3m,j4,j4m] num | num == 0 =  j1 
                                                                            | num == 1 =  j2 
                                                                            | num == 2 =  j3 
                                                                            | num == 3 =  j4 
  --desenhaJogador' _ _ _  = []
  