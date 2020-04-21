-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g088 where

  import LI11920
  import Tarefa0_2019li1g088
  import Tarefa1_2019li1g088
  import Tarefa2_2019li1g088
  
  -- * Testes
  
  -- | Testes unitários da Tarefa 3.
  --
  -- Cada teste é um 'Mapa'.
  testesT3 :: [Mapa]
  testesT3 = [gera a b c | a<-[2..15], b<-[2..20],c<-[0..9]]
  --
  -- * Funções principais da Tarefa 3.
  --
  -- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
  --
  -- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
  --
  -- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
  desconstroi :: Mapa -> Instrucoes
  desconstroi l1 =horizontais (verticais l1)
    
  -- _NB:_ Algumas das funcoes usadas aqui estão localizadas na tarefa 0                  
  
  -- | Função que converte um 'Mapa' numa sequencia de instrucoes
  convertMapaparaInstrucoes::Int->Mapa->Instrucoes
  convertMapaparaInstrucoes _ [] = []
  convertMapaparaInstrucoes num (l:r) = (aux num l) ++ (convertMapaparaInstrucoes num r)
                                                      where 
                                                        aux::Int->Pista->Instrucoes
                                                        aux _ [] = []
                                                        aux num (a:b) = (convertPeca num a):(aux (num+1) b)
  -- | Função que converte apenas uma 'Peca'
  convertPeca::Int->Peca->Instrucao
  convertPeca n (Recta piso h) = Anda [n] piso
  convertPeca n (Rampa piso h1 h2) | h2>h1 = Sobe [n] piso (h2-h1)
                                   | otherwise = Desce [n] piso (h1-h2)

  -- * Função Principal que executa as 'verticais'

  verticais::Mapa->Instrucoes                   
  verticais l1 = dele mapaNovo 0 elementosamais
    where     
    comprimento = (length l1)
    l = ( retirar1elemento (transposta l1))          
    p = iSorted (lista2 (aux20 (l) 0 comprimento))
    mapaNovo = modificar p (convertMapaparaInstrucoes 0 (l))
    elementosamais= iSorted (listaEliminar p)

  -- ** Funções Auxiliares da Função principal 'verticais' 

  -- | Verifica quais sao as posicoes iguais num 'Mapa'
  aux20::Mapa->Int->Int->[[[Int]]]
  aux20 [] _ _ = []
  aux20 (a:b) num comprimento = (aux22 a a [] num):(aux20 b (num+comprimento) comprimento)
  
  -- | Função que nos devolve as posicoes iguais, numa dada 'Pista'  
  aux22::Pista->Pista->Pista->Int->[[Int]]
  aux22 [a] l1 q num | elem a q = []
                     | otherwise = (aux24 a l1 num):[]
  aux22 l@(a:b:c) l1 q num | elem a q = (aux22 (b:c) l1 q (num)) 
                           | otherwise = (aux24 a l1 num):(aux22 (b:c) l1 (q++[a]) (num))
  
  -- | Função que devolve as posicoes das 'Pecas' iguais, a uma determinada 'Peca' dada
  aux24::Peca->Pista->Int->[Int]
  aux24 _ [] _ = []  
  aux24 q (m:n) num | verigual q m = num:(aux24 q n (num+1))
                    | otherwise = (aux24 q n (num+1))
                      where   
                        verigual::Peca->Peca->Bool
                        verigual (Rampa piso1 h1 h2) (Rampa piso2 a1 a2) = (piso1==piso2) && (h1==a1)&&(h2== a2 )
                        verigual (Recta piso1 h1) (Recta piso2 a1) = (piso1==piso2) && (h1==a1)
                        verigual _ _ = False

  -- | Devolve, unicamente uma lista ordenada, as posicoes que são para eliminar
  listaEliminar::[[Int]]->[Int] 
  listaEliminar lista = iSorted (posicaoeliminar lista)
  
  -- | Função que dada as posicoes iguais, devolve as posicoes que serão para eliminar 
  posicaoeliminar::[[Int]]->[Int]
  posicaoeliminar [] = []                                 
  posicaoeliminar (a:b) = (linhas a)++(posicaoeliminar b)
                where
                     linhas (a:b) = b
                     linhas _ = []
  
  -- | Aplica o repete nas 'Instrucoes' pretendidas.
  modificar::[[Int]]->Instrucoes->Instrucoes
  modificar [] mapa=mapa
  modificar ((a:b):r) mapa = (take (a) mapa2)++[f]++(drop (a+1) mapa2)
                  where
                      f = umElemento (daPosicoes (a:b) mapa)
                      mapa2 = modificar r mapa
  
  -- | Transforma uma lista de 'Instrucoes' iguais num repete
  umElemento::Instrucoes->Instrucao
  umElemento [a] = a
  umElemento l@(a:b) = Repete 1 [(f (averigua a))]
             where     
                       f::Instrucao->Instrucao
                       f (Anda _ piso)= Anda qqq piso 
                       f (Sobe _ piso a)= Sobe qqq piso a  
                       f (Desce _ piso a) = Desce qqq piso a 
                       --Utiliza a função 'eliminariguais' para retirar os elementos iguais
                       qqq= eliminariguais (qq l)
                       qq::Instrucoes->[Int]
                       qq [] = []
                       qq (a:b) = (h (averigua a))++(qq b) 
                      
  -- * Função Principal das 'horizontais'
  -- | Função que constrói as 'horizontais'.     
  horizontais::Instrucoes->Instrucoes                 
  horizontais l@(a:b) = dele mapanovo 0 eliminar
            where 
              posicoeahorizontais l = retirariguais (poshorizontais l 0 [])
              eliminar = iSorted (lista1 (retirar (posicoeahorizontais l)))
              mapanovo = modificar (posicoeahorizontais l) l
              --Função que aplica o repete nas posicoes pretendidas
              modificar [] mapa=mapa
              modificar ((a:b):r) mapa = (take (a) mapa2)++[f]++(drop (a+1) mapa2)
                              where
                                  mapa2 = modificar r mapa
                                  f = Repete (length (a:b)) (daPosicoes [a] mapa)

  -- ** Funções Auxiliares das 'horizontais'
  -- | Função que verifica quais são as posicoes iguais numa lista de 'Instrucoes'
  poshorizontais::Instrucoes->Int->[Int]->[[Int]]
  poshorizontais [] _ _ = []
  poshorizontais l@(m:n) num ver | lista == [] = poshorizontais n (num+1) ver
                                 | elem num ver = poshorizontais n (num+1) ver
                                 | otherwise =  [lista] ++ poshorizontais n (num+1) (iSorted (ver ++ lista))
                                      where  lista = procurar m l num

  -- | Função que dando uma 'Instrucao' verifica se a 'Instrucao', imediatamente a seguir, sempre na mesma pista, é igual. Caso se verifique, devolve a sua posição. 
  procurar::Instrucao->Instrucoes->Int->[Int]
  procurar _ [] _ = []
  procurar m (n:a) num | length (h m)/=1 = []
                       | length (h n)==1 && (averigua m == averigua n) =[num] ++ (procurar m a (num+1))
                       | length (h n)==1 && not (averigua m == averigua n) && not (elem (head (h m)) (h n)) = procurar m a (num+1)
                       | length (h n)==1 && not (averigua m == averigua n) && (elem (head (h m)) (h n)) = []
                       | not (averigua m == averigua n) && not (elem (head (h m)) (h n)) = procurar m a (num+1)
                       | length (h n)/=1 && not (averigua m == averigua n) && (elem (head (h m)) (h n)) = []
                       | otherwise = []
  
                   