{-
- Lógica Computacional 2021-2
- Profesor: Favio Ezequiel Miranda Perea
- Ayudante: Javier Enríquez Mendoza
- Ayudante: Fernando Abigail Galicia Mendoza
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: América Montserrat García Coronado
- Practica 1: Recordando Haskell. Listas
- Integrantes:
- David Hernández Uriostegui
- Diego Arturo Velázquez Trejo
-}

module Practica01 where

data List a = Void | Cons a (List a) -- deriving (Show, Eq)

instance (Show a) => Show (List a) where
  show Void = "[]"
  show (Cons a Void) = "[" ++ show a ++ "]"
  show (Cons a b) =
    let showHelp Void = "]"
        showHelp (Cons a1 Void) = show a1 ++ "]"
        showHelp (Cons a1 b1) = show a1 ++ ", " ++ showHelp b1
     in "[" ++ show a ++ ", " ++ showHelp b





--showList   :: Time -> String

instance (Eq a) => Eq (List a) where
  (==) a b = equalHelp a b  

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------


-- | Funcion auxiliar para comparar 2 listas
equalHelp :: (Eq a) => List a -> List a -> Bool
equalHelp Void Void = True
equalHelp a Void = False
equalHelp Void b = False
equalHelp (Cons a b) (Cons c d) = if (a == c) then (equalHelp b d) else False

-- | Funcion que regVoid = resa tal vez la cabeza de la lista.
myHead :: List a -> a
myHead Void = error "La lista no tiene elementos"
myHead (Cons a Void) = a
myHead (Cons a b) = a

-- | Funcion que regresa tal vez la cola de la lista.
myTail :: List a -> (List a)
myTail Void = Void
myTail (Cons a Void) = (Cons a Void)
myTail (Cons a b) = b

-- | Funcion que concatena recursivamente dos listas.
myConcat :: List a -> List a -> List a
myConcat Void a = a
myConcat a Void = a
myConcat Void b = b
myConcat (Cons a b) l  = Cons a (myConcat b l)


-- | Funcion que dado un elemento =e= y una lista =l=, regresa la
-- lista =l= sin =e=.
remove :: (Eq a) => a -> List a -> List a
remove a Void = Void
remove a (Cons c d) = if(a == c) then remove a d
                      else (Cons c (remove a d))

-- | Funcion que nos dice si un elemento está contenido en una lista.
myElem :: (Eq a) => a -> List a -> Bool
myElem a Void = False
myElem a (Cons c d) = if(a == c) then True
                      else myElem a d

-- Funcion que filtra en nuestras listas.
myFilter :: (a -> Bool) -> List a -> List a
myFilter f Void = Void
myFilter f (Cons a b) = if( f(a) ) then Cons a (myFilter f b)
                        else myFilter f b

-- Funcion que mapea una funcion a nuestras listas.
myMap :: (a -> b) -> List a -> List b
myMap f Void = Void
myMap f (Cons a b) = Cons (f(a)) (myMap f b)



-- Funcion que devuelve la intersección de dos listas.
interseccion :: (Eq a) => List a -> List a -> List a
interseccion Void a = Void
interseccion a Void = Void
interseccion (Cons a b) l = if( myElem a l) then Cons a (interseccion b l)
                            else interseccion b l

-- Funcion que devuelve la intersección de dos listas.
intersection :: (Eq a) => [a] -> [a] -> [a]
intersection [] l = []
intersection l [] = []
intersection (x:xs) l = if(x `elem` l) then [x]++ (intersection xs l)
                        else intersection xs l

-- Funcion que calcula la diferencia simétrica de dos listas.
symetric :: (Eq a) => [a] -> [a] -> [a]
symetric = error "D:"

-- Función que devuelve el conjunto potencia de una lista.
potencia :: Eq a => [a] -> [[a]]
potencia = error "D:"

--------------------------------------------------------------------------------
--------                             PRUEBAS                            --------
--------------------------------------------------------------------------------

-- Lista que contiene a los primeros cinco elementos.
l1 = (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Void)))))

-- Lista que contiene a los elementos del 6-10.
l2 = (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 Void)))))

-- instancia Eq

eq1 = l1 == l2
-- Regresa: False

eq2 = l1 == l1
-- Regresa: True

myHead1 = myHead l1
-- Regresa: Just 1

myHead2 = myHead Void
-- Regresa: Nothing

myTail1 = myTail l1
-- Regresa: Just (2:(3:(4:(5:[]))))

myTail2 = myTail  Void
-- Regresa: Nothing

myConcat1 = myConcat l1 l2
-- Regresa: (1:(2:(3:(4:(5:(6:(7:(8:(9:(10:[]))))))))))

myConcat2 = myConcat l2 l1
-- Regresa: (6:(7:(8:(9:(10:(1:(2:(3:(4:(5:[]))))))))))

remove1 = remove 1 l1
-- Regresa: (2:(3:(4:(5:[]))))

remove2 = remove 4 l2
-- Regresa: (6:(7:(8:(9:(10:[])))))

myElem1 = myElem 1 l1
-- Regresa: True

myElem2 = myElem 4 l2
-- Regresa: False

myFilter1 = myFilter odd l1
-- Regresa: (1:(3:(5:[])))

myFilter2 = myFilter even l1
-- Regresa: (2:(4:[]))

myMap1 = myMap (+5) l1
-- Regresa: (6:(7:(8:(9:(10:[])))))

myMap2 = myMap (*0) l2
-- Regresa: (0:(0:(0:(0:(0:[])))))

intersection1 = intersection [1,2,3] [2,3]
-- Regresa: [2,3]

symetric1 = symetric [1,2,3] [2,3]
-- Regresa: [1]

potencia1 = potencia [1,2,3]
-- Regresa: [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
