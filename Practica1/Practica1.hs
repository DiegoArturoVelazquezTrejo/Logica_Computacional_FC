{-
-- | Lógica Computacional 2022-01
-- | Práctica 1: Introducción a Haskell
-- | Profesor: Dr. Favio E. Miranda Perea
-- | Ayudante: Javier Enríquez Mendoza
-- | Ayudante: Ramón Arenas Ayala
-- | Laboratorio: Daniel Lugo Cano
-}

module Practica1 where

-- | Tipo de dato Binario, es la representacion de todos los numero binarios que empiezan con uno
data Binario = U | C | Cero Binario | Uno Binario deriving Eq

-- |1| Definicion de la clase Show para el tipo de dato Binario
instance Show Binario where
    show (Cero bin) = show bin ++ "0"
    show (Uno  bin) = show bin ++ "1"
    show (U) = "1"
    show (C) = "0"

-- |2| sucesor. Regresa el sucesor de un Binario
-- -> Ejemplo sucesor de U (uno)  = Cero U , sucesor de 1 es 10
sucesor  :: Binario -> Binario
sucesor (U) = (Cero U)
sucesor (Cero bin) = (Uno bin)
sucesor (Uno  bin) = (Cero  $ sucesor bin)

-- |3| suma. Regresa la suma de 2 numeros de un Binarios
-- -> ejemplo suma de U U = Cero U , suma de 1 y 1 es 10
suma :: Binario -> Binario -> Binario
suma (U) (U) = (Cero U)
suma (U) (C) = (U)
suma (C) (U) = (U)
suma binario1 binario2 = suma_aux binario1 binario2 0

-- Función auxiliar con el acarrero
suma_aux :: Binario -> Binario -> Int -> Binario

-- Casos base

suma_aux U U ac = if(ac == 0) then (Cero U)
                  else (Uno U)

suma_aux (Cero binario1) C ac = if(ac == 0) then (Cero binario1)
                                else (Uno binario1)

suma_aux (Uno binario1) C ac = if(ac == 0) then (Uno binario1)
                                else (Cero binario1)

-- Casos en donde las longitudes son distintas

suma_aux (Uno binario)  (U) ac = if(ac == 0) then Cero (suma_aux binario C 0)
                                 else Uno (suma_aux binario C 0)

suma_aux (Cero binario) (U) ac = if(ac == 0) then Uno (suma_aux binario C 0)
                                 else Cero (suma_aux binario C 0)

suma_aux (U) cualquier_sumando  ac = suma_aux cualquier_sumando (U) ac  -- realmente, la suma es conmutativa, entonces para ahorrarnos casos, vamos a regresar a los dos de arriba

-- Casos en donde las longitudes de bits son las mismas
suma_aux (Uno binario1) (Uno binario2)   ac = if(ac == 0) then Cero (suma_aux binario1 binario2 1)
                                          else Uno (suma_aux binario1 binario2 0)

suma_aux (Uno binario1) (Cero binario2)  ac = if(ac == 0) then Uno (suma_aux binario1 binario2 0)
                                          else Cero (suma_aux binario1 binario2 0)

suma_aux (Cero binario1) (Uno binario2)  ac = if(ac == 0) then Uno (suma_aux binario1 binario2 0)
                                          else Cero (suma_aux binario1 binario2 0)

suma_aux (Cero binario1) (Cero binario2) ac = if(ac == 0) then Cero (suma_aux binario1 binario2 0)
                                          else Uno (suma_aux binario1 binario2 0)

-- |4| producto. Regresa el producto de 2 numeros Binarios
-- -> Ejemplo producto de (Cero U) (Cero U) = (Cero (Cero U)) , producto de 10 con 10 es 100
producto :: Binario -> Binario -> Binario
producto a b = natABin ((binANat a) * (binANat b))

-- |5| natBinLista. Regresa el la representacion en Binario de un numero Decimal en forma de Lista
-- -> ejemplo natBinLista 8 = [1,0,0,0]
natBinLista :: Int -> [Int]
natBinLista numero = binLista (natABin numero)

-- Necesitamos una función para pasar de binario a lista
binLista :: Binario -> [Int]
binLista (U) = [1]
binLista (Cero binario) = (binLista binario) ++ [0]
binLista (Uno binario)  = (binLista binario) ++ [1]

-- |6| sumaBinLista. Regresa la suma de 2 listas que representan 2 numeros binarios.
-- -> ejemplo sumaBinLista de [1] [1,0] = (Uno U)
sumaBinLista :: [Int] -> [Int] -> Binario
sumaBinLista [0] b = error "No se permite sumar el elemento neutro"
sumaBinLista a [0] = error "No se permite sumar el elemento neutro"
sumaBinLista a b = suma (listaAbin a) (listaAbin b)

{-- PUNTOS EXTRA --}

-- |1| natABin: Recibe un natural (mayor que 1) y devuelve un binario
natABin :: Int -> Binario
natABin numero = natABin_aux numero

-- Función auxiliar para realizar la conversión del número
natABin_aux :: Int -> Binario
natABin_aux 1 = (U)
natABin_aux 0 = (C)
natABin_aux n = if((mod n 2) == 0) then (Cero (natABin_aux (div n 2)))
                else (Uno (natABin_aux (div n 2)))

-- Podemos pasar el número a una lista y de la lista a binario
listaAbin :: [Int] -> Binario
listaAbin a = listaAbin_aux (reversa a )

listaAbin_aux :: [Int] -> Binario
listaAbin_aux [1] = U
listaAbin_aux (x:xs) = if(x == 0) then Cero (listaAbin_aux xs)
                       else Uno (listaAbin_aux xs)

-- Función reversa de una lista
reversa :: [a] -> [a]
reversa [] = []
reversa [x]= [x]
reversa (x:xs) = (reversa xs) ++ [x]

-- |2| binANat: Recibe un binario y devuelve su reprentacion en numero natural (mayor que 1).
binANat :: Binario -> Int
binANat a = binANat_aux a 0

binANat_aux :: Binario -> Int -> Int
binANat_aux (U) n = 2 ^ n
binANat_aux (Cero bin) n = (binANat_aux bin (n+1))
binANat_aux (Uno  bin) n = (2 ^ n) + (binANat_aux bin (n+1))

-- |3| predecesor: Recibe un binario y devuelve su binario anterior
predecesor :: Binario -> Binario
predecesor numero = natABin ((binANat numero )- 1)
