module LaboratorioIntroduccion where

--Area de un circulo
areaCirculo :: (Num a, Fractional a) => a -> a
areaCirculo radio = (3.141592*radio^2)

--Area de un triangulo
areaTriangulo :: (Num a, Fractional a) => a -> a -> a
areaTriangulo base altura = (base * altura) / 2

--Nos indica si un entero es par
par :: Int -> Bool
par numero = if(numero `mod` 2 == 0)
             then True
             else False

par2 :: Int -> Bool
par2 numero = if(mod numero 2 == 0)
             then True
             else False

--Dada una lista nos regresa la lista volteada
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = (reversa xs) ++ [x]

--Tipo de dato para crear lógica proposicional
data Proposicion = Falso                                      -- False | 0
                 | Verdadero                                  -- True | 1
                 | Variable Var                               -- Cualquier letra del abecedario en mayúscula
                 | Neg Proposicion                            -- ¬P
                 | Con Proposicion Proposicion                -- P & Q
                 | Dis Proposicion Proposicion                -- P | Q
                 | Imp Proposicion Proposicion                -- P -> Q
                 | Syss Proposicion Proposicion deriving Eq   -- P <-> Q

--Tipo para definir variables en la lógica proposicional (sinonimo)
type Var = Char

--Conjunto donde guardaremos el valor de verdad de las variables
--Si la variable se encuentra en el conjunto, su valor será TRUE
--Si la variable no esta en el conjunto, su valor es FALSE
type Estado = [Var]

--Lista por comprensión para tomar todas las letras del abecedario
abc = [x | x <- ['A'..'Z']]

--Nos regresa la variable que le pedimos del abecedario
tomaVar :: Var -> [Char] -> Var
tomaVar _ [] = error "no se encuentra el caraceter"
tomaVar p (x:xs) = if(p == x)
                   then x
                   else tomaVar p xs

--Hacemos parte de la clase Show a Proposicion
--Para poder mostrar en pantalla de mejor manera las proposiciones
--Por ejemplo
-- Con (Neg (Variable (tomaVar 'P' abc))) (Variable (tomaVar 'Q' abc)) = (¬('P')) & ('Q')
instance Show Proposicion where
    show (Variable p) = "(" ++ show p ++ ")"
    show (Neg p) = "(¬" ++ show p ++ ")"
    -- Se deben de implementar las instancias a show de:
    -- Con
    show (Con p q) = "(" ++ show p ++ " & " ++ show q ++ ")"
    -- Dis
    show (Dis p q) = "(" ++ show p ++ " | " ++ show q ++ ")"
    -- Imp
    show (Imp p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
    -- Syss
    show (Syss p q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"
    -- Falso
    show (Falso) = "FALSO"
    -- Verdadero
    show (Verdadero) = "VERDADERO"

--Dada una proposición y un estado, determinar el valor de verdad de la proposición
interpretacion :: Proposicion -> Estado -> Bool
interpretacion prop est = case prop of
    Falso -> False
    Verdadero -> True
    Variable var -> (inter (Variable var) est)
    (Neg p) -> (not(interpretacion p est))
    (Con p q) -> (interpretacion p est) && (interpretacion q est)
    (Dis p q) -> (interpretacion p est) || (interpretacion q est)
    (Imp p q) -> (not(interpretacion p est)) || (interpretacion q est)
    (Syss p q) -> ((interpretacion p est) && (interpretacion q est)) || (not(interpretacion p est) && (not(interpretacion q est)))


inter :: Proposicion -> Estado -> Bool
inter (Variable var) [] = False
inter (Variable var) (x:xs) = if(var == x)
                              then True
                              else inter (Variable var) xs



{-- Pruebas vistas en la sesión --}

--Debe imprimir (('P') -> (¬('Q'))) <-> (('R') & (¬('S')))
prueba1 = Syss (Imp (Variable $ tomaVar 'P' abc) (Neg (Variable $ tomaVar 'Q' abc))) (Con (Variable $ tomaVar 'R' abc) (Neg (Variable $ tomaVar 'S' abc)))

--Debe imprimir ((¬('A')) | ('B')) -> (('C') & (¬('B')))
prueba2 = Imp (Dis (Neg (Variable $ tomaVar 'A' abc)) (Variable $ tomaVar 'B' abc)) (Con (Variable $ tomaVar 'C' abc) (Neg(Variable $ tomaVar 'B' abc)))

--Debe regresar True
prueba3 = interpretacion prueba2 ['C']

-- NO A | B  ->  C & NO B

-- NO FALSE | FALSE -> TRUE & NO FALSE

-- TRUE | FALSE -> TRUE & TRUE

-- TRUE -> TRUE

-- TRUE

{-- Pruebas extra --}

--Debe regresar False
prueba5 = interpretacion prueba1 ['P','S']

--Debe imprimir ((('P') -> ('Q')) & (('P') -> ('R'))) -> (('Q') -> ('R'))
prueba6 = Imp (Con (Imp (Variable $ tomaVar 'P' abc) (Variable $ tomaVar 'Q' abc)) (Imp (Variable $ tomaVar 'P' abc) (Variable $ tomaVar 'R' abc))) (Imp (Variable $ tomaVar 'Q' abc) (Variable $ tomaVar 'R' abc))

--Debe regresar True
prueba7 = interpretacion prueba6 ['P']

--Debe imprimir (((('R') & ('S')) -> ('T')) & (¬('T'))) <-> (('T') -> ('Q'))
prueba8 = Syss (Con (Imp (Con (Variable $ tomaVar 'R' abc) (Variable $ tomaVar 'S' abc)) (Variable $ tomaVar 'T' abc)) (Neg (Variable $ tomaVar 'T' abc))) (Imp (Variable $ tomaVar 'T' abc) (Variable $ tomaVar 'Q' abc))

--Debe regresar True
prueba9 = interpretacion prueba8 ['T']
