
{--
-- Autómatas y Lenguajes Formales
-- Version: 0.1
-- Author: Jesus-Vila
--}

module TM where

-- | Basic types
type Simbolo = Char
type Estado = [Char]
type Alfabeto = [Simbolo]

-- | TM motion values
data Movimiento = L | R deriving (Show, Eq)

-- | Transition function type alias
type Delta = Estado -> Simbolo -> (Estado, Simbolo, Movimiento)

-- | Turing machine structure
data MaqT = MaqT { q :: [Estado],  -- conjunto de estados
                   q0 :: Estado,   -- estado inicial
                   qf :: Estado,   -- estado de aceptación
                   qr :: Estado,   -- estado de rechazo
                   s :: Alfabeto,  -- alfabeto de entrada
                   g :: Alfabeto   -- alfabeto de cinta
                 } deriving Show

-- | Turing machine definition
data MT = MT { mtupla :: MaqT,  -- Turing machine tuple structure
               dltfun :: Delta  -- transition function
             }


-- | Show functions for standard Turing Machines
pintaEstados :: [Estado] -> String
pintaEstados [] = ""
pintaEstados (l:le) = l ++ "  " ++ pintaEstados le

pintaEstado :: Estado -> String
pintaEstado e = e ++ ""

pintaAlfabeto :: Alfabeto -> String
pintaAlfabeto [] = ""
pintaAlfabeto (l:la) = [l] ++ " " ++ pintaAlfabeto la

-- | Auxiliar function to produce the cross product between
-- | a set of states and an alphabet
generaPares :: [Estado] -> Alfabeto -> [(Estado, Simbolo)]
generaPares le ls = [(e, s)| e <- le, s <- ls]

-- | Show functions for a TM delta function
pintaDeltaAux :: Delta -> [(Estado, Simbolo)] -> String
pintaDeltaAux _ [] = "\n"
pintaDeltaAux t ((e, s):xs) =
  case t e s of
      (e', s', m) -> "                        d " ++
                     e ++ " " ++ [s] ++ " = " ++ " " ++e' ++
                     " " ++ [s'] ++ " " ++ show m ++ "\n" ++ pintaDeltaAux t xs

pintaDelta :: [Estado] -> Alfabeto -> Delta -> String
pintaDelta le ls t = pintaDeltaAux t (generaPares le ls)


instance Show MT where
  show mt = "\nEstados:: "
            ++ pintaEstados (estados mt) ++ "\n" ++
            "\nEstado Inicial:: "
            ++ pintaEstado (estadoInicial mt) ++ "\n" ++
            "\nEstado de Aceptación:: "
            ++ pintaEstado (estadoAcept mt) ++ "\n" ++
            "\nEstado de Rechazo:: "
            ++ pintaEstado (estadoRechazo mt) ++ "\n" ++
            "\nAlfabeto de Entrada:: "
            ++ pintaAlfabeto (sigma mt) ++ "\n" ++
            "\nAlfabeto de la Cinta:: "
            ++ pintaAlfabeto (gamma mt) ++ "\n" ++
            "\nFunción de Transición::\n"
            ++ pintaDelta (estados mt) (gamma mt) d
    where
      d = funTransicion mt


-- | Gets the input alphabet from a Turing Machine
sigma :: MT -> Alfabeto
sigma = s . mtupla

-- | Gets the tape alphabet from a Turing Machine
gamma :: MT -> Alfabeto
gamma = g . mtupla

-- | Gets the state set from a Turing Machine
estados :: MT -> [Estado]
estados = q . mtupla

-- | Gets the initial state from a Turing Machine
estadoInicial :: MT -> Estado
estadoInicial = q0 . mtupla

-- | Gets the accepting state from a Turing Machine
estadoAcept :: MT -> Estado
estadoAcept = qf . mtupla

-- | Gets the rejecting state from a Turing Machine
estadoRechazo :: MT -> Estado
estadoRechazo = qr . mtupla

-- | Gets the transition function
funTransicion :: MT -> Delta
funTransicion = dltfun


-- | TM that accepts binary strings with even number of 0's
maqtP :: MaqT
maqtP = MaqT { q=["q0", "q1", "qf"], q0="q0", qf="qf", qr="qr", s="01", g="01_"}

delP :: Delta
delP "q0" s
  | s == '_' = ("qf", '_', R)
  | s == '0' = ("q1", '_', R)
  | s == '1' = ("q0", '1', R)
delP "q1" s
  | s == '0' = ("q0", '_', R)
  | s == '1' = ("q1", '1', R)
delP _ _ = ("qr", 'E', R)

tmPares :: MT
tmPares = MT { mtupla=maqtP, dltfun=delP }

-- | TM for a^n b^n c^n
maqt :: MaqT
maqt = MaqT {q=["q0", "q1", "q2", "q3", "q4", "qf"], q0="q0", qf="qf", qr="qr", s="abc", g="abcXYZE_"}

del :: Delta
del "q0" s
  | s == 'a' = ("q1", 'X', R)
  | s == 'Y' = ("q4", 'Y', R)
  | s == '_' = ("qf", '_', L)
del "q1" s
  | s == 'a' = ("q1", 'a', R)
  | s == 'Y' = ("q1", 'Y', R)
  | s == 'b' = ("q2", 'Y', R)
del "q2" s
  | s == 'b' = ("q2", 'b', R)
  | s == 'Z' = ("q2", 'Z', R)
  | s == 'c' = ("q3", 'Z', L)
del "q3" s
  | s == 'a' = ("q3", 'a', L)
  | s == 'b' = ("q3", 'b', L)
  | s == 'Y' = ("q3", 'Y', L)
  | s == 'Z' = ("q3", 'Z', L)
  | s == 'X' = ("q0", 'X', R)
del "q4" s
  | s == 'Y' = ("q4", 'Y', R)
  | s == 'Z' = ("q4", 'Z', R)
  | s == '_' = ("qf", '_', R)
del _ _ = ("qr", 'E', R)

tmanbncn :: MT
tmanbncn = MT { mtupla=maqt, dltfun=del }

-- | Configuration-useful types
type Cadena = [Simbolo]
type Configuracion = (Estado, Cadena, Int)

-- | This function takes a string str, an integer i, and a symbol s
-- | and substitutes s in the i-th position of str
sustituye :: Cadena -> Int -> Simbolo -> Cadena
sustituye [] _ _ = []
sustituye (w:ws) 0 a = a:ws
sustituye (w:ws) n a
  | n < 0 = error "invalid index!"
  | otherwise = w: sustituye ws (n-1) a

headBlank :: Cadena -> Simbolo
headBlank [] = '_'
headBlank xs = head xs

lastBlank :: Cadena -> Simbolo
lastBlank [] = '_'
lastBlank xs = last xs

-- | Delta function
delta :: Delta -> Configuracion -> Configuracion
delta t (q, w, n)
	| wn == n =
		case t q (lastBlank w) of
			(p, s, L) -> (p, sustituye w n s, n-1)
			(p, s, R) -> (p, sustituye w n s ++ ['_'], n+1)
	| n == 0 =
		case t q (headBlank w) of
			(p, s, L) -> (p, ['_'] ++ sustituye w n s, 0)
			(p, s, R) -> (p, sustituye w n s, 1)
	| otherwise =
		case t q (w!!n) of
			(p, s, L) -> (p, sustituye w n s, n-1)
			(p, s, R) -> (p, sustituye w n s, n+1)
	where wn = length w

-- | Closure for delta function
deltaEstrella :: MT -> Configuracion -> Bool
deltaEstrella mt (q, w, n)
  | q == estadoAcept mt = True
  | q == estadoRechazo mt = False
  | otherwise = deltaEstrella mt (delta d (q, w, n))
  where
    d = funTransicion mt

-- | Decides if the given string is accepted by the given TM
aceptaCadena :: MT -> Cadena -> Bool
aceptaCadena mt w = deltaEstrella mt (estadoInicial mt, w, 0)

-- | Auxiliar for Kleene star operation
kln :: Alfabeto -> Int -> [Cadena]
kln s 0 = [""]
kln s n
  | n < 0 = error "invalid index!"
  | otherwise = [a:w | a <- s , w <- kln s (n-1)]

-- | Kleene star of an alphabet
klns :: Alfabeto -> [Cadena]
klns s = concat [kln s n | n <- [0..] ]

-- | Lazy construction of the language accepted by a TM
lenguajeAceptado :: MT -> [Cadena]
lenguajeAceptado mt = let kleene = klns s in filter (aceptaCadena mt) kleene
  where s = sigma mt
