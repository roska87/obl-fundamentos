-- Autor:
-- Numero de estudiante:
-- Autor:
-- Numero de estudiante:

{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs#-}

module Prog where

type Var = String

data Exp where { V    :: Var -> Exp;
				 I    :: Int -> Exp;
				(:+)  :: Exp -> Exp -> Exp;
				(:-)  :: Exp -> Exp -> Exp;
				(:*)  :: Exp -> Exp -> Exp ;
				(:&&) :: Exp -> Exp -> Exp;
				(:||) :: Exp -> Exp -> Exp;
				 Not   :: Exp -> Exp;
				(:==) :: Exp -> Exp -> Exp} 
			deriving(Eq,Show)

-- Se define que :* tiene mayor precedencia que :+ y :-
infixl 8 :*
infixl 6 :+
infixl 6 :-
-- Se define que :&& tiene mayor precedencia que :||
infixl 7 :&&
infixl 6 :||
-- :== tiene la menor precedencia
infixl 4 :==

type Memoria = [(Var,Int)]

data Prog where {
    Asig  :: [(Var,Exp)] -> Prog;
    (:>)  :: Prog -> Prog -> Prog;
    Cond  :: [(Exp,Prog)] -> Prog;
    While :: Exp -> Prog -> Prog  }
  deriving(Eq,Show)

-- Se define que :> tiene la menor precedencia
infixr 3 :>

-- Memoria y evaluación de expresiones
-- 1 
(@@) :: Var -> Memoria -> Int
(@@) = undefined

-- 2
upd :: (Var,Int) -> Memoria -> Memoria
upd = undefined

-- 3
eval :: Exp -> Memoria -> Int
eval = undefined



-- Ejecución de programas
-- 4
run :: Prog -> Memoria -> Memoria
run = undefined
										


-- Ejemplos de programas
p0 :: Prog
p0 = Asig [("x", I 1)] :> Asig [("x", V "x" :+ I 10)]


p1 :: Prog
p1 = Asig [("x" , I 1), ("y", I 2)] 
     :> Cond [(V "y" :- V "x" , Asig [("z", I 10)]),
              (I 1, Asig [("z", I 0)])]

p2 :: Prog
p2 = Asig [("x" , I 27), ("y", I 5)] 
     :> While (V "x") (Asig [("y", V "y" :+ I 2)] :> Asig [("x", V "x" :- V "y")])

-- Programas a definir

-- 5
swap:: Prog
swap = undefined

-- 6
fact :: Int -> Prog
fact = \n -> undefined

-- 7
par :: Int -> Prog
par = \n -> undefined
			
-- 8
mini :: Int -> Int -> Prog
mini = \m n -> undefined

-- 9
fib :: Int -> Prog 
fib = \n -> undefined



-- Para probar los programas fact, par, mini y fib recomendamos utilizar las siguientes funciones:									

factorial = \n -> pre (n >= 0) "fact: argumento negativo"   ("fact" @@ run (fact n) [])

esPar = \n -> pre (n >= 0) "par: argumento negativo"  ("par" @@ (run (par n) []) /= 0)

minimo = \m n -> pre (m >= 0 && n >= 0) "mini: argumento negativo"   ("min" @@ run (mini m n) [])

fibonacci = \n -> pre (n >= 0) "fib: argumento negativo"   ("fib" @@ run (fib n) [])


-- Se usa pre para poder verificar que los argumentos sean válidos

pre :: Bool -> String -> a -> a 
pre = \b s x -> case b of {False -> error s ; True -> x}
