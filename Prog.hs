-- Autor: Ari Rostkier
-- Numero de estudiante: 136300
-- Autor: Vicente Bermúdez
-- Numero de estudiante: 214831

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
(@@) = \v -> \m -> case m of {
	[] -> error "La variable no se encuentra en la memoria";
	x:xs -> case v == (fst x) of {
		True -> snd x;
		False -> v @@ xs;
	}
}

-- 2
upd :: (Var,Int) -> Memoria -> Memoria
upd = \t -> \m -> case m of {
	[] -> [t];
	x:[] -> case (fst t) == (fst x) of {
		True -> [(fst t, snd t)];
		False -> x:(upd t []);
	};
	x:xs -> case (fst t) == (fst x) of {
		True -> (fst t, snd t):(upd t xs);
		False -> x:(upd t xs);
	}
}

-- 3

(&&&&) :: Int -> Int -> Int
(&&&&) = \a -> \b -> case (b == 0) of {
	True -> 0;
	False -> case (a > 0) of {
		True -> 1;
		False -> 0;
	}
}

(||||) :: Int -> Int -> Int
(||||) = \a -> \b -> case ((a + b) > 0) of {
	True -> 1;
	False -> 0;
}

(====) :: Int -> Int -> Int
(====) = \a -> \b -> case ((a - b) == 0) of {
	True -> 1;
	False -> 0;
}

eval :: Exp -> Memoria -> Int
eval = \e -> \m -> case m of {
	[] -> 0;
	x:xs -> case e of {
		V n -> n @@ m;
		I n -> n;
		(:+) i d -> case i of {
			I n -> n + (eval d m);
			V v -> (v @@ m) + (eval d m);
		};
		(:*) i d -> case i of {
			I n -> n * (eval d m);
			V v -> (v @@ m) * (eval d m);
		};
		(:-) i d -> case i of {
			I n -> n - (eval d m);
			V v -> (v @@ m) - (eval d m);
		};
		(:&&) i d -> case i of {
			I n -> n &&&& (eval d m);
			V v -> (v @@ m) &&&& (eval d m);
		};
		(:||) i d -> case i of {
			I n -> n |||| (eval d m);
			V v -> (v @@ m) |||| (eval d m);
		};
		(:==) i d -> case i of {
			I n -> n ==== (eval d m);
			V v -> (v @@ m) ==== (eval d m);
		};
		Not n -> case (eval n m) of {
			0 -> 1;
			1 -> 0;
		};
	}
}



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
