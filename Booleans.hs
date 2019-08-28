module Booleans where
import Prelude(Show)

data Bool = True | False
    deriving(Show)

not::Bool->Bool
not x = case x of {
    True->False;
    False->True;
    }

--Pruebas

not2:: Bool->Bool
not2 True=False
not2 False=True

--And

and::Bool->Bool->Bool
and True True = True
and _ _ = False

andCase::Bool->Bool->Bool
andCase x y = case x of {
    True -> y;
    False -> False;
    }

--Or

or::Bool->Bool->Bool
or False False = False
or _ _ = True

orCase::Bool->Bool->Bool
orCase x y = case x of {
    True -> True;
    False -> y;
    }

--Condicional
condicional::Bool->Bool->Bool
condicional True False = False
condicional _ _ = True

condicionalCase::Bool->Bool->Bool
condicionalCase x y = case x of {
    True -> y;
    _ -> True;
    }

--Condicional
bicondicional::Bool->Bool->Bool
bicondicional True True = True
bicondicional False False = True
bicondicional _ _ = False

bicondicionalCase::Bool->Bool->Bool
bicondicionalCase x y = case x of {
    True -> y;
    False -> not y;
    }

--And en una linea usando not y or
andUnaLinea:: Bool->Bool->Bool
andUnaLinea x y = not ( or (not x) (not y))

--Or en una linea usando and y not
orUnaLineaAndNot:: Bool->Bool->Bool
orUnaLineaAndNot x y = not (and (not x) (not y))

--And en una linea usando not y entonces
andUnaLineaNotEntonces:: Bool->Bool->Bool
andUnaLineaNotEntonces x y =  not (condicional x (not y))

--bicondicional usando entonces y not
bicondicionalUnaLineaNotEntonces:: Bool->Bool->Bool
bicondicionalUnaLineaNotEntonces x y = not (condicional (condicional x y) (not (condicional y x))) 