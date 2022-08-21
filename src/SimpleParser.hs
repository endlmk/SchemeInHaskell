module SimpleParser
    ( parse, Atom(Symbol, Number), LispVal(Atom, List)
    ) where
import qualified Control.Applicative as String
import Prelude (String, Double, Show, Eq)
import qualified Text.Read as Atom

data Atom = Symbol String  
          | Number Double 
          deriving (Eq, Show)

data LispVal = Atom Atom
             | List [LispVal]
             deriving (Eq, Show)

parse :: String -> LispVal
parse _ = Atom (Symbol "")