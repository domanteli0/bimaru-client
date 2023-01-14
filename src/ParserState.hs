module ParserState(Parser) where

import Control.Monad.Trans.Except (ExceptT)
import MyState
import Token

-- type ParserStateUnpacked a = ExceptT (MyState [Token]) String a
type Parser a = ExceptT String (MyState [Token]) a

