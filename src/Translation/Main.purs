module Translation.Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Node.Process as Node
import Translation (translate)

main :: Effect Unit
main = do
  argv <- Node.argv # map (Array.drop 2)
  -- Console.logShow { argv }
  str <- case argv of
    [ str ] -> pure str
    _ -> throwError (error "you must provide a string to translate")
  Console.log $ translate str