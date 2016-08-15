module Sound.Tidal.Hint where

import Sound.Tidal.Context


import Language.Haskell.Interpreter as Hint

import Data.Map

data Response = OK {parsed :: ParamPattern}
              | Error {errorMessage :: String}

instance Show Response where
  show (OK p) = "Ok: " ++ show p
  show (Error s) = "Error: " ++ s

runJob :: String -> IO (Response)
runJob job = do putStrLn $ "Parsing: " ++ job
                result <- hintParamPattern job
                let response = case result of
                      Left err -> Error (show err)
                      Right p -> OK p
                return response

libs = ["Prelude","Sound.Tidal.Context","Sound.OSC.Type","Sound.OSC.Datum","Data.Map"]

hintParamPattern  :: String -> IO (Either InterpreterError ParamPattern)
hintParamPattern s = Hint.runInterpreter $ do
  Hint.set [languageExtensions := [OverloadedStrings]]
  Hint.setImports libs
  Hint.interpret s (Hint.as :: ParamPattern)

