module Main where

{-import HM.Normal-}
import HM.HigherRank
import HM.Monad
import HM.Types
import HM.Parser

import Control.Monad.IO.Class (liftIO)
import Text.PrettyPrint.HughesPJ
import Text.ParserCombinators.Parsec
import System.Exit (exitWith, ExitCode(..))
import System.Console.Haskeline
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)


main :: IO ()
main = do 
  args <- getArgs
  case args of 
    [] -> runInputT defaultSettings loop
    [f] -> tcf f
    _ -> do hPutStrLn stderr "Usage: program [FILE] "
            exitWith (ExitFailure 1)
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "% "
      case minput of
        Nothing -> return ()
        Just input -> do 
          liftIO $ tcs input
          loop



tcs :: String -> IO ()
tcs s = tc_help (parseString s)


tcf :: String -> IO ()
tcf f = tc_help (parseFile f)


tyvarA :: TyVar
tyvarA = BoundTv "a"


initTypeEnv :: [(Name,Sigma)]
initTypeEnv
      = [ ("+",    intType --> intType --> intType)
        , ("if",    ForAll [tyvarA] (boolType --> TyVar tyvarA --> TyVar tyvarA))
        , ("True",  boolType)
        , ("False", boolType)
        ]


tc_help :: IO (Maybe Term) -> IO ()
tc_help get_term
  = do  { mb_e <- get_term
        ; case mb_e of {
            Nothing -> return () ;
            Just e  -> do {
          res <- runTc initTypeEnv (typecheck e)
        ; case res of
            Left err -> putStrLn (docToString err)
            Right ty -> putStrLn (docToString (sep [pprParendTerm e, nest 2 (dcolon <+> ppr ty)]))
       }}}


parseFile :: String -> IO (Maybe Term)
parseFile filename
  = do { r <- parseFromFile parseTerm filename
         ; case r of
         Left err -> do { putStrLn ("Parse error: " ++ show err) 
                ; return Nothing }
         Right ans -> return (Just ans) }


parseString :: String -> IO (Maybe Term)
parseString str
  = do { let r = parse parseTerm "<interactive>" str
       ; case r of
         Left err -> do { putStrLn ("Parse error: " ++ show err) 
                ; return Nothing }
         Right ans -> return (Just ans) }

