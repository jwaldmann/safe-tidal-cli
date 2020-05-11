{-# language PatternSignatures #-}

import qualified Language.Haskell.Interpreter as I
import qualified Sound.Tidal.Safe.Context as C
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Catch
-- import qualified Mueval.Resources as MR
import System.Timeout
import System.IO
import Data.Char (isSpace)

main :: IO()
main = do
  
  -- from BootTidal.hs:
  tidal <- C.startTidal
    (C.superdirtTarget
      { C.oLatency = 0.1, C.oAddress = "127.0.0.1"
      , C.oPort = 57120})
    (C.defaultConfig {C.cFrameTimespan = 1/20})
    
  void $ I.runInterpreter
    $ catch (core tidal)
    $ \ (e :: SomeException) -> message $ show e

core tidal = do
  -- more settings at
  -- https://github.com/tidalcycles/tidali/blob/master/src/Main.hs
    I.set [ I.languageExtensions
              I.:= [ I.OverloadedStrings ]
            , I.installedModulesInScope I.:= False
            ]
    I.setImports
      [ "Prelude"
      , "Sound.Tidal.Safe.Context"
      , "Sound.Tidal.Safe.Boot"
      ]

    -- FIXME: replace lazy IO by some streaming mechanism?
    input <- liftIO getContents 
    mapM_ (work tidal . unlines) $ blocks $ lines input
    message "finished"

message :: String -> I.InterpreterT IO ()
message s = liftIO $ hPutStrLn stderr s

work :: C.Stream -> String -> I.InterpreterT IO ()
work tidal contents = 
        ( do
           -- TODO: need timeout for evaluation of pattern:
           x <- I.interpret contents (I.as :: C.Op ())
           -- have timeout for execution of pattern:
           liftIO $ void $ timeout (1*10^ 6) $ C.exec tidal x
        )
      `catch` \ (e :: I.InterpreterError) -> message (unlines $ case e of
          I.UnknownError s -> [ "UnknownError", s ]
          I.WontCompile gs -> "WontCompile" : map I.errMsg gs
          I.NotAllowed s   -> [ "NotAllowed", s ]
          I.GhcException s -> [ "GhcException", s ]
        )
      `catch` \ (e :: SomeException) -> message $ show e

-- | What is a "block"? Depends on flok,
-- https://github.com/munshkr/flok/issues/64#issuecomment-614589330
blocks :: [String] -> [[String]]
blocks [] = []
blocks css =
  let blank = all isSpace
      (pre, midpost) = span blank css
      (mid, post) = span (not . blank) midpost
  in  mid : blocks post



