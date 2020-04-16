{-# language PatternSignatures #-}

import qualified Language.Haskell.Interpreter as LHI
import qualified Sound.Tidal.Safe.Context as STC
import qualified Language.Haskell.Exts as LHE
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Catch
import System.IO
import Data.Char (isSpace)

main :: IO()
main = do
  
  -- from BootTidal.hs:
  tidal <- STC.startTidal
    (STC.superdirtTarget
      { STC.oLatency = 0.1, STC.oAddress = "127.0.0.1"
      , STC.oPort = 57120})
    (STC.defaultConfig {STC.cFrameTimespan = 1/20})
    
  void $ LHI.runInterpreter
    $ catch (core tidal)
    $ \ (e :: SomeException) -> message $ show e

core tidal = do
  -- more settings at
  -- https://github.com/tidalcycles/tidali/blob/master/src/Main.hs
    LHI.set [ LHI.languageExtensions
              LHI.:= [ LHI.OverloadedStrings ]
            , LHI.installedModulesInScope LHI.:= False
            ]
    LHI.setImports
      [ "Prelude", "Data.Map"
      , "Sound.Tidal.Safe.Context"
      , "Sound.Tidal.Safe.Boot"
      ]

    -- FIXME: replace lazy IO by some streaming mechanism?
    input <- liftIO getContents 
    mapM_ (work tidal . concat) $ blocks $ lines input
    message "finished"

message :: String -> LHI.InterpreterT IO ()
message s = liftIO $ hPutStrLn stderr s

work :: STC.Stream -> String -> LHI.InterpreterT IO ()
work tidal contents = 
  case LHE.parseExp contents of
      LHE.ParseFailed srclog msg -> do
        message $ "ParseFailed" <> msg
      LHE.ParseOk e -> do
        message $ "ParseOk" <> show e
        catch ( do
           x <- LHI.interpret contents (LHI.as :: STC.Op ())
           message $ "InterpreterOK"
           liftIO $ STC.exec tidal x
         ) $ \ (e :: SomeException) -> message $ show e

-- | What is a "block"? Depends on flok,
-- https://github.com/munshkr/flok/issues/64#issuecomment-614589330
blocks :: [String] -> [[String]]
blocks [] = []
blocks css =
  let blank = all isSpace
      (pre, midpost) = span blank css
      (mid, post) = span (not . blank) midpost
  in  mid : blocks post



