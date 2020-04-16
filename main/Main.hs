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
    
  -- more settings at
  -- https://github.com/tidalcycles/tidali/blob/master/src/Main.hs
  void $ LHI.runInterpreter $ do
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

message :: String -> InterpreterT IO _
message s = liftIO $ hPutStrLn stderr s

work :: Stream -> String -> IO ()
work tidal contents = 
  case LHE.parseExp contents of
      LHE.ParseFailed srclog msg -> do
        message $ "ParseFailed" <> msg
      LHE.ParseOk e -> do
        message $ "ParseOk" <> show e
        res <- catch ( Right <$> LHI.interpret contents
              (LHI.as :: STC.IO ())
            ( \ (e :: SomeException) -> return $ Left e)
        case res of
          Left e -> do
            message $ "InterpreterLeft"
            message $ show e
          Right x -> do
            message $ "InterpreterRight"
            liftIO $ STC.exec tidal x

-- | What is a "block"? Depends on flok,
-- https://github.com/munshkr/flok/issues/64#issuecomment-614589330
blocks :: [String] -> [[String]]
blocks [] = []
blocks css =
  let blank = all isSpace
      (pre, midpost) = span blank css
      (mid, post) = span (not . blank) midpost
  in  mid : blocks post



