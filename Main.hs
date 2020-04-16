{-# language PatternSignatures #-}

import qualified Language.Haskell.Interpreter as LHI
import qualified Sound.Tidal.Context as STC
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
    LHI.setImports
      [ "Prelude", "Sound.Tidal.Context", "Data.Map" ]
    LHI.set [ LHI.languageExtensions
              LHI.:= [ LHI.OverloadedStrings ] ]
       
    -- FIXME: replace lazy IO by some streaming mechanism?
    input <- liftIO getContents 
    mapM_ (work tidal) $ blocks $ lines input
    message "finished"

message s = liftIO $ hPutStrLn stderr s

-- | for now, parse expn. of type (Int, ControlPattern)
work tidal block = do
  let contents = concat block 
  case LHE.parseExp contents of
      LHE.ParseFailed srclog msg -> do
        message $ "ParseFailed" <> msg
      LHE.ParseOk e -> do
        message $ "ParseOk" <> show e
        res <- catch ( Right <$> LHI.interpret contents
              (LHI.as :: (Int, STC.ControlPattern)) )
            ( \ (e :: SomeException) -> return $ Left e)
        case res of
          Left e -> do
            message $ show e
          Right (i,p) -> do
            message $ "InterpreterRight" ++ show (i,p)
            if 1 <= i && i <= 12
              then do
                liftIO $ STC.streamReplace tidal i
                  $ (STC.|< STC.orbit (pure $ i-1)) p
              else message $ "non-existing channel"

-- | What is a "block"? Depends on flok,
-- https://github.com/munshkr/flok/issues/64#issuecomment-614589330
blocks :: [String] -> [[String]]
blocks [] = []
blocks css =
  let blank = all isSpace
      (pre, midpost) = span blank css
      (mid, post) = span (not . blank) midpost
  in  mid : blocks post



