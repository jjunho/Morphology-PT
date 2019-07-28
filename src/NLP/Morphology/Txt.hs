module NLP.Morphology.Txt where

import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

tshow :: Show a => a -> Text
tshow = T.pack . show

putTxt :: Txt a => a -> IO ()
putTxt = TIO.putStr . txt

putTxtLn :: Txt a => a -> IO ()
putTxtLn = TIO.putStrLn . txt

class Txt a where
  txt :: a -> Text

instance Txt Text where
  txt = id
