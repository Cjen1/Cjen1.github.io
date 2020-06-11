module Utils where

import Control.Monad
import Hakyll

mkCompiler f = do
    s <- getResourceBody
    return $ fmap f s