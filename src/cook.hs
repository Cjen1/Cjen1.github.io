module Cook where

import Data.List
import Data.Function
import GHC.TypeLits.Printf
import Control.Monad
import Text.Read
import Utils

class Sym repr where
  ing :: String -> Float -> repr
  mix :: [repr] -> repr
  process :: String -> [repr] -> repr

newtype D r r'= D{unD :: (r, r')}
instance (Sym r, Sym r')=> Sym (D r r') where
  ing s q   = D $ (ing s q, ing s q)
  mix ss    = D $ (mix ss', mix ss'') where (ss',ss'') = unzip (map unD ss)
  process s rs = D $ (process s rs', process s rs'') where (rs',rs'') = unzip (map unD rs)
duplicate x = unD x

-- Need to feed back down colspan (to increase the size of children)
-- colspan needs to be the difference between that ones span and the maximum
-- Need to feed up rowspan to ensure correct alignment
-- So at each step rowspan needs to be the sum of all previous rowspans

-- To make the actual html, need to feed the remainder of each row into the html
-- Finally we concatenate each row with the <tr>

-- (rowspan, remainder of row -> repr), rowspan, colspan
type HtmlTable = (Int -> [String], Int)
instance Sym HtmlTable where
  ing s q   = ((\(col) -> [printf @"<th colspan=%d>%.0fg of %s</th>" col q s]), 1)
  mix ss    = process "mix together" ss
  process s ss = (sf,colspan + 1)
    where
      colspan = maximum $ map snd ss
      f (sf, col) = sf (colspan - col + 1)
      evald   = foldr mappend [] $ map f ss
      rowspan = length evald
      sf col  =
        let item = printf @"%s\n<td rowspan=%d colspan=%d>%s</td>" (head evald) rowspan col s in
        item : (tail evald)
compile_to_html :: HtmlTable -> String
compile_to_html t =
  printf @"<table>\n%s\n</table>" html
  where
    (htmls,_) = t
    html = printf @"<tr>\n%s\n</tr>" (intercalate "\n</tr><tr>\n" (htmls 1))

data Tree = L String | N [Tree] String deriving (Eq, Read, Show)

instance Sym Tree where
  ing s q      = N [L s, L $ show q] "ing" 
  mix ss       = N ss "mix" 
  process s ss = N ((L s):ss) "process" 
toTree :: Tree -> Tree
toTree t = t

type ErrMsg = String
safeRead :: Read a => String -> Either ErrMsg a
safeRead s = case reads s of
  [(x,"")] -> return x
  _ -> Left $ "Read error: " ++ s

fromTree :: (Sym repr) => Tree -> Either ErrMsg repr
fromTree (N [L s, L q] "ing")     = return (ing s) `ap` (safeRead q)
fromTree (N xs "mix")             = return mix `ap` (mapM fromTree xs)
fromTree (N (L s: ss) "process")  = return (process s) `ap` (mapM fromTree ss)
fromTree e                        = Left $ "Invalid tree: " ++ show e

parse s = safeRead s >>= fromTree

(|>) a f = f [a]

dup_consume ev x = print (ev x1) >> return x2
  where (x1,x2) = duplicate x

store_to_html_compiler = Utils.mkCompiler wrapper 
  where 
    wrapper str = 
      case parse str >>= (return . compile_to_html) of
        Left err -> err
        Right html -> html