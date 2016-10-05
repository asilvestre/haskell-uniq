{-# LANGUAGE OverloadedStrings #-}

module Lib (proc, maxproc) where

import ClassyPrelude (tshow)
import Data.Conduit
import Data.Monoid ((<>))
import Control.Monad.State
import Control.Monad.Trans.Resource
import System.IO (stdin, stdout)
import Text.Read (readMaybe)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Text as T

proc :: IO ()
proc = procg calc

maxproc :: IO ()
maxproc = procg calcmax

procg :: Conduit [(T.Text, Integer)] (ResourceT IO) (T.Text, Integer) -> IO ()
procg f = runResourceT $ CB.sourceHandle stdin =$= CT.decodeUtf8 =$= CT.lines =$= process f =$= CT.encodeUtf8 $$ CB.sinkHandle stdout


process :: Monad m => Conduit [(T.Text, Integer)] m (T.Text, Integer) -> Conduit T.Text m T.Text
process f = parse =$= CL.groupBy (\(a, _) (b, _) -> a == b) =$= f =$= serialize


parse :: Monad m => Conduit T.Text m (T.Text, Integer)
parse = awaitForever $ \l -> case parseline l of
                               Just x -> yield x
                               _      -> return ()


parseline :: T.Text -> Maybe (T.Text, Integer)
parseline l = fields $ T.splitOn " " l where
  fields :: [T.Text] -> Maybe (T.Text, Integer)
  fields (x : y : _) = value x ((readMaybe . T.unpack) y)
  fields _           = Nothing
  value x (Just val) = Just (x, val)
  value _ _          = Nothing


serialize :: Monad m => Conduit (T.Text, Integer) m T.Text
serialize = awaitForever $ \(ident, value) -> yield $ ident <> " " <> tshow value <> "\n"


calc :: Monad m => Conduit [(T.Text, Integer)] m (T.Text, Integer)
calc = awaitForever $ \val -> yield (avg val) where
  avg []                = ("", 0)
  avg xs@((ident, _):_) = (ident, sum (map snd xs) `quot` (toInteger . length) xs)


calcmax :: Monad m => Conduit [(T.Text, Integer)] m (T.Text, Integer)
calcmax = awaitForever $ \val -> yield (calcmax' val) where
  calcmax' []                = ("", 0)
  calcmax' xs@((ident, _):_) = (ident, maximum (map snd xs))
