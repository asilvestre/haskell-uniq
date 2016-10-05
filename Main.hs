{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit
import Control.Monad.Trans.Resource
import System.IO (stdin, stdout)
import qualified Data.Conduit.Binary as CB

main :: IO ()
main = runResourceT $ CB.sourceHandle stdin =$= process $$ CB.sinkHandle stdout


process = _
