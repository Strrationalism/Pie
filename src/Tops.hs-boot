module Tops (runAction) where

import AST
import {-# SOURCE #-} Eval

runAction :: PieValue -> [PieValue'] -> PieEval ()
