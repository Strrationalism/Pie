module Tops (runAction) where

import AST
import {-# SOURCE #-} Eval
import {-# SOURCE #-} Task

runAction :: PieValue -> [PieValue'] -> PieEval [PieTaskObj]
