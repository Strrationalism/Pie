module Task (PieTaskDefinition, PieTaskObj, applyTask) where

import {-# SOURCE #-} AST
import {-# SOURCE #-} Eval

data PieTaskDefinition
data PieTaskObj

applyTask :: PieTaskDefinition -> [PieValue'] -> PieEval (PieTaskObj, PieValue')
