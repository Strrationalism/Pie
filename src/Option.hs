{-# LANGUAGE PatternSynonyms #-}
module Option (PieOption, parseOptions) where

data PieOption = PieOption
  { pieOptionActionName :: String
  , pieOptionActionArgs :: [String]
  , pieOptionSingleThread :: Bool
  , pieOptionOptions :: [(String, Maybe String)] }

defaultOption :: PieOption
defaultOption = PieOption
  { pieOptionActionName = []
  , pieOptionActionArgs = []
  , pieOptionSingleThread = False
  , pieOptionOptions = [] }

parseOptions :: [String] -> PieOption
parseOptions ("-s":next) =
  parseOptions1 next $ defaultOption { pieOptionSingleThread = True }
parseOptions next = parseOptions1 next defaultOption

pattern PieOptionLabel :: String -> String
pattern PieOptionLabel x = '-':'-':x

parseOptions1 :: [String] -> PieOption -> PieOption
parseOptions1 [] x = x
parseOptions1 args@(PieOptionLabel _ : _) x = parseOptions3 args x
parseOptions1 (actionName : next) x =
  parseOptions2 next $ x { pieOptionActionName = actionName }

parseOptions2 :: [String] -> PieOption -> PieOption
parseOptions2 [] x = x
parseOptions2 args@(PieOptionLabel _ : _) x = parseOptions3 args x
parseOptions2 (a : next) x =
  parseOptions2 next $ x { pieOptionActionArgs = pieOptionActionArgs x ++ [a] }

parseOptions3 :: [String] -> PieOption -> PieOption
parseOptions3 [] x = x
parseOptions3 [PieOptionLabel a] x =
  x { pieOptionOptions = pieOptionOptions x ++ [(a, Nothing)] }
parseOptions3 (PieOptionLabel a : PieOptionLabel b : next) x =
  parseOptions3 (PieOptionLabel b : next) $
    x { pieOptionOptions = pieOptionOptions x ++ [(a, Nothing)] }
parseOptions3 (PieOptionLabel a : b : next) x =
  parseOptions3 next $
    x { pieOptionOptions = pieOptionOptions x ++ [(a, Just b)] }
parseOptions3 (x:_) _ = error $ "Unknown argument: " ++ x
