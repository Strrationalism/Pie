{-# LANGUAGE PatternSynonyms #-}
module Error
  ( WithErrorInfo ( WithErrorInfo )
  , ErrorInfo (..)
  , pattern UnError
  , unError
  , noErrorInfo) where

data ErrorInfo = ErrorInfo
  { errorInfoFilePath :: FilePath
  , errorInfoRow :: Int
  , errorInfoCol :: Int }
  deriving Eq

data WithErrorInfo a = WithErrorInfo a (Maybe ErrorInfo)
                       deriving (Eq, Show)

instance Show ErrorInfo where
  show x =
    errorInfoFilePath x ++ "(" ++
    show (errorInfoRow x) ++ ":" ++
    show (errorInfoCol x) ++ ")"

instance Functor WithErrorInfo where
  fmap f (WithErrorInfo x y) = WithErrorInfo (f x) y

pattern UnError :: a -> WithErrorInfo a
pattern UnError x <- WithErrorInfo x _

unError :: WithErrorInfo a -> a
unError (WithErrorInfo x _) = x

noErrorInfo :: a -> WithErrorInfo a
noErrorInfo x = WithErrorInfo x Nothing
