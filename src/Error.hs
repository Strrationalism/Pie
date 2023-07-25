{-# LANGUAGE PatternSynonyms #-}
module Error
  ( WithErrorInfo ( WithErrorInfo )
  , ErrorInfo (..)
  , pattern UnError
  , unError) where

data ErrorInfo = ErrorInfo
  { errorInfoFilePath :: FilePath
  , errorInfoRow :: Int
  , errorInfoCol :: Int }
  deriving (Eq, Show)

data WithErrorInfo a = WithErrorInfo a (Maybe ErrorInfo)
                       deriving (Eq, Show)

instance Functor WithErrorInfo where
  fmap f (WithErrorInfo x y) = WithErrorInfo (f x) y

pattern UnError :: a -> WithErrorInfo a
pattern UnError x <- WithErrorInfo x _

unError :: WithErrorInfo a -> a
unError (WithErrorInfo x _) = x
