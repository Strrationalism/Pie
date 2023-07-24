module Error
  ( WithErrorInfo ( WithErrorInfo )
  , ErrorInfo (..)) where

data ErrorInfo = ErrorInfo
  { errorInfoFilePath :: FilePath
  , errorInfoRow :: Int
  , errorInfoCol :: Int }
  deriving (Eq, Show)

data WithErrorInfo a = WithErrorInfo a ErrorInfo
                       deriving (Eq, Show)




