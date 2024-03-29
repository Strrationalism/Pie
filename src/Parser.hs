{-# LANGUAGE OverloadedStrings #-}
module Parser (parseFromFile, parseFromText) where

import AST
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.SExpresso.Parse (decode, plainSExprParser)
import Data.SExpresso.Parse.Generic (setSpace)
import Data.Text (Text)
import qualified Data.Text.IO.Utf8 as Utf8 (readFile)
import Data.Void (Void)
import Error
import Prelude hiding (readFile)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer (space, skipLineComment)
import Control.Monad (void)

type PieParser = Parsec Void Text

numberParser :: PieParser Double
numberParser = do
  sign <- optional $ (: []) <$> char '-'
  x <- oneOf ['0' .. '9']
  a <- many digitChar
  b <- optional $ char '.' *> (some digitChar <|> return "0")
  return $ read $ fromMaybe "" sign ++ [x] ++ a ++ maybe "" ('.' :) b

hexNumberParser :: PieParser Int
hexNumberParser = do
  sign <- optional $ (: []) <$> char '-'
  _ <- char '0'
  _ <- char 'x'
  a <- many hexDigitChar
  return $ read $ fromMaybe "" sign ++ "0x" ++ a

pieNumberParser :: PieParser PieValue'
pieNumberParser = PieNumber <$> innerParser
  where innerParser = choice [ try (fromIntegral <$> hexNumberParser)
                             , numberParser ]

symbolParser :: PieParser String
symbolParser = do
  let symbolFirstCharacter = concat [ [ 'a' .. 'z' ]
                                    , [ 'A' .. 'Z' ]
                                    , [ '-', '+', '*', '/', '>', '<', '=' ]
                                    , [ '?', '!', '\'', '_' ] ]
      symbolChars = symbolFirstCharacter ++ [ '0' .. '9' ]
  firstCharacter <- oneOf symbolFirstCharacter
  nextCharacters <- many $ oneOf symbolChars
  return $ firstCharacter : nextCharacters

quotedCharParser :: PieParser Char
quotedCharParser = escape <|> noneOf [ '\"', '\'', '\\' ]
  where
    escape = do
      _ <- char '\\'
      second <- oneOf [ 't', '\\', '\'', '\"', 'n', 'r' ]
      return $
        case second of
          't' -> '\t'
          '\\' -> '\\'
          '\'' -> '\''
          '\"' -> '\"'
          'n' -> '\n'
          'r' -> '\r'
          _ -> undefined

quotedStringParser :: PieParser String
quotedStringParser = between (char '\"') (char '\"') $ many quotedCharParser

boolParser :: PieParser Bool
boolParser = (True <$ string "true") <|> (False <$ string "false")

nilParser :: PieParser ()
nilParser = void (string "nil")

pieAtomParser' :: PieParser PieValue'
pieAtomParser' = choice $ fmap try [ pieNumberParser
                                   , PieNil <$ nilParser
                                   , PieBool <$> boolParser
                                   , PieString <$> quotedStringParser
                                   , PieSymbol <$>  symbolParser ]

pieAtomParser :: PieParser PieValue
pieAtomParser = do
  pos <- getSourcePos
  x <- pieAtomParser'
  return $ WithErrorInfo x $ Just $ ErrorInfo
    { errorInfoFilePath = sourceName pos
    , errorInfoCol = unPos $ sourceColumn pos
    , errorInfoRow = unPos $ sourceLine pos }

withComments :: PieParser [PieExpr]
withComments = decode $
               setSpace (space space1 (skipLineComment ";") empty) $
               plainSExprParser pieAtomParser

parseFromText :: FilePath -> Text -> Either String [PieExpr]
parseFromText srcName =
  first errorBundlePretty . runParser withComments srcName

parseFromFile :: FilePath -> IO (Either String [PieExpr])
parseFromFile srcPath = do
  content <- Utf8.readFile srcPath
  pure $ parseFromText srcPath content
