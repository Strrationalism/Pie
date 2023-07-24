{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Parser where

import Text.Megaparsec.Char hiding ( space )
import Text.Megaparsec
import Data.Text (Text, pack)
import Data.Void (Void)
import Data.Maybe (fromMaybe)
import AST
import Data.SExpresso.Parse (decode, plainSExprParser)
import Data.SExpresso.Parse.Generic (setSpace)
import Text.Megaparsec.Char.Lexer ( space,  skipLineComment )
import Error

type PieParser = Parsec Void Text

numberParser :: PieParser Double
numberParser = do
  sign <- optional $ (: []) <$> char '-'
  x <- oneOf ['1' .. '9']
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
  where innerParser = choice [ try numberParser
                             , fromIntegral <$> hexNumberParser ]

symbolParser :: PieParser String
symbolParser = do
  let symbolFirstCharacter = concat [ [ 'a' .. 'z' ]
                                    , [ 'A' .. 'Z' ]
                                    , [ '-', '+', '*', '/', '>', '<', '=' ]
                                    , [ '?', '!' ] ]
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

pieAtomParser' :: PieParser PieValue'
pieAtomParser' = choice $ fmap try [ pieNumberParser
                                   , PieBool <$> boolParser
                                   , PieString <$> quotedStringParser
                                   , PieSymbol <$>  symbolParser ]

pieAtomParser :: PieParser PieValue
pieAtomParser = do
  pos <- getSourcePos
  x <- pieAtomParser'
  return $ WithErrorInfo x $ ErrorInfo
    { errorInfoFilePath = sourceName pos
    , errorInfoCol = unPos $ sourceColumn pos
    , errorInfoRow = unPos $ sourceLine pos }

withComments :: PieParser [PieExpr]
withComments = decode $
               -- See megaparsec Space in Megaparsec.Char.Lexer
               setSpace (space space1 (skipLineComment ";") empty) $
               plainSExprParser pieAtomParser

test :: String
 -> Parsec e Text a
 -> Either (ParseErrorBundle Text e) a
test s x = runParser x "" (pack s)
