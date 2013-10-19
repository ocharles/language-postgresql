{
module Lexer (lex, Token(..)) where

import Prelude hiding (lex)

import Data.Char (toLower)
}

%wrapper "basic"

$letter = [a-zA-Z]

tokens :-
  $white+ ;
  $letter+ { keyword }
  ";" { const TokenSemicolon }

{

keywordMap =
  [ ("add", TokenAdd)
  , ("alter", TokenAlter)
  , ("column", TokenColumn)
  , ("enable", TokenEnable)
  , ("table", TokenTable)
  , ("trigger", TokenTrigger)
  ]

keyword :: String -> Token
keyword s =
  case lookup (map toLower s) keywordMap of
    Just t -> t
    Nothing -> TokenIdent s

data Token
  = TokenAdd
  | TokenAlter
  | TokenColumn
  | TokenEnable
  | TokenIdent String
  | TokenTable
  | TokenTrigger

  | TokenSemicolon
  deriving (Eq, Show)

lex = alexScanTokens

}
