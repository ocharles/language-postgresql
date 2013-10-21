{
module Lexer (lex, Token(..)) where

import Prelude hiding (lex)

import Data.Char (toLower)
}

%wrapper "basic"

$letter = [a-zA-Z_]
$int = [0-9]

tokens :-
  $white+ ;

  $letter ($letter | $int)* { keyword }

  ";" { const TokenSemicolon }

  "(" { const TokenOpenParenthesis }
  ")" { const TokenCloseParenthesis }

  [>=]+ { TokenOperator }

  $int { TokenInt . read }

{

keywordMap =
  [ ("add", TokenAdd)
  , ("alter", TokenAlter)
  , ("column", TokenColumn)
  , ("constraint", TokenConstraint)
  , ("default", TokenDefault)
  , ("enable", TokenEnable)
  , ("rename", TokenRename)
  , ("table", TokenTable)
  , ("to", TokenTo)
  , ("trigger", TokenTrigger)
  , ("unique", TokenUnique)
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
  | TokenConstraint
  | TokenDefault
  | TokenEnable
  | TokenIdent String
  | TokenInt Int
  | TokenRename
  | TokenTable
  | TokenTo
  | TokenTrigger
  | TokenUnique

  | TokenCloseParenthesis
  | TokenOpenParenthesis
  | TokenOperator String

  | TokenSemicolon
  deriving (Eq, Show)

lex = alexScanTokens

}
