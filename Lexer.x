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

  $letter ($letter | $int)* "[]" { keyword }

  ";" { const TokenSemicolon }

  "(" { const TokenOpenParenthesis }
  ")" { const TokenCloseParenthesis }

  "," { const TokenComma }

  [>=]+ { TokenOperator }

  $int { TokenInt . read }

  "'" { const (TokenStringLiteral "") }

  "--" .* ;

{

keywordMap =
  [ ("add", TokenAdd)
  , ("alter", TokenAlter)
  , ("check", TokenCheck)
  , ("create", TokenCreate)
  , ("column", TokenColumn)
  , ("comment", TokenComment)
  , ("constraint", TokenConstraint)
  , ("default", TokenDefault)
  , ("drop", TokenDrop)
  , ("enable", TokenEnable)
  , ("exists", TokenExists)
  , ("foreign", TokenForeign)
  , ("full", TokenFull)
  , ("if", TokenIf)
  , ("inherit", TokenInherit)
  , ("is", TokenIs)
  , ("key", TokenKey)
  , ("match", TokenMatch)
  , ("no", TokenNo)
  , ("not", TokenNot)
  , ("on", TokenOn)
  , ("references", TokenReferences)
  , ("rename", TokenRename)
  , ("table", TokenTable)
  , ("to", TokenTo)
  , ("trigger", TokenTrigger)
  , ("unique", TokenUnique)
  , ("valid", TokenValid)
  , ("validate", TokenValidate)
  ]

keyword :: String -> Token
keyword s =
  case lookup (map toLower s) keywordMap of
    Just t -> t
    Nothing -> TokenIdent s

data Token
  = TokenAdd
  | TokenAlter
  | TokenCheck
  | TokenColumn
  | TokenComment
  | TokenConstraint
  | TokenCreate
  | TokenDefault
  | TokenDrop
  | TokenEnable
  | TokenExists
  | TokenForeign
  | TokenFull
  | TokenIf
  | TokenIdent String
  | TokenInherit
  | TokenIs
  | TokenKey
  | TokenMatch
  | TokenNo
  | TokenNot
  | TokenOn
  | TokenReferences
  | TokenRename
  | TokenTable
  | TokenTo
  | TokenTrigger
  | TokenUnique
  | TokenValid
  | TokenValidate

  | TokenStringLiteral String
  | TokenInt Int

  | TokenCloseParenthesis
  | TokenComma
  | TokenOpenParenthesis
  | TokenOperator String

  | TokenSemicolon
  deriving (Eq, Show)

lex = alexScanTokens

}
