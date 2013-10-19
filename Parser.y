{
module Parser (sql) where

import Lexer (Token(..))

}

%name sql
%tokentype { Token }
%error { parseError }

%token
  ADD { TokenAdd }
  ALTER { TokenAlter }
  COLUMN { TokenColumn }
  ENABLE { TokenEnable }
  TABLE { TokenTable }
  TRIGGER { TokenTrigger }
  IDENT { TokenIdent $$ }
  ';' { TokenSemicolon }

%%

Statements : StatementSemi { [ $1 ] }
           | StatementSemi Statements { $1 : $2 }

StatementSemi : Statement ';' { $1 }

Statement : ALTER TABLE relation AlterTableCmd { AlterTable $3 $4 }

AlterTableCmd
  : ENABLE TRIGGER name { EnableTrigger $3 }
  | ADD COLUMN ColumnDefinition { AddColumn $3 }

ColumnDefinition
  : ColumnIdentifier Typename { ColumnDefinition $1 $2 }

ColumnIdentifier : IDENT { $1 }

Typename : IDENT { $1 } -- There are many more forms than this

name: ColumnIdentifier { $1 }

relation: IDENT { $1 }

{

type Relation = String

type TriggerName = String

type ColumnIdentifier = String

type Typename = String

data ColumnDefinition = ColumnDefinition ColumnIdentifier Typename
  deriving (Show)

data Statement
  = AlterTable Relation AlterTableCommand
  deriving (Show)

data AlterTableCommand
  = EnableTrigger TriggerName
  | AddColumn ColumnDefinition
  deriving (Show)

parseError :: [Token] -> a
parseError = error . show

}
