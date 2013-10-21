{
module Parser where

import Lexer (Token(..))

}

%name sql
%tokentype { Token }
%error { parseError }

%token
  ADD { TokenAdd }
  ALTER { TokenAlter }
  COLUMN { TokenColumn }
  CONSTRAINT { TokenConstraint }
  DEFAULT { TokenDefault }
  ENABLE { TokenEnable }
  RENAME { TokenRename }
  TABLE { TokenTable }
  TO { TokenTo }
  TRIGGER { TokenTrigger }
  UNIQUE { TokenUnique }

  IDENT { TokenIdent $$ }
  ';' { TokenSemicolon }
  '(' { TokenOpenParenthesis }
  ')' { TokenCloseParenthesis }
  int { TokenInt $$ }

%%

Statements : StatementSemi { [ $1 ] }
           | StatementSemi Statements { $1 : $2 }

StatementSemi : Statement ';' { $1 }

Statement : ALTER TABLE relation AlterTableCmd { AlterTable $3 $4 }

AlterTableCmd
  : ENABLE TRIGGER name { EnableTrigger $3 }
  | ADD COLUMN ColumnDefinition { AddColumn $3 }
  | RENAME TO name { RenameTo $3 }
  | ADD CONSTRAINT name TableConstraint { AddConstraint $3 $4 }

TableConstraint
  : UNIQUE '(' ColumnIdentifier ')' { Unique [$3] }

ColumnDefinition
  : ColumnIdentifier Typename ColumnConstraints { ColumnDefinition $1 $2 $3 }

ColumnConstraints
  : ColumnConstraints ColumnConstraint { $1 ++ [$2] }
  | { [] }

ColumnConstraint
  : DEFAULT Expression { Default $2 }

Expression
  : IDENT { $1 }
  | int { show $1 }

ColumnIdentifier : IDENT { $1 }

Typename : IDENT { $1 } -- There are many more forms than this

name: ColumnIdentifier { $1 }

relation: IDENT { $1 }

{

type Relation = String

type TriggerName = String

type ConstraintName = String

type ColumnIdentifier = String

type Typename = String

type Expression = String

data ColumnConstraint = Default Expression
  deriving (Eq, Show)

data ColumnDefinition = ColumnDefinition ColumnIdentifier Typename [ColumnConstraint]
  deriving (Eq, Show)

data Statement
  = AlterTable Relation AlterTableCommand
  deriving (Eq, Show)

data AlterTableCommand
  = EnableTrigger TriggerName
  | AddColumn ColumnDefinition
  | RenameTo ColumnIdentifier
  | AddConstraint ConstraintName TableConstraint
  deriving (Eq, Show)

data TableConstraint
  = Unique [ColumnIdentifier]
  deriving (Eq, Show)

parseError :: [Token] -> a
parseError = error . show

}
