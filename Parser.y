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
  CHECK { TokenCheck }
  COLUMN { TokenColumn }
  CONSTRAINT { TokenConstraint }
  DEFAULT { TokenDefault }
  DROP { TokenDrop }
  ENABLE { TokenEnable }
  EXISTS { TokenExists }
  FOREIGN { TokenForeign }
  FULL { TokenFull }
  IF { TokenIf }
  INHERIT { TokenInherit }
  KEY { TokenKey }
  MATCH { TokenMatch }
  NO { TokenNo }
  NOT { TokenNot }
  REFERENCES { TokenReferences }
  RENAME { TokenRename }
  TABLE { TokenTable }
  TO { TokenTo }
  TRIGGER { TokenTrigger }
  UNIQUE { TokenUnique }
  VALID { TokenValid }
  VALIDATE { TokenValidate }

  IDENT { TokenIdent $$ }
  ';' { TokenSemicolon }
  '(' { TokenOpenParenthesis }
  ')' { TokenCloseParenthesis }
  int { TokenInt $$ }
  op { TokenOperator $$ }

%%

Statements : StatementSemi { [ $1 ] }
           | StatementSemi Statements { $1 : $2 }

StatementSemi : Statement ';' { $1 }

Statement : ALTER TABLE Existance relation AlterTableCmd { AlterTable $3 $4 $5 }

Existance
  : IF EXISTS { IfExists }
  | { Always }

AlterTableCmd
  : ENABLE TRIGGER name { EnableTrigger $3 }
  | ADD COLUMN ColumnDefinition { AddColumn $3 }
  | RENAME TO name { RenameTo $3 }
  | ADD CONSTRAINT name TableConstraint ConstraintAttributes Validity { AddConstraint (Just $3) $4 $5 $6 }
  | ADD TableConstraint ConstraintAttributes Validity { AddConstraint Nothing $2 $3 $4 }
  | DROP CONSTRAINT name { DropConstraint $3 }
  | RENAME CONSTRAINT name TO name { RenameConstraint $3 $5 }
  | VALIDATE CONSTRAINT name { ValidateConstraint $3 }

TableConstraint
  : UNIQUE '(' ColumnList ')' { Unique $3 }
  | CHECK '(' Expression ')' { Check $3 }
  | FOREIGN KEY '(' ColumnList ')' REFERENCES relation OptionalColumnList Matching { ForeignKey $4 $7 $8 $9 }

Matching
  : MATCH FULL { MatchFull }
  | { MatchFull }

Validity
  : { Valid }
  | NOT VALID { NotValid }

ColumnList
  : ColumnList ColumnIdentifier { $1 ++ [$2] }
  | { [] }

OptionalColumnList
  : '(' ColumnList ')' { $2 }
  | { [] }

ColumnDefinition
  : ColumnIdentifier Typename ColumnConstraints { ColumnDefinition $1 $2 $3 }

ColumnConstraints
  : ColumnConstraints ColumnConstraint { $1 ++ [$2] }
  | { [] }

ConstraintAttributes
  : ConstraintAttributes ConstraintAttribute { $1 ++ [$2] }
  | { [] }

ConstraintAttribute
  : NO INHERIT { NoInherit }

ColumnConstraint
  : DEFAULT Expression { Default $2 }

Expression
  : IDENT { ColumnReference $1 }
  | int { LInt $1 }
  | Expression op Expression { Operator $1 $2 $3 }

ColumnIdentifier : IDENT { $1 }

Typename : IDENT { $1 } -- There are many more forms than this

name: ColumnIdentifier { $1 }

relation: IDENT { $1 }

{

type Relation = String

type TriggerName = String

type ColumnName = String

type Operator = String

type ConstraintName = String

type ColumnIdentifier = String

type Typename = String

data ColumnConstraint = Default Expression
  deriving (Eq, Show)

data ColumnDefinition = ColumnDefinition ColumnIdentifier Typename [ColumnConstraint]
  deriving (Eq, Show)

data Statement
  = AlterTable IfExists Relation AlterTableCommand
  deriving (Eq, Show)

data AlterTableCommand
  = EnableTrigger TriggerName
  | AddColumn ColumnDefinition
  | RenameTo ColumnIdentifier
  | AddConstraint (Maybe ConstraintName) TableConstraint [ConstraintAttribute] Validity
  | DropConstraint ConstraintName
  | RenameConstraint ConstraintName ConstraintName
  | ValidateConstraint ConstraintName
  deriving (Eq, Show)

data ConstraintAttribute
  = NoInherit
  deriving (Eq, Show)

data TableConstraint
  = Unique [ColumnIdentifier]
  | Check Expression
  | ForeignKey [ColumnIdentifier] Relation [ColumnIdentifier] Matching
  deriving (Eq, Show)

data Expression = ColumnReference ColumnName
                | Operator Expression Operator Expression
                | LInt Int
  deriving (Eq, Show)

data IfExists = Always | IfExists
  deriving (Eq, Show)

data Matching = MatchFull
  deriving (Eq, Show)

data Validity = Valid | NotValid
  deriving (Eq, Show)

parseError :: [Token] -> a
parseError = error . show

}
