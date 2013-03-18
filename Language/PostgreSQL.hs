module Language.PostgreSQL where

import Prelude
import Control.Applicative
import Data.Foldable (traverse_)
import Data.Traversable (mapM, sequenceA, traverse)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

--------------------------------------------------------------------------------
data Statement =
    AlterEventTrigger Identifier TriggerEnable
  | AlterDatabase Identifier AlterDatabaseBody
  | AlterDatabaseSet Identifier SetResetClause
  | AlterDefaultPrivileges
  | AlterDomain
  | AlterEnum
  | AlterExtension
  | AlterExtensionContents
  | AlterForeignDataWrapper
  | AlterForeignServer
  | AlterForeignTable
  | AlterFunction
  | AlterGroup
  | AlterObject
  | AlterOwner
  | AlterSequence
  | AlterTable
  | AlterCompositeType
  | AlterRoleSet
  | AlterRole
  | AlterTextSearchConfiguration
  | AlterTextSearchDictionary
  | AlterUserMapping
  | AlterUserSet
  | AlterUser
  | Analyze
  | CheckPoint
  | ClosePortal
  | Cluster
  | Comment
  | ConstraintSet
  | Copy
  | CreateAs
  | CreateAssert
  | CreateCast
  | CreateConversion
  | CreateDomain
  | CreateExtension
  | CreateForeignDataWrapper
  | CreateForeignServer
  | CreateForeignTable
  | CreateFunction
  | CreateGroup
  | CreateMaterializedView
  | CreateOperatorClass
  | CreateOperatorFamily
  | AlterOperatorFamily
  | CreatePLang
  | CreateSchema
  | CreateSequence
  | Create
  | CreateTableSpace
  | CreateTrigger
  | CreateEventTrigger
  | CreateRole
  | CreateUser
  | CreateUserMapping
  | CreateDatabase
  | Deallocate
  | DeclareCursor
  | Define
  | Delete
  | Discard
  | Do
  | DropAssert
  | DropCast
  | DropForeignDataWrapper
  | DropForeignServer
  | DropGroup
  | DropOperatorClass
  | DropOperatorFamily
  | DropOwned
  | DropPLang
  | DropRule
  | Drop
  | DropTableSpace
  | DropTrigger
  | DropRole
  | DropUser
  | DropUserMapping
  | DropDatabase
  | Execute
  | Explain
  | Fetch
  | Grant
  | GrantRole
  | Index
  | Insert
  | Listen
  | RefreshMaterializedView
  | Load
  | Lock
  | Notify
  | Prepare
  | ReassignOwned
  | Reindex
  | RemoveAggregate
  | RemoveFunction
  | RemoveOperator
  | Rename
  | Revoke
  | RevokeRole
  | Rule
  | SecurityLabel
  | Select
  | Transaction
  | Truncate
  | Unlisten
  | Update
  | Vacuum
  | VariableReset
  | VariableSet
  | VariableShow
  | View
  deriving (Eq, Show)

data TriggerEnable = Enable | EnableReplica | EnableAlways | Disable
  deriving (Eq, Show)

data AlterDatabaseBody = AlterDatabaseSetting [AlterDatabaseSetting]
                       | SetTableSpace Identifier
  deriving (Eq, Show)

data AlterDatabaseSetting = ConnectionLimit Integer
  deriving (Eq, Show)

data ConnectionLimit = Limit Int | NoLimit
  deriving (Eq, Show)

data SetResetClause = SetResetClause
  deriving (Eq, Show)


alterEventTrigger :: TokenParsing m => m Statement
alterEventTrigger =
  AlterEventTrigger <$> (traverse symbol (words "ALTER EVENT TRIGGER") *> identifier)
                    <*> enableTrigger
 where
  enableTrigger = (try $ EnableReplica <$ traverse symbol (words "ENABLE REPLICA"))
              <|> (try $ EnableAlways <$ traverse symbol (words "ENABLE ALWAYS"))
              <|> (Enable <$ symbol "ENABLE")
              <|> (Disable <$ symbol "DISABLE")



alterDatabase :: TokenParsing m => m Statement
alterDatabase =
  AlterDatabase <$> (traverse symbol (words "ALTER DATABASE") *> identifier)
                <*> alteration
 where
  alteration = alterSetting <|> setTableSpace

  alterSetting = AlterDatabaseSetting <$> (optional (symbol "WITH") *> some setting)
    where setting = ConnectionLimit <$> (traverse symbol (words "CONNECTION LIMIT") *> optional (symbolic '=') *> integer)

  setTableSpace = SetTableSpace <$> (traverse symbol (words "SET TABLESPACE") *> identifier)


type Identifier = String

-- type QualifiedName = String

-- type Type = String

-- --------------------------------------------------------------------------------
-- data Abort = AbortWork | AbortTransaction
--   deriving (Show)

-- abort :: TokenParsing m => m Abort
-- abort = symbol "ABORT" *> (AbortWork <$ symbol "WORK"
--                        <|> AbortTransaction <$ symbol "TRANSACTION")


-- --------------------------------------------------------------------------------
-- data AlterAggregate = AlterAggregate QualifiedName [Type] AlterAggregateOperation
--   deriving (Show)

-- data AlterAggregateOperation = RenameTo QualifiedName
--                              | ChangeOwnerTo Identifier
--                              | SetSchemaTo Identifier
--   deriving (Show)

-- alterAggregate :: TokenParsing m => m AlterAggregate
-- alterAggregate =
--   AlterAggregate
--     <$> (traverse_ symbol (words "ALTER AGGREGATE") *> qualifiedName)
--     <*> parens (commaSep dataType)
--     <*> (rename <|> changeOwner) -- <|> setSchema)
--  where
--   rename = RenameTo <$> ((traverse_ symbol (words "RENAME TO")) *> identifier)
--   changeOwner = ChangeOwnerTo <$> ((traverse_ symbol (words "OWNER TO")) *> identifier)


-- --------------------------------------------------------------------------------
-- data AlterAggregate = AlterAggregate QualifiedName [Type] AlterAggregateOperation
--   deriving (Show)

-- data AlterAggregateOperation = RenameTo QualifiedName
--                              | ChangeOwnerTo Identifier
--                              | SetSchemaTo Identifier
--   deriving (Show)

-- alterAggregate :: TokenParsing m => m AlterAggregate
-- alterAggregate =
--   AlterAggregate
--     <$> (traverse_ symbol (words "ALTER AGGREGATE") *> qualifiedName)
--     <*> parens (commaSep dataType)
--     <*> (rename <|> changeOwner) -- <|> setSchema)
--  where
--   rename = RenameTo <$> ((traverse_ symbol (words "RENAME TO")) *> identifier)
--   changeOwner = ChangeOwnerTo <$> ((traverse_ symbol (words "OWNER TO")) *> identifier)


-- --------------------------------------------------------------------------------
-- qualifiedName :: TokenParsing m => m QualifiedName
-- qualifiedName = identifier

-- dataType :: TokenParsing m => m Type
-- dataType = identifier

-- data Insert = Insert Identifier [Identifier] [Tuple]
--   deriving (Show)

-- data Tuple = Tuple [Value]
--   deriving (Show)

-- data Value = String String
--   deriving (Show)

-- insert :: (Monad m, TokenParsing m) => m Insert
-- insert = do
--   mapM_ symbol (words "INSERT INTO")
--   Insert
--     <$> identifier
--     <*> parens (commaSep identifier)
--     <*> (symbol "VALUES" *> commaSep1 (Tuple <$> parens (commaSep1 value)))

-- value :: TokenParsing m => m Value
-- value = String <$> between (char '\'') (char '\'') strParser
--   where
--     strParser = many (noneOf "'")

identifier :: TokenParsing m => m Identifier
identifier = token (concat <$> sequenceA [ pure <$> letter
                                         , many (alphaNum <|> oneOf "$_")
                                         ]) <?> "identifier"
