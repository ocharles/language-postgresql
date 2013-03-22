module Language.PostgreSQL where

import Prelude
import Control.Applicative
import Control.Monad (void)
import Data.Foldable (asum, traverse_)
import Data.Monoid (mappend, mconcat)
import Data.Traversable (mapM, sequenceA, traverse)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

--------------------------------------------------------------------------------
type Identifier = String

data Statement =
    AlterEventTrigger Identifier TriggerEnable
  | AlterDatabase Identifier AlterDatabaseBody
  | AlterDatabaseSet Identifier Setting
  | AlterDefaultPrivileges -- TODO
  | AlterDomain -- TODO
  | AlterEnum -- TODO
  | AlterExtension -- TODO
  | AlterExtensionContents -- TODO
  | AlterForeignDataWrapper -- TODO
  | AlterForeignServer -- TODO
  | AlterForeignTable -- TODO
  | AlterFunction -- TODO
  | AlterGroup -- TODO
  | AlterObject -- TODO
  | AlterOwner -- TODO
  | AlterSequence -- TODO
  | AlterTable -- TODO
  | AlterCompositeType -- TODO
  | AlterRoleSet -- TODO
  | AlterRole -- TODO
  | AlterTextSearchConfiguration -- TODO
  | AlterTextSearchDictionary -- TODO
  | AlterUserMapping -- TODO
  | AlterUserSet -- TODO
  | AlterUser -- TODO
  | Analyze Verbosity AnalyzeScope
  | CheckPoint
  | ClosePortal -- TODO (til end)
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

data Setting =
    TransactionMode [TransactionMode]
  | SessionCharacteristics [TransactionMode]
  | SetVariable Identifier VariableSetting
  | Reset ResetTarget
  deriving (Eq, Show)

data ResetTarget = Variable Identifier | All
  deriving (Eq, Show)

data VariableSetting = Default | SettingList [VariableSettingValue] | Current
  deriving (Eq, Show)

data VariableSettingValue = Bool Bool | String String
  deriving (Eq, Show)

data TransactionMode = IsolationLevel IsolationLevel | ReadWrite Bool | Deferrable Bool
  deriving (Eq, Show)

data IsolationLevel = ReadUncommitted | ReadCommitted | RepeatableRead | Serializable
  deriving (Eq, Show)

data Verbosity = Verbose | Quiet
  deriving (Eq, Show)

data AnalyzeScope = Everything | Relation Identifier (Maybe [Identifier])
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


alterDatabaseSet :: TokenParsing m => m Statement
alterDatabaseSet =
  AlterDatabaseSet <$> (traverse symbol (words "ALTER DATABASE") *> identifier)
                   <*> ((symbol "SET" *> setRest) <|> variableReset)
 where
  setRest = asum [ transactionMode
                 , sessionCharacteristics
                 , try varTo
		 , varFromCurrent
		 , timeZone
		 , schema
		 , names
		 , role
		 , sessionAuth
		 , xml
		 , snapshot
		 ]
    where
      transactionModes = sepBy1 transactionModeItem (void comma <|> void whiteSpace)
        where
          transactionModeItem = asum [ traverse symbol (words "ISOLATION LEVEL") *> isolationLevel
                                     , ReadWrite <$> (symbol "READ" *> asum [ True <$ symbol "WRITE"
                                                                            , False <$ symbol "ONLY"
                                                                            ])
                                     , Deferrable <$> asum [ True <$ symbol "DEFERRABLE"
                                                           , False <$ traverse symbol (words "NOT DEFERRABLE")
                                                           ]
                                     ]
            where
              isolationLevel = IsolationLevel <$> asum [ try (ReadUncommitted <$ traverse symbol (words "READ UNCOMMITTED"))
                                                       , ReadCommitted <$ traverse symbol (words "READ COMMITTED")
                                                       , RepeatableRead <$ traverse symbol (words "REPEATABLE READ")
                                                       , Serializable <$ symbol "SERIALIZABLE"
                                                       ]

      transactionMode = TransactionMode <$> (symbol "TRANSACTION" *> transactionModes)

      sessionCharacteristics = SessionCharacteristics <$>
        (traverse symbol (words "SESSION CHARACTERISTICS") *> transactionModes)

      varTo = SetVariable <$> (varName <* ((void $ symbolic '=') <|> (void $ symbol "TO"))) <*> (def <|> varList)
        where
          def = Default <$ symbol "DEFAULT"
          varList = SettingList <$> commaSep1 (bool <|> stringLit)
            where
              bool = Bool <$> asum [ True <$ (symbol "TRUE" <|> symbol "ON")
                                   , False <$ (symbol "FALSE" <|> symbol "OFF")
                                   ]
              stringLit = String <$> value

      varFromCurrent = SetVariable <$> varName <*> (Current <$ traverse symbol (words "FROM CURRENT"))

      timeZone = empty -- TODO equiv. to SET timezone =

      schema = empty -- TODO equiv. to SET search_path =

      names = empty -- TODO equiv. to SET client_encoding =

      role = empty -- TODO equiv. to SET role =

      sessionAuth = empty -- TODO equiv. to SET session_authorization =

      xml = empty -- TODO equiv. to SET xmloption =

      snapshot = empty -- TODO

  variableReset = symbol "RESET" *>
    (Reset <$> asum [ All <$ symbol "ALL"
                    , Variable "timezone" <$ traverse symbol (words "TIME ZONE")
                    , Variable "transaction_isolation" <$ traverse symbol (words "TRANSACTION ISOLATION LEVEL")
                    , Variable "session_authorization" <$ traverse symbol (words "SESSION AUTHORIZATION")
                    , Variable <$> identifier
                    ])

-- TODO
-- alterDefaultPrivileges :: TokenParsing m => m Statement
-- alterDefaultPrivileges =
--   AlterDefaultPrivileges <$> (traverse symbol (words "ALTER DEFAULT PRIVILEGES") *> defAclOptions)
--                          <*> defAclAction
--  where
--   defAclOptions = sepBy defAclOption whiteSpace
--    where
--     defAclOption = asum [ IN SCHEMA
--                         , FOR ROLE
--                         , FOR USER
--                         ]

varName :: TokenParsing m => m Identifier
varName = mconcat <$> sepBy1 identifier (char '.')

value :: TokenParsing m => m String
value = between (char '\'') (char '\'') strParser
 where
  strParser = many (noneOf "'")

identifier :: TokenParsing m => m Identifier
identifier = token (concat <$> sequenceA [ pure <$> letter
                                         , many (alphaNum <|> oneOf "$_")
                                         ]) <?> "identifier"

analyze :: TokenParsing m => m Statement
analyze = Analyze <$> (symbol "ANALYZE" *> ( (Verbose <$ symbol "VERBOSE") <|> pure Quiet))
                  <*> ( (Relation <$> identifier <*> optional (parens (commaSep identifier)))  <|> pure Everything)


checkPoint :: TokenParsing m => m Statement
checkPoint = CheckPoint <$ symbol "CHECKPOINT"
