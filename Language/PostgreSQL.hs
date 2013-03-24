module Language.PostgreSQL where

import Prelude
import Control.Applicative
import Control.Monad (void)
import Data.Foldable (asum, traverse_)
import Data.Maybe (isJust)
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
  | ClosePortal CloseTarget
  | Cluster Verbosity ClusterScope
  | Comment -- TODO
  | ConstraintsSet ConstraintsSetScope ConstraintSetting
  | Copy -- TODO
  | CreateAs -- TODO
  | CreateAssert -- TODO - not even implemented by PG yet?
  | CreateCast  -- TODO and on
  | CreateConversion Bool Identifier Identifier Identifier Identifier
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

data ResetTarget = Variable Identifier | ResetAll
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

data AnalyzeScope = AnalyzeEverything | AnalyzeRelation Identifier (Maybe [Identifier])
  deriving (Eq, Show)

data CloseTarget = Cursor Identifier | CloseAll
  deriving (Eq, Show)

data ClusterScope = ClusterEverything | ClusterRelation Identifier (Maybe Identifier)
  deriving (Eq, Show)

data ConstraintsSetScope = ConstraintsEverything | ConstraintsRelations [Identifier]
  deriving (Eq, Show)

data ConstraintSetting = Immediate | Deferred
  deriving (Eq, Show)

alterEventTrigger :: TokenParsing m => m Statement
alterEventTrigger =
  AlterEventTrigger <$> (symbols "ALTER EVENT TRIGGER" *> identifier)
                    <*> enableTrigger
 where
  enableTrigger = (try $ EnableReplica <$ symbols "ENABLE REPLICA")
              <|> (try $ EnableAlways <$ symbols "ENABLE ALWAYS")
              <|> (Enable <$ symbol "ENABLE")
              <|> (Disable <$ symbol "DISABLE")



alterDatabase :: TokenParsing m => m Statement
alterDatabase =
  AlterDatabase <$> (symbols "ALTER DATABASE" *> identifier)
                <*> alteration
 where
  alteration = alterSetting <|> setTableSpace

  alterSetting = AlterDatabaseSetting <$> (optional (symbol "WITH") *> some setting)
    where setting = ConnectionLimit <$> (symbols "CONNECTION LIMIT" *> optional (symbolic '=') *> integer)

  setTableSpace = SetTableSpace <$> (symbols "SET TABLESPACE" *> identifier)


alterDatabaseSet :: TokenParsing m => m Statement
alterDatabaseSet =
  AlterDatabaseSet <$> (symbols "ALTER DATABASE" *> identifier)
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
          transactionModeItem = asum [ symbols "ISOLATION LEVEL" *> isolationLevel
                                     , ReadWrite <$> (symbol "READ" *> asum [ True <$ symbol "WRITE"
                                                                            , False <$ symbol "ONLY"
                                                                            ])
                                     , Deferrable <$> asum [ True <$ symbol "DEFERRABLE"
                                                           , False <$ symbols "NOT DEFERRABLE"
                                                           ]
                                     ]
            where
              isolationLevel = IsolationLevel <$> asum [ try $ ReadUncommitted <$ symbols "READ UNCOMMITTED"
                                                       , ReadCommitted <$ symbols "READ COMMITTED"
                                                       , RepeatableRead <$ symbols "REPEATABLE READ"
                                                       , Serializable <$ symbol "SERIALIZABLE"
                                                       ]

      transactionMode = TransactionMode <$> (symbol "TRANSACTION" *> transactionModes)

      sessionCharacteristics = SessionCharacteristics <$>
        (symbols "SESSION CHARACTERISTICS" *> transactionModes)

      varTo = SetVariable <$> (varName <* ((void $ symbolic '=') <|> (void $ symbol "TO"))) <*> (def <|> varList)
        where
          def = Default <$ symbol "DEFAULT"
          varList = SettingList <$> commaSep1 (bool <|> stringLit)
            where
              bool = Bool <$> asum [ True <$ (symbol "TRUE" <|> symbol "ON")
                                   , False <$ (symbol "FALSE" <|> symbol "OFF")
                                   ]
              stringLit = String <$> value

      varFromCurrent = SetVariable <$> varName <*> (Current <$ symbols "FROM CURRENT")

      timeZone = empty -- TODO equiv. to SET timezone =

      schema = empty -- TODO equiv. to SET search_path =

      names = empty -- TODO equiv. to SET client_encoding =

      role = empty -- TODO equiv. to SET role =

      sessionAuth = empty -- TODO equiv. to SET session_authorization =

      xml = empty -- TODO equiv. to SET xmloption =

      snapshot = empty -- TODO

  variableReset = symbol "RESET" *>
    (Reset <$> asum [ ResetAll <$ symbol "ALL"
                    , Variable "timezone" <$ symbols "TIME ZONE"
                    , Variable "transaction_isolation" <$ symbols "TRANSACTION ISOLATION LEVEL"
                    , Variable "session_authorization" <$ symbols "SESSION AUTHORIZATION"
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
analyze = Analyze <$> (symbol "ANALYZE" *> verbosity)
                  <*> asum [ AnalyzeRelation <$> identifier <*> optional (parens (commaSep identifier))
                           , pure AnalyzeEverything
                           ]


checkPoint :: TokenParsing m => m Statement
checkPoint = CheckPoint <$ symbol "CHECKPOINT"


closePortal :: TokenParsing m => m Statement
closePortal = ClosePortal <$> (symbol "CLOSE" *> asum [ CloseAll <$ symbol "ALL"
                                                      , Cursor <$> identifier
                                                      ])


cluster :: TokenParsing m => m Statement
cluster = Cluster <$> (symbol "CLUSTER" *> verbosity)
                  <*> asum [ try $ flip ClusterRelation <$> (Just <$> identifier) <*> (symbol "ON" *> identifier)
                           , ClusterRelation <$> identifier <*> optional (symbol "USING" *> identifier)
                           , pure ClusterEverything
                           ]


verbosity :: TokenParsing m => m Verbosity
verbosity = Verbose <$ (symbol "VERBOSE") <|> pure Quiet

constraintsSet :: TokenParsing m => m Statement
constraintsSet = ConstraintsSet <$> (symbols "SET CONSTRAINTS" *> asum [ ConstraintsEverything <$ symbol "ALL"
                                                                       , ConstraintsRelations <$> commaSep identifier
                                                                       ])
                                <*> constraintSetMode
  where constraintSetMode = asum [ Deferred <$ symbol "DEFERRED"
                                 , Immediate <$ symbol "IMMEDIATE"
                                 ]

symbols :: TokenParsing m => String -> m [String]
symbols = traverse symbol . words


createConversion :: TokenParsing m => m Statement
createConversion = CreateConversion <$> (symbols "CREATE CONVERSION" *> (isJust <$> optional (symbol "DEFAULT")))
                                    <*> identifier
                                    <*> (symbol "FOR" *> sconst)
                                    <*> (symbol "TO" *> sconst)
                                    <*> (symbol "FROM" *> identifier)


sconst :: TokenParsing m => m String
sconst = token (between (char '\'') (char '\'') (many $ noneOf "'"))
