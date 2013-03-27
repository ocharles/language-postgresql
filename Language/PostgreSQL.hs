module Language.PostgreSQL where

import Prelude
import Control.Applicative
import Control.Monad (void)
import Data.Foldable (asum, traverse_)
import Data.Maybe (isJust)
import Data.Monoid (mappend, mconcat)
import Data.Traversable (mapM, sequenceA, traverse)
import Data.Void (Void)
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
  | CreateCast
  | CreateConversion Bool Identifier Identifier Identifier Identifier
  | CreateDomain Identifier Identifier  -- TODO constraints
  | CreateExtension Identifier [ExtensionOption]
  | CreateForeignDataWrapper Identifier [ForeignDataWrapperOption] [Void] -- TODO Handlers, validators, generic options
  | CreateForeignServer String (Maybe String) (Maybe String) Identifier [Void] -- TODO generic options, better types?
  | CreateForeignTable -- TODO
  | CreateFunction -- TODO
  | CreateGroup -- TODO
  | CreateMaterializedView -- TODO
  | CreateOperatorClass -- TODO
  | CreateOperatorFamily -- TODO
  | AlterOperatorFamily -- TODO
  | CreatePLang -- TODO
  | CreateSchema -- TODO
  | CreateSequence -- TODO
  | Create -- TODO
  | CreateTableSpace -- TODO
  | CreateTrigger -- TODO
  | CreateEventTrigger -- TODO
  | CreateRole -- TODO
  | CreateUser -- TODO
  | CreateUserMapping -- TODO
  | CreateDatabase -- TODO
  | Deallocate -- TODO
  | DeclareCursor -- TODO
  | Define -- TODO
  | Delete -- TODO
  | Discard -- TODO
  | Do -- TODO
  | DropAssert Identifier DropBehavior
  | DropCast Bool Identifier Identifier DropBehavior
  | DropForeignDataWrapper Bool Identifier DropBehavior
  | DropForeignServer Bool Identifier DropBehavior
  | DropGroup -- TODO
  | DropOperatorClass -- TODO
  | DropOperatorFamily -- TODO
  | DropOwned -- TODO
  | DropPLang -- TODO
  | DropRule -- TODO
  | Drop -- TODO
  | DropTableSpace -- TODO
  | DropTrigger -- TODO
  | DropRole -- TODO
  | DropUser -- TODO
  | DropUserMapping -- TODO
  | DropDatabase -- TODO
  | Execute -- TODO
  | Explain -- TODO
  | Fetch -- TODO
  | Grant -- TODO
  | GrantRole -- TODO
  | Index -- TODO
  | Insert -- TODO
  | Listen Identifier
  | RefreshMaterializedView -- TODO
  | Load -- TODO
  | Lock -- TODO
  | Notify -- TODO
  | Prepare -- TODO
  | ReassignOwned -- TODO
  | Reindex -- TODO
  | RemoveAggregate -- TODO
  | RemoveFunction -- TODO
  | RemoveOperator -- TODO
  | Rename -- TODO
  | Revoke -- TODO
  | RevokeRole -- TODO
  | Rule -- TODO
  | SecurityLabel -- TODO
  | Select -- TODO
  | Transaction -- TODO
  | Truncate -- TODO
  | Unlisten UnlistenScope
  | Update -- TODO
  | Vacuum -- TODO
  | VariableReset -- TODO
  | VariableSet -- TODO
  | VariableShow -- TODO
  | View -- TODO
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

data ExtensionOption = Schema Identifier | Version String | OldVersion String
  deriving (Eq, Show)

data ForeignDataWrapperOption = Validator (Maybe Void) | Handler (Maybe Void)
  deriving (Eq, Show)

data UnlistenScope = UnlistenEvent Identifier | UnlistenEverything
  deriving (Eq, Show)

data DropBehavior = DropCascade | DropRestrict
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


createDomain :: TokenParsing m => m Statement
createDomain = CreateDomain <$> (symbols "CREATE DOMAIN" *> identifier)
                            <*> (optional (symbol "AS") *> identifier)


createExtension :: TokenParsing m => m Statement
createExtension = CreateExtension <$> (symbols "CREATE EXTENSION" *> identifier)
                                  <*> many (asum [ Schema <$> (symbol "SCHEMA" *> identifier)
                                                 , Version <$> (symbol "VERSION" *> sconst)
                                                 , OldVersion <$> (symbol "FROM" *> sconst)
                                                 ])


createForeignDataWrapper :: TokenParsing m => m Statement
createForeignDataWrapper = CreateForeignDataWrapper
    <$> (symbols "CREATE FOREIGN DATA WRAPPER" *> identifier)
    <*> (many fdwOption)
    <*> genericOptions
  where fdwOption = asum [ try $ Handler Nothing <$ symbols "NO HANDLER"
                         , Validator Nothing <$ symbols "NO VALIDATOR"
                         ]


genericOptions :: TokenParsing m => m [Void]
genericOptions = pure []


createForeignServer :: TokenParsing m => m Statement
createForeignServer = CreateForeignServer
    <$> (symbols "CREATE SERVER" *> identifier)
    <*> optional fsType
    <*> optional foreignServerVersion
    <*> (symbols "FOREIGN DATA WRAPPER" *> identifier)
    <*> genericOptions
  where fsType = symbol "TYPE" *> sconst
        foreignServerVersion = symbol "VERSION" *> sconst


listen :: TokenParsing m => m Statement
listen = Listen <$> (symbol "LISTEN" *> identifier)


unlisten :: TokenParsing m => m Statement
unlisten = symbol "UNLISTEN" *>
  (Unlisten <$> asum [ UnlistenEverything <$ token (string "'*'") -- TODO -- too restrictive?
                     , UnlistenEvent <$> identifier
                     ])


dropAssert :: TokenParsing m => m Statement
dropAssert = DropAssert <$> (symbols "DROP ASSERTION" *> identifier)
                        <*> dropBehavior

dropBehavior :: TokenParsing m => m DropBehavior
dropBehavior = asum [ DropCascade <$ symbol "CASCADE"
                    , DropRestrict <$ symbol "RESTRICT"
                    , pure DropRestrict
                    ]


ifExists :: TokenParsing m => m Bool
ifExists = isJust <$> optional (symbols "IF EXISTS")

dropCast :: TokenParsing m => m Statement
dropCast = DropCast <$> (symbols "DROP CAST" *> ifExists)
                    `mUncurry` (parens ( (,) <$> identifier <*> (symbol "AS" *> identifier)))
                    <*> dropBehavior

infixl 4 `mUncurry`
mUncurry :: Applicative m => m (a -> b -> c) -> m (a, b) -> m c
mUncurry f x = fmap uncurry f <*> x

dropForeignDataWrapper :: TokenParsing m => m Statement
dropForeignDataWrapper = DropForeignDataWrapper <$> (symbols "DROP FOREIGN DATA WRAPPER" *> ifExists)
                                                <*> identifier
                                                <*> dropBehavior

dropForeignServer :: TokenParsing m => m Statement
dropForeignServer = DropForeignServer <$> (symbols "DROP SERVER" *> ifExists)
                                      <*> identifier
                                      <*> dropBehavior
