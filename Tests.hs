import Language.PostgreSQL
import Test.Framework
import Test.Framework.Providers.HUnit
import Text.Parsec.Prim (parse)
import Text.Parsec.String
import Text.Parser.Parsec ()
import Test.HUnit (assertBool, (@?=), Assertion)

main :: IO ()
main = defaultMain $
  map (\(sql, expected) -> testCase sql (assertStatement sql expected))
    [ ("ALTER EVENT TRIGGER foo ENABLE", AlterEventTrigger "foo" Enable)
    , ("ALTER EVENT TRIGGER foo ENABLE REPLICA", AlterEventTrigger "foo" EnableReplica)
    , ("ALTER EVENT TRIGGER foo ENABLE ALWAYS", AlterEventTrigger "foo" EnableAlways)
    , ("ALTER EVENT TRIGGER foo DISABLE", AlterEventTrigger "foo" Disable)

    , ("ALTER DATABASE foo WITH CONNECTION LIMIT = -1", AlterDatabase "foo" (AlterDatabaseSetting [ConnectionLimit (-1)]))
    , ("ALTER DATABASE foo SET TABLESPACE buzz", AlterDatabase "foo" (SetTableSpace "buzz"))

    , ("ALTER DATABASE foo SET TRANSACTION ISOLATION LEVEL SERIALIZABLE ISOLATION LEVEL READ COMMITTED", AlterDatabaseSet "foo" (TransactionMode [IsolationLevel Serializable, IsolationLevel ReadCommitted]))
    , ("ALTER DATABASE foo SET TRANSACTION READ ONLY NOT DEFERRABLE", AlterDatabaseSet "foo" (TransactionMode [ReadWrite False, Deferrable False]))
    , ("ALTER DATABASE foo SET SESSION CHARACTERISTICS ISOLATION LEVEL SERIALIZABLE ISOLATION LEVEL READ COMMITTED", AlterDatabaseSet "foo" (SessionCharacteristics [IsolationLevel Serializable, IsolationLevel ReadCommitted]))
    , ("ALTER DATABASE foo SET SESSION CHARACTERISTICS READ ONLY NOT DEFERRABLE", AlterDatabaseSet "foo" (SessionCharacteristics [ReadWrite False, Deferrable False]))
    , ("ALTER DATABASE foo SET waffle TO 'syrup'", AlterDatabaseSet "foo" (SetVariable "waffle" (SettingList [String "syrup"])))
    , ("ALTER DATABASE foo SET waffle = DEFAULT", AlterDatabaseSet "foo" (SetVariable "waffle" Default))
    , ("ALTER DATABASE foo SET waffle FROM CURRENT", AlterDatabaseSet "foo" (SetVariable "waffle" Current))
    , ("ALTER DATABASE foo RESET waffle", AlterDatabaseSet "foo" (Reset (Variable "waffle")))
    , ("ALTER DATABASE foo RESET ALL", AlterDatabaseSet "foo" (Reset ResetAll))

    , ("ANALYZE", Analyze Quiet AnalyzeEverything)
    , ("ANALYZE VERBOSE", Analyze Verbose AnalyzeEverything)
    , ("ANALYZE VERBOSE foo", Analyze Verbose (AnalyzeRelation "foo" Nothing))
    , ("ANALYZE VERBOSE foo (bar, baz)", Analyze Verbose (AnalyzeRelation "foo" (Just ["bar", "baz"])))

    , ("CHECKPOINT", CheckPoint)

    , ("CLOSE foo", ClosePortal (Cursor "foo"))
    , ("CLOSE ALL", ClosePortal CloseAll)

    , ("CLUSTER VERBOSE", Cluster Verbose ClusterEverything)
    , ("CLUSTER", Cluster Quiet ClusterEverything)
    , ("CLUSTER foo USING bar", Cluster Quiet (ClusterRelation "foo" (Just "bar")))
    , ("CLUSTER foo", Cluster Quiet (ClusterRelation "foo" Nothing))
    , ("CLUSTER bar ON foo", Cluster Quiet (ClusterRelation "foo" (Just "bar")))

    , ("SET CONSTRAINTS ALL DEFERRED", ConstraintsSet ConstraintsEverything Deferred)
    , ("SET CONSTRAINTS foo, bar IMMEDIATE", ConstraintsSet (ConstraintsRelations ["foo", "bar"]) Immediate)

    , ("CREATE CONVERSION myconv FOR 'LATIN1' TO 'UTF8' FROM iso8859_1_to_utf8", CreateConversion False "myconv" "LATIN1" "UTF8" "iso8859_1_to_utf8")

    , ("CREATE DOMAIN domaintext AS text", CreateDomain "domaintext" "text")
    -- main, "CREATE DOMAIN dcheck varchar(15) NOT NULL CHECK (VALUE = 'a' OR VALUE = 'c' OR VALUE = 'd')", CreateDomain "dcheck" "varchar(15)" [NotNull, Check ...]) TODO

    , ("CREATE EXTENSION cube SCHEMA public VERSION '1.9' FROM '1.8'", CreateExtension "cube" [Schema "public", Version "1.9", OldVersion "1.8"])

    , ("CREATE FOREIGN DATA WRAPPER mongo NO VALIDATOR NO HANDLER", CreateForeignDataWrapper "mongo" [Validator Nothing, Handler Nothing] [])

    , ("CREATE SERVER s5 TYPE 'oracle' VERSION '15.0' FOREIGN DATA WRAPPER foo", CreateForeignServer "s5" (Just "oracle") (Just "15.0") "foo" [])

    , ("LISTEN foo_event", Listen "foo_event")

    , ("UNLISTEN foo_event", Unlisten (UnlistenEvent "foo_event"))
    , ("UNLISTEN '*'", Unlisten UnlistenEverything)

    , ("DROP ASSERTION waffles RESTRICT", DropAssert "waffles" DropRestrict)

    , ("DROP CAST (testdomain1 AS testdomain3b)", DropCast False "testdomain1" "testdomain3b" DropRestrict)

    , ("DROP FOREIGN DATA WRAPPER IF EXISTS test_fdw_exists", DropForeignDataWrapper True "test_fdw_exists" DropRestrict)

    , ("DROP SERVER s9 CASCADE", DropForeignServer False "s9" DropCascade)

    , ("DROP GROUP IF EXISTS tg1, tg2", DropGroup True ["tg1", "tg2"])

    , ("DROP OPERATOR CLASS IF EXISTS test_operator_class USING btree", DropOperatorClass True "test_operator_class" "btree" DropRestrict)

    , ("DROP DATABASE IF EXISTS fooble", DropDatabase True "fooble")
    ]

assertStatement :: String -> Statement -> Assertion
assertStatement sql expected =
  case parse statement "" sql of
    Left e -> assertBool (show e) False
    Right ok -> ok @?= expected
