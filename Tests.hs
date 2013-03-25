import Language.PostgreSQL
import Test.Framework
import Test.Framework.Providers.HUnit
import Text.Parsec.Prim (parse)
import Text.Parsec.String
import Text.Parser.Parsec ()
import Test.HUnit (assertBool, (@?=), Assertion)

main :: IO ()
main = defaultMain $
  map (\(parser, sql, expected) -> testCase sql (assertParser parser sql expected))
    [ (alterEventTrigger, "ALTER EVENT TRIGGER foo ENABLE", AlterEventTrigger "foo" Enable)
    , (alterEventTrigger, "ALTER EVENT TRIGGER foo ENABLE REPLICA", AlterEventTrigger "foo" EnableReplica)
    , (alterEventTrigger, "ALTER EVENT TRIGGER foo ENABLE ALWAYS", AlterEventTrigger "foo" EnableAlways)
    , (alterEventTrigger, "ALTER EVENT TRIGGER foo DISABLE", AlterEventTrigger "foo" Disable)

    , (alterDatabase, "ALTER DATABASE foo WITH CONNECTION LIMIT = -1", AlterDatabase "foo" (AlterDatabaseSetting [ConnectionLimit (-1)]))
    , (alterDatabase, "ALTER DATABASE foo SET TABLESPACE buzz", AlterDatabase "foo" (SetTableSpace "buzz"))

    , (alterDatabaseSet, "ALTER DATABASE foo SET TRANSACTION ISOLATION LEVEL SERIALIZABLE ISOLATION LEVEL READ COMMITTED", AlterDatabaseSet "foo" (TransactionMode [IsolationLevel Serializable, IsolationLevel ReadCommitted]))
    , (alterDatabaseSet, "ALTER DATABASE foo SET TRANSACTION READ ONLY NOT DEFERRABLE", AlterDatabaseSet "foo" (TransactionMode [ReadWrite False, Deferrable False]))
    , (alterDatabaseSet, "ALTER DATABASE foo SET SESSION CHARACTERISTICS ISOLATION LEVEL SERIALIZABLE ISOLATION LEVEL READ COMMITTED", AlterDatabaseSet "foo" (SessionCharacteristics [IsolationLevel Serializable, IsolationLevel ReadCommitted]))
    , (alterDatabaseSet, "ALTER DATABASE foo SET SESSION CHARACTERISTICS READ ONLY NOT DEFERRABLE", AlterDatabaseSet "foo" (SessionCharacteristics [ReadWrite False, Deferrable False]))
    , (alterDatabaseSet, "ALTER DATABASE foo SET waffle TO 'syrup'", AlterDatabaseSet "foo" (SetVariable "waffle" (SettingList [String "syrup"])))
    , (alterDatabaseSet, "ALTER DATABASE foo SET waffle = DEFAULT", AlterDatabaseSet "foo" (SetVariable "waffle" Default))
    , (alterDatabaseSet, "ALTER DATABASE foo SET waffle FROM CURRENT", AlterDatabaseSet "foo" (SetVariable "waffle" Current))
    , (alterDatabaseSet, "ALTER DATABASE foo RESET waffle", AlterDatabaseSet "foo" (Reset (Variable "waffle")))
    , (alterDatabaseSet, "ALTER DATABASE foo RESET ALL", AlterDatabaseSet "foo" (Reset ResetAll))

    , (analyze, "ANALYZE", Analyze Quiet AnalyzeEverything)
    , (analyze, "ANALYZE VERBOSE", Analyze Verbose AnalyzeEverything)
    , (analyze, "ANALYZE VERBOSE foo", Analyze Verbose (AnalyzeRelation "foo" Nothing))
    , (analyze, "ANALYZE VERBOSE foo (bar, baz)", Analyze Verbose (AnalyzeRelation "foo" (Just ["bar", "baz"])))

    , (checkPoint, "CHECKPOINT", CheckPoint)

    , (closePortal, "CLOSE foo", ClosePortal (Cursor "foo"))
    , (closePortal, "CLOSE ALL", ClosePortal CloseAll)

    , (cluster, "CLUSTER VERBOSE", Cluster Verbose ClusterEverything)
    , (cluster, "CLUSTER", Cluster Quiet ClusterEverything)
    , (cluster, "CLUSTER foo USING bar", Cluster Quiet (ClusterRelation "foo" (Just "bar")))
    , (cluster, "CLUSTER foo", Cluster Quiet (ClusterRelation "foo" Nothing))
    , (cluster, "CLUSTER bar ON foo", Cluster Quiet (ClusterRelation "foo" (Just "bar")))

    , (constraintsSet, "SET CONSTRAINTS ALL DEFERRED", ConstraintsSet ConstraintsEverything Deferred)
    , (constraintsSet, "SET CONSTRAINTS foo, bar IMMEDIATE", ConstraintsSet (ConstraintsRelations ["foo", "bar"]) Immediate)

    , (createConversion, "CREATE CONVERSION myconv FOR 'LATIN1' TO 'UTF8' FROM iso8859_1_to_utf8", CreateConversion False "myconv" "LATIN1" "UTF8" "iso8859_1_to_utf8")

    , (createDomain, "CREATE DOMAIN domaintext AS text", CreateDomain "domaintext" "text")
    -- , (createDomain, "CREATE DOMAIN dcheck varchar(15) NOT NULL CHECK (VALUE = 'a' OR VALUE = 'c' OR VALUE = 'd')", CreateDomain "dcheck" "varchar(15)" [NotNull, Check ...]) TODO

    , (createExtension, "CREATE EXTENSION cube SCHEMA public VERSION '1.9' FROM '1.8'", CreateExtension "cube" [Schema "public", Version "1.9", OldVersion "1.8"])

    , (createForeignDataWrapper, "CREATE FOREIGN DATA WRAPPER mongo NO VALIDATOR NO HANDLER", CreateForeignDataWrapper "mongo" [Validator Nothing, Handler Nothing] [])

    , (createForeignServer, "CREATE SERVER s5 TYPE 'oracle' VERSION '15.0' FOREIGN DATA WRAPPER foo", CreateForeignServer "s5" (Just "oracle") (Just "15.0") "foo" [])

    , (listen, "LISTEN foo_event", Listen "foo_event")

    , (unlisten, "UNLISTEN foo_event", Unlisten (UnlistenEvent "foo_event"))
    , (unlisten, "UNLISTEN '*'", Unlisten UnlistenEverything)
    ]

assertParser :: (Eq a, Show a) => Parser a -> String -> a -> Assertion
assertParser parser sql expected =
  case parse parser "" sql of
    Left e -> assertBool (show e) False
    Right ok -> ok @?= expected
