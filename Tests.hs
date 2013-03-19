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

    , (alterDatabaseSet, "ALTER DATABASE foo SET TRANSACTION ISOLATION LEVEL SERIALIZABLE ISOLATION LEVEL READ COMMITTED", AlterDatabaseSet "foo" [TransactionMode [IsolationLevel Serializable, IsolationLevel ReadCommitted]])
    , (alterDatabaseSet, "ALTER DATABASE foo SET TRANSACTION READ ONLY NOT DEFERRABLE", AlterDatabaseSet "foo" [TransactionMode [ReadWrite False, Deferrable False]])
    , (alterDatabaseSet, "ALTER DATABASE foo SET SESSION CHARACTERISTICS ISOLATION LEVEL SERIALIZABLE ISOLATION LEVEL READ COMMITTED", AlterDatabaseSet "foo" [SessionCharacteristics [IsolationLevel Serializable, IsolationLevel ReadCommitted]])
    , (alterDatabaseSet, "ALTER DATABASE foo SET SESSION CHARACTERISTICS READ ONLY NOT DEFERRABLE", AlterDatabaseSet "foo" [SessionCharacteristics [ReadWrite False, Deferrable False]])
    , (alterDatabaseSet, "ALTER DATABASE foo SET waffle TO 'syrup'", AlterDatabaseSet "foo" [SetVariable "waffle" [String "syrup"]])
    ]

assertParser :: (Eq a, Show a) => Parser a -> String -> a -> Assertion
assertParser parser sql expected =
  case parse parser "" sql of
    Left e -> assertBool (show e) False
    Right ok -> ok @?= expected
