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

    ]

assertParser :: (Eq a, Show a) => Parser a -> String -> a -> Assertion
assertParser parser sql expected =
  case parse parser "" sql of
    Left e -> assertBool (show e) False
    Right ok -> ok @?= expected
