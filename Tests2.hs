import Prelude hiding (lex)

import Test.Tasty
import Test.Tasty.HUnit

import Lexer (lex)
import Parser

tests = testGroup "PostgreSQL regression tests"
  [ testGroup "ALTER TABLE" $ statements
      [ ("ALTER TABLE tmp ADD COLUMN xmin integer;", AlterTable "tmp" (AddColumn (ColumnDefinition "xmin" "integer" [])))
      , ("ALTER TABLE tmp ADD COLUMN a int4 default 3;", AlterTable "tmp" (AddColumn (ColumnDefinition "a" "int4" [Default "3"])))
      , ("ALTER TABLE tmp ADD COLUMN b name;", AlterTable "tmp" (AddColumn (ColumnDefinition "b" "name" [])))
      , ("ALTER TABLE tmp RENAME TO tmp_new;", AlterTable "tmp" (RenameTo "tmp_new"))
      , ("ALTER TABLE onek ADD CONSTRAINT onek_unique1_constraint UNIQUE (unique1);", undefined)
      , ("ALTER TABLE onek DROP CONSTRAINT onek_unique1_constraint_foo;", undefined)
      , ("ALTER TABLE onek ADD CONSTRAINT onek_check_constraint CHECK (unique1 >= 0);", undefined)
      , ("ALTER TABLE onek RENAME CONSTRAINT onek_unique1_constraint TO onek_unique1_constraint_foo;", undefined)
      , ("ALTER TABLE constraint_rename_test ADD CONSTRAINT con2 CHECK (b > 0) NO INHERIT;", undefined)
      , ("ALTER TABLE IF EXISTS constraint_rename_test ADD CONSTRAINT con4 UNIQUE (a);", undefined)
      , ("ALTER TABLE tmp3 add constraint tmpconstr foreign key(c) references tmp2 match full;", undefined)
      , ("ALTER TABLE tmp3 add constraint tmpconstr foreign key (a) references tmp2 match full NOT VALID;", undefined)
      , ("ALTER TABLE tmp3 validate constraint tmpconstr;", undefined)
      , ("ALTER TABLE FKTABLE ADD FOREIGN KEY(ftest1) references pktable;", undefined)
      , ("ALTER TABLE FKTABLE ADD FOREIGN KEY(ftest1) references pktable(ptest1);", undefined)
      ]
  ]

statements = map (\(q, expected) -> testCase q (parse q @?= [expected]))

parse = sql . lex

main = defaultMain tests
