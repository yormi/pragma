module Parser3.ParserSpec where

import Test.Hspec hiding (context)

import qualified Parser3.Combinator as C
import qualified Parser3.Model.Error as Error
import qualified Parser3.Parser as Parser
import Parser3.Model.Position (Position(..))
import qualified Utils.Either as Either


spec :: Spec
spec =
    let
        aFilePath = "some/File/Path.pa"

        run parser sourceCode =
            Parser.run aFilePath sourceCode parser
    in
    describe "Parser" <| do
        describe "Most important error selection" <| do
            fit "Fails with the number of character left to parse as metric" <|
                let
                    sourceCode =
                        "''"

                    parser1 = do
                        _ <- C.char '\''
                        _ <- C.anyCharBut [ '\'' ]
                        C.char '\''

                    parser2 =
                        C.char 'a'

                    parser = do
                        errorRank1 <- Parser.catch parser1
                        r1 <- Parser.getRemaining
                        errorRank2 <- Parser.catch parser2
                        r2 <- Parser.getRemaining
                        trace (show (r1, r2)) <|
                            case (errorRank1, errorRank2) of
                                (Left e1, Left e2) ->
                                    Parser.mostRelevant e1 e2

                                _ ->
                                    return ()

                    expected =
                        Left Error.EndOfFileReached
                in
                run parser sourceCode `shouldBe` expected
