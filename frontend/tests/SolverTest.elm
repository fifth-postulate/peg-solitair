module SolverTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Solver exposing (solve)
import Task exposing (fail)
import Test exposing (..)


suite : Test
suite =
    describe "Solver module"
        [ describe "solve"
            [ test "a solved problem has no moves" <|
                \_ ->
                    solve (always True) (always []) (always (always [])) []
                        |> expectSolutionTo isEmpty
            , test "a unsolved problem that has exhausted possible moves is unsolvable" <|
                \_ ->
                    solve (always False) (always []) (always (always [])) []
                        |> isUnsolvable
            , test "a unsolved problem that has a move to a solution is solvable" <|
                \_ ->
                    solve List.isEmpty identity (always (always [])) [ 0 ]
                        |> expectSolutionTo (Expect.equalLists [ 0 ])
            , test "a unsolved problem that has a move to a solution picks the first solution is solvable" <|
                \_ ->
                    solve List.isEmpty identity (always (always [])) [ 1, 0 ]
                        |> expectSolutionTo (Expect.equalLists [ 1 ])
            , test "a unsolved problem that has a move to a solution shows the moves" <|
                \_ ->
                    solve List.isEmpty identity (always (List.tail >> Maybe.withDefault [])) [ 0, 2, 1 ]
                        |> expectSolutionTo (Expect.equalLists [ 0, 2, 1 ])
            ]
        ]


isEmpty : List a -> Expectation
isEmpty =
    Expect.equalLists []


expectSolutionTo : (List b -> Expectation) -> Maybe (List b) -> Expectation
expectSolutionTo expectation solution =
    case solution of
        Just moves ->
            expectation moves

        Nothing ->
            Expect.fail "problem to be solvable"


isUnsolvable : Maybe (List b) -> Expectation
isUnsolvable solution =
    case solution of
        Just _ ->
            Expect.fail "problem should be unsolveable"

        Nothing ->
            Expect.pass
