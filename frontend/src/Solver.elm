module Solver exposing (solve)


solve : (a -> Bool) -> (a -> List b) -> (b -> a -> a) -> a -> Maybe (List b)
solve =
    solveWith []


solveWith : List b -> (a -> Bool) -> (a -> List b) -> (b -> a -> a) -> a -> Maybe (List b)
solveWith partialSolution isSolved generateMoves applyMove start =
    if isSolved start then
        partialSolution
            |> List.reverse
            |> Just

    else
        solveByTryingMoves partialSolution isSolved generateMoves applyMove start


solveByTryingMoves : List b -> (a -> Bool) -> (a -> List b) -> (b -> a -> a) -> a -> Maybe (List b)
solveByTryingMoves partialSolution isSolved generateMoves applyMove start =
    let
        candidates =
            generateMoves start
    in
    solveByCandidates candidates partialSolution isSolved generateMoves applyMove start


solveByCandidates : List b -> List b -> (a -> Bool) -> (a -> List b) -> (b -> a -> a) -> a -> Maybe (List b)
solveByCandidates candidates partialSolution isSolved generateMoves applyMove start =
    case candidates of
        candidate :: others ->
            let
                solution =
                    solveWith (candidate :: partialSolution) isSolved generateMoves applyMove (applyMove candidate start)
            in
            case solution of
                Just moves ->
                    solution

                Nothing ->
                    solveByCandidates others partialSolution isSolved generateMoves applyMove start

        [] ->
            Nothing
