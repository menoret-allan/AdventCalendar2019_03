module Tests

open Xunit
open FsUnit
open Core.CableFix

[<Theory>]
[<InlineData("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83", 159)>]
[<InlineData("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 135)>]
let ``Get manhattan distance`` (cables:string, expectedDistance:int) =
    cables |> manhattanDistance |> should equal expectedDistance


[<Fact>]
let ``Get cables`` () =
    let (cable1, cable2) = "R75,D30,D30,U1\nD58" |> getCablesPosition
    cable1 |> should equivalent ["R75";"D30";"D30";"U1"]
    cable2 |> should equivalent ["D58"]

[<Fact>]
let ``Extract coord R75`` () =
    "R75" |> extract |> should equal <| Some {direction= Right; step= 75}

[<Fact>]
let ``Extract coord L1`` () =
    "L1" |> extract |> should equal <| Some {direction= Left; step= 1}

[<Fact>]
let ``Extract coord U42`` () =
    "U42" |> extract |> should equal <| Some {direction= Up; step= 42}

[<Fact>]
let ``Extract coord D129`` () =
    "D129" |> extract |> should equal <| Some {direction= Down; step= 129}


[<Fact>]
let ``dirToIteration Right`` () =
    Right |> dirToIteration |> should equal (1, 0)

[<Fact>]
let ``dirToIteration Left`` () =
    Left |> dirToIteration |> should equal (-1, 0)

[<Fact>]
let ``dirToIteration Up`` () =
    Up |> dirToIteration |> should equal (0, 1)

[<Fact>]
let ``dirToIteration Down`` () =
    Down |> dirToIteration |> should equal (0, -1)

