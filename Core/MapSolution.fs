namespace Core

module MapSolution = 
    type Direction = Right | Left | Up | Down
    
    type Move = {direction: Direction; step: int}
    
    type Position = {x:int; y:int; dist:int}
    
    let getCablesPosition (cables: string) =
        let result = cables.Split [|'\n'|] |> Seq.map(fun cable -> cable.Split [|','|])
        (Seq.head result, Seq.last result)
    
    let extract (input: string) =
        match input.[0] with
            | 'R' -> Some {direction= Right; step=input.[1..] |> int}
            | 'L' -> Some {direction= Left; step=input.[1..] |> int}
            | 'D' -> Some {direction= Down; step=input.[1..] |> int}
            | 'U' -> Some {direction= Up; step=input.[1..] |> int}
            | _ -> None
    
    let dirToIteration (dir: Direction) =
        match dir with
            | Right -> (1, 0)
            | Left -> (-1, 0)
            | Down -> (0, -1)
            | Up  -> (0, 1)

    let loop (x1, y1) ((x, y, dist), step) =
        if (step = 0) then None
        else Some({x=x+x1; y=y+y1;dist=dist+1}, ((x+x1, y+y1, dist+1), step - 1))
    
    let generateCoord (coords:seq<seq<Position>>) (move: Move) =
        let iteration = dirToIteration move.direction
        let last = coords |> Seq.last |> Seq.last
        let generatePosition = loop iteration
        let news = Seq.unfold generatePosition ((last.x, last.y, last.dist), move.step)
        Seq.append coords (seq {news})
    

    let enumeratePositions (cable: seq<string>) =
        cable
        |> Seq.choose(extract)
        |> Seq.fold generateCoord (seq {seq {{x=0;y=0;dist=0}}})
        |> Seq.concat
        |> Seq.where (fun pos -> pos.dist <> 0)
        |> Seq.groupBy (fun r -> (r.x, r.y))
        |> Seq.map (fun (_, pos) -> (pos |> Seq.minBy (fun r -> r.dist)))

    let findSmallestIntersectionBetween cable1 cable2 = 
        Seq.append cable1 cable2
        |> Seq.groupBy (fun r -> (r.x, r.y))
        |> Seq.where (fun (_, pos) -> pos |> Seq.length = 2)
        |> Seq.map (fun ((x,y), pos) -> {x=x;y=y;dist=(pos |> Seq.sumBy (fun pos -> pos.dist))})
        |> Seq.minBy (fun r -> r.dist)
    
    let manhattanDistance (cables: string) =
        let (cable1, cable2) = cables |> getCablesPosition
        let cable1Positions = cable1 |> enumeratePositions
        let cable2Positions = cable2 |> enumeratePositions
        let result = findSmallestIntersectionBetween cable1Positions cable2Positions
        result.dist
        