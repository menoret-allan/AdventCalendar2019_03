namespace Core


module CableFix =
    type Direction = Right | Left | Up | Down

    type Move = {direction: Direction; step: int}

    type Position = {x:int; y:int}

    let (|StartWith|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None

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

    let loop (x1, y1) ((x, y), step) =
        if (step = 0) then None
        else Some({x=x+x1; y=y+y1}, ((x+x1, y+y1), step - 1))
    
    let generateCoord (coords:seq<seq<Position>>) (move: Move) =
        let iteration = dirToIteration move.direction
        let last = coords |> Seq.last |> Seq.last
        let generatePosition = loop iteration
        let news = Seq.unfold generatePosition ((last.x, last.y), move.step)
        Seq.append coords (seq {news})
    
    let findPosition (cable: seq<string>) =
        cable |> Seq.choose(extract) |> Seq.fold generateCoord (seq {seq {{x=0;y=0}}}) |> Seq.concat

    let manhattanDistance (cables: string) =
        let (cable1, cable2) = cables |> getCablesPosition
        let cable1Positions = cable1 |> findPosition |> Set.ofSeq
        let cable2Positions = cable2 |> findPosition |> Set.ofSeq
        let distances = cable1Positions |> Set.intersect cable2Positions
        let result = distances |> Set.toSeq |> Seq.map (fun pos -> abs pos.x + abs pos.y) |> Seq.filter (fun dist -> dist <> 0) |> Seq.min
        result
