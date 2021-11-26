module Act
open Types

let Act table event =
    let checkValid x y =
        let (tx, ty) = table.Size
        x >= 0 && x < tx && y >= 0 && y < ty

    let move x y heading =
        let tryMove x y = 
            if checkValid x y then Some (x,y)
            else None
    
        match heading with
        | Heading.NORTH -> tryMove x (y + 1)
        | Heading.EAST -> tryMove (x + 1) y
        | Heading.SOUTH -> tryMove x (y - 1)
        | Heading.WEST -> tryMove (x - 1) y

    let switchHeading heading direction =
        match direction with
        | Action.LEFT ->
            match heading with
            | Heading.NORTH -> Heading.WEST
            | Heading.WEST -> Heading.SOUTH
            | Heading.SOUTH -> Heading.EAST
            | Heading.EAST -> Heading.NORTH
        | Action.RIGHT -> 
            match heading with
            | Heading.NORTH -> Heading.EAST
            | Heading.EAST -> Heading.SOUTH
            | Heading.SOUTH -> Heading.WEST
            | Heading.WEST -> Heading.NORTH
        | _ -> heading

    match (table.Placed, event) with
    | (_, (Action.PLACE, (x,y), heading)) ->
        let valid = checkValid x y
        if valid then
            table.Placed <- true
            ("", Some event)
        else
            table.Placed <- false
            ("Invalid Position \n", None)
    | (true, (Action.MOVE, (x,y), heading)) ->
        let doMove = move x y heading
        match doMove with
        | Some (x,y) -> 
            ("", (Action.MOVE, (x,y),heading) |> Some)
        | None -> 
            ("Invalid Move \n", None)
    | (true, (Action.REPORT, (x,y), heading)) ->
        ((x, y, heading.ToString())  |||> sprintf "%i,%i,%s", Some (Action.FINISHED, (x,y), heading))
    | (true, (direction, pos, heading)) ->
        let newHeading = switchHeading heading direction
        ("",Some (direction, pos, newHeading))
    | _ -> 
        ("Not on table \n", None)

