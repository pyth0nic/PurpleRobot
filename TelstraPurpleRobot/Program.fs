open System.Text.RegularExpressions
open System

type Table = { Size: int * int; mutable Placed: bool; }

type Heading = NORTH | SOUTH| EAST | WEST
type Action = PLACE=0 | MOVE=1 | LEFT=2 | RIGHT=3 | REPORT=4 | START=5 | FINISHED = 6
        
type Position = int*int

type ActionEvent = 
    Action*Position*Heading

let actionToString (event : ActionEvent) =
    let str x = x.ToString()
    (event |> fst3 |> str, event |> snd3 |> fst |> str, event |> snd3 |> snd |> str, event |> thd3 |> str)

let Parse (line: string, event: ActionEvent) =
    let parse (input: string) = 
        let actionsReg = "^(MOVE|LEFT|RIGHT|REPORT)"
        let placeReg = "^(PLACE)\s\d,\d,(NORTH|SOUTH|EAST|WEST)"
        let updatePos (x: Action) = (x, snd3 event, thd3 event)
        if Regex.IsMatch(input, actionsReg) then System.Enum.Parse(typedefof<Action>, input) :?> Action |> updatePos |> Some
        elif Regex.IsMatch(input, placeReg) then
            let (placeStr,positionStrX,positionStrY,headingStr) = 
                let reg = Regex.Split(input, placeReg) |> Array.toList
                match reg with
                | [placeStr;positionStrX;positionStrY;headingStr] -> (placeStr,positionStrX,positionStrY,headingStr)
                | _ -> event |> actionToString
            let place = Enum.Parse(typedefof<Action>, placeStr) :?> Action
            let positionX = positionStrX |> int
            let positionY = positionStrY |> int
            let heading = Enum.Parse(typedefof<Heading>, headingStr) :?> Heading
            (place, Position (positionX, positionY), heading) |> Some
        else None

    parse(line)

let Act table event =
    let checkValid x y =
        let (tx, ty) = table.Size
        x >= 0 && x < tx && y >= 0 && y < ty

    let move x y heading =
        let tryMove x y = 
            if checkValid x y then Some (x,y)
            else None
        
        match heading with
        | NORTH -> tryMove x (y + 1)
        | EAST -> tryMove (x + 1) y
        | SOUTH -> tryMove x (y - 1)
        | WEST -> tryMove (x - 1) y

    let switchHeading heading direction =
        match direction with
        | Action.LEFT ->
            match heading with
            | NORTH -> WEST
            | WEST -> SOUTH
            | SOUTH -> EAST
            | EAST -> NORTH
        | Action.RIGHT -> 
            match heading with
            | NORTH -> EAST
            | EAST -> SOUTH
            | SOUTH -> WEST
            | WEST -> NORTH
        | _ -> heading

    match (table.Placed, event) with
    | (_, (Action.PLACE, (x,y), heading)) ->
        let valid = checkValid x y
        if valid then
            table.Placed <- true
            Some event
        else
            table.Placed <- false
            printf "Invalid Position \n"
            None
    | (true, (Action.MOVE, (x,y), heading)) ->
        let doMove = move x y heading
        match doMove with
        | Some (x,y) -> (Action.MOVE, (x,y),heading) |> Some
        | None -> 
            printf "Invalid Move \n"
            None
    | (true, (Action.REPORT, (x,y), heading)) ->
        (x, y, heading.ToString())  |||> printf "%i,%i,%s"
        None
    | (true, (direction, pos, heading)) ->
        let newHeading = switchHeading heading direction
        Some (direction, pos, newHeading)
    | _ -> 
        printf "Not on table \n"
        None

[<EntryPoint>]
let main argv =
    let rec readline (table : Table, event: ActionEvent) =
        let line = System.Console.ReadLine()
        if line <> null then
            let action = Parse(line, event)
            match action with
            | Some (validAction : ActionEvent) ->
                let act = Act table validAction
                match act with
                | None -> 
                    readline(table, validAction)
                | Some updatedAction ->
                    if fst3 updatedAction = Action.FINISHED then
                        "\n"
                    else
                        readline(table, updatedAction)
            | None -> "There was an error  \n"
        else
            "No input  \n"
    
    let table = { Size=(6,6); Placed=false } 
    let start = (Action.START, (0,0), NORTH)
    readline(table, start) |> printf "\n %s"
    0
