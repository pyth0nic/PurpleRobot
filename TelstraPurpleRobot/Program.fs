open System.Text.RegularExpressions
open System

type Table = { Size: int * int; mutable Placed: bool; }

type Heading = NORTH=0 | SOUTH=1 | EAST=2 | WEST=3
type Action = PLACE=0 | MOVE=1 | LEFT=2 | RIGHT=3 | REPORT=4 | START=5 | FINISHED = 6
        
type Position = int*int

type ActionEvent = 
    Action*Position*Heading

let str x = x.ToString()

let actionToString (event : ActionEvent) =
    (event |> fst3 |> str, event |> snd3 |> fst |> str, event |> snd3 |> snd |> str, event |> thd3 |> str)

let Parse (line: string, event: ActionEvent, placed:bool) =
    let parse (input: string) = 
        let actionsReg = "^(MOVE|LEFT|RIGHT|REPORT)"
        let placeReg = "^(PLACE)\s(\d),(\d),(NORTH|SOUTH|EAST|WEST)"
        let placeRegNoHeading = "^(PLACE)\s(\d),(\d)"
        let updatePos (x: Action) = (x, snd3 event, thd3 event)

        let placeVer input placeReg =
            let reg = Regex.Split(input, placeReg) |> Array.toList
            match reg with
            | [_;placeStr;positionStrX;positionStrY;_;] -> (placeStr,positionStrX,positionStrY,thd3 event |> str)
            | [_;placeStr;positionStrX;positionStrY;headingStr;_;] -> (placeStr,positionStrX,positionStrY,headingStr)
            | _ -> event |> actionToString

        if Regex.IsMatch(input, actionsReg) then System.Enum.Parse(typedefof<Action>, input) :?> Action |> updatePos |> Some
        elif Regex.IsMatch(input, placeReg) then
            let (placeStr,positionStrX,positionStrY,headingStr) = placeVer input placeReg
            let place = Enum.Parse(typedefof<Action>, placeStr) :?> Action
            let positionX = positionStrX |> int
            let positionY = positionStrY |> int
            let heading = Enum.Parse(typedefof<Heading>, headingStr) :?> Heading
            (place, Position (positionX, positionY), heading) |> Some
        elif placed = true && Regex.IsMatch(input, placeRegNoHeading) then
            let (placeStr,positionStrX,positionStrY,headingStr) = placeVer input placeRegNoHeading
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
        Some (Action.FINISHED, (x,y), heading)
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
            let action = Parse(line, event, table.Placed)
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
    let start = (Action.START, (0,0), Heading.NORTH)
    readline(table, start) |> printf "\n %s"
    0
