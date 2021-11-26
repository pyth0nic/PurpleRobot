module Run

open System
open Types
open Parse
open Act

let Run reader printer =
    let rec readline (reader, printer, table : Table, event: ActionEvent) =
        let line = reader()
        if line <> null then
            let action = Parse(line, event, table.Placed)
            match action with
            | Some (validAction : ActionEvent) ->
                let (result, act) = Act table validAction
                match act with
                | None -> 
                    printer result
                    readline(reader, printer, table, validAction)
                | Some updatedAction ->
                    printer result
                    if fst3 updatedAction = Action.FINISHED then
                        "\n"
                    else
                        readline(reader, printer, table, updatedAction)
            | None -> "There was an error  \n"
        else
            "No input  \n"

    let table = { Size=(6,6); Placed=false } 
    let start = (Action.START, (0,0), Heading.NORTH)
    readline(reader, printer, table, start) |> printf "\n %s"
