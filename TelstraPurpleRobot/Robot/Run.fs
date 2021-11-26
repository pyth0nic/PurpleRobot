module Run

open Types
open Parse
open Act

let processActionItem action table next =
    match action with
    | Some (validAction : ActionEvent) ->
        let (result, act) = Act table validAction
        match act with
        | None -> next table validAction result
        | Some updatedAction -> next table updatedAction result
    | None -> 
        let error = (Action.FINISHED, (0,0), Heading.NORTH)
        next table error "There was an error\n"

let rec readline (reader, printer, table : Table, event: ActionEvent) =
    let line = reader()
    let next table event result =
        printer result
        let finished = fst3 event = Action.FINISHED
        if finished then
            "\n"
        else
            readline(reader, printer, table, event)

    if line <> null then
        let action = Parse(line, event, table.Placed)
        processActionItem action table next
    else
        "No input  \n"

let Run reader printer =
    let table = { Size=(6,6); Placed=false } 
    let start = (Action.START, (0,0), Heading.NORTH)
    readline(reader, printer, table, start) |> printf "\n %s"
