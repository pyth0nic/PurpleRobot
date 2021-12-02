module Parse

open System
open System.Text.RegularExpressions
open Types

let actionsReg = "^(MOVE|LEFT|RIGHT|REPORT|JUMP|EXIT)"
let placeReg = "^(PLACE)\s(\d),(\d),(NORTH|SOUTH|EAST|WEST)"
let placeRegNoHeading = "^(PLACE)\s(\d),(\d)"

let Parse (line: string, event: ActionEvent, placed:bool) =
    let parse (input: string) = 
        let updatePos (x: Action) = (x, snd3 event, thd3 event)
        let parsePlacement input placeReg =
            let reg = Regex.Split(input, placeReg) |> Array.toList
            match reg with
            | [_;placeStr;positionStrX;positionStrY;_;] -> (placeStr,positionStrX,positionStrY,thd3 event |> str)
            | [_;placeStr;positionStrX;positionStrY;headingStr;_;] -> (placeStr,positionStrX,positionStrY,headingStr)
            | _ -> event |> actionToString

        let createPlacementResult regex = 
            let (placeStr,positionStrX,positionStrY,headingStr) = parsePlacement input regex
            let place = Enum.Parse(typedefof<Action>, placeStr) :?> Action
            let positionX = positionStrX |> int
            let positionY = positionStrY |> int
            let heading = Enum.Parse(typedefof<Heading>, headingStr) :?> Heading
            (place, Position (positionX, positionY), heading) |> Some  

        if Regex.IsMatch(input, actionsReg) then System.Enum.Parse(typedefof<Action>, input) :?> Action |> updatePos |> Some
        elif Regex.IsMatch(input, placeReg) then
           createPlacementResult placeReg
        elif placed = true && Regex.IsMatch(input, placeRegNoHeading) then
              createPlacementResult placeRegNoHeading
        else None

    parse(line)
