module Parse

open System
open System.Text.RegularExpressions
open Types

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
