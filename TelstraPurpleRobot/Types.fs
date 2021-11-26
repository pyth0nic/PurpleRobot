module Types

type Table = { Size: int * int; mutable Placed: bool; }

type Heading = NORTH=0 | SOUTH=1 | EAST=2 | WEST=3
type Action = PLACE=0 | MOVE=1 | LEFT=2 | RIGHT=3 | REPORT=4 | START=5 | FINISHED = 6
    
type Position = int*int

type ActionEvent = 
    Action*Position*Heading

let actionToString (event : ActionEvent) =
    (event |> fst3 |> str, event |> snd3 |> fst |> str, event |> snd3 |> snd |> str, event |> thd3 |> str)