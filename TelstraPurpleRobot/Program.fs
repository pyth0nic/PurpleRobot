module Program

open Robot

[<EntryPoint>]
let main argv =
    Run System.Console.ReadLine (fun (x: string) -> printf "%s" x)
    0
