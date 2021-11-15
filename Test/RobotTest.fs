module RobotTest

open NUnit.Framework
open Robot

let integrationTest linesIn output =
    let mutable lines = [] @ linesIn
    let reader () =
        let head = lines.Head
        let tail = lines.Tail
        lines <- tail
        head

    let mutable results = []
    let printer x =
        results <- [x] @ results
        printf "%s" x

    Run reader printer
    Assert.AreEqual(output, results.Head)

[<Test>]
let TestCaseOne () =
    let lines = ["PLACE 0,0,NORTH";"MOVE";"REPORT"]
    let output = "0,1,NORTH"
    integrationTest lines output

[<Test>]
let TestCaseTwo () =
    let lines = ["PLACE 0,0,NORTH";"LEFT";"REPORT"]
    let output = "0,0,WEST"
    integrationTest lines output

[<Test>]
let TestCaseThree () =
    let lines = ["PLACE 1,2,EAST";"MOVE";"MOVE";"LEFT";"MOVE";"REPORT"]
    let output = "3,3,NORTH"
    integrationTest lines output

[<Test>]
let TestCaseFour () =
    let lines = ["PLACE 1,2,EAST";"MOVE";"LEFT";"MOVE";"PLACE 3,1";"MOVE";"REPORT"]
    let output = "3,2,NORTH"
    integrationTest lines output