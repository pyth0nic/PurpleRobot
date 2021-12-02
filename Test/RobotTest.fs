module RobotTest

open NUnit.Framework
open Run

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

    if results.Length > 0 then
        Assert.AreEqual(output, results.Item(1))
    else
        Assert.Fail()

[<Test>]
let TestCaseOne () =
    let lines = ["PLACE 0,0,NORTH";"MOVE";"REPORT";"EXIT"]
    let output = "0,1,NORTH \n"
    integrationTest lines output

[<Test>]
let TestCaseTwo () =
    let lines = ["PLACE 0,0,NORTH";"LEFT";"REPORT";"EXIT"]
    let output = "0,0,WEST \n"
    integrationTest lines output

[<Test>]
let TestCaseThree () =
    let lines = ["PLACE 1,2,EAST";"MOVE";"MOVE";"LEFT";"MOVE";"REPORT";"EXIT"]
    let output = "3,3,NORTH \n"
    integrationTest lines output

[<Test>]
let TestCaseFour () =
    let lines = ["PLACE 1,2,EAST";"MOVE";"LEFT";"MOVE";"PLACE 3,1";"MOVE";"REPORT";"EXIT"]
    let output = "3,2,NORTH \n"
    integrationTest lines output

[<Test>]
let TestCaseFive () =
    let lines = ["PLACE 0,0,EAST";"PLACE 1,2,EAST";"MOVE";"MOVE";"LEFT";"MOVE";"REPORT";"EXIT"]
    let output = "3,3,NORTH \n"
    integrationTest lines output

[<Test>]
let TestCaseSix () =
    let lines = ["PLACE 3,1";"EXIT"]
    let output = "There was an error\n"
    integrationTest lines output

[<Test>]
let TestCaseSeven () =
    let lines = ["REPORT";"";"EXIT"]
    let output = "There was an error\n"
    integrationTest lines output

[<Test>]
let TestCaseEight () =
    let lines = ["MOVE";"REPORT";"";"EXIT"]
    let output = "There was an error\n"
    integrationTest lines output

[<Test>]
let TestCaseNine () =
    let lines = ["asd";"EXIT"]
    let output = "There was an error\n"
    integrationTest lines output

[<Test>]
let TestJump () =
    let lines = ["PLACE 0,0,NORTH";"JUMP";"EXIT"]
    let output = "JUMPED \n"
    integrationTest lines output