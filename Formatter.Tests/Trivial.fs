module Tests

open System.IO
open Expecto
open Expecto.Flip
open Formatter

[<Tests>]
let tests =
    testList
        "Trivial tests"
        [ testCase "SELECT * FROM"
          <| fun _ ->
              let input = new StringReader("SELECT * FROM [table]")
              let actual, errors = ppScript input
              Expect.equal "Should equal" "SELECT *\nFROM [table]\n\n\n" actual ]
