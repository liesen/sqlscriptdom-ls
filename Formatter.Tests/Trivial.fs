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
              Expect.equal "Should equal" "SELECT *\nFROM [table]\n\n\n" actual

          testCase "Align list items"
          <| fun _ ->
              let input = new StringReader("SELECT 1, 2, 3 FROM [table]")
              let actual, errors = ppScript input
              Expect.equal "Equal" "SELECT 1,\n  2,\n  3\nFROM [table]\n\n\n" actual ]
