module Tests

open System.IO
open Expecto
open Expecto.Flip
open Formatter
open EditorConfig.Core

let editorconfig = EditorConfigParser().Parse("test.sql")

[<Tests>]
let tests =
    testList
        "Trivial tests"
        [ //
          testCase "IndentationSize"
          <| fun _ ->
              Expect.equal
                  "Indentation size from editorconfig is 2"
                  2
                  editorconfig.IndentSize.NumberOfColumns.Value

          testCase "TrimTrailingWhitespace"
          <| fun _ ->
              editorconfig.TrimTrailingWhitespace.GetValueOrDefault(false)
              |> Expect.isTrue "Trim trailing whitespace"

          testCase "SELECT * FROM"
          <| fun _ ->
              let input = new StringReader("SELECT * FROM [table]")
              let actual, errors = ppScript editorconfig input
              Expect.equal "Should equal" "SELECT *\nFROM [table]" actual

          testCase "Align list items"
          <| fun _ ->
              let input = new StringReader("SELECT 1, 2, 3 FROM [table]")
              let actual, errors = ppScript editorconfig input

              Expect.equal "Equal" "SELECT 1,\n  2,\n  3\nFROM [table]" actual ]
