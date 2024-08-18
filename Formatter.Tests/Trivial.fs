module Tests

open System.IO
open Expecto
open Expecto.Flip
open Formatter
open EditorConfig.Core
open Microsoft.SqlServer.TransactSql.ScriptDom

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

          testCase "Parser"
          <| fun _ ->
              let parser = TSql160Parser(true)
              let reader = new StringReader("SELECT 1 FROM a\nSELECT 2 FROM b")
              let fragment, errors = parser.Parse(reader)
              Expect.isEmpty "No errors" errors

              let opts = SqlScriptGeneratorOptions()
              let gen = Sql160ScriptGenerator(opts)
              let actual: string = gen.GenerateScript(fragment)

              Expect.equal
                  "Generated script contains both statements"
                  "SELECT 1\nFROM   a;\n\nSELECT 2\nFROM   b;\n\n"
                  actual

              let reader = new StringReader("SELECT 1 FROM a\nSELECT 2 FROM b")
              let actual2, errors = ppScript editorconfig reader

              Expect.equal
                  "Generated script contains both statements"
                  "SELECT 1\nFROM a\n\nSELECT 2\nFROM   b\n\n"
                  actual2

          testCase "SELECT * FROM"
          <| fun _ ->
              let input = new StringReader("SELECT * FROM [table]")
              let actual, errors = ppScript editorconfig input
              Expect.equal "Should equal" "SELECT *\nFROM [table]" actual

          testCase "Align list items"
          <| fun _ ->
              let input = new StringReader("SELECT 1, 2, 3 FROM [table]")
              let actual, errors = ppScript editorconfig input

              Expect.equal "Equal" "SELECT 1,\n  2,\n  3\nFROM [table]" actual
          //
          ]
