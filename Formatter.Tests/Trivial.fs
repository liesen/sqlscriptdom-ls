module Tests

open System.IO
open Expecto
open Expecto.Flip
open Formatter
open EditorConfig.Core
open Microsoft.SqlServer.TransactSql.ScriptDom

let editorconfig = EditorConfigParser().Parse("test.sql")

(*
[<Tests>]
let scriptDomTests =
    testList
        "ScriptDOM tests"
        [
          //
          testCase "SELECT ... INTO ..."
          <| fun _ ->
              let input = new StringReader("SELECT 1 INTO #temp")
              let parser = TSql160Parser(true)
              let script, errors = parser.Parse(input)
              Expect.isEmpty "No errors" errors

              let generator = Sql160ScriptGenerator()

              Expect.stringContains "Contains INTO" "INTO"
              <| generator.GenerateScript(script)

          //
          ]
*)

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

          testCase "Multiple statements"
          <| fun _ ->
              let reader = new StringReader("SELECT 1 FROM a\nSELECT 2 FROM b")
              let actual, errors = ppScript editorconfig reader

              Expect.equal
                  "Generated script contains both statements"
                  "SELECT 1\nFROM a\n\nSELECT 2\nFROM b"
                  actual

          (*
          testCase "SELECT ... INTO ..."
          <| fun _ ->
              let actual, errors =
                  ppScript
                      editorconfig
                      (new StringReader("SELECT 1 INTO #temp"))

              Expect.equal "Keeps INTO" "SELECT 1\nINTO #temp" actual
          *)

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

          (*
          testCase "X"
          <| fun _ ->
              let input =
                  new StringReader(
                      @"create database d1 containment = partial with nested_triggers = on, 
trustworthy on, 
transform_noise_words=off, 
default_language=[french],
default_fulltext_language=1033,
two_digit_year_cutoff=2000"
                  )

              let actual, errors = ppScript editorconfig input

              Expect.equal "Equal" "SELECT 1,\n  2,\n  3\nFROM [table]" actual
          *)
          //
          ]
