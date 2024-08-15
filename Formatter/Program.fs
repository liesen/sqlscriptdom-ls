open System
open System.IO
open System.Web
open Microsoft.SqlServer.TransactSql.ScriptDom
open FsPretty.Rendering
open EditorConfig.Core

open Formatter

let test (opts: FileConfiguration) =
    let sql =
        "select 1, 2, 3 from f(1, 2, 3) where a = b and b = c and c = d AND (a = b OR c = d)"

    let opts = SqlScriptGeneratorOptions()
    let generator = Sql160ScriptGenerator(opts)
    let formatter = MySqlScriptGenerator(opts, generator)
    let parser = TSql160Parser(true)
    use reader = new StringReader(sql)
    let fragment, errors = parser.Parse(reader)
    fragment.Accept(formatter)
    let script = displayString formatter.Doc

    let expected =
        @"
SELECT 1,
  2,
  3
FROM f(1,
  2,
  3)
WHERE a = b
  AND b = c
  AND c = d
  AND (a = b
    OR c = d
  )"

    printfn "%s" script

[<EntryPoint>]
let main args =
    let editorconfig = EditorConfigParser().Parse(args.[0])
    test editorconfig
    printfn "---"

    let opts = SqlScriptGeneratorOptions()

    opts.IndentationSize <-
        editorconfig.IndentSize.NumberOfColumns.GetValueOrDefault(2)

    opts.AlignClauseBodies <- false
    opts.AlignColumnDefinitionFields <- false

    let generator = Sql160ScriptGenerator(opts)
    let formatter = MySqlScriptGenerator(opts, generator)

    let parser = TSql160Parser(true)
    use reader = new StreamReader(args.[0])
    let fragment, errors = parser.Parse(reader)

    errors |> Seq.iter (fun parseError -> printfn "%O" parseError)
    fragment.Accept(formatter)
    let script = displayString formatter.Doc
    printfn "%s" script
    0
