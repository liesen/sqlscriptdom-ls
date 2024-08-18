open System.IO
open Microsoft.SqlServer.TransactSql.ScriptDom
open EditorConfig.Core

open Formatter

let test (opts: FileConfiguration) =
    let sql =
        "select 1, 2, 3 from f(1, 2, 3) where a = b and b = c and c = d AND (a = b OR c = d)"

    let reader = new StringReader(sql)
    let script, errors = ppScript reader

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

    use reader = new StreamReader(args.[0])
    let script, errors = ppScript reader

    errors |> Seq.iter (fun parseError -> printfn "%O" parseError)
    printfn "%s" script
    0
