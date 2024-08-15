open System
open System.IO
open System.Web
open Microsoft.SqlServer.TransactSql.ScriptDom
open FsPretty.Rendering
open EditorConfig.Core

open Formatter

[<EntryPoint>]
let main args =
    let editorconfig = EditorConfigParser().Parse(args.[0])
    printfn "%A" editorconfig
    printfn "%A" editorconfig.Properties

    let opts = SqlScriptGeneratorOptions()
    opts.IndentationSize <- editorconfig.IndentSize.NumberOfColumns.GetValueOrDefault(2)
    opts.AlignClauseBodies <- false
    opts.AlignColumnDefinitionFields <- false

    let generator = Sql160ScriptGenerator(opts)
    let formatter = MySqlScriptGenerator(opts, generator)

    let parser = TSql160Parser(true)
    use reader = new StreamReader(args.[0])
    let fragment, errors = parser.Parse(reader)
    fragment.Accept(formatter)
    let script = displayString formatter.Doc
    printfn "%s" script
    0
