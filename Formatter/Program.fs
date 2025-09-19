open System.IO
open Microsoft.SqlServer.TransactSql.ScriptDom
open EditorConfig.Core
open FsPretty.PrettyPrint
open FsPretty.Rendering

open Formatter

let test (opts: FileConfiguration) =
    let sql =
        "select 1, 2, 3 from f(1, 2, 3) where a = b and b = c and c = d AND (a = b OR c = d)"

    let reader = new StringReader(sql)
    let script, errors = ppScript opts reader

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

type SExpr =
    | SExpr of SExpr list
    | Atom of string

let abcd = SExpr [ Atom "a"; Atom "b"; Atom "c"; Atom "d" ]

let abcd4 = SExpr [ abcd; abcd; abcd; abcd ]

let testData =
    SExpr [ SExpr [ Atom "abcde"; abcd4 ]; SExpr [ Atom "abcdefgh"; abcd4 ] ]

let inline (<|>) x y = Union(x, y)
let inline (<&>) x y = Union(x, y)

let sep =
    function
    | [] -> empty
    | xs -> hsep xs <|> vcat xs

let rec prettySExpr =
    function
    | Atom s -> text s
    | SExpr xs -> text "(" <<>> sep (List.map prettySExpr xs) <<>> text ")"

[<EntryPoint>]
let main args =
    let editorconfig = EditorConfigParser().Parse("test.sql")
    (*
    let editorconfig = EditorConfigParser().Parse(args.[0])
    test editorconfig
    printfn "---"

    let opts = SqlScriptGeneratorOptions()

    opts.IndentationSize <-
        editorconfig.IndentSize.NumberOfColumns.GetValueOrDefault(2)

    opts.AlignClauseBodies <- false
    opts.AlignColumnDefinitionFields <- false

    use reader = new StreamReader(args.[0])
    let script, errors = ppScript editorconfig reader

    errors |> Seq.iter (fun parseError -> printfn "%O" parseError)
    printfn "%s" script
    *)

    (*
    let (^|) x y =
        match x, y with
        | DocNil, _ -> y
        | _, DocNil -> x
        | _, _ -> x ^^ break_ ^^ y

    let binop left op right =
        // group (nest 2 (group (text left ^| text op) ^| text right))
        group (nest 2 (group (text left ^| text op) ^| text right))

    let cond = binop "a" "==" "b"
    let expr1 = binop "a" "<<" "2"
    let expr2 = binop "a" "+" "b"

    let ifthen c e1 e2 =
        group (
            group (nest 2 (text "if" ^| c))
            ^| group (nest 2 (text "then" ^| e1))
            ^| group (nest 2 (text "else" ^| e2))
        )

    let doc = ifthen cond expr1 expr2
    let s = pretty 6 doc
    printfn "%s" s
*)
    (*
    SELECT a,
      b
    FROM c,
      d
    *)

    let list xs = punctuate comma xs

    let continuousIndent: Doc list -> Doc =
        function
        | [] -> empty
        | [ x ] -> x
        | x :: ys -> x <*> indent 2 (vcat ys)

    let from (xs: Doc list) =
        // text "FROM" <+> group (nest 2 (group (hcat <| List.map (nest 2) (list xs))))
        // text "FROM" <+> continuousIndent (punctuate comma xs)
        text "FROM" <+> continuousIndent (punctuateBack comma xs)
    // text "FROM" <+> continuousIndent (punctuateBack_ comma xs)

    let select xs ys =
        (text "SELECT" <+> group (nest 2 (group (vcat (list xs))))) <*> from ys

    select [ text "a"; text "b" ] [ text "c"; text "d"; text "e" ]
    |> displayStringW 20
    |> printfn "%s"

    printfn "---"

    let sql =
        "select 1, 2, 3 from f(1, 2, 3) where a = b and b = c and c = d AND (a = b OR c = d AND e = f)"

    let reader =
        new StringReader(
            // "SELECT a, b FROM c, d, e WHERE a = b AND b = c OR 1 = 2 AND (a = b OR c = d) OR a = b AND b = c OR 1 = 2 AND (a = b OR c = d) OR a = b AND b = c OR 1 = 2 AND (a = b OR c = d) OR a = b AND b = c OR 1 = 2 AND (a = b OR c = d) OR a = b AND b = c OR 1 = 2 AND (a = b OR c = d)"
            sql
        )

    let script, errors = ppScript editorconfig reader
    printfn "%s" script

    (*
    prettySExpr testData
    |> displayStringW 20 
    |> printfn "%s"

    printfn "---"

    prettySExpr testData
    |> displayStringW 80 
    |> printfn "%s"

    printfn "---"
    *)

    0
