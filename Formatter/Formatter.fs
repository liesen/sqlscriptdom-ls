module Formatter

open FsPretty.PrettyPrint
open FsPretty.Rendering
// open StrictlyPretty
open Microsoft.SqlServer.TransactSql.ScriptDom
open System.IO
open EditorConfig.Core
open System.Collections.Generic

let ppIfNotNull pp =
    Option.defaultValue empty << Option.map pp << Option.ofObj

let pp (gen: SqlScriptGenerator) =
    Option.map (text << gen.GenerateScript) << Option.ofObj

let indent' (opts: SqlScriptGeneratorOptions) = indent opts.IndentationSize

/// <example>
/// SELECT a,
///   b,
///   c
/// FROM table
/// </example>
let continuousIndent (opts: SqlScriptGeneratorOptions) : Doc list -> Doc =
    function
    | [] -> empty
    | [ x ] -> x
    | x :: ys -> x <*> indent' opts (vcat ys)

// let continuousIndent (opts: FileConfiguration) = align << vcat

// Adds a separator between all elements, but not at the beginning or end
let punctuateBack sep =
    List.mapi (fun i doc -> if i = 0 then doc else sep <<>> doc)

// Adds a separator between all elements and inserts a space between separator and doc, but not at the beginning or end
let punctuateBack_ sep =
    List.mapi (fun i doc -> if i = 0 then doc else sep <+> doc)

let sepByCommaBack = punctuateBack (comma <<>> space)

let ppFragmentIfNotNull' (gen: SqlScriptGenerator) =
    ppIfNotNull (text << gen.GenerateScript)

let ppJoinTableReference
    (gen: SqlScriptGenerator)
    (join: JoinTableReference)
    : Doc =
    // text "JoinTableReference"
    ppFragmentIfNotNull' gen join.FirstTableReference

let ppTableReference (gen: SqlScriptGenerator) : TableReference -> Doc =
    // ppFragmentIfNotNull' gen
    function
    | :? JoinTableReference as tableReference ->
        ppJoinTableReference gen tableReference
    | :? NamedTableReference as tableReference ->
        ppFragmentIfNotNull' gen tableReference
    | :? SchemaObjectFunctionTableReference as tableReference ->
        let schemaObjectName =
            ppFragmentIfNotNull' gen tableReference.SchemaObject

        let parameters =
            Seq.map (ppFragmentIfNotNull' gen) tableReference.Parameters
            |> Seq.toList

        schemaObjectName
        <*> parens (continuousIndent gen.Options (sepByCommaBack parameters))
    | tableReference -> failwith <| tableReference.ToString()

// https://github.com/microsoft/SqlScriptDOM/blob/main/SqlScriptDom/ScriptDom/SqlServer/ScriptGenerator/SqlScriptGeneratorVisitor.FromClause.cs
let ppFromClause gen =
    ppIfNotNull
    <| fun (fromClause: FromClause) ->
        ppIfNotNull
            (fun tableReferences ->
                let items =
                    tableReferences
                    |> Seq.map (ppTableReference gen)
                    |> Seq.toList

                let newLineBeforeFromClause =
                    if gen.Options.NewLineBeforeFromClause then
                        linebreak
                    else
                        empty

                text "FROM"
                <+> continuousIndent gen.Options (punctuate comma items))
            fromClause.TableReferences

let generateFromClause
    (gen: SqlScriptGenerator)
    (node: FromClause)
    : Doc option =
    Option.ofObj node
    |> Option.bind (fun fromClause -> Option.ofObj fromClause.TableReferences)
    |> Option.map (fun tableReferences ->
        let items =
            tableReferences |> Seq.map (ppTableReference gen) |> Seq.toList

        let newLineBeforeFromClause =
            if gen.Options.NewLineBeforeFromClause then
                linebreak
            else
                empty

        continuousIndent gen.Options (punctuate comma items))

let ppSimpleCaseExpression
    (gen: SqlScriptGenerator)
    (caseExpression: SimpleCaseExpression)
    : Doc =
    let whenClauses =
        caseExpression.WhenClauses
        |> Seq.map (ppFragmentIfNotNull' gen)
        |> Seq.toList
        |> hcat

    [ text "CASE"; indent gen.Options.IndentationSize whenClauses; text "END" ]
    |> vcat
    |> align

let ppScalarExpression (gen: SqlScriptGenerator) : ScalarExpression -> Doc =
    function
    | :? SimpleCaseExpression as caseExpression ->
        ppSimpleCaseExpression gen caseExpression
    | fragment -> ppFragmentIfNotNull' gen fragment

let ppBooleanComparisonType: BooleanComparisonType -> Doc =
    function
    | BooleanComparisonType.Equals -> text "="
    | BooleanComparisonType.GreaterThan -> text ">"

let ppBooleanBinaryExpressionType: BooleanBinaryExpressionType -> Doc =
    function
    | BooleanBinaryExpressionType.And -> text "AND"
    | BooleanBinaryExpressionType.Or -> text "OR"

let ppBooleanComparisonExpression gen (expr: BooleanComparisonExpression) =
    let firstExpression = ppScalarExpression gen expr.FirstExpression
    let secondExpression = ppScalarExpression gen expr.SecondExpression
    let comparisonType = ppBooleanComparisonType expr.ComparisonType
    firstExpression <+> comparisonType <+> secondExpression

let rec ppBooleanExpression
    (gen: SqlScriptGenerator)
    : BooleanExpression -> Doc =
    printfn "Generate boolean expression"

    function
    | :? BooleanComparisonExpression as expr ->
        ppBooleanComparisonExpression gen expr
    | :? BooleanParenthesisExpression as expr ->
        ppBooleanExpression gen expr.Expression |> parens
    | :? BooleanBinaryExpression as expr ->
        let firstExpression = ppBooleanExpression gen expr.FirstExpression
        let secondExpression = ppBooleanExpression gen expr.SecondExpression

        let binaryExpressionType =
            ppBooleanBinaryExpressionType expr.BinaryExpressionType

        continuousIndent
            gen.Options
            [ firstExpression; (binaryExpressionType <+> secondExpression) ]
    | _ -> failwith "not implemented"

let generateWhereClause (gen: SqlScriptGenerator) (node: WhereClause) : Doc =
    Option.ofObj node
    |> Option.bind (fun whereClause -> Option.ofObj whereClause.SearchCondition)
    |> Option.map (fun searchCondition ->
        let newLineBeforeWhereClause =
            if gen.Options.NewLineBeforeWhereClause then
                linebreak
            else
                empty

        text "WHERE" <+> ppBooleanExpression gen searchCondition)
    |> Option.defaultValue empty

type FormattingVisitor
    (editorconfig: FileConfiguration, underlying: SqlScriptGenerator) =
    inherit TSqlConcreteFragmentVisitor()

    let Docs: Stack<Doc> = Stack<Doc>()

    member this.Doc: Doc = Docs.Peek()

    member this.Format(node: TSqlFragment) : Doc =
        printfn "Format: %A" node
        node.Accept(this)

        if Docs.Count <> 1 then
            failwithf
                "%A: Expected exactly one doc on stack, but got %d"
                node
                Docs.Count

        Docs.Pop()

    member this.Format_(node: TSqlFragment) : Doc =
        Option.ofObj node |> Option.map this.Format |> Option.defaultValue empty

    override this.ExplicitVisit(node: SelectStarExpression) : unit =
        Docs.Push(text <| underlying.GenerateScript node)

    override this.ExplicitVisit(node: NamedTableReference) =
        Docs.Push(text <| underlying.GenerateScript node)

    override this.ExplicitVisit(node: TSqlBatch) =
        let statements = node.Statements |> Seq.map this.Format |> Seq.toList

        Docs.Push(
            if underlying.Options.IncludeSemicolons then
                punctuate semi statements
            else
                statements
            |> punctuate linebreak
            |> vcat
        )

    override this.ExplicitVisit(node: SimpleCaseExpression) =
        printfn "SimpleCaseExpression"
        Docs.Push(text <| underlying.GenerateScript node)

    override this.ExplicitVisit(node: BooleanParenthesisExpression) =
        let expr = this.Format_ node.Expression
        group (lparen <<>> group (expr <<>> rparen)) |> Docs.Push

    override this.ExplicitVisit(node: BooleanBinaryExpression) =
        let op =
            match node.BinaryExpressionType with
            | BooleanBinaryExpressionType.And -> "AND"
            | BooleanBinaryExpressionType.Or -> "OR"
            | _ -> failwith "Unexpected binary expression type"

        let first = this.Format_ node.FirstExpression
        let second = this.Format_ node.SecondExpression

        // group (first </> indent 4 (text op <+> second)) |> Docs.Push
        (*
        WHERE a = b
              AND b = c
              AND c = d
        *)
        // align (first <*> (text op </> second)) |> Docs.Push
        (*
        WHERE a = b AND 
              b = c AND
              c = d
        *)
        // first </> nest 4 (text op <*> second) |> Docs.Push
        (*
        WHERE a = b
            AND c = d
                OR e = f
        *)
        continuousIndent underlying.Options [ first; text op <+> second ]
        |> Docs.Push
    // first </> indent 4 (text op <+> second) |> Docs.Push

    override this.ExplicitVisit(node: BooleanComparisonExpression) =
        ppBooleanComparisonExpression underlying node |> Docs.Push

    override this.ExplicitVisit(node: SelectScalarExpression) =
        printfn "SelectScalarExpression"
        Docs.Push(text <| underlying.GenerateScript(node))

    override this.ExplicitVisit(querySpecification: QuerySpecification) =
        (*
        <query_specification> ::=   
        SELECT [ ALL | DISTINCT ]   
            [TOP ( expression ) [PERCENT] [ WITH TIES ] ]   
            < select_list >   
            [ INTO new_table ]   
            [ FROM { <table_source> } [ ,...n ] ]   
            [ WHERE <search_condition> ]   
            [ <GROUP BY> ]   
            [ HAVING < search_condition > ]
        *)
        // let uniqueRowFilter = pp underlying querySpecification.UniqueRowFilter
        let selectList =
            querySpecification.SelectElements
            // |> Seq.map (text << underlying.GenerateScript)
            |> Seq.map this.Format
            |> Seq.toList

        printfn "Select list: %A" selectList

        let selectElement =
            Some(
                text "SELECT",
                continuousIndent underlying.Options (punctuate comma selectList)
            )

        let fromClause =
            Option.ofObj querySpecification.FromClause
            |> Option.bind (fun fromClause ->
                Option.ofObj fromClause.TableReferences)
            |> Option.map (fun tableReferences ->
                let items =
                    tableReferences
                    // |> Seq.map (ppTableReference underlying)
                    |> Seq.map this.Format
                    |> Seq.toList

                let newLineBeforeFromClause =
                    if underlying.Options.NewLineBeforeFromClause then
                        linebreak
                    else
                        empty

                continuousIndent underlying.Options (punctuate comma items))
            |> Option.map (fun body -> text "FROM", body)

        let whereClause =
            Option.ofObj querySpecification.WhereClause
            |> Option.bind (fun whereClause ->
                Option.ofObj whereClause.SearchCondition)
            |> Option.map (fun searchCondition ->
                let newLineBeforeWhereClause =
                    if underlying.Options.NewLineBeforeWhereClause then
                        linebreak
                    else
                        empty

                printfn "Where clause: %A" searchCondition
                this.Format_ searchCondition)
            // ppBooleanExpression underlying searchCondition)
            |> Option.map (fun body -> text "WHERE", body)

        let orderByClause =
            pp underlying querySpecification.OrderByClause
            |> Option.map (fun body -> text "ORDER BY", body)

        let groupByClause =
            pp underlying querySpecification.GroupByClause
            |> Option.map (fun body -> text "GROUP BY", body)

        let havingClause =
            pp underlying querySpecification.HavingClause
            |> Option.map (fun body -> text "HAVING", body)

        let clauses =
            [ selectElement
              fromClause
              whereClause
              orderByClause
              groupByClause
              havingClause ]
            |> List.collect Option.toList

        Docs.Push(vcat (List.map (fun (label, body) -> label <+> body) clauses))

    member this.keyword(kw: string) : Doc =
        match underlying.Options.KeywordCasing with
        | KeywordCasing.Lowercase -> kw.ToLowerInvariant()
        | KeywordCasing.PascalCase -> kw
        | KeywordCasing.Uppercase -> kw.ToUpperInvariant()
        | _ -> failwith "Unknown keyword casing"
        |> text

    override this.ExplicitVisit(node: OrderByClause) =
        Docs.Push(
            if node = null then
                empty
            else
                text "ORDER BY"
                <+> (node.OrderByElements
                     |> Seq.map this.Format
                     |> Seq.toList
                     |> punctuate comma
                     |> continuousIndent underlying.Options)
        )

    override this.ExplicitVisit(node: SchemaObjectFunctionTableReference) =
        Docs.Push(text <| underlying.GenerateScript(node))

let ppScript (editorconfig: FileConfiguration) (reader: TextReader) =
    let opts = SqlScriptGeneratorOptions()

    opts.IndentationSize <-
        editorconfig.IndentSize.NumberOfColumns.GetValueOrDefault(
            opts.IndentationSize
        )

    let generator = Sql160ScriptGenerator(opts)
    let formatter = FormattingVisitor(editorconfig, generator)

    let parser = TSql160Parser(true)
    let fragment, errors = parser.Parse(reader)
    errors |> Seq.iter (fun e -> printfn "%A" e.Message)

    fragment.Accept(formatter)
    let mutable output = displayStringW 40 formatter.Doc

    if editorconfig.TrimTrailingWhitespace.GetValueOrDefault(false) then
        output <- output.TrimEnd()

    if editorconfig.InsertFinalNewline.GetValueOrDefault(false) then
        output <- output + "\n"

    output, errors
