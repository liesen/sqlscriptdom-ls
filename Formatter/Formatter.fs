namespace Formatter

open FsPretty.PrettyPrint
// open Ionide.LanguageServerProtocol
// open Ionide.LanguageServerProtocol.Types
open Microsoft.SqlServer.TransactSql.ScriptDom

type BooleanExpressionVisitor(gen: SqlScriptGenerator) =
    inherit TSqlConcreteFragmentVisitor()

    override this.ExplicitVisit(node: BooleanBinaryExpression) =
        node.FirstExpression.Accept(this)

module PrettyPrint =
    let hangy (opts: SqlScriptGeneratorOptions) =
        function
        | [] -> empty
        | [ x ] -> x
        | x :: y :: ys -> x <+> y <**> indent opts.IndentationSize (vcat ys)

    let hangz (x: Doc) : Doc list -> Doc =
        function
        | [] -> x
        | y :: ys -> x <+> y <**> align (indent 2 (vcat ys))

    let ppFragmentIfNotNull'
        (gen: SqlScriptGenerator)
        (fragment: TSqlFragment)
        =
        printfn "Generate fragment if not null"

        if fragment = null then
            empty
        else
            text <| gen.GenerateScript fragment

    let ppCommaSeparatedList xs = ()

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

    // https://github.com/microsoft/SqlScriptDOM/blob/main/SqlScriptDom/ScriptDom/SqlServer/ScriptGenerator/SqlScriptGeneratorVisitor.FromClause.cs
    let generateFromClause (gen: SqlScriptGenerator) (node: FromClause) : Doc =
        Option.ofObj node
        |> Option.bind (fun fromClause ->
            Option.ofObj fromClause.TableReferences)
        |> Option.map (fun tableReferences ->
            let items =
                tableReferences
                |> Seq.map (ppTableReference gen)
                |> Seq.toList
                |> punctuate comma

            printfn "%A" items
            text "FROM" <+> hcat items)
        |> Option.defaultValue empty

    let ppScalarExpression (gen: SqlScriptGenerator) : ScalarExpression -> Doc =
        function
        | :? SimpleCaseExpression as caseExpression ->
            let whenClauses =
                caseExpression.WhenClauses
                |> Seq.map (ppFragmentIfNotNull' gen)
                |> Seq.toList
                |> hcat

            [ text "CASE"
              indent gen.Options.IndentationSize whenClauses
              text "END" ]
            |> vcat
            |> align
        | fragment -> ppFragmentIfNotNull' gen fragment

    let ppComparisonType: BooleanComparisonType -> Doc =
        function
        | BooleanComparisonType.Equals -> text "="
        | BooleanComparisonType.GreaterThan -> text ">"

    let ppBooleanBinaryExpressionType: BooleanBinaryExpressionType -> Doc =
        function
        | BooleanBinaryExpressionType.And -> text "AND"
        | BooleanBinaryExpressionType.Or -> text "OR"

    let rec ppBooleanExpression
        (gen: SqlScriptGenerator)
        : BooleanExpression -> Doc =
        printfn "Generate boolean expression"

        function
        | :? BooleanComparisonExpression as expr ->
            let firstExpression = ppScalarExpression gen expr.FirstExpression
            let secondExpression = ppScalarExpression gen expr.SecondExpression
            let comparisonType = ppComparisonType expr.ComparisonType
            firstExpression <+> comparisonType <+> secondExpression
        | :? BooleanParenthesisExpression as expr ->
            let booleanExpression = ppBooleanExpression gen expr.Expression

            lparen
            <*> indent gen.Options.IndentationSize booleanExpression
            <*> rparen
            |> align
        | :? BooleanBinaryExpression as expr ->
            let firstExpression = ppBooleanExpression gen expr.FirstExpression
            let secondExpression = ppBooleanExpression gen expr.SecondExpression

            let binaryExpressionType =
                ppBooleanBinaryExpressionType expr.BinaryExpressionType

            firstExpression
            <*> indent
                gen.Options.IndentationSize
                (binaryExpressionType <+> secondExpression)
            |> ignore

            vcat
                [ firstExpression; (binaryExpressionType <+> secondExpression) ]
            |> align
        | _ -> failwith "not implemented"
    // generateFragmentIfNotNull' gen node

    let generateWhereClause
        (gen: SqlScriptGenerator)
        (node: WhereClause)
        : Doc =
        Option.ofObj node
        |> Option.bind (fun whereClause ->
            Option.ofObj whereClause.SearchCondition)
        |> Option.map (fun searchCondition ->
            text "WHERE" <+> ppBooleanExpression gen searchCondition)
        |> Option.defaultValue empty

type MySqlScriptGenerator
    (
        // client: LspClient,
        options: SqlScriptGeneratorOptions,
        underlying: SqlScriptGenerator
    ) =
    inherit TSqlFragmentVisitor()

    [<DefaultValue>]
    val mutable Doc: Doc

    (*
    override this.Visit(fragment: TSqlFragment) =
        // client.WindowShowMessage
        //     { Type = MessageType.Info
        //       Message = "Formatting" }
        // |> Async.RunSynchronously
        // fragment.AcceptChildren(this)
        printfn "%O" fragment
    *)

    override this.ExplicitVisit(node: SimpleCaseExpression) =
        printfn "SimpleCaseExpression"

    override this.ExplicitVisit(node: BooleanBinaryExpression) =
        this.Doc <- text "BooleanBinaryExpression"

    override this.ExplicitVisit(node: SelectStatement) =
        match node.QueryExpression with
        | :? QuerySpecification as querySpecification ->
            let selectElements =
                querySpecification.SelectElements
                |> Seq.map (fun (selectElement: SelectElement) ->
                    let script: string =
                        underlying.GenerateScript(selectElement)

                    text script)
                |> Seq.toList
                |> punctuate comma
            (*
            -- cat
            SELECT 1, 2, 3

            -- I like this one: hangy?
            SELECT 1,
                2,
                3

            -- hcat
            SELECT 1,
                   2,
                   3
            *)
            let selectElement =
                // PrettyPrint.hangy options (text "SELECT" :: selectElements)
                text "SELECT" <+> align (vcat selectElements)

            (*
            let fromClause =
                Option.ofObj querySpecification.FromClause
                |> Option.map (text << underlying.GenerateScript)
                |> Option.defaultValue empty
            *)
            let fromClause =
                PrettyPrint.generateFromClause
                    underlying
                    querySpecification.FromClause

            let whereClause =
                PrettyPrint.generateWhereClause
                    underlying
                    querySpecification.WhereClause
            (*
            let whereClauseVisitor =
                MySqlScriptGenerator(client, options, underlying)

            querySpecification.FromClause.Accept(whereClauseVisitor)
            let whereClause = whereClauseVisitor.Doc
            *)
            this.Doc <- selectElement </> fromClause </> whereClause
        | _ -> ()
