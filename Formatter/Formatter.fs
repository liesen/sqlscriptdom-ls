namespace Formatter

open FsPretty.PrettyPrint
// open Ionide.LanguageServerProtocol
// open Ionide.LanguageServerProtocol.Types
open Microsoft.SqlServer.TransactSql.ScriptDom

module PrettyPrint =
    /// <example>
    /// SELECT a,
    ///   b,
    ///   c
    /// FROM table
    /// </example>
    let hangy (opts: SqlScriptGeneratorOptions) =
        function
        | [] -> empty
        | [ x ] -> x
        | x :: y :: ys -> x <+> y <**> indent opts.IndentationSize (vcat ys)

    let hangz (opts: SqlScriptGeneratorOptions) (x: Doc) : Doc list -> Doc =
        function
        | [] -> x
        | y :: ys -> x <+> y <**> align (indent opts.IndentationSize (vcat ys))

    let ppFragmentIfNotNull'
        (gen: SqlScriptGenerator)
        (fragment: TSqlFragment)
        =
        if fragment = null then
            empty
        else
            text <| gen.GenerateScript fragment

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

            text "FROM" <+> hcat items)
        |> Option.defaultValue empty

    let ppSimpleCaseExpression
        (gen: SqlScriptGenerator)
        (caseExpression: SimpleCaseExpression)
        : Doc =
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
        options: SqlScriptGeneratorOptions, underlying: SqlScriptGenerator
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
                PrettyPrint.hangy options (text "SELECT" :: selectElements)
            // text "SELECT" <+> align (vcat selectElements)
            // text "SELECT" <+> PrettyPrint.hangy options selectElements

            let fromClause =
                PrettyPrint.generateFromClause
                    underlying
                    querySpecification.FromClause

            let whereClause =
                PrettyPrint.generateWhereClause
                    underlying
                    querySpecification.WhereClause

            // let orderByClause =
            //     PrettyPrint.ppFragmentIfNotNull'
            //         underlying
            //         querySpecification.OrderByClause
            let v = new MySqlScriptGenerator(options, underlying)
            querySpecification.OrderByClause.Accept(v)
            let orderByClause = v.Doc

            let groupByClause =
                PrettyPrint.ppFragmentIfNotNull'
                    underlying
                    querySpecification.GroupByClause

            this.Doc <-
                selectElement
                </> vcat
                        [ fromClause
                          whereClause
                          orderByClause
                          groupByClause ] // </> whereClause
        | _ -> ()

    member this.keyword(kw: string) : Doc =
        match options.KeywordCasing with
        | KeywordCasing.Lowercase -> kw.ToLowerInvariant()
        | KeywordCasing.PascalCase -> kw
        | KeywordCasing.Uppercase -> kw.ToUpperInvariant()
        | _ -> failwith "Unknown keyword casing"
        |> text

    override this.ExplicitVisit(node: OrderByClause) =
        this.Doc <-
            if node = null then
                empty
            else
                this.keyword "ORDER BY"
                :: (node.OrderByElements
                    |> Seq.map (fun expr ->
                        text (underlying.GenerateScript expr))
                    |> Seq.toList
                    |> punctuate comma)
                |> PrettyPrint.hangy options
