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
    let continuousIndent (opts: SqlScriptGeneratorOptions) : Doc list -> Doc =
        function
        | [] -> empty
        | [ x ] -> x
        | x :: ys -> x <**> align (indent opts.IndentationSize (vcat ys))

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
        | :? NamedTableReference as tableReference ->
            ppFragmentIfNotNull' gen tableReference
        | :? SchemaObjectFunctionTableReference as tableReference ->
            let schemaObjectName =
                ppFragmentIfNotNull' gen tableReference.SchemaObject

            let parameters =
                Seq.map (ppFragmentIfNotNull' gen) tableReference.Parameters
                |> Seq.toList

            schemaObjectName
            </> parens (
                continuousIndent gen.Options (punctuate comma parameters)
            )
        | tableReference -> failwith <| tableReference.ToString()

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
            (*
            -- cat
            SELECT 1, 2, 3

            -- I like this one: "continuous line indent"
            SELECT 1,
                2,
                3
                
            EXEC f 1,
                2,
                3
                
            SELECT *
            FROM f(1,
                2,
                3)
                
            -- use_continuous_line_indent_in_method_pars
            SELECT *
            FROM f(1,
                2, 
                3
                )

            -- align_multiline_{argument, parameter} = hcat
            -- sql: align_multiline_{select_elements, 
            -- ... catch all with align_multiline_lists?
            SELECT 1,
                   2,
                   3
                   
            SELECT *
            FROM f(1,
                   2,
                   3)
            *)

            let selectElement =
                // text "SELECT" <+> PrettyPrint.hangy options selectElements
                text "SELECT"
                <+> PrettyPrint.continuousIndent
                        underlying.Options
                        (punctuate comma selectElements)

            let fromClause =
                PrettyPrint.generateFromClause
                    underlying
                    querySpecification.FromClause

            let whereClause =
                PrettyPrint.generateWhereClause
                    underlying
                    querySpecification.WhereClause

            let orderByClause =
                PrettyPrint.ppFragmentIfNotNull'
                    underlying
                    querySpecification.OrderByClause

            let groupByClause =
                PrettyPrint.ppFragmentIfNotNull'
                    underlying
                    querySpecification.GroupByClause

            this.Doc <-
                selectElement
                <**> fromClause
                <**> whereClause
                <**> orderByClause
                <**> groupByClause
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
                |> PrettyPrint.continuousIndent options
