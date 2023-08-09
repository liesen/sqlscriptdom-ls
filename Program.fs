// For more information see https://aka.ms/fsharp-console-apps
open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO
open System.Web
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Microsoft.SqlServer.TransactSql.ScriptDom

// https://github.com/microsoft/lsprotocol/blob/main/packages/python/lsprotocol/validators.py#L31
let UINTEGER_MAX_VALUE: int = (1 <<< 31) - 1

let convertParseErrorToDiagnostic (error: ParseError) : Diagnostic =
    { Range =
        { Start =
            { Line = error.Line
              Character = error.Column }
          End =
            { Line = error.Line
              Character = error.Column } }
      Severity = Some DiagnosticSeverity.Error
      Code = Some(sprintf "%d" error.Number)
      CodeDescription = None
      Source = Some "sqlscriptdom-ls"
      Message = error.Message
      RelatedInformation = None
      Tags = None
      Data = None }

type ScriptDomLspClient
    (
        sendServerNotification: ClientNotificationSender,
        sendServerRequest: ClientRequestSender
    ) =
    inherit LspClient()

    override this.WindowLogMessage(paramz: LogMessageParams) : Async<unit> =
        sendServerNotification "window/logMessage" (box paramz) |> Async.Ignore

    override this.WindowShowMessage(paramz: ShowMessageParams) : Async<unit> =
        sendServerNotification "window/showMessage" (box paramz) |> Async.Ignore

    override this.TextDocumentPublishDiagnostics
        (paramz: PublishDiagnosticsParams)
        : Async<unit> =
        sendServerNotification "textDocument/publishDiagnostics" (box paramz)
        |> Async.Ignore

    member this.PublishDiagnosticsForParseErrors
        (textDocumentUri: string)
        (errors: IList<ParseError>)
        : Async<unit> =
        this.TextDocumentPublishDiagnostics
            { Uri = textDocumentUri
              Version = None
              Diagnostics =
                errors |> Seq.map convertParseErrorToDiagnostic |> Seq.toArray }

    member this.GetDocumentStreamFromText(textDocument: TextDocumentItem) =
        new StringReader(textDocument.Text)

    member this.GetDocumentStream(textDocument: TextDocumentIdentifier) =
        let localPath = Uri(HttpUtility.UrlDecode(textDocument.Uri)).LocalPath
        new StreamReader(localPath)

    member this.AddOrUpdateTokensFromText
        (textDocument: TextDocumentItem)
        : Async<IList<TSqlParserToken> * IList<ParseError>> =
        async {
            let parser = TSql160Parser(true)
            use reader = this.GetDocumentStreamFromText textDocument
            let tokens, errors = parser.GetTokenStream(reader)
            do! this.PublishDiagnosticsForParseErrors textDocument.Uri errors
            return tokens, errors
        }

    member this.AddOrUpdateTokens
        (textDocument: TextDocumentIdentifier)
        : Async<IList<TSqlParserToken> * IList<ParseError>> =
        async {
            let parser = TSql160Parser(true)
            use reader = this.GetDocumentStream textDocument
            let tokens, errors = parser.GetTokenStream(reader)
            do! this.PublishDiagnosticsForParseErrors textDocument.Uri errors
            return tokens, errors
        }

    member this.AddOrUpdateDocumentFromText
        (textDocument: TextDocumentItem)
        : Async<TSqlFragment * IList<ParseError>> =
        async {
            let! tokens, errors = this.AddOrUpdateTokensFromText(textDocument)
            do! this.PublishDiagnosticsForParseErrors textDocument.Uri errors
            let fragment = TSql160Parser(true).Parse(tokens, ref errors)
            return fragment, errors
        }

    member this.AddOrUpdateDocument
        (textDocument: TextDocumentIdentifier)
        : Async<TSqlFragment * IList<ParseError>> =
        async {
            let parser = TSql160Parser(true)
            use reader = this.GetDocumentStream textDocument
            let fragments, errors = parser.Parse(reader)
            do! this.PublishDiagnosticsForParseErrors textDocument.Uri errors
            return fragments, errors
        }

let initialize
    (client: ScriptDomLspClient)
    (paramz: InitializeParams)
    : AsyncLspResult<InitializeResult> =
    async {
        return
            { InitializeResult.Default with
                Capabilities =
                    { ServerCapabilities.Default with
                        DocumentFormattingProvider = Some true
                        TextDocumentSync =
                            Some
                                { TextDocumentSyncOptions.Default with
                                    OpenClose = Some true }
                        SemanticTokensProvider =
                            Some
                                { Legend =
                                    { TokenTypes = [| "keyword" |]
                                      TokenModifiers = [||] }
                                  Range = Some false
                                  Full = Some(First true) } } }
            |> LspResult.success
    }

let initialized
    (client: ScriptDomLspClient)
    (paramz: InitializedParams)
    : AsyncLspResult<unit> =
    async {
        do!
            client.WindowShowMessage
                { Type = MessageType.Info
                  Message = "sqlscriptdom-ls initialized" }

        return LspResult.success ()
    }

let textDocumentDidOpen
    (client: ScriptDomLspClient)
    (paramz: DidOpenTextDocumentParams)
    : AsyncLspResult<unit> =
    async {
        do!
            client.AddOrUpdateDocumentFromText(paramz.TextDocument)
            |> Async.Ignore

        return LspResult.success ()
    }

let textDocumentFormatting
    (client: ScriptDomLspClient)
    (paramz: DocumentFormattingParams)
    : AsyncLspResult<TextEdit[] option> =
    async {
        let! fragment, errors = client.AddOrUpdateDocument(paramz.TextDocument)

        if errors.Count > 0 then
            return None |> LspResult.success
        else
            let generator = Sql160ScriptGenerator()
            let script: string = generator.GenerateScript(fragment)

            let textEdit: TextEdit =
                { Range =
                    { Start = { Line = 0; Character = 0 }
                      End =
                        { Line = UINTEGER_MAX_VALUE
                          Character = 0 } }
                  NewText = script }

            return Some [| textEdit |] |> LspResult.success
    }

let textDocumentSemanticTokens
    (client: ScriptDomLspClient)
    (paramz: SemanticTokensParams)
    : AsyncLspResult<SemanticTokens option> =
    async {
        let! tokens, errors = client.AddOrUpdateTokens(paramz.TextDocument)

        if errors.Count > 0 then
            return LspResult.success None
        else
            return LspResult.success None
    }

let setupRequestHandlings client =
    Map
        [ ("initialize", requestHandling (initialize client))
          ("initialized", requestHandling (initialized client))
          ("textDocument/didOpen", requestHandling (textDocumentDidOpen client))
          ("textDocument/formatting",
           requestHandling (textDocumentFormatting client))
          ("textDocument/semanticTokens/full",
           requestHandling (textDocumentSemanticTokens client)) ]

let stdin = Console.OpenStandardInput()
let stdout = Console.OpenStandardOutput()

startWithSetup setupRequestHandlings stdin stdout ScriptDomLspClient defaultRpc
|> printfn "%O"
