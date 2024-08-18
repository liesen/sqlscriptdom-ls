open System
open System.Net
open System.Net.Sockets
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO
open System.Web
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Microsoft.SqlServer.TransactSql.ScriptDom
open FsPretty.Rendering

open Workspace
open Formatter

// Idea from clangd -input-mirror-file=
let inputMirrorFile = "./input-mirror.log"

// https://github.com/microsoft/lsprotocol/blob/main/packages/python/lsprotocol/validators.py#L31
let UINTEGER_MAX_VALUE: int = (1 <<< 31) - 1

let convertParseErrorToDiagnostic (error: ParseError) : Diagnostic =
    let pos =
        { Line = error.Line
          Character = error.Column }

    { Range = { Start = pos; End = pos }
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
        (textDocumentUri: DocumentUri)
        (errors: IList<ParseError>)
        : Async<unit> =
        this.TextDocumentPublishDiagnostics
            { Uri = textDocumentUri
              Version = None
              Diagnostics =
                errors |> Seq.map convertParseErrorToDiagnostic |> Seq.toArray }

    member this.GetDocumentStream(textDocumentUri: DocumentUri) =
        let localPath = Uri(HttpUtility.UrlDecode(textDocumentUri)).LocalPath
        new StreamReader(localPath)

    member this.AddOrUpdateTokensFromText
        (textDocument: TextDocumentItem)
        : Async<IList<TSqlParserToken> * IList<ParseError>> =
        async {
            let parser = TSql160Parser(true)
            use reader = new StringReader(textDocument.Text)
            let tokens, errors = parser.GetTokenStream(reader)
            do! this.PublishDiagnosticsForParseErrors textDocument.Uri errors
            return tokens, errors
        }

    member this.AddOrUpdateTokens
        (textDocument: TextDocumentIdentifier)
        : Async<IList<TSqlParserToken> * IList<ParseError>> =
        async {
            let parser = TSql160Parser(true)
            use reader = this.GetDocumentStream textDocument.Uri
            let tokens, errors = parser.GetTokenStream(reader)
            do! this.PublishDiagnosticsForParseErrors textDocument.Uri errors
            return tokens, errors
        }

    member this.AddOrUpdateDocumentFromText
        (textDocumentUri: DocumentUri)
        (text: string)
        : Async<TSqlFragment * IList<ParseError>> =
        async {
            let parser = TSql160Parser(true)
            use reader = new StringReader(text)
            let fragment, errors = parser.Parse(reader)
            do! this.PublishDiagnosticsForParseErrors textDocumentUri errors
            return fragment, errors
        }

    member this.AddOrUpdateDocument
        (textDocumentUri: string)
        : Async<TSqlFragment * IList<ParseError>> =
        async {
            let parser = TSql160Parser(true)
            use reader = this.GetDocumentStream textDocumentUri
            let fragments, errors = parser.Parse(reader)
            do! this.PublishDiagnosticsForParseErrors textDocumentUri errors
            return fragments, errors
        }

type ScriptDomLspServer(client: ScriptDomLspClient) =
    inherit LspServer()

    override this.Dispose() = ()

    override this.WorkspaceSymbolResolve(paramz: WorkspaceSymbol) =
        // Why no default implementation for this?
        async.Return LspResult.notImplemented

    override this.Initialize
        (paramz: InitializeParams)
        : AsyncLspResult<InitializeResult> =
        async {
            return
                { InitializeResult.Default with
                    Capabilities =
                        { ServerCapabilities.Default with
                            CodeActionProvider = Some(First true)
                            DocumentFormattingProvider = Some true
                            TextDocumentSync =
                                Some
                                    { TextDocumentSyncOptions.Default with
                                        Change = Some TextDocumentSyncKind.Full
                                        OpenClose = Some true
                                        Save = Some { IncludeText = Some true } }
                            SemanticTokensProvider =
                                Some
                                    { Legend =
                                        { TokenTypes = [| "keyword" |]
                                          TokenModifiers = [||] }
                                      Range = Some false
                                      Full = Some(First true) } } }
                |> LspResult.success
        }

    override this.Initialized(paramz: InitializedParams) : Async<unit> =
        client.WindowShowMessage
            { Type = MessageType.Info
              Message = "sqlscriptdom-ls initialized" }

    override this.TextDocumentDidChange
        (paramz: DidChangeTextDocumentParams)
        : Async<unit> =
        client.AddOrUpdateDocument paramz.TextDocument.Uri |> Async.Ignore

    override this.TextDocumentDidOpen
        (paramz: DidOpenTextDocumentParams)
        : Async<unit> =
        client.AddOrUpdateDocumentFromText
            paramz.TextDocument.Uri
            paramz.TextDocument.Text
        |> Async.Ignore

    override this.TextDocumentDidSave
        (paramz: DidSaveTextDocumentParams)
        : Async<unit> =
        match paramz.Text with
        | None -> async.Return()
        | Some text ->
            client.AddOrUpdateDocumentFromText paramz.TextDocument.Uri text
            |> Async.Ignore

    override this.TextDocumentFormatting
        (paramz: DocumentFormattingParams)
        : AsyncLspResult<TextEdit[] option> =
        async {
            let! fragment, errors =
                client.AddOrUpdateDocument paramz.TextDocument.Uri

            do!
                client.WindowLogMessage
                    { Type = MessageType.Warning
                      Message =
                        (serialize paramz.Options.AdditionalData).ToString() }

            if errors.Count > 0 then
                return None |> LspResult.success
            else
                let opts = SqlScriptGeneratorOptions()
                opts.IndentationSize <- paramz.Options.TabSize
                opts.AlignClauseBodies <- false
                opts.AlignColumnDefinitionFields <- false
                let reader = new StreamReader(paramz.TextDocument.Uri)
                let script, errors = ppScript reader
                (*
                let script: string = generator.GenerateScript(fragment)
                return None |> LspResult.success
                *)

                let textEdit: TextEdit =
                    { Range =
                        { Start = { Line = 0; Character = 0 }
                          End =
                            { Line = UINTEGER_MAX_VALUE
                              Character = 0 } }
                      NewText = script }

                return Some [| textEdit |] |> LspResult.success
        }

    override this.TextDocumentCodeAction
        (paramz: CodeActionParams)
        : AsyncLspResult<TextDocumentCodeActionResult option> =
        async {
            let codeAction: CodeAction =
                { Title = "Where do you want to go today?"
                  Kind = None
                  Diagnostics = None
                  IsPreferred = None
                  Disabled = None
                  Edit = None
                  Command = None
                  Data = None }

            return Some [| Second codeAction |] |> LspResult.success
        }

    override this.TextDocumentSemanticTokensFull
        (paramz: SemanticTokensParams)
        : AsyncLspResult<SemanticTokens option> =
        async {
            let! tokens, errors = client.AddOrUpdateTokens paramz.TextDocument

            let data =
                tokens
                // Keep only keywords
                |> Seq.filter (fun token -> token.IsKeyword())
                // Make tokens 0-indexed
                |> Seq.map (fun token ->
                    TSqlParserToken(
                        token.TokenType,
                        token.Offset,
                        token.Text,
                        token.Line - 1,
                        token.Column - 1
                    ))
                // Make token positions relative
                |> Seq.scan
                    (fun (prev: TSqlParserToken) (curr: TSqlParserToken) ->
                        let deltaLine = curr.Line - prev.Line

                        let deltaStartChar =
                            if deltaLine = 0 then
                                curr.Column - prev.Column
                            else
                                curr.Column

                        TSqlParserToken(
                            curr.TokenType,
                            curr.Offset,
                            curr.Text,
                            deltaLine,
                            deltaStartChar
                        ))
                    (TSqlParserToken(TSqlTokenType.None, 0, "", 0, 0))
                // Encode
                |> Seq.collect (fun token ->
                    [| uint32 token.Line
                       uint32 token.Column
                       uint32 token.Text.Length
                       0u // Keyword
                       0u |])
                |> Seq.toArray

            return Some { Data = data; ResultId = None } |> LspResult.success
        }

(*
let server = new TcpListener(IPAddress.Parse("127.0.0.1"), 6008)
server.Start()

let client = server.AcceptTcpClient()
printfn "Connected!"

let input = client.GetStream()
let output = input;
*)

let input = Console.OpenStandardInput()
let output = Console.OpenStandardOutput()

start
    (defaultRequestHandlings ())
    input
    output
    ScriptDomLspClient
    (fun client -> new ScriptDomLspServer(client))
    defaultRpc
|> printfn "%O"
