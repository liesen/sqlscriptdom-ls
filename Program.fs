﻿// For more information see https://aka.ms/fsharp-console-apps
open System
open System.IO
open System.Web
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Microsoft.SqlServer.TransactSql.ScriptDom

// https://github.com/microsoft/lsprotocol/blob/main/packages/python/lsprotocol/validators.py#L31
let UINTEGER_MAX_VALUE: int = (1 <<< 31) - 1

type ScriptDomLspClient
    (
        sendServerNotification: ClientNotificationSender,
        sendServerRequest: ClientRequestSender
    ) =
    inherit LspClient()

    override this.TextDocumentPublishDiagnostics
        (paramz: PublishDiagnosticsParams)
        : Async<unit> =
        sendServerNotification "textDocument/publishDiagnostics" (box paramz)
        |> Async.Ignore

let initialize
    (client: ScriptDomLspClient)
    (paramz: InitializeParams)
    : AsyncLspResult<InitializeResult> =
    async {
        do!
            client.WindowLogMessage
                { Type = MessageType.Warning
                  Message = "Initialize called" }

        do!
            client.WindowShowMessage
                { Type = MessageType.Warning
                  Message = "Initialize called" }

        return
            { InitializeResult.Default with
                Capabilities =
                    { ServerCapabilities.Default with
                        DocumentFormattingProvider = Some true } }
            |> LspResult.success
    }

let initialized
    (client: ScriptDomLspClient)
    (paramz: InitializedParams)
    : AsyncLspResult<unit> =
    async { return LspResult.success () }

let convertParseErrorToDiagnostic (error: ParseError) : Diagnostic =
    { Range =
        { Start =
            { Line = error.Line
              Character = error.Column }
          End =
            { Line = error.Line
              Character = error.Column } }
      Severity = Some DiagnosticSeverity.Error
      Code = None
      CodeDescription = None
      Source = Some "sqlscriptdom-ls"
      Message = error.Message
      RelatedInformation = None
      Tags = None
      Data = None }

let textDocumentFormatting
    (client: ScriptDomLspClient)
    (paramz: DocumentFormattingParams)
    : AsyncLspResult<TextEdit[] option> =
    async {
        let parser = TSql160Parser(true)
        let uri = new Uri(HttpUtility.UrlDecode(paramz.TextDocument.Uri))
        use reader = new StreamReader(uri.LocalPath)
        let fragment, errors = parser.Parse(reader)

        if errors.Count = 0 then
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
        else
            do!
                client.TextDocumentPublishDiagnostics
                    { Uri = paramz.TextDocument.Uri
                      Version = None
                      Diagnostics =
                        errors
                        |> Seq.map convertParseErrorToDiagnostic
                        |> Seq.toArray }

            return None |> LspResult.success

    }

let setupRequestHandlings client =
    Map.ofList
        [ ("initialize", requestHandling (initialize client))
          ("initialized", requestHandling (initialized client))
          ("textDocument/formatting",
           requestHandling (textDocumentFormatting client)) ]

let stdin = Console.OpenStandardInput()
let stdout = Console.OpenStandardOutput()

startWithSetup setupRequestHandlings stdin stdout ScriptDomLspClient defaultRpc
|> printfn "%O"
