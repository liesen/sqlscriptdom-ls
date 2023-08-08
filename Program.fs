// For more information see https://aka.ms/fsharp-console-apps
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

let textDocumentFormatting
    (client: ScriptDomLspClient)
    (paramz: DocumentFormattingParams)
    : AsyncLspResult<TextEdit[] option> =
    async {
        do!
            client.WindowLogMessage
                { Type = MessageType.Warning
                  Message = sprintf "%O" paramz.TextDocument.Uri }

        do!
            client.WindowShowMessage
                { Type = MessageType.Warning
                  Message = sprintf "%O" paramz.TextDocument.Uri }

        let parser = TSql160Parser(true)
        let uri = new Uri(HttpUtility.UrlDecode(paramz.TextDocument.Uri))
        use reader = new StreamReader(uri.LocalPath)
        let fragment, errors = parser.Parse(reader)
        let generator = Sql160ScriptGenerator()
        let script: string = generator.GenerateScript(fragment)

        let textEdit: TextEdit =
            { Range =
                { Start = { Line = 0; Character = 0 }
                  End =
                    { Line = UINTEGER_MAX_VALUE
                      Character = 0 } }
              NewText = script }

        let! formattingChanges = Array.singleton textEdit |> async.Return
        return formattingChanges |> Some |> LspResult.success
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
