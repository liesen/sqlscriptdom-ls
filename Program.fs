// For more information see https://aka.ms/fsharp-console-apps
open System
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types


type ScriptDomLspClient(sendServerNotification: ClientNotificationSender, sendServerRequest: ClientRequestSender) =
    inherit LspClient()


let initialize (client: ScriptDomLspClient) (paramz: InitializeParams) : AsyncLspResult<InitializeResult> =
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

let initialized (client: ScriptDomLspClient) (paramz: InitializedParams) : AsyncLspResult<unit> =
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

        let textEdit: TextEdit =
            { Range =
                { Start = { Line = 0; Character = 0 }
                  End = { Line = 0; Character = 10 } }
              NewText = sprintf "Hello from my own LSP: %O" paramz.TextDocument.Uri }

        let! formattingChanges = Array.singleton textEdit |> async.Return
        return formattingChanges |> Some |> LspResult.success
    }

let setupRequestHandlings client =
    Map.ofList
        [ ("initialize", requestHandling (initialize client))
          ("initialized", requestHandling (initialized client))
          ("textDocument/formatting", requestHandling (textDocumentFormatting client)) ]


printfn "Hello from F#"
let stdin = Console.OpenStandardInput()
let stdout = Console.OpenStandardOutput()

printfn "%O"
<| startWithSetup<ScriptDomLspClient> setupRequestHandlings stdin stdout ScriptDomLspClient defaultRpc
