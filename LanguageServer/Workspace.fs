module Workspace

open System.Collections.Generic
open System.Collections.Concurrent
open Ionide.LanguageServerProtocol.Types

type Workspace<'contents>(RootUri: DocumentUri) =
    member this.Documents = new ConcurrentDictionary<DocumentUri, 'contents>()

    member this.GetDocument(uri: DocumentUri) : 'contents option =
        match this.Documents.TryGetValue(uri) with
        | true, contents -> Some contents
        | _ -> None
