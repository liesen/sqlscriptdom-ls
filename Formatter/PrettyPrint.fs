module PrettyPrint

type AnnotDetails<'a> =
    | AnnotStart
    | NoAnnot of string * int
    | AnnotEnd of 'a

type Doc<'a> =
    | Empty
    | NilAbove of Doc<'a>
    | TextBeside of AnnotDetails<'a> * Doc<'a>
    | Nest of int * Doc<'a>
    | Union of Doc<'a> * Doc<'a>
    | NoDoc
    | Beside of Doc<'a> * bool * Doc<'a>
    | Above of Doc<'a> * bool * Doc<'a>

let textBeside_ a x = TextBeside(a, x)

let text s =
    textBeside_ (NoAnnot(s, s.Length)) Empty

let sizedText l s = textBeside_ (NoAnnot(s, l)) Empty

let zeroWidthText = sizedText 0

let union_ p q = Union(p, q)

let nest_ k p = Nest(k, p)

let union_ p q = Union(p, q)

let empty = Empty

let isEmpty =
    function
    | Empty -> true
    | _ -> false

let indent n = String.replicate n " "

let spaceText = NoAnnot(" ", 1)

let nlText = NoAnnot ("\n", 1)

let nilAbove_ = NilAbove

let rec nest k p = mkNest k (reduceDoc p)
and mkNest k =
    function
    | Nest(k1, p) -> mkNest (k + k1) p
    | NoDoc -> NoDoc
    | Empty -> Empty
    | p -> if k = 0 then p else nest_ k p

let above_ p g q =
    match q with
    | Empty -> p
    | _ -> match p with
           | Empty -> q
           | _ -> Above(p, g, q)

let rec above p g q =
    match p with
    | Above(p, g1, q1) -> above p g1 (above q1 g q)
    | Beside _ -> aboveNest (reduceDoc p) g 0 (reduceDoc q)
    | otherwise -> aboveNest p g 0 (reduceDoc q)
and aboveNest p g k q =
    match p with
    | NoDoc -> NoDoc
    | Union(p1, p2) -> union_ (aboveNest p1 g k q) (aboveNest p2 g k q)
    | Empty -> mkNest k q

let beside_ p g q =
    match p with
    | Empty -> q
    | _ ->
        match q with
        | Empty -> p
        | _ -> Beside(p, g, q)

let rec beside d g q =
    match d with
    | NoDoc -> NoDoc
    | Union(p1, p2) -> union_ (beside p1 g q) (beside p2 g q)
    | Empty -> q
    | Nest(k, p) -> nest_ k <| beside p g q
    | Beside(p1, g1, q1) ->
        if g1 = g then
            beside p1 g1 <| beside q1 g q
        else
            beside (reduceDoc p) g q
    | Above _ -> beside (reduceDoc p) g q
    | NilAbove p -> nilAbove_ <| beside p g q
    | TextBeside(t, p) ->
        let rest =
            match p with
            | Empty -> nilBeside g q
            | _ -> beside p g q

        TextBeside(t, rest)

let rec nilBeside g =
    function
    | Empty -> Empty
    | Nest(_, p) -> nilBeside g p
    | p when g -> textBeside_ spaceText p
    | p -> p

let annotate a d =
    TextBeside(
        AnnotStart,
        beside (reduceDoc d) false (TextBeside(AnnotEnd a, Empty))
    )

(*
let rec best w r =
    function
    | Text s -> Text s
    | TextAbove(s, x) -> TextAbove(s, (best w r x))
    | Nest(k, x) -> Nest(k, best (w - k) r x)
    | Union(x, y) -> nicest w r (best w r x) (best w r y)

and nicest w r x y =
    if shorter (firstline x) (min w r) then x else y

and shorter xs n = Seq.isEmpty (Seq.skip n xs)

and firstline =
    function
    | Text s -> s
    | TextAbove(s, x) -> s

let text s = Text s

let nest k x = Nest(k, x)

let rec (^&&^) (x: Doc) (y: Doc) : Doc =
    match (x, y) with
    | (Text s, y) -> TextAbove(s, y)
    | (TextAbove(s, x), y) -> TextAbove(s, x ^&&^ y)
    | (Nest(k, x), y) -> Nest(k, x ^&&^ Nest(-k, y))
    | (Union(x, y), z) -> Union((x ^&&^ z), (y ^&&^ z))

let rec (^<>^) (x: Doc) (y: Doc) : Doc =
    match (x, y) with
    | (Text s, Text t) -> Text(s + t)
    | (Text s, TextAbove(t, x)) -> TextAbove(s + t, Nest(s.Length, x))
    | (Text s, Nest(k, x)) -> Text s ^<>^ x
    | (Text s, Union(x, y)) -> Union(Text s ^<>^ x, Text s ^<>^ y)
    | (TextAbove(s, x), y) -> TextAbove(s, x ^<>^ y)
    | (Nest(k, x), y) -> Nest(k, (x ^<>^ y))
    | (Union(x, y), z) -> Union(x ^<>^ y, y ^<>^ z)

let (^<+>^) x y = Union(x, Union(text " ", y))

let rec fit =
    function
    | Text s -> Text s
    | TextAbove(s, x) -> Empty
    | Nest(k, x) ->
        match fit x with
        | Empty -> Empty
        | y -> Nest(k, y)
    | Union(x, y) -> fit x

let rec sep xs =
    if Seq.length xs = 1 then
        Seq.head xs
    else
        match Seq.tryHead xs with
        | Some(Nest(k, x)) ->
            Nest(
                k,
                sep (
                    Seq.append
                        (Seq.singleton x)
                        (Seq.map (nest (-k)) (Seq.tail xs))
                )
            )
        | None ->
            let u x y =
                match x with
                | Empty -> y
                | _ -> Union(x, y)

            u (fit (Seq.reduceBack (^<+>^) xs)) (Seq.reduceBack (^&&^) xs)

let indent k s = (String.replicate k " ") + s + "\n"

let rec layout k =
    function
    | Text s -> indent k s
    | TextAbove(s, x) -> indent k s + layout k x
    | Nest(k', x) -> layout (k + k') x

// let example = sep [sep [sep [text "hello"; text "a"; text "b"; text "c"]]]
*)
