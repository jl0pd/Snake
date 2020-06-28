namespace Snake

module Tuple3 =
    let mapEach f ((a, b, c): 'a * 'a * 'a) = (f a, f b, f c)

module Color =
    open System

    [<Struct>]
    type RGB =
        { R: float
          G: float
          B: float }

        member t.AsTuple = (t.R, t.G, t.B)

        static member OfTuple(r, g, b) = { R = r; G = g; B = b }

        static member OfString(str: string) =
            let tryParse (s: string) =
                let mutable res = 0

                let success =
                    Int32.TryParse
                        (s, Globalization.NumberStyles.HexNumber, Globalization.CultureInfo.InvariantCulture, &res)

                if success then Some res else None

            let tryParseF = tryParse >> Option.map float

            if str.Length = 6 then
                let r, g, b =
                    Tuple3.mapEach tryParseF (str.[0..1], str.[2..3], str.[4..5])

                match r, g, b with
                | Some sr, Some sg, Some sb -> Some { R = sr; G = sg; B = sb }
                | _ -> None
            else
                None

    [<Struct>]
    type HSV = { H: float; S: float; V: float }

    let private normalizeValue c = max 0. (min 255. c)

    let hexByte (b: byte) = BitConverter.ToString [| b |]

    let toColorString { R = r; G = g; B = b } =
        let stringifier = normalizeValue >> int >> byte >> hexByte
        Tuple3.mapEach stringifier (r, g, b)
        |||> sprintf "#%s%s%s"
