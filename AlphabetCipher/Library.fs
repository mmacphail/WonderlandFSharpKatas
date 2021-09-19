namespace AlphabetCipher

module String =
    let toListChar (s: string) = s.ToCharArray() |> Array.toList

    let cycle (s: string) = Seq.initInfinite (fun i -> s.[(i % s.Length)])

    let stretch length (key : string) = cycle key
                                            |> Seq.take length
                                            |> System.String.Concat

module Alphabet =
    let alphabet = ['a' .. 'z']

    let index c = alphabet |> List.findIndex (fun x -> x = c)

    let charAt index = alphabet.[index]

module List =
    let headToTail l =
        match l with
        | [] -> []
        | head :: tail -> List.append tail [ head ]

module Key =
    let stretch key (msg : string) = String.stretch msg.Length key

    let find (stretchedKey : string) =
        let patterns = seq { for i in 0..(stretchedKey.Length - 1) -> stretchedKey.[0..i] }
        let isStretchedKey pattern = (String.stretch stretchedKey.Length pattern) = stretchedKey
        Seq.find isStretchedKey patterns

module Cipher =
    type Rotation = 
        | Line
        | Column
    let rotate rotation (cipher : char [,]) =
        let newCipher = Array2D.init 26 26 (fun _  _ -> '_')
        for i in 0 .. 25 do
            for j in 0 .. 25 do
                let c = cipher.[i, j]
                let indexOfc = Alphabet.index c
                match rotation with
                | Line -> newCipher.[indexOfc, j] <- (Alphabet.charAt i)
                | Column -> newCipher.[i, indexOfc] <- (Alphabet.charAt j)
        newCipher

    let apply (cipher : char [,]) x y = 
        let lx = Alphabet.index x
        let ly = Alphabet.index y
        cipher.[lx, ly]

    let transcribe (algorithm : char -> char -> char) (key : string) (msg : string) =
        let stretchedKey = Key.stretch key msg
        List.zip (String.toListChar msg) (String.toListChar stretchedKey)
         |> List.map (fun (a, b) -> algorithm a b)
         |> System.String.Concat

    let debug (cipher : char [,]) = 
        printf "  "
        for x in 0 .. 25 do
            printf "%c" <| System.Char.ToUpper (Alphabet.charAt x)
        printfn ""
        for x in 0 .. 25 do
            printf "%c " <| System.Char.ToUpper (Alphabet.charAt x)
            for y in 0 .. 25 do
                printf "%c" cipher.[x, y]
            printfn ""

module Ciphers =
    open Cipher

    let rec repeat f i x = if i = 0 then x else f (repeat f (i - 1) x)

    let alphabetList = seq { for _ in 0..25 -> Alphabet.alphabet }
                        |> Seq.mapi (repeat List.headToTail)
                        |> Seq.toList

    let encoding = Array2D.init 26 26 (fun x y -> List.item x alphabetList |> List.item y)
    let decoding = rotate Line encoding
    let deciphering = rotate Column encoding

module Api =
    open Cipher
    open Ciphers
    
    let encoding = apply encoding
    let decoding = apply decoding
    let deciphering = apply deciphering

    let encode = transcribe encoding
    let decode = transcribe decoding
    let decipher encrypted original = 
        let stretchedKey = transcribe deciphering encrypted original
        Key.find stretchedKey