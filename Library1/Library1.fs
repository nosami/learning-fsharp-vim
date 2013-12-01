module Stuff
open Xunit
open Swensen.Unquote


type position = int * int
type buffer = string list * position

let dd buffer =
    
    match buffer with
    | (lines, (col, line)) -> (lines |> List.filter ((<>) lines.[line]), (col,line))

let moveleft buffer = 
    let (lines,(row,col)) = buffer
    (lines,(row - 1, col))

let isDigit cmd = 
    let isDigit d = '0' <= d && '9' >= d 
    cmd |> String.forall isDigit

let applyRepeat cmds =
    seq { 
        let lastRepeat = ref None

        let foundRepeat cmd =
            lastRepeat := Some (int cmd)
            Seq.empty

        let checkRepeat cmd = 
            match lastRepeat.Value with
            | Some repeat -> Seq.init lastRepeat.Value.Value (fun _ -> cmd) 
            | None -> seq {yield cmd}
            
        for cmd in cmds do
        match cmd with
        | cmd when isDigit cmd -> yield! foundRepeat cmd
        | _ -> yield! checkRepeat cmd
    }

let exec buffer cmd =
    match cmd with
    | "dd" -> dd buffer
    | "h" -> moveleft buffer
    | _ -> buffer

let vim buffer cmd =
    let f buffer cmd = exec buffer cmd
    (Seq.fold f buffer (applyRepeat cmd))

let mkBuf input =
    let lineWithDollar l = 
        l |> (String.exists ((=) '$'))
    let positionOfDollar (l:string) = 
        l.IndexOf '$'   


    let lineWithDollar = input |> List.findIndex lineWithDollar

    let removeDollar pos (line:string) =
        match pos with
        | lineWithDollar -> line.Replace ("$","")
        | _ -> line

    (input |> List.mapi removeDollar,
     (positionOfDollar input.[lineWithDollar],lineWithDollar))


[<Fact>]
let ``mking a buf`` () =
    let (buf,pos)  = mkBuf ["a$line"]
    test <@ pos = (1,0) @>
    test <@ buf = ["aline"] @>   

[<Fact>]
let ``Some assertion `` () =
    let buffer = mkBuf["this i$s line1"
                       "this is line 2"
                       "this is line 3"
                       "this is line 4"]
   
    let (buf,pos) = vim buffer ["dd"]
    test <@ pos = (6,0) @>
    test <@ buf = ["this is line 2"
                   "this is line 3"
                   "this is line 4"] @>

[<Fact>]
let ``Should delete two lines `` () =
    let buffer = mkBuf["this i$s line1"
                       "this is line 2"
                       "this is line 3"
                       "this is line 4"]
   
    let (buf,pos) = vim buffer ["dd"; "dd"]

    test <@ pos = (6,0) @>
    test <@ buf = ["this is line 3"
                   "this is line 4"] @>

[<Fact>]
let ``Should also delete two lines `` () =
    let buffer = mkBuf["this i$s line1"
                       "this is line 2"
                       "this is line 3"
                       "this is line 4"]
   
    let (buf,pos) = vim buffer ["2"; "dd"]

    test <@ pos = (6,0) @>
    test <@ buf = ["this is line 3"
                   "this is line 4"] @>

[<Fact>]
let ``Should move the cursor left`` () =
    let buffer = mkBuf["this i$s line1"]
   
    let (buf,pos) = vim buffer ["h"]

    test <@ pos = (5,0) @>