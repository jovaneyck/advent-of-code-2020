#r "nuget: Unquote"
#r "nuget: FParsec"
open Swensen.Unquote
open FParsec

//How to explain:
//https://www.quanttec.com/fparsec/tutorial.html#
//https://tyrrrz.me/blog/parsing-with-fparsec
//1. model your types: operators, values, nested expressions
//2. write parsers for your "building blocks"
//3. combine them into parser
// Notice the symmetry between your types and your parsers!

type Operator = Add | Mul
type OperationData = { operator : Operator; lOperand : Expression; rOperand : Expression }
and Expression = 
    | Value of int64
    | Operation of OperationData

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    //fun s  -> p s
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let pexpression, pExpressionRef = createParserForwardedToRef<Expression, unit>()
let pvalue = pint64 .>> spaces |>> Value <!> "pvalue"
let poperator = 
    ((charReturn '*' Mul) <|> (charReturn '+' Add)) .>> spaces
    <!> "poperator"
let poperation = 
    pipe3 pexpression poperator pexpression (fun l o r -> Operation { lOperand = l; operator = o; rOperand = r})
    <!> "poperation"
do pExpressionRef := pvalue <|> poperation <!> "pexpression"

//Het gaat em hierboven zitten: http://www.quanttec.com/fparsec/users-guide/looking-ahead-and-backtracking.html
//welke parser eerst proberen? Moeten we backtracken met attempt? Ofwa? :P
//OF zit het probleem in de types? Die zijn ook mutually recursive?
//Of we weten root node 100% zeker dat we een operation moeten parsen!
let parse (text : string) =
    match run (poperation .>> eof) text with
    | Success (r,_,_) -> r
    | Failure (f,s,e) -> failwithf $"Failure: {(f,s,e)}"

test <@ parse "1 + 2" 
    = Operation { operator = Add; lOperand = Value 1L; rOperand = Value 2L } @>
test <@ parse "8 * 3" 
    = Operation { operator = Mul; lOperand = Value 8L; rOperand = Value 3L } @>
test <@ parse "1 + 2 + 3" 
    = Operation 
        {   operator = Add; 
            lOperand = Value 1L; 
            rOperand = Operation { 
                operator = Add; 
                lOperand = Value 2L; 
                rOperand = Value 3L }} @>

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"  

printf "Test.."
printfn "..done!"