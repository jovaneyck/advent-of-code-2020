#r "nuget: Unquote"
#r "nuget: FParsec"
open Swensen.Unquote
open FParsec

type Operator = Add | Mul
type OperationData = { operator : Operator; left : Expression; right : Expression }
and Expression = 
    | Value of int64
    | Operation of OperationData
    | Parentheses of Expression

let poperator = 
    ((charReturn '*' Mul) <|> (charReturn '+' Add)) .>> spaces
let pvalue = pint64 .>> spaces |>> Value
//Right-recursive variant:
let pexpression, pexpressionRef = createParserForwardedToRef<Expression, unit>()
let pexpression' = 
    (pipe2 poperator pexpression (fun op exp -> (op,exp))) |>> Some
    <|> preturn None
let constructExpression exp exp' = 
    match exp' with 
    | None -> exp
    | Some (op, right) -> Operation { left = exp; operator = op; right = right}
let pbetweenparens = ((between (pchar '(') (pchar ')'.>> spaces) pexpression) |>> Parentheses)
pexpressionRef :=
    pipe2 pvalue pexpression' constructExpression
    <|> pipe2 pbetweenparens pexpression' constructExpression
    <|> pbetweenparens

let parse (text : string) =
    match run (pexpression .>> eof) text with
    | Success (r,_,_) -> r
    | Failure (f,s,e) -> failwithf $"Failure: {(f,s,e)}"

let operate l op r =
    match op with
    | Add -> l + r
    | Mul -> l * r

let rec eval (exp : Expression) : int64 = 
    match exp with
    | Value v -> v
    | Parentheses e -> eval e
    | Operation { left = l; operator = op; right = r } ->
        match r with
        | Value rv -> operate (eval l) op rv
        | Parentheses r ->
            let rv = eval r
            operate (eval l) op rv
        //+ takes precedence over * as long as we don't encounter parentheses
        | Operation { left = rl; operator = rop; right = rr } as next ->
            let lv = eval l
            match op with
            | Add ->
                let operated = operate lv op (eval rl)
                eval <| Operation { left = Value operated; operator = rop; right = rr }
            | Mul ->
                let rightEvaluated = eval next
                operate lv op rightEvaluated

let input = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"  
let parsed = input |> Seq.map parse
parsed |> Seq.sumBy eval

printf "Test.."
test <@ parse "1 + 2" 
    = Operation { operator = Add; left = Value 1L; right = Value 2L } @>
test <@ parse "8 * 3" 
    = Operation { operator = Mul; left = Value 8L; right = Value 3L } @>
test <@ parse "1 + 2 + 3" 
    = Operation 
        {   operator = Add; 
            left = Value 1L; 
            right = Operation { 
                operator = Add; 
                left = Value 2L; 
                right = Value 3L }} @>
test <@ parse "(1 + 2)" = (Parentheses <| Operation { operator = Add; left = Value 1L; right = Value 2L }) @>
test <@ parse "(1 + 2) + 3" = Operation { operator = Add; left = Parentheses (Operation { operator = Add; left = Value 1L; right = Value 2L }) ; right = Value 3L } @>

test <@ eval <| parse "123" = 123L @>
test <@ eval <| parse "1 + 2" = 3L @>
test <@ eval <| parse "1 + 2 + 3" = 6L @>
test <@ eval <| parse "1 + 2 * 3" = 9L @>
test <@ eval <| parse "1 + (2 * 3)" = 7L @>

printfn "..done!"

(*
Grammar:

Operator -> +
Operator -> *

//problem: FParsec does not like left recursive grammars and will descend in an infinite loop
//Expression -> Value
//Expression -> Expression Operator Expression <-- INFINITE LOOP!
//Solution: rewrite grammar to be right-recursive:
Expression -> Value Expression'
Expression' -> Operator Expression
Expression' -> ()

Parentheses -> ( Expression ) Expression'
Parentheses -> ( Expression )
*)