//KAROLINA ŁUKASIK 255703

type Parser<'t> = seq<char> -> option<'t * seq<char>>

module Parsers = 
    let pChar c (input: seq<char>) =       
        if Seq.isEmpty input |> not && Seq.head input = c
        then Some (c, Seq.skip 1 input)             
        else None

    let char2 c : Parser<char> =  
        fun (input: seq<char>) ->      
            if Seq.isEmpty input |> not && Seq.head input = c
            then Some (c, Seq.skip 1 input)             
            else None

//Parsers.char2 'f' "fooBar";;


// Excercise 1. 
//    a. Define function 'pAny' that eats single (any) character from the input Parser<char>
    let pAny : Parser<char> =  
        fun (input: seq<char>) ->      
            if Seq.isEmpty input |> not 
            then Some (Seq.head input, Seq.skip 1 input)             
            else None

//Parsers.pAny "fooBar";;     

//    b. Define function 'pDigit' that parse single digit from the input and return Parser<int>
    let pDigit : Parser<int> =  
        fun (input: seq<char>) ->      
            if Seq.isEmpty input |> not then 
                match Seq.head input with
                | '0' -> Some (0, Seq.skip 1 input) 
                | '1' -> Some (1, Seq.skip 1 input)
                | '2' -> Some (2, Seq.skip 1 input)
                | '3' -> Some (3, Seq.skip 1 input)
                | '4' -> Some (4, Seq.skip 1 input)
                | '5' -> Some (5, Seq.skip 1 input)
                | '6' -> Some (6, Seq.skip 1 input)
                | '7' -> Some (7, Seq.skip 1 input)
                | '8' -> Some (8, Seq.skip 1 input)
                | '9' -> Some (9, Seq.skip 1 input)
                | _ -> None            
            else None
//Parsers.pDigit "s0ooBar";;
//    c. Define function 'pSpace' that eats all spaces from the begining of the input input  


//implemented in best way to cooperate with 4 exercise
    let rec pSpace : Parser<char> =  
        fun (input: seq<char>) ->      
            if Seq.isEmpty input |> not then 
                match Seq.head input with
                | ' ' ->  if (Seq.isEmpty (Seq.skip 1 input)) then Some (' ', seq [])   
                          else pSpace (Seq.skip 1 input)
                | _ -> Some (' ', input)  
            else Some (' ', seq [])       
        
        
//    d. Define function 'pWord w' that tries to parse w at the begining of the input  
    let pWord w : Parser<char> =  
        fun (input: seq<char>) ->      
            if Seq.isEmpty input |> not && Seq.head input = w
            then Some (w, Seq.skip 1 input)             
            else None

// Excercise 2.


module Combinators = 

    let combineL (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        match p1 input with  
        | Some (result, restOfInput) -> 
            match p2 restOfInput with 
            | Some (_, restOfInput2) -> Some (result, restOfInput2) 
            | None -> None 
        | _ -> None 

//a. Define 'combineR p1 p2' similar to combineL but return result of second parser
    let combineR (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        match p1 input with  
        | Some (_, restOfInput) -> 
            match p2 restOfInput with 
            | Some (result, restOfInput2) -> Some (result, restOfInput2) 
            | None -> None 
        | _ -> None 

    

//    b. Try to rewrite combineR using Option.bind. Hint: Did you use >> ? .
    let combineR2 (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        let modified_parser parser some_input= 
            match parser some_input with
            | Some(_,restOfInput)-> Some restOfInput
            | None -> None
        in (modified_parser p1 >> Option.bind p2) input

//Combinators.combineR2 Parsers.pDigit Parsers.pAny "8a8"

//    c. Define 'combine p1 p2' that produce tuple of result from both parsers
    let combine (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        match p1 input with  
        | Some (result1, restOfInput) ->
            match p2 restOfInput with 
            | Some (result2, restOfInput2) -> Some ((result1, result2),restOfInput2) 
            | None -> None 
        | _ -> None 

//Combinators.combine Parsers.pDigit Parsers.pAny "9a8"

//    e. Is it possible to rewrite comblineL and combine the same way combineR was in part b.?

//It is imposible to do it directly because we have to follow  ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c convention 
//the only way is to modify parsing functions (Problem!: following function doesnt work while combining parsers!)

    let combineL2 (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        let modified_parser parser some_input= 
            match some_input with
            | (result1,restOfInput)-> 
                match parser restOfInput with
                | Some(_,restOfInput2)-> Some (result1, restOfInput)
                | None -> None
            //| _ -> None    
        in (p1 >> Option.bind (modified_parser p2)) input

//to change combine function we would have to modify both parsing functions

//Combinators.combineL2 Parsers.pDigit Parsers.pAny "10a9a8"    


//    e. Define 'orP p1 p2' which will atempt the first parser and if fail attempt second. or should fail if both p1 and p2 fails 

    let orP (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        match p1 input with 
        | None -> p2 input
        | _-> p1 input
        

        
//Combinators.orP Parsers.pDigit Parsers.pDigit  "610a9a8"       

//    d. Define 'map p f' which will map the result of a praser with function f:
    let map (p: Parser<'t1>) (f) (input: seq<char>) =
            match p input with
            | Some (result, restOfInput) -> Some (f result, restOfInput) 
            | None -> None 
        
//Combinators.map Parsers.combine (fun (x,y) -> x + y) "10a9a8"        

module Operators = 
    open Parsers
    open Combinators
    let (.>>) : Parser<'a> -> Parser<'b> -> Parser<'a> = combineL
    let (>>.) : Parser<'a> -> Parser<'b> -> Parser<'b> = combineR 
    let (.>>.) : Parser<'a> -> Parser<'b> -> Parser<'a*'b> = combine
    let (|>>) : Parser<'a> -> ('a -> 'b) -> Parser<'b> = map
    let (<|>) : Parser<'a> -> Parser<'a> -> Parser<'a> = orP


// here is an example of operator usage = 
    //let addParser : Parser<int> = pDigit .>> pChar '+' .>>. pDigit //|>> fun (x,y) -> x + y  

//Operators.addParser "1+9";;

module Expressions = 
    open Parsers
    open Combinators
    open Operators

    [<AutoOpen>]
    module helpers = 
        let pPlus: Parser<_> = pChar '+'
        let pMinus: Parser<_> = pChar '-'
        let pMult: Parser<_> = pChar '*'
        let pDiv: Parser<_> = pChar '/'
        let pOpen: Parser<_> = pChar '('
        let pClose: Parser<_> = pChar ')'
        let endInput : Parser<_> =  
            fun (input: seq<char>) ->      
                if Seq.isEmpty input then Some (' ', input)             
                else None 
            


        let sum (a,b) = a+b;     
        let minus (a,b) = a-b;
        let mul (a,b) = a*b;
        let div (a,b) = a/b;
    
    let rec expression (input:seq<char>) : option<int * seq<char>>= 
        let pExpression = (term .>> pSpace .>> pPlus .>> pSpace .>>. expression |>> sum ) 
                      <|> (term .>> pSpace .>> pMinus .>> pSpace.>>. expression |>> minus )    
                      <|> (pSpace >>. term .>> pSpace)
        
        input |> pExpression 

    and term (input:seq<char>) : option<int * seq<char>> = 
        let pTerm = (factor .>> pSpace .>> pMult .>> pSpace .>>. term|>> mul )
                    <|> (factor .>> pSpace .>> pDiv .>> pSpace .>>. term |>> div )
                    <|> (pSpace >>.factor.>> pSpace)
        input |> pTerm

    and factor  (input:seq<char>) : option<int * seq<char>>  =  
        let pFactor = (pSpace >>. pOpen >>. expression .>> pClose.>> pSpace) <|> (pSpace >>. pDigit.>> pSpace)
        input |> pFactor

    let safeExpression(input:seq<char>) : option<int * seq<char>>=
        input |> (expression .>> endInput)
    
// Excercise 4+5+6 :Write some test for different expresions + Extent the parse so it ignores whitespaces.
//Create safeExpression which combines expression and endInput so that "5+7foobar" is no longer valid input.
Expressions.safeExpression " 4 + 5 "
Expressions.safeExpression " 6 * 4 "
Expressions.safeExpression "2-6*4 "
Expressions.safeExpression "2*5-4"
Expressions.safeExpression " ( 3 - 4 ) * 7 "
Expressions.safeExpression "(3-4)*7+8/(2+2)"
Expressions.safeExpression "5+7foobar"   //None



