type exp =
  | Num   of int
  | Plus  of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div   of exp * exp

type formula =
  | And of exp list
  | Or  of exp list

let showArg xs = 
  let mutable s = ""
  for x in xs do
      s <- s + x + ","
  s.Remove (s.Length - 1)
    
type term =
  | V of string
  | F of string * term array
    
  override this.ToString() =
    match this with
    | V x      -> x
    | F (f,ss) ->
      f 
      + "(" 
      + (showArg 
        <| Array.map string ss)
      + ")"
    