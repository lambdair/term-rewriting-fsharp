open Term
open TRS

let fac x =
  let rec faci x a =
    match x with
    | 0 -> a
    | _ -> faci (x-1) (a*x) in
  faci x 1

let rec map f xs =
  match xs with
  | [] -> []
  | x::ys -> f x::(map f ys)
    
let rec reverse xs =
  match xs with
  | [] -> []
  | head::tail ->
    (reverse tail) @ [head]

let x = [|1;2;3|]
let y = [1;2;3]
let a : term = V "x"
let e = [|V "x";F ("g",[|V "y"|])|]
let b : term = F ("f",e)

//let d = reverse [1;2]

[<EntryPoint>]
let main argv =
  printfn "hello"
  printfn "%A" (fac 5)
  printfn "%A" <| b
  0