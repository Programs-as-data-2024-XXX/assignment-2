(* Ex 2.4 - assemble to integers *)

(*

    Run using `dotnet run`

*)

open Absyn
open Expr

(* SCST = 0, SVAR = 1, SADD = 2, SSUB = 3, SMUL = 4, SPOP = 5, SSWAP = 6; *)
let sinstrToInt = function
  | SCstI i -> [0;i]
  | SVar i  -> [1;i]
  | SAdd    -> [2]
  | SSub    -> [3]
  | SMul    -> [4]
  | SPop    -> [5]
  | SSwap   -> [6]

let assemble instrs = 
    instrs |> List.collect sinstrToInt

(* Output the integers in list inss to the text file called fname: *)
let v = assemble (scomp e1 [])
printf "%A\n" v

let intsToFile (inss : int list) (fname : string) = 
    let text = String.concat " " (List.map string inss)
    System.IO.File.WriteAllText(fname, text);;

intsToFile (assemble (scomp e1 [])) "is1.txt"
