(* Ex 2.4 - assemble to integers *)

(*

    Run using `dotnet run`

*)

open Absyn
open Expr

(* SCST = 0, SVAR = 1, SADD = 2, SSUB = 3, SMUL = 4, SPOP = 5, SSWAP = 6; *)
(* Our code *)
let sinstrToInt = function
  | SCstI i -> [0;i]
  | SVar i  -> [1;i]
  | SAdd    -> [2]
  | SSub    -> [3]
  | SMul    -> [4]
  | SPop    -> [5]
  | SSwap   -> [6]

(* Our code *)
let assemble instrs = 
    instrs |> List.collect sinstrToInt

(* Our code *)
let v = assemble (scomp e1 [])
printf "%A\n" v

let intsToFile (inss : int list) (fname : string) = 
    let text = String.concat " " (List.map string inss)
    System.IO.File.WriteAllText(fname, text);;

(* Our code *)
intsToFile (assemble (scomp e1 [])) "is1.txt"
