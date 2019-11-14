(* Lógica Computacional 2019/2020
 * Problema B
 *
 * Alunos:
 *    41358 - Beatriz Tavares da Costa
 *    41381 - Igor Cordeiro Bordalo Nunes
 *)

open Printf
open Stack
open List

type token =
  | ParLeft
  | ParRight
  | Var
  | BiOper
  | UniOper

type expression =
  | Variable
  | UnaryOperation of expression
  | BinaryOperation of expression * expression

exception Invalid

(* Separa lista num dado índice com descarte do caracter nesse mesmo índice *)
let splitd lst at =
  let rec take l b c =
    match l with
    | [] -> (b, [])
    | h :: t -> if c = 0 then (b, t) else take t (h :: b) (c - 1)
  in (fun (x, y) -> (rev x, y)) (take lst [] at)

(* Extrai conteúdo de parêntesis
 * Devolve tuplo com interior dos parêntesis e o que resta em frente *)
let extract expr =
  let rec index e c i =
    match e with
    | [] -> -1
    | ParLeft :: t -> index t (c + 1) (i + 1)
    | ParRight :: t -> if c = 0 then i else index t (c - 1) (i + 1)
    | _ :: t -> index t c (i + 1)
  in
  let i = index expr 0 0 in
    if i = -1 then raise Invalid else splitd expr i

(* Avalia se uma expressão proposicional é sintacticamente válida *)
let evaluate expr =
  let rec execute s e =
    match e with
    | [] -> if Stack.is_empty s then raise Invalid else Stack.pop s
    | Var :: t -> Stack.push Variable s; execute s t
    | BiOper :: t -> BinaryOperation (Stack.pop s, execute s t)
    | ParRight :: _ -> raise Invalid
    | ParLeft :: t ->
        let (inn, out) = extract t in
          Stack.push (execute (Stack.create ()) inn) s;
          execute s out
    | UniOper :: t ->
        (
          match t with
          | [] -> raise Invalid
          | x :: xs ->
              let (adj, aft) = if x = ParLeft then extract xs else ([x], xs) in
                Stack.push (UnaryOperation (execute (Stack.create ()) adj)) s;
                execute s aft
        )
  in
    try
      let _ = execute (Stack.create ()) expr in true
    with
      | _ -> false

(* Abstrai as strings em tokens *)
let abstract =
  let convert s =
    match s with
    | "(" -> ParLeft
    | ")" -> ParRight
    | "!" -> UniOper
    | "&" | "|" | "->" | "<->" -> BiOper
    | "TRUE" | "FALSE" -> Var
    | _ -> Var
  in map convert

(* Leitura do input *)
let get_input () =
  let rec get_list k =
    if k = 0 then []
    else let line = String.capitalize_ascii (read_line ()) in line :: get_list (k - 1)
  in
    get_list (read_int ())

(* Função MAIN *)
let () =
  let input = abstract (get_input ()) in
  let result = evaluate input in
    printf "%s\n" (if result then "YES" else "NO")