(* Lógica Computacional 2019/2020
 * Problema C
 *
 * Alunos:
 *    41358 - Beatriz Tavares da Costa
 *    41381 - Igor Cordeiro Bordalo Nunes
 *
 * Changelog:
 *    v1.0 (20/nov/2019) -> Primeira versão
 *    v1.1 (21/nov/2019) -> Correcção de bug (linha só com "1" dava inválido)
 *                          Adicionado suporte para múltiplas negações (e.g. "~~p")
 *    v1.2 (21/nov/2019) -> Falta de estudo: um FNC não assume duplas negações
 *                          Efeitos da madrugada: código de debug não tinha sido removido na totalidade
 *                          Cafeína kicks in: pequenas melhorias e simplificações no código
 *)

open List

(* -------------------------------------------------------------------------------- *)
(* Tipos de dados *)
type literal = string
type maybe = True | False | Unknown
type disj =
  | Bot
  | Top
  | Var of literal
  | Not of literal

(* Operadores . e $ do Haskell *)
let (<|) f g x = f (g x)
let (<<) f x = f x

(* Converte um maybe num bool *)
let bool_of_maybe = function
  | True -> true
  | _    -> false

(* Disjunção *)
let oper_or x y =
  match x, y with
  | True, _ | _, True -> True
  | False, False      -> False
  | _, _              -> Unknown

(* -------------------------------------------------------------------------------- *)
(* Avalia se a disjunção é válida *)
let rec disj_valid = function
  | []      -> Unknown
  | h :: [] -> (
      match h with
      | Top -> True 
      | Bot -> False
      | _   -> Unknown
    )
  | h :: t  ->
    let this =
      fold_left oper_or False <|
      map (
        fun x ->
          match h, x with 
          | Top, _ | _, Top         -> True
          | Bot, _ | _, Bot         -> False
          | Var a, Not b when a = b -> True
          | Not a, Var b when a = b -> True
          | _, _                    -> Unknown
      ) << t
    in oper_or this << disj_valid t

(* Avalia se a fórmula FNC é válida *)
let fnc = fold_left (&&) true <| map (bool_of_maybe <| disj_valid)

(* -------------------------------------------------------------------------------- *)
(* Converte uma string de input num literal *)
let tok = function
  | "0" -> Bot
  | "1" -> Top
  | x   -> if x.[0] = '~' then Not (String.sub x 1 << String.length x - 1) else Var x

(* Obtém o input do problema (ordem por linha não é importante) *)
let get_input () =
  let rec get_list k =
    if k = 0 then []
    else (String.split_on_char ' ' << read_line ()) :: get_list (k - 1)
  in get_list << read_int ()

(* -------------------------------------------------------------------------------- *)
(* Função MAIN *)
let () =
  let result = fnc <| map (map tok) << get_input ()
  in Printf.printf "%sVALIDA\n" << if result then "" else "NAO E "