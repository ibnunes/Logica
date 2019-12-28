(* Lógica Computacional 2019/2020
 * Problema C
 *
 * Alunos:
 *    41358 - Beatriz Tavares da Costa
 *    41381 - Igor Cordeiro Bordalo Nunes
 *
 * Versão 2.0 (25/dez/2019): Algoritmo simplificado
 *
 * Esta nova versão permite uma leitura bastante mais objectiva da essência do
 * algoritmo no próprio código: os casos que validam as disjunções estão agora
 * mais claros e concisos na função "disjunction".
 *)

open List

type literal = Bot | Top | Var of string | Not of string

(* Operadores . e $ do Haskell *)
let (<|) f g x = f (g x)
let (<<) f x   = f x

(* Avalia se uma disjunção é válida *)
let rec disjunction = function
  | []         -> false
  | Top   :: _ -> true
  | Var a :: t -> if mem (Not a) t then true else disjunction t
  | Not a :: t -> if mem (Var a) t then true else disjunction t
  | _     :: t -> disjunction t

(* Avalia se uma FNC é válida *)
let fnc = fold_left (&&) true <| map disjunction

(* Converte uma string de input num literal *)
let tok = function
  | "0" -> Bot
  | "1" -> Top
  | x   -> if x.[0] = '~' then Not (String.sub x 1 << String.length x - 1) else Var x

(* Obtém o input do problema já mapeado em literais (ordem não é importante) *)
let get_input () =
  let rec get_list k =
    if k = 0 then []
    else (map tok <| String.split_on_char ' ' << read_line ()) :: get_list (k - 1)
  in get_list << read_int ()

(* Função MAIN *)
let () =
  let result = fnc << get_input ()
  in Printf.printf "%sVALIDA\n" << if result then "" else "NAO E "