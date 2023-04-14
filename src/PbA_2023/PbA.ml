(* Lógica Computacional 2022/2023
 *  Autor:  Igor Nunes, UBI
 *  Changelog:
 *      1.0 (12/04/2023) -> Primeira versão (com erros).
 *      1.1 (14/04/2023) -> Correcção da transição do False e do caso especial da definição do NOR enquanto NOT OR.
 *
 * Mais uma vez, para desenferrujar o meu OCaml.
 *
 * Um agradecimento especial ao Prof. Doutor Simão Melo de Sousa por me ter esclarecido dúvidas em pessoa, apesar de
 * já nem ser seu aluno.
 * *)

(* Biblioteca fornecida pelos docentes. Disponibiliza a função `parse` em particular. *)
open F_parser

(* Definição de uma fórmula composta só de portas NOR. *)
type formula_nor_t =
  | Var of string
  | Nor of formula_nor_t * formula_nor_t

(* Representação textual de uma fórmula do tipo `formula_nor_t`. *)
let rec string_of_formula_nor = function
  | Var v      -> v
  | Nor (f, g) -> "(" ^ (string_of_formula_nor f) ^ " % " ^ (string_of_formula_nor g) ^ ")"

(* Obtém a variável lexicograficamente mais pequena da árvore sintática, `Z` por omissão. *)
let rec get_smaller_var (l : formula_t) : string =
  match l with
    | Var v                         -> v
    | Not f                         -> get_smaller_var f
    | And (f, g) | Or (f, g)
    | Implies (f, g) | Equiv (f, g) -> let f, g = get_smaller_var f, get_smaller_var g in min f g
    | True | False                  -> "Z"

(* Converte `formula_t` em `formula_nor_t` usando a variável lexicograficamente mais pequena para transicionar True e False. *)
let nor_of_logic l =
  let small = get_smaller_var l in
  let rec nor_of_logic (l : formula_t) : formula_nor_t =
    match l with
      | Var v           -> Var v
      | Not (Or (f, g)) -> let f, g = nor_of_logic f, nor_of_logic g in Nor (f, g)                      (* A ⊕ B ≡ ¬(A ∨ B)           <-- Definição de NOR                   *)
      | Not f           -> let f = nor_of_logic f in Nor (f, f)                                         (*    ¬A ≡ A ⊕ A                                                     *)
      | And (f, g)      -> let f, g = nor_of_logic f, nor_of_logic g in Nor (Nor (f, f), Nor (g, g))    (* A ∧ B ≡ (A ⊕ A) ⊕ (B ⊕ B)                                         *)
      | Or (f, g)       -> let f, g = nor_of_logic f, nor_of_logic g in Nor (Nor (f, g), Nor (f, g))    (* A ∨ B ≡ (A ⊕ B) ⊕ (A ⊕ B)                                         *)
      | Implies (f, g)  -> nor_of_logic (Or (Not f, g))                                                 (* A ⇒ B ≡ ¬A ∨ B                                                    *)
      | Equiv (f, g)    -> nor_of_logic (And (Implies (f, g), Implies (g, f)))                          (* A ⇔ B ≡ (A ⇒ B) ∧ (B ⇒ A)                                         *)
      | True            -> nor_of_logic (Not False)                                                     (*     ⊤ ≡ ¬⊥                                                        *)
      | False           -> Nor (Var small, Nor (Var small, Var small))                                  (*     ⊥ ≡ A ⊕ (A ⊕ A)        <-- Transição directa com 3 portas NOR *)
  in nor_of_logic l

(* Extrai o valor de um 'option'. Falha caso seja `None`. *)
let extract_option = function
  | Some x -> x
  | None   -> failwith "Error extracting option: expected 'Some x', got 'None'."

(* MAIN *)
let () = parse "stdin" |> extract_option |> List.hd |> nor_of_logic |> string_of_formula_nor |> print_endline
