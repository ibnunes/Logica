(* Lógica Computacional 2019/2020
 * Problema A
 *
 * Alunos:
 *    41358 - Beatriz Tavares da Costa
 *    41381 - Igor Cordeiro Bordalo Nunes
 *)

open Printf
open String
open List


(* Movimentos possíveis do cavalo *)
let movements = [(1, -2); (2, -1); (2, 1); (1, 2); (-1, 2); (-2, 1); (-2, -1); (-1, -2)]

(* A posição final está dentro dos limites do tabuleiro? *)
let valid_position n (x, y) = (x >= 0) && (y >= 0) && (x < n) && (y < n)

(* Mapeia as posições válidas do cavalo a partir do ponto inicial (x, y) *)
let positions n (x, y) = filter (valid_position n) (map (fun (a, b) -> (x+a, y+b)) movements)


(* Calcula o total de k movimentos partindo de (a, b) num tabuleiro n*n *)
let rec moves n k (a, b) =
  match k with
    | 0 -> 1
    | _ ->
      let pos = positions n (a, b) in
        fold_left ((+)) 0 (map (moves n (k-1)) pos)


(* Input: "N k a b" *)
let get_int_list () = map int_of_string (split_on_char ' ' (read_line ()))

let get_data () =
  match get_int_list () with
    | [n; k; a; b] -> Some (n, k, a, b)
    | _ -> None


(* Validação do input *)
let is_data_valid (n, k, a, b) =
  let c1 = (n >= 1) && (n <= 50) in
  let c2 = (k >= 1) && (k <= 8) in
  let c3 = (a >= 0) && (a < n) in
  let c4 = (b >= 0) && (b < n) in
    c1 && c2 && c3 && c4


(* MAIN *)
let () =
  match get_data () with
    | Some xs ->
      if is_data_valid xs then
        let (n, k, a, b) = xs in
        let result = moves n k (a, b) in
          printf "%d\n" result
      else
        printf "Data not valid\n"
    | None -> printf "Not 4 values\n"