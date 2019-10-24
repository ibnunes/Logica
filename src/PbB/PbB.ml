(* Lógica Computacional 2019/2020
 * Problema B
 *
 * Alunos:
 *    41358 - Beatriz Tavares da Costa
 *    41381 - Igor Cordeiro Bordalo Nunes
 *)

exception UnmatchBracket          (* Há parêntesis não emparelhados *)
exception EmptyStack              (* Levantado quando se tenta aceder à stack vazia *)
exception InvalidExpression       (* A expressão não é válida *)
exception UnknownToken            (* Token não reconhecido pela linguagem *)


(* Classe stack - simplificação ao essencial do tipo do módulo Stack do OCaml
 *    Funções:        push, peek, pop
 *    Propriedades:   len, isEmpty
 *    Iterador:       iter
 * Podem existir métodos implementados apenas para fins de debug.
 *)
class ['a] stack init = object
  val mutable l : 'a list = init

  method pop =
    match l with
    | x :: xs -> l <- xs; x
    | []      -> raise EmptyStack

  method push x = l <- x :: l
  
  method peek =
    match l with
    | x :: _  -> x
    | []      -> raise EmptyStack 
  
  method isEmpty  = (l = [])
  method len      = List.length l
  method iter f   = List.iter f l
end


(* Definição dos tokens da linguagem proposicional:
 *    Uma vez que não interessa o conteúdo, mas apenas a estrutura sintáctica,
 *    os tokens são reduzidos a uma expression que abstrai totalmente os operadores.
 *)
type token =
  | True | False | Var
  | And  | Or    | Then  | Eq    | Not
  | BracketL
  | BracketR

type expression =
  | Bracket         of expression
  | UnaryOperation  of expression
  | BinaryOperation of expression * expression
  | Constant
  | Nothing


(* Obtém lista de tokens envolvidos por um par de parêntesis
 *    Devolve tuplo (r, t) onde:
 *        r (result)  -> lista de tokens dentro dos parêntesis;
 *        t (tail)    -> lista de tokens restantes após os parêntesis.
 *)
let bracket_expression expr =
  let rec bktexpr e r c =
    match e with
    | []            -> raise UnmatchBracket
    | BracketL :: t -> bktexpr t (BracketL :: r) (c + 1)
    | BracketR :: t -> if c = 0 then (r, t) else bktexpr t (BracketR :: r) (c - 1)
    | h :: t        -> bktexpr t (h :: r) c
  in
    (fun (a, b) -> (List.rev a, b)) (bktexpr expr [] 0)


(* Redutor de token list para expression
 *    Recebe uma lista de tokens a reduzir e devolve a expressão abstracta correspondente.
 *    Levanta excepções adequadas caso detecte erros de sintaxe.
 *    A expressão só é válida no final se a stack tiver apenas um elemento.
 *)
let rec lexer expr =
  let s = new stack [] in
  let rec run ex = 
    match ex with
    | []             -> s#pop
    | BracketL :: xs ->
      let b, bs = bracket_expression xs in
        s#push (Bracket (lexer b)); run bs
    | BracketR :: _                         -> raise UnmatchBracket
    | True :: xs | False :: xs | Var :: xs  -> s#push Constant; run xs
    | Not :: xs                             -> UnaryOperation (run xs)
    | And :: xs | Or :: xs | Eq :: xs | Then :: xs  -> BinaryOperation (s#pop, run xs)
  in
    let result = run expr in
      if s#isEmpty then result else raise InvalidExpression


(* Tokenizer, versão altamente simplificada:
 *    Recebe lista de strings, cada string representa um token,
 *    e abstrai-o num enumerador
 *)
let token_of = function
  | "TRUE"  -> True
  | "FALSE" -> False
  | "("     -> BracketL
  | ")"     -> BracketR
  | "!"     -> Not
  | "&"     -> And
  | "|"     -> Or
  | "->"    -> Then
  | "<->"   -> Eq
  | _       -> Var

let tokenizer = List.map token_of


(* Leitura do input:
 *    Foi evitado o uso do módulo Scanf.
 *)
let get_expression () =
  let n = read_int () in
  let rec get_list k =
    if k = 0 then []
    else let line = String.capitalize_ascii (read_line ()) in line :: get_list (k-1)
  in
    get_list n


(* Função MAIN:
 *    Caso haja algum erro de sintaxe, uma excepção é levantada,
 *    pelo que uma excepção representa resultado negativo.
 * O resultado do tipo expression é descartado - pronto para adaptação a outros usos.
 *)
let () =
  let expr = get_expression () in
  try
    let _ = lexer (tokenizer expr) in Printf.printf "YES\n"
  with
    _ -> Printf.printf "NO\n"