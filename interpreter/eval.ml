open Ast

type builtin = value list -> environment -> value

and procedure =
  | ProcBuiltin of builtin
  | ProcLambda of variable list * environment * expression list

and value =
  | ValDatum of datum
  | ValProcedure of procedure

and binding = value ref Environment.binding
and environment = value ref Environment.environment

(* substitute value ref wherever you see 'a in environment.ml to see
how the type is actually declared in terms of smaller quantities *)
(* 
'a environment = ('a binding) list = (variable * value ref) list
'a binding = variable * value ref


 *)
(* EXCEPTIONS *)
exception InvalidIdentifier
exception InvalidExpression
exception InvalidDatum
exception VariableNotDefined

exception Placeholder

(* let rec print_datum input =
  let string_of_cons car cdr =
    let rec strings_of_cons cdr =
      match cdr with
      | Nil -> []
      | Cons (car, cdr) -> (print_datum car) :: (strings_of_cons cdr)
      | _ -> ["."; print_datum cdr;] in
    let string_list = (print_datum car) :: (strings_of_cons cdr) in
    "(" ^ (String.concat " " string_list) ^ ")"  in
  match input with
  | Atom (Boolean b) -> "Atom (Boolean " ^ (if b then "#t" else "#f") ^ ")"
  | Atom (Integer n) -> "Atom (Integer " ^ (string_of_int n) ^ ")"
  | Atom (Identifier id) -> "Atom (Identifier " ^ (Identifier.string_of_identifier id) ^")"
  | Nil -> "Nil"
  | Cons (car, cdr) -> "Cons (" ^ string_of_cons car cdr ^ ")" *)


(* Parses a datum into an expression. *)
let rec read_expression (input : datum) : expression =
  match input with
  | Atom (Identifier id) when Identifier.is_valid_variable id ->
      ExprVariable (Identifier.variable_of_identifier id)
  | Atom (Identifier id) -> raise InvalidIdentifier
  | Atom (Integer x) -> ExprSelfEvaluating (SEInteger x)
  | Atom (Boolean x) -> ExprSelfEvaluating (SEBoolean x)
  | Nil -> raise InvalidExpression
  | Cons (Atom (Identifier id), Cons (e , Nil)) when
    ((Identifier.string_of_identifier id) = "quote") ->
      ExprQuote e
  (* if expression parsing *)
  | Cons ((Atom (Identifier id)), Cons (e1, Cons (e2, Cons (e3, Nil)))) 
      when ((Identifier.string_of_identifier id) = "if") ->
      ExprIf ((read_expression e1),(read_expression e2),(read_expression e3))

  (* TODO -------------------------- *)
   (* two param procedure call *)
  | Cons (Atom (Identifier id), Cons (e1, Cons(e2, Nil))) ->
    if ((Identifier.string_of_identifier id) = "equal?") then
      ExprProcCall (ExprVariable (Identifier.variable_of_identifier id),
        [(read_expression e1);(read_expression e2)])
    else 
      raise Placeholder
  (* if first identifier is not a keyword, must be procedure call *)
  | Cons (Atom (Identifier id), tl) ->
    if (Identifier.is_valid_variable id) then
      ExprProcCall (ExprVariable (Identifier.variable_of_identifier id), 
        [(read_expression tl)])
    else
      raise Placeholder
  | _ -> raise InvalidExpression





(* (
Cons (Atom (Identifier (Identifier.identifier_of_string "if")),
Cons(Cons (Atom (Identifier (Identifier.identifier_of_string "equal?")), 
  Cons (Atom (Integer 5), Cons (Atom (Integer 5), Nil))),
Cons(Atom (Integer 1), Cons(Atom (Integer 0), Nil))
)))
 *)

(*   | Cons (first, Nil) -> read_expression first
  | Cons (one, two) ->  *)
(* | Atom (Identifier id) when Identifier.is_valid_variable id ->
      if ((Identifier.string_of_identifier id) = "if") then
        ExprIf
      else if (id = "quote") then
        ExprQuote
      else
        raise Placeholder *)

(* Parses a datum into a toplevel input. *)
let read_toplevel (input : datum) : toplevel =
  (* print_string (print_datum input); *)
  match input with
  | Cons (Atom (Identifier id), Cons (Atom (Identifier name), tl)) 
    when ((Identifier.string_of_identifier id) = "define") &&
         (Identifier.is_valid_variable name) ->
    ToplevelDefinition (Identifier.variable_of_identifier name,
      (read_expression tl))
  | _ -> ToplevelExpression (read_expression input)








let rec process_variable (var:Identifier.variable) (env:environment) : value =
  if (Environment.is_bound env var) then
    !(Environment.get_binding env var)
  else 
    raise VariableNotDefined




(* This function returns an initial environment with any built-in
   bound variables. *)
let rec initial_environment () : environment =
  let open Identifier in
  let first = variable_of_identifier (identifier_of_string "course") in
  Environment.add_binding Environment.empty_environment
    (first,ref (ValDatum(Atom (Integer 3110))))


and process_if e1 e2 e3 env =
  if ((eval e1 env) = (ValDatum (Atom (Boolean false)))) then
    eval e3 env
  else 
    eval e2 env

(* Evaluates an expression down to a value in a given environment. *)
(* You may want to add helper functions to make this function more
   readable, because it will get pretty long!  A good rule of thumb
   would be a helper function for each pattern in the match
   statement. *)
and eval (expression : expression) (env : environment) : value =
  match expression with
  | ExprSelfEvaluating (SEBoolean x) -> ValDatum (Atom (Boolean x))
  | ExprSelfEvaluating (SEInteger x) -> ValDatum (Atom (Integer x))
  | ExprVariable x -> process_variable x env
  | ExprQuote x -> ValDatum x
  | ExprLambda (_, _)
  | ExprProcCall _        ->
     failwith "Sing along with me as I row my boat!'"
  | ExprIf (e1,e2,e3) -> process_if e1 e2 e3 env
  | ExprAssignment (_, _) ->
     failwith "Say something funny, Rower!"
  | ExprLet (_, _)
  | ExprLetStar (_, _)
  | ExprLetRec (_, _)     ->
     failwith "Ahahaha!  That is classic Rower."

(* Evaluates a toplevel input down to a value and an output environment in a
   given environment. *)
let eval_toplevel (toplevel : toplevel) (env : environment) :
      value * environment =
  match toplevel with
  | ToplevelExpression expression -> (eval expression env, env)
  | ToplevelDefinition (_, _)     ->
     failwith "I couldn't have done it without the Rower!"

let rec string_of_value value =
  let rec string_of_datum datum =
    match datum with
    | Atom (Boolean b) -> if b then "#t" else "#f"
    | Atom (Integer n) -> string_of_int n
    | Atom (Identifier id) -> Identifier.string_of_identifier id
    | Nil -> "()"
    | Cons (car, cdr) -> string_of_cons car cdr

  and string_of_cons car cdr =
    let rec strings_of_cons cdr =
      match cdr with
      | Nil -> []
      | Cons (car, cdr) -> (string_of_datum car) :: (strings_of_cons cdr)
      | _ -> ["."; string_of_datum cdr;] in
    let string_list = (string_of_datum car) :: (strings_of_cons cdr) in
    "(" ^ (String.concat " " string_list) ^ ")" in
  
  match value with
  | ValDatum (datum) -> string_of_datum datum
  | ValProcedure (ProcBuiltin p) -> "#<builtin>"
  | ValProcedure (ProcLambda (_, _, _)) -> "#<lambda>"
