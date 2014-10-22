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
exception InvalidArguments

exception Placeholder

let rec print_datum input =
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
  | Cons (car, cdr) -> "Cons (" ^ string_of_cons car cdr ^ ")"

(* let rec print_datum input =
  match input with
  | Atom (Boolean b) -> "Atom (Boolean " ^ (if b then "#t" else "#f") ^ ")"
  | Atom (Integer n) -> "Atom (Integer " ^ (string_of_int n) ^ ")"
  | Atom (Identifier id) -> "Atom (Identifier " ^ (Identifier.string_of_identifier id) ^")"
  | Nil -> "Nil"
  | Cons (car, cdr) -> "Cons (" ^ (print_datum car) ^ ", " ^ (print_datum cdr) ^ ")" *)




(* Parses a datum into an expression. *)
let rec read_expression (input : datum) : expression =
  match input with
  | Atom (Identifier id) when Identifier.is_valid_variable id ->
      ExprVariable (Identifier.variable_of_identifier id)
  | Atom (Identifier id) -> raise InvalidIdentifier
  | Atom (Integer x) -> ExprSelfEvaluating (SEInteger x)
  | Atom (Boolean x) -> ExprSelfEvaluating (SEBoolean x)
  | Nil -> raise InvalidExpression
  (* quote parsing *)
  | Cons (Atom (Identifier id), Cons (e , Nil)) when
    ((Identifier.string_of_identifier id) = "quote") ->
      ExprQuote e
  (* if expression parsing *)
  | Cons ((Atom (Identifier id)), Cons (e1, Cons (e2, Cons (e3, Nil))))
    when ((Identifier.string_of_identifier id) = "if") ->
      ExprIf ((read_expression e1),(read_expression e2),(read_expression e3))
  (* equal? and cons parsing *)
  | Cons ((Atom (Identifier id)), Cons (e1, Cons (e2, Nil))) 
    when (((Identifier.string_of_identifier id) = "equal?") ||
        ((Identifier.string_of_identifier id) = "cons")) ->
      ExprProcCall (ExprVariable (Identifier.variable_of_identifier id), 
        [read_expression e1;read_expression e2])
    (* eval parsing *)
  | Cons (Atom (Identifier id), e1) when
      (((Identifier.string_of_identifier id) = "+") || 
        ((Identifier.string_of_identifier id) = "*")) ->
      ExprProcCall (ExprVariable (Identifier.variable_of_identifier id), 
        (parse e1))
  | Cons ((Atom (Identifier id)), Cons (e1, Nil))
      when ((Identifier.string_of_identifier id) = "eval") ->
      ExprProcCall (ExprVariable (Identifier.variable_of_identifier id),
        [read_expression e1])
  | Cons (Atom (Identifier id), Cons (vars, Cons (body,Nil))) when
      ((Identifier.string_of_identifier id) = "lambda") ->
        ExprLambda ((parse_vars vars),(parse body))
  (* car/cdr parsing *)
  | Cons (Atom (Identifier id), Cons (all, Nil)) ->
      ExprProcCall (ExprVariable (Identifier.variable_of_identifier id), 
        [read_expression all])
  | _ -> failwith "end of matches"


and parse (dat:datum) : expression list =
  match dat with
  | Cons (first, Nil) -> [read_expression first]
  | Cons (first,second) -> [read_expression first] @ (parse second)
  | _ -> raise InvalidArguments

and parse_vars (dat:datum) : variable list =
  match dat with
  | Cons (Atom (Identifier first), Nil) -> 
      [Identifier.variable_of_identifier first]
  | Cons (Atom (Identifier first),tail) -> 
      [Identifier.variable_of_identifier first] @ (parse_vars tail)
  | _ -> raise InvalidArguments
















(*   | Cons ((Atom (Identifier id)), Cons (e1, Cons (e2, Nil))) 
    when  ->
      ExprProcCall (ExprVariable (Identifier.variable_of_identifier id), 
        [read_expression e1;read_expression e2]) *)
  (* TODO -------------------------- *)
   (* two param procedure call *)
(*   | Cons (Atom (Identifier id), Cons (e1, Cons(e2, Nil)))
    when ((Identifier.string_of_identifier id) = "car") ->

      ExprProcCall (ExprVariable (Identifier.variable_of_identifier id),
        [(read_expression e1);(read_expression e2)])
    else 
      raise Placeholder *)
  (* if first identifier is not a keyword, must be procedure call *)
(*   | Cons (Atom (Identifier id), tl) ->
    if (Identifier.is_valid_variable id) then
      ExprProcCall (ExprVariable (Identifier.variable_of_identifier id), 
        [(read_expression tl)])
    else
      raise Placeholder *)
  
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
  print_string (print_datum input);
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

let equal_helper (lst:value list) (env:environment) : value = 
  match lst with
  | hd1::hd2::[] -> 
      if (hd1 = hd2) then
        ValDatum (Atom (Boolean true))
      else 
        ValDatum (Atom (Boolean false))
  | _ -> raise InvalidArguments   

let eval_helper (lst:value list) (env:environment) : value =
  match lst with
  | hd::[] -> hd
  | _ -> raise InvalidArguments

let cons_helper (lst:value list) (env:environment) : value =
  match lst with
  | (ValDatum hd1)::(ValDatum hd2)::[] -> ValDatum (Cons (hd1,hd2))
  | _ -> raise InvalidArguments

let car_helper (lst:value list) (env:environment) : value =
  match lst with
  | (ValDatum (Cons (hd, tail)))::[] -> ValDatum hd
  | _ -> raise InvalidArguments

let cdr_helper (lst:value list) (env:environment) : value =
  match lst with
  | (ValDatum (Cons (hd, tail)))::[] -> ValDatum tail
  | _ -> raise InvalidArguments

let add_two_datums (val1:value) (val2:value) : value =
  match (val1,val2) with
  | (ValDatum (Atom (Integer num1)), ValDatum (Atom (Integer num2))) ->
      ValDatum (Atom (Integer (num1+num2)))
  | _ -> raise InvalidArguments

let rec add_helper (lst:value list) (env:environment) : value =
  match lst with
  | [] -> ValDatum (Atom (Integer 0))
  | hd::tl -> add_two_datums hd (add_helper tl env)

let multiply_two_datums (val1:value) (val2:value) : value =
  match (val1,val2) with
  | (ValDatum (Atom (Integer num1)), ValDatum (Atom (Integer num2))) ->
      ValDatum (Atom (Integer (num1*num2)))
  | _ -> raise InvalidArguments

let rec multiply_helper (lst:value list) (env:environment) : value =
  match lst with
  | [] -> ValDatum (Atom (Integer 1))
  | hd::tl -> multiply_two_datums hd (multiply_helper tl env)



(* This function returns an initial environment with any built-in
   bound variables. *)
let rec initial_environment () : environment =
  let open Identifier in
  let first = variable_of_identifier (identifier_of_string "course") in
  let env1 = Environment.add_binding Environment.empty_environment
    (first,ref (ValDatum(Atom (Integer 3110)))) in
  let second = variable_of_identifier (identifier_of_string "equal?") in
  let env2 = Environment.add_binding env1 
    (second,ref (ValProcedure (ProcBuiltin equal_helper))) in
  let third = variable_of_identifier (identifier_of_string "eval") in
  let env3 = Environment.add_binding env2 
    (third, ref (ValProcedure (ProcBuiltin eval_helper))) in
  let fourth = variable_of_identifier (identifier_of_string "cons") in
  let env4 = Environment.add_binding env3 
    (fourth, ref (ValProcedure (ProcBuiltin cons_helper))) in
  let fifth = variable_of_identifier (identifier_of_string "car") in
  let env5 = Environment.add_binding env4
    (fifth, ref (ValProcedure (ProcBuiltin car_helper))) in
  let sixth = variable_of_identifier (identifier_of_string "cdr") in
  let env6 = Environment.add_binding env5
    (sixth, ref (ValProcedure (ProcBuiltin cdr_helper))) in
  let seventh = variable_of_identifier (identifier_of_string "+") in
  let env7 = Environment.add_binding env6
    (seventh, ref (ValProcedure (ProcBuiltin add_helper))) in
  let eighth = variable_of_identifier (identifier_of_string "*") in
  let env8 = Environment.add_binding env7
    (eighth, ref (ValProcedure (ProcBuiltin multiply_helper))) in env8


and process_if e1 e2 e3 env =
  if ((eval e1 env) = (ValDatum (Atom (Boolean false)))) then
    eval e3 env
  else 
    eval e2 env

and process_proc_call var lst env =
  if (("equal?" = (Identifier.string_of_variable var)) || 
      ("cons" = (Identifier.string_of_variable var))) then       
    match (lst,!(Environment.get_binding env var)) with
    | (hd1::hd2::[],ValProcedure (ProcBuiltin f)) -> 
      begin
        let first = (eval hd1 env) in
        let second = (eval hd2 env) in f [first;second] env
      end
    | _ -> raise InvalidArguments
  else if (("eval" = (Identifier.string_of_variable var)) || 
    ("car" = (Identifier.string_of_variable var)) ||
      ("cdr" = (Identifier.string_of_variable var))) then
        match (lst,!(Environment.get_binding env var)) with
        | (hd::[],ValProcedure (ProcBuiltin f)) -> 
            let first = eval hd env in f [first] env
        | _ -> raise InvalidArguments
  else if (("+" = (Identifier.string_of_variable var)) ||
        ("*" = (Identifier.string_of_variable var))) then
    let changed = List.map (fun ele -> eval ele env) lst in
    match !(Environment.get_binding env var) with
    | ValProcedure (ProcBuiltin f) -> f changed env
    | _ -> raise InvalidArguments
  else
    raise InvalidArguments




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
  | ExprProcCall (ExprVariable var,lst) -> process_proc_call var lst env
  | ExprIf (e1,e2,e3) -> process_if e1 e2 e3 env
  | ExprLambda (_, _) -> failwith "blah"
  | ExprAssignment (_, _) ->
     failwith "Say something funny, Rower!"
  | ExprLet (_, _)
  | ExprLetStar (_, _)
  | ExprLetRec (_, _)     ->
     failwith "Ahahaha!  That is classic Rower."
  | _ -> failwith "temporary"

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
