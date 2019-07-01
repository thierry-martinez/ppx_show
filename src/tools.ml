module Versioned = Migrate_parsetree.OCaml_407

module T = Versioned.Ast.Asttypes

module P = Versioned.Ast.Parsetree

module H = Versioned.Ast.Ast_helper

let rec rev_map_append f list accu =
  match list with
  | [] -> accu
  | hd :: tl -> rev_map_append f tl (List.rev_append (f hd) accu)

let flatten_map f list =
  List.rev (rev_map_append f list [])

let map_loc (f : 'a -> 'b) ({ loc; txt } : 'a T.loc) : 'b T.loc =
  { loc; txt = f txt }

type affix =
  | Prefix of string
  | Suffix of string
  | PrefixSuffix of string * string

let mangle ?(fixpoint = "t") affix name =
  if name = fixpoint then
    match affix with
    | Prefix x | Suffix x -> x
    | PrefixSuffix (x, y) -> x ^ "_" ^ y
  else
    match affix with
    | Prefix x -> x ^ "_" ^ name
    | Suffix x -> name ^ "_" ^ x
    | PrefixSuffix (x, y) -> x ^ "_" ^ name ^ "_" ^ y

let mangle_type_decl ?fixpoint affix (td : P.type_declaration) : string T.loc =
  map_loc (mangle ?fixpoint affix) td.ptype_name

let mangle_lid ?fixpoint affix (lid : Longident.t) : Longident.t =
  match lid with
  | Lident s -> Lident (mangle ?fixpoint affix s)
  | Ldot (p, s) -> Ldot (p, mangle ?fixpoint affix s)
  | Lapply _ -> invalid_arg "mangle_lid"

let seq ?(loc = !Ast_helper.default_loc) list : P.expression =
  match List.rev list with
  | [] -> [%expr ()]
  | hd :: tl ->
      List.fold_left begin fun acc item : P.expression ->
        [%expr [%e item]; [%e acc]]
      end hd tl

let separate separator l =
  match l with
  | [] | [_] -> l
  | hd :: tl ->
      let revl =
        List.fold_left begin fun acc x ->
          x :: separator :: acc
        end [] tl in
      hd :: List.rev revl

let poly_var x =
  "poly_" ^ x

let var_of_type (ty : P.core_type) =
  match ty.ptyp_desc with
  | Ptyp_var x -> x
  | _ -> invalid_arg "var_of_type"

let poly_fun_of_type_decl (td : P.type_declaration) (e : P.expression)
    : P.expression =
  let loc = !H.default_loc in
  List.fold_left begin fun acc (ty, _) : P.expression ->
    let var = var_of_type ty in
    [%expr fun [%p H.Pat.var { loc; txt = poly_var var }] -> [%e acc]]
  end e (List.rev td.ptype_params)

let poly_arrow_of_type_decl (mkvar : P.core_type -> P.core_type)
    (td : P.type_declaration) (ty : P.core_type)
    : P.core_type =
  let loc = !H.default_loc in
  List.fold_left begin fun acc ((ty : P.core_type), _) : P.core_type ->
    [%type: [%t mkvar ty] -> [%t acc]]
  end ty (List.rev td.ptype_params)

let core_type_of_type_decl (td : P.type_declaration) : P.core_type =
  H.Typ.constr
    (td.ptype_name |> map_loc (fun x : Longident.t -> Lident x))
    (List.map fst td.ptype_params)

let expand_path ~path ident =
  String.concat "." (path @ [ident])

let path_of_type_decl ~path (td : P.type_declaration) =
  match td.ptype_manifest with
  | Some { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, _); _ } ->
    begin match lid with
    | Lident _ -> []
    | Ldot (lid, _) -> Longident.flatten lid
    | Lapply _ -> assert false
    end
  | _ -> path

let pat_var_of_string s =
  let loc = !H.default_loc in
  H.Pat.var { loc; txt = s }

let ident_of_string s =
  let loc = !H.default_loc in
  H.Exp.ident { loc; txt = Lident s }

let ident_of_str ({ loc; txt } : string Location.loc) =
  H.Exp.ident { loc; txt = Lident txt }

let poly_apply_of_type_decl (td : P.type_declaration) (e : P.expression) =
  match td.ptype_params with
  | [] -> e
  | _ ->
      H.Exp.apply e begin td.ptype_params |> List.map begin
        fun (ty, _) : (T.arg_label * P.expression) ->
          Nolabel, ident_of_string (poly_var (var_of_type ty))
      end end
