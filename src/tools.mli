module Versioned = Migrate_parsetree.OCaml_407

module T = Versioned.Ast.Asttypes

module P = Versioned.Ast.Parsetree

module H = Versioned.Ast.Ast_helper

val flatten_map : ('a -> 'b list) -> 'a list -> 'b list
(** [flatten_map f list] is equal to [List.flatten (List.map f list)]. *)

val map_loc : ('a -> 'b) -> 'a T.loc -> 'b T.loc

type affix =
  | Prefix of string
  | Suffix of string
  | PrefixSuffix of string * string

val mangle : ?fixpoint : string -> affix -> string -> string

val mangle_lid : ?fixpoint : string -> affix -> Longident.t -> Longident.t

val mangle_type_decl :
    ?fixpoint : string -> affix -> P.type_declaration -> string T.loc

val seq : ?loc : Location.t -> P.expression list -> P.expression

val separate : 'a -> 'a list -> 'a list

val poly_var : string -> string

val poly_fun_of_type_decl : P.type_declaration -> P.expression -> P.expression

val poly_arrow_of_type_decl :
    (P.core_type -> P.core_type) -> P.type_declaration -> P.core_type
      -> P.core_type

val core_type_of_type_decl : P.type_declaration -> P.core_type

val expand_path : path : string list -> string -> string

val path_of_type_decl : path : string list -> P.type_declaration -> string list

val pat_var_of_string : string -> P.pattern

val ident_of_string : string -> P.expression

val ident_of_str : string Location.loc -> P.expression

val poly_apply_of_type_decl : P.type_declaration -> P.expression -> P.expression

val var_of_type : P.core_type -> string
