open Ppxlib
open! Ast_helper

module Attributes = struct
  let key =
    Attribute.declare "key" Attribute.Context.Constructor_declaration
      Ast_pattern.(single_expr_payload (estring __))
      Fun.id
end

let deriver = "jsont"

let jsont_enum ~loc ~kind assoc =
  let open Ast_builder.Default in
  pexp_apply ~loc (evar ~loc "Jsont.enum")
    [ (Labelled "kind", estring ~loc kind); (Nolabel, assoc) ]

let of_type_declaration ~loc
    ({ ptype_name = { txt = type_name; _ }; ptype_kind; _ } :
      Parsetree.type_declaration) =
  let jsont_name = Printf.sprintf "%s_jsont" type_name in
  let type_name = String.capitalize_ascii type_name in
  match ptype_kind with
  | Ptype_variant constrs ->
      let open Ast_builder.Default in
      let all_constrs =
        List.map
          (fun ({ pcd_name = { txt = default; _ }; _ } as cd) ->
            let name =
              Attribute.get Attributes.key cd |> Option.value ~default
            in
            [%expr [%e estring ~loc name], [%e econstruct cd None]])
          constrs
      in
      [
        pstr_value ~loc Nonrecursive
          [
            value_binding ~loc ~pat:(pvar ~loc jsont_name)
              ~expr:(jsont_enum ~loc ~kind:type_name (elist ~loc all_constrs));
          ];
      ]
  | _ -> []

let generate_impl ~ctxt (_, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  (* let return s =
       (* See "Generating code" chapter *)
       let app =
         Exp.apply
           (Exp.ident (Loc.make ~loc (lident "print_endline")))
           [ (Nolabel, Ast_builder.Default.estring ~loc s) ]
       in
       [ Ast_builder.Default.(pstr_eval ~loc app []) ]
     in *)
  let jsont_convs =
    List.concat_map (of_type_declaration ~loc) type_declarations
  in
  jsont_convs

let generator () = Deriving.Generator.V2.make Deriving.Args.empty generate_impl
let _jsont : Deriving.t = Deriving.add "jsont" ~str_type_decl:(generator ())
