open Ppxlib
open! Ast_helper

module Attributes = struct
  let key ctx =
    Attribute.declare "deriving.jsont.key" ctx
      Ast_pattern.(single_expr_payload (estring __))
      Fun.id

  let cd_key = key Attribute.Context.Constructor_declaration
  let ld_key = key Attribute.Context.Label_declaration

end

let deriver = "jsont"

let jsont_name type_name =
  match type_name with
  | "t" -> "jsont"
  | _ -> Printf.sprintf "%s_jsont" type_name

let jsont_enum ~loc ~kind assoc =
  let open Ast_builder.Default in
  pexp_apply ~loc (evar ~loc "Jsont.enum")
    [ (Labelled "kind", estring ~loc kind); (Nolabel, assoc) ]

let jsont_str_item ~loc ~name expr =
  let open Ast_builder.Default in
  pstr_value ~loc Nonrecursive
    [ value_binding ~loc ~pat:(pvar ~loc @@ jsont_name name) ~expr ]

let rec of_core_type (core_type : Parsetree.core_type) =
  let loc = core_type.ptyp_loc in
  match core_type with
  | [%type: string] -> [%expr Jsont.string]
  | [%type: [%t? typ] option] ->
      let jsont = of_core_type typ in
      [%expr Jsont.option [%e jsont]]
  | _ -> failwith "not implemented"

(* Example from Jsont documentation:
    module Status = struct
      type t = Todo | Done | Cancelled
      let assoc = ["todo", Todo; "done", Done; "cancelled", Cancelled ]
      let jsont = Jsont.enum ~kind:"Status" assoc
    end

    module Item = struct
      type t = { task : string; status : Status.t; tags : string list; }
      let make task status tags = { task; status; tags }
      let task i = i.task
      let status i = i.status
      let tags i = i.tags
      let jsont =
        Jsont.Object.map ~kind:"Item" make
        |> Jsont.Object.mem "task" Jsont.string ~enc:task
        |> Jsont.Object.mem "status" Status.jsont ~enc:status
        |> Jsont.Object.mem "tags" Jsont.(list string) ~enc:tags
          ~dec_absent:[] ~enc_omit:(( = ) [])
        |> Jsont.Object.finish
    end
*)
let of_type_declaration ~loc
    ({ ptype_name = { txt = type_name; _ }; ptype_kind; _ } :
      Parsetree.type_declaration) =
  let kind = String.capitalize_ascii type_name in
  match ptype_kind with
  | Ptype_variant constrs ->
      let open Ast_builder.Default in
      let all_constrs =
        List.map
          (fun ({ pcd_name = { txt = default; _ }; _ } as cd) ->
            let name =
              Attribute.get Attributes.cd_key cd |> Option.value ~default
            in
            [%expr [%e estring ~loc name], [%e econstruct cd None]])
          constrs
      in
      [
        jsont_str_item ~loc ~name:type_name
          (jsont_enum ~loc ~kind (elist ~loc all_constrs));
      ]
  | Ptype_record labels ->
      (* Translates records to javascript objects *)
      let open Ast_builder.Default in
      (* Jsont needs a function to construct the record *)
      let make_fun =
        let record =
          let fields =
            List.map
              (fun { pld_name = { txt = name; _ }; _ } ->
                (Loc.make ~loc @@ lident name, evar ~loc name))
              labels
          in
          pexp_record ~loc fields None
        in
        List.fold_left
          (fun acc { pld_name = { txt = name; _ }; _ } ->
            pexp_fun ~loc Nolabel None (pvar ~loc name) acc)
          record (List.rev labels)
      in
      let mems =
        List.fold_left
          (fun acc ({ pld_name = { txt = default; _ }; pld_type; _ } as ld) ->
            let jsont_name =
              Attribute.get Attributes.ld_key ld |> Option.value ~default
            in
            let type_jsont = of_core_type pld_type in
            let field_access =
              [%expr
                fun t ->
                  [%e
                    pexp_field ~loc [%expr t] (Loc.make ~loc @@ lident default)]]
            in
            [%expr
              Jsont.Object.mem [%e estring ~loc jsont_name] [%e type_jsont]
                ~enc:[%e field_access] [%e acc]])
          [%expr Jsont.Object.map ~kind:[%e estring ~loc kind] [%e make_fun]]
          labels
      in
      [
        jsont_str_item ~loc ~name:type_name
          [%expr Jsont.Object.finish [%e mems]];
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
