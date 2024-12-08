open Ppxlib
open! Ast_helper

module Attributes = struct
  let key ctx =
    Attribute.declare "deriving.jsont.key" ctx
      Ast_pattern.(single_expr_payload (estring __))
      Fun.id

  let cd_key = key Attribute.Context.Constructor_declaration
  let ld_key = key Attribute.Context.Label_declaration

  let default context =
    (* This is for compatibility with [ppx_yojson_conv], [absent] is more
       idiomatic to Jsont. *)
    Attribute.declare "deriving.jsont.default" context
      Ast_pattern.(single_expr_payload __)
      Fun.id

  let absent context =
    Attribute.declare "deriving.jsont.absent" context
      Ast_pattern.(single_expr_payload __)
      Fun.id

  let ld_default = default Attribute.Context.label_declaration
  let ld_absent = absent Attribute.Context.label_declaration

  let omit context =
    Attribute.declare "deriving.jsont.omit" context
      Ast_pattern.(single_expr_payload __)
      Fun.id

  let ld_omit = omit Attribute.Context.label_declaration
  let option context = Attribute.declare_flag "deriving.jsont.option" context
  let ld_option = option Attribute.Context.label_declaration
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

let jsont_sig_item ~loc ~name type_ =
  let open Ast_builder.Default in
  let value_description =
    value_description ~loc
      ~name:(Loc.make ~loc @@ jsont_name name)
      ~type_ ~prim:[]
  in
  psig_value ~loc value_description

let rec of_core_type (core_type : Parsetree.core_type) =
  let loc = core_type.ptyp_loc in
  (* TODO we should provide finer user control for handling int and floats *)
  match core_type with
  | [%type: unit] -> [%expr Jsont.null ()]
  | [%type: string] -> [%expr Jsont.string]
  | [%type: bool] -> [%expr Jsont.bool]
  | [%type: float] -> [%expr Jsont.number]
  | [%type: int] -> [%expr Jsont.int]
  | [%type: int32] -> [%expr Jsont.int32]
  | [%type: int64] -> [%expr Jsont.int64]
  | [%type: [%t? typ] option] ->
      let jsont = of_core_type typ in
      [%expr Jsont.option [%e jsont]]
  | [%type: [%t? typ] list] ->
      let jsont = of_core_type typ in
      [%expr Jsont.list [%e jsont]]
  | [%type: [%t? typ] array] ->
      let jsont = of_core_type typ in
      [%expr Jsont.array [%e jsont]]
  | { ptyp_desc = Ptyp_constr ({ txt = lid; loc }, _args); _ } ->
      (* TODO: arguments ? quoting ? *)
      Exp.ident
        (Loc.make ~loc
           (Ppxlib.Expansion_helpers.mangle_lid (Suffix "jsont") lid))
  | ct ->
      let msg =
        Printf.sprintf "Not implemented: core_type %s"
          (Ppxlib.string_of_core_type ct)
      in
      failwith msg

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
let of_type_declaration ~derived_item_loc
    ({ ptype_name = { txt = type_name; _ }; ptype_kind; ptype_manifest; _ } :
      Parsetree.type_declaration) =
  (* TODO it would be better to have the loc of the annotation here *)
  let loc = derived_item_loc in
  let kind = String.capitalize_ascii type_name in
  match ptype_kind with
  | Ptype_variant constrs ->
      let open Ast_builder.Default in
      let all_constrs =
        List.map
          (fun ({ pcd_name = { txt = default; loc }; _ } as cd) ->
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
              (fun { pld_name = { txt = name; loc }; _ } ->
                (Loc.make ~loc @@ lident name, evar ~loc name))
              labels
          in
          pexp_record ~loc fields None
        in
        List.fold_left
          (fun acc { pld_name = { txt = name; loc }; _ } ->
            pexp_fun ~loc Nolabel None (pvar ~loc name) acc)
          record (List.rev labels)
      in
      let mems =
        List.fold_left
          (fun acc
               ({ pld_name = { txt = default; loc = name_loc }; pld_type; _ } as
                ld) ->
            let jsont_name =
              Attribute.get Attributes.ld_key ld |> Option.value ~default
            in
            let dec_absent, enc_omit =
              match Attribute.get Attributes.ld_option ld with
              | None -> ([%expr None], [%expr None])
              | Some () -> ([%expr Some None], [%expr Some Option.is_none])
            in
            let dec_absent =
              let absent_or_default =
                (* These are the same meaning. [default] is handled for
                   compatibility with ppx_yojson_conv *)
                match Attribute.get Attributes.ld_absent ld with
                | None -> Attribute.get Attributes.ld_default ld
                | Some attr -> Some attr
              in
              match absent_or_default with
              | None -> dec_absent
              | Some e ->
                  let loc = e.pexp_loc in
                  [%expr Some [%e e]]
            in
            let enc_omit =
              match Attribute.get Attributes.ld_omit ld with
              | None -> enc_omit
              | Some e ->
                  let loc = e.pexp_loc in
                  [%expr Some [%e e]]
            in
            let type_jsont = of_core_type pld_type in
            let field_access =
              let loc = pld_type.ptyp_loc in
              [%expr
                fun t ->
                  [%e
                    pexp_field ~loc [%expr t] (Loc.make ~loc @@ lident default)]]
            in
            let loc = ld.pld_loc in
            [%expr
              Jsont.Object.mem
                [%e estring ~loc:name_loc jsont_name]
                [%e type_jsont] ~enc:[%e field_access]
                ?dec_absent:[%e dec_absent] ?enc_omit:[%e enc_omit] [%e acc]])
          [%expr Jsont.Object.map ~kind:[%e estring ~loc kind] [%e make_fun]]
          labels
      in
      [
        jsont_str_item ~loc ~name:type_name
          [%expr Jsont.Object.finish [%e mems]];
      ]
  | Ptype_abstract -> (
      match ptype_manifest with
      | Some core_type ->
          let value = of_core_type core_type in
          [ jsont_str_item ~loc ~name:type_name value ]
      | _ -> failwith "Not implemented: abstract types")
  | _ -> []

let sig_of_type_decl ~derived_item_loc
    ({ ptype_name = { txt = name; _ }; _ } : Parsetree.type_declaration) =
  (* TODO it would be better to have the loc of the annotation here *)
  let loc = derived_item_loc in
  [
    jsont_sig_item ~loc ~name
      [%type: [%t Typ.constr (Loc.make ~loc @@ lident name) []] Jsont.t];
  ]

let generate_impl ~ctxt (_, type_declarations) =
  let derived_item_loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat_map (of_type_declaration ~derived_item_loc) type_declarations

let generate_sig ~ctxt (_, type_declarations) =
  let derived_item_loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat_map (sig_of_type_decl ~derived_item_loc) type_declarations

let _jsont : Deriving.t =
  let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl in
  let sig_type_decl = Deriving.Generator.V2.make_noarg generate_sig in
  Deriving.add "jsont" ~str_type_decl ~sig_type_decl
