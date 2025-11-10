open Ppxlib
open! Ast_helper
open Analysis
open Names

module Attributes = struct
  let key ctx =
    Attribute.declare "deriving.jsont.key" ctx
      Ast_pattern.(single_expr_payload (estring __))
      Fun.id

  let cd_key = key Attribute.Context.Constructor_declaration
  let ld_key = key Attribute.Context.Label_declaration
  let rtag_key = key Attribute.Context.Rtag

  let type_key ctx =
    Attribute.declare "deriving.jsont.type_key" ctx
      Ast_pattern.(single_expr_payload (estring __))
      Fun.id

  let td_type_key = type_key Attribute.Context.type_declaration
  let ct_type_key = type_key Attribute.Context.core_type

  let wrap_key ctx =
    Attribute.declare "deriving.jsont.wrap_key" ctx
      Ast_pattern.(single_expr_payload (estring __))
      Fun.id

  let td_wrap_key = wrap_key Attribute.Context.type_declaration
  let ct_wrap_key = wrap_key Attribute.Context.core_type
  let nowrap ctx = Attribute.declare_flag "deriving.jsont.nowrap" ctx
  let cd_nowrap = nowrap Attribute.Context.constructor_declaration
  let rtag_nowrap = nowrap Attribute.Context.rtag

  let jsont context =
    Attribute.declare "deriving.jsont.jsont" context
      Ast_pattern.(single_expr_payload __)
      Fun.id

  let ct_jsont = jsont Attribute.Context.core_type

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

  let kind context =
    Attribute.declare "deriving.jsont.kind" context
      Ast_pattern.(single_expr_payload (estring __))
      Fun.id

  let td_kind = kind Attribute.Context.type_declaration
  let ct_kind = kind Attribute.Context.core_type
  let cd_kind = kind Attribute.Context.constructor_declaration
  let rtag_kind = kind Attribute.Context.rtag

  let doc context =
    Attribute.declare "deriving.jsont.doc" context
      Ast_pattern.(single_expr_payload (estring __))
      Fun.id

  let td_doc = doc Attribute.Context.type_declaration
  let ct_doc = doc Attribute.Context.core_type
  let ld_doc = doc Attribute.Context.label_declaration
  let cd_doc = doc Attribute.Context.constructor_declaration
  let rtag_doc = doc Attribute.Context.rtag
end

module A = struct
  let labelled l e = Some (Labelled l, e)
  let no_label e = Some (Nolabel, e)
  let make = List.filter_map Fun.id
end

let epipe ~loc =
  let open Ast_builder.Default in
  let lid = evar ~loc "|>" in
  fun lhs rhs -> pexp_apply ~loc lid [ (Nolabel, lhs); (Nolabel, rhs) ]

let deriver = "jsont"

let jsont_name type_name =
  match type_name with
  | "t" -> "jsont"
  | _ -> Printf.sprintf "%s_jsont" type_name

let jsont_rec_value label = "jsont_rec__" ^ label

let jsont_enum ~loc ~kind ?doc assoc =
  let open Ast_builder.Default in
  let args = [ (Labelled "kind", estring ~loc kind); (Nolabel, assoc) ] in
  pexp_apply ~loc [%expr Jsont.enum]
    (Option.fold ~none:args
       ~some:(fun doc -> (Labelled "doc", estring ~loc doc) :: args)
       doc)

type generic_constructor = {
  real_name : string with_loc;
  user_name : string option;
  kind : string option;
  doc : string option;
  nowrap : bool;
  args : constructor_arguments;
}

let jsont_sig_item ~loc ~name type_ =
  let open Ast_builder.Default in
  let value_description =
    value_description ~loc
      ~name:(Loc.make ~loc @@ jsont_name name)
      ~type_ ~prim:[]
  in
  psig_value ~loc value_description

let rec of_core_type ?kind ?doc ~current_decls (core_type : Parsetree.core_type)
    =
  let of_core_type = of_core_type ~current_decls in
  let loc = core_type.ptyp_loc in
  let user_provided = Attribute.get Attributes.ct_jsont core_type in
  match user_provided with
  | Some expr -> expr
  | None -> (
      (* TODO we should provide finer user control for handling int and floats *)
      match core_type with
      | [%type: unit] -> [%expr Jsont.null ()]
      | [%type: string] -> [%expr Jsont.string]
      | [%type: bool] -> [%expr Jsont.bool]
      | [%type: float] -> [%expr Jsont.number]
      | [%type: int] -> [%expr Jsont.int]
      | [%type: int32] -> [%expr Jsont.int32]
      | [%type: int64] -> [%expr Jsont.int64]
      | [%type: [%t? typ] option] -> [%expr Jsont.option [%e of_core_type typ]]
      | [%type: [%t? typ] list] -> [%expr Jsont.list [%e of_core_type typ]]
      | [%type: [%t? typ] array] -> [%expr Jsont.array [%e of_core_type typ]]
      | { ptyp_desc = Ptyp_constr ({ txt = lid; loc }, args); _ } ->
          (* TODO: quoting ? *)
          let is_rec =
            match lid with
            | Lident name -> (
                match Map.find_opt name current_decls with
                | None -> false
                | Some { self_rec; _ } -> self_rec)
            | _ -> false
          in
          let args = List.map of_core_type args in
          let ident =
            Exp.ident
              (Loc.make ~loc
                 (Ppxlib.Expansion_helpers.mangle_lid (Suffix "jsont") lid))
          in
          let with_args =
            match args with
            | _ :: _ ->
                Exp.apply ident
                  (List.map (fun arg -> (Nolabel, arg)) (List.rev args))
            | _ -> ident
          in
          let expr =
            if is_rec then [%expr Jsont.rec' [%e with_args]] else with_args
          in
          expr
      | { ptyp_desc = Ptyp_var label; ptyp_loc; _ } ->
          Exp.ident (Loc.make ~loc:ptyp_loc (Lident (jsont_type_var label)))
      | { ptyp_desc = Ptyp_variant (rfs, _, _); ptyp_loc; _ } ->
          let kind =
            match Attribute.get Attributes.ct_kind core_type with
            | Some kind -> kind
            | None -> Option.value kind ~default:"variant"
          in
          let doc =
            Attribute.get Attributes.ct_doc core_type
            |> Option.fold ~none:doc ~some:Option.some
          in
          let type_key = Attribute.get Attributes.ct_type_key core_type in
          let wrap_key = Attribute.get Attributes.ct_wrap_key core_type in
          let constrs =
            List.filter_map
              (fun ({ prf_desc; _ } as rtag) ->
                match prf_desc with
                | Rinherit _ -> None
                | Rtag (real_name, empty, cts) ->
                    let user_name = Attribute.get Attributes.rtag_key rtag in
                    let args =
                      if empty || List.is_empty cts then Pcstr_tuple []
                      else Pcstr_tuple cts
                    in
                    let kind = Attribute.get Attributes.rtag_kind rtag in
                    let doc = Attribute.get Attributes.rtag_doc rtag in
                    let nowrap =
                      Attribute.get Attributes.rtag_nowrap rtag
                      |> Option.fold ~none:false ~some:(fun () -> true)
                    in
                    Some { real_name; user_name; kind; doc; nowrap; args })
              rfs
          in
          of_variant_type ~loc:ptyp_loc ~kind ?doc ?type_key ?wrap_key
            ~current_decls ~poly:true constrs
      | { ptyp_desc = Ptyp_tuple cts; ptyp_loc; _ } ->
          let open Ast_builder.Default in
          let kind =
            Attribute.get Attributes.ct_kind core_type
            |> Option.fold ~none:kind ~some:Option.some
            |> Option.map (estring ~loc)
          in
          let doc =
            Attribute.get Attributes.ct_doc core_type
            |> Option.fold ~none:doc ~some:Option.some
            |> Option.map (estring ~loc)
          in
          of_tuple ~current_decls ~loc:ptyp_loc ?kind ?doc cts
      | ct ->
          let msg =
            Printf.sprintf "ppx_deriving_jsont: not implemented: core_type %s"
              (Ppxlib.string_of_core_type ct)
          in
          (* TODO better ppx error handling *)
          failwith msg)

(* Tuples are encoded as json arrays *)
and of_tuple ~current_decls ~loc ?kind ?doc cts =
  let open Ast_builder.Default in
  let elements =
    List.mapi
      (fun i ct ->
        (i, "e" ^ string_of_int i, ct.ptyp_loc, of_core_type ~current_decls ct))
      cts
  in
  let tuple_pat =
    List.map (fun (_, txt, loc, _) -> pvar ~loc txt) elements |> ppat_tuple ~loc
  in
  let enc =
    let application =
      let list =
        List.mapi
          (fun i (_, txt, loc, _) ->
            [%expr [%e eint ~loc i], [%e evar ~loc txt]])
          elements
        |> elist ~loc
      in
      epipe ~loc list [%expr List.fold_left (fun acc (i, e) -> f acc i e) acc]
    in
    let body =
      List.fold_left
        (fun acc (_, txt, loc, arg_jsont) ->
          let pat = pvar ~loc txt in
          let expr =
            [%expr
              Jsont.Json.encode' [%e arg_jsont] [%e evar ~loc txt]
              |> get_or_raise]
          in
          pexp_let ~loc Nonrecursive [ value_binding ~loc ~pat ~expr ] acc)
        application (List.rev elements)
    in
    [%expr fun f acc [%p tuple_pat] -> [%e body]]
  in
  let dec_empty =
    let nones = pexp_tuple ~loc @@ List.map (fun _ -> [%expr None]) elements in
    [%expr fun () -> [%e nones]]
  in
  let dec_add =
    let cases =
      List.mapi
        (fun i (_, _, loc, arg_jsont) ->
          let lhs = pint ~loc i in
          let rhs =
            let tuple =
              pexp_tuple ~loc
              @@ List.mapi
                   (fun j (_, txt, loc, _) ->
                     if i = j then [%expr Some e] else evar ~loc txt)
                   elements
            in
            [%expr
              let e = Jsont.Json.decode' [%e arg_jsont] elt |> get_or_raise in
              [%e tuple]]
          in
          case ~lhs ~guard:None ~rhs)
        elements
    in
    let cases =
      List.append cases
        [
          (let rhs =
             [%expr
               Jsont.Error.msgf Jsont.Meta.none "Too many elements for tuple."]
           in
           case ~lhs:(ppat_any ~loc) ~guard:None ~rhs);
        ]
    in
    [%expr fun i elt [%p tuple_pat] -> [%e pexp_match ~loc [%expr i] cases]]
  in
  let dec_finish =
    let tuple =
      pexp_tuple ~loc
      @@ List.mapi
           (fun i (_, txt, loc, _) ->
             [%expr get_or_raise [%e eint ~loc i] [%e evar ~loc txt]])
           elements
    in
    [%expr
      fun meta _ [%p tuple_pat] ->
        let get_or_raise i o =
          match o with
          | Some v -> v
          | None -> Jsont.Error.msgf meta "Missing tuple member #%i" i
        in
        [%e tuple]]
  in
  let jsont_array_map =
    let args =
      let open A in
      let kind = Option.bind kind (labelled "kind") in
      let doc = Option.bind doc (labelled "doc") in
      make
        [
          kind;
          doc;
          labelled "enc" [%expr { enc }];
          labelled "dec_empty" (evar ~loc "dec_empty");
          labelled "dec_add" (evar ~loc "dec_add");
          labelled "dec_finish" (evar ~loc "dec_finish");
          no_label [%expr Jsont.json];
        ]
    in
    pexp_apply ~loc [%expr Jsont.Array.map] args
  in
  [%expr
    let get_or_raise = function
      | Ok r -> r
      | Error err -> raise (Jsont.Error err)
    in
    let enc = [%e enc] in
    let dec_empty = [%e dec_empty] in
    let dec_add = [%e dec_add] in
    let dec_finish = [%e dec_finish] in
    [%e jsont_array_map] |> Jsont.Array.array]

and of_variant_type ~loc ~kind ?doc ?(type_key = "type") ?(wrap_key = "v")
    ~current_decls ?(poly = false) (constrs : generic_constructor list) =
  let open Ast_builder.Default in
  let lid name = { name with txt = Lident name.txt } in
  let econstruct name =
    if poly then pexp_variant name.txt
    else
      let lid = lid name in
      pexp_construct lid
  in
  let pconstruct name =
    if poly then ppat_variant name.txt
    else
      let lid = lid name in
      ppat_construct lid
  in
  let as_enum constrs =
    (* Constructors have no argument, we use an enumeration *)
    let all_constrs =
      List.map
        (fun { real_name; user_name; _ } ->
          let name = Option.value ~default:real_name.txt user_name in
          let construct = econstruct ~loc real_name None in
          [%expr [%e estring ~loc name], [%e construct]])
        constrs
    in
    jsont_enum ~loc ~kind ?doc (elist ~loc all_constrs)
  in
  let as_object_cases constrs =
    let constrs =
      List.fold_left
        (fun acc { real_name; user_name; kind; doc; nowrap; args } ->
          let name = Option.value ~default:real_name.txt user_name in
          let arg =
            match args with
            | Pcstr_tuple [] -> `No_arg
            | Pcstr_tuple [ first ] ->
                let ct = of_core_type ~current_decls first in
                if nowrap then `No_wrap ct else `Should_wrap ct
            | Pcstr_tuple cts ->
                let kind = Option.map (estring ~loc) kind in
                let doc = Option.map (estring ~loc) doc in
                `Should_wrap (of_tuple ~current_decls ~loc ?kind ?doc cts)
            | Pcstr_record labels ->
                (* Inlined record are tricky because they need to be kept
                     under their type constructor at any time. *)
                let inlined_constr = lid real_name in
                let kind = Option.value kind ~default:real_name.txt in
                `Inline_record
                  (of_record_type ~current_decls ~loc ~kind ?doc ~inlined_constr
                     labels)
          in
          let wrapped_arg =
            (* There is no need to wrap when there is no argument *)
            (* TODO We could also detect when the argument is a record and
                 not wrap it. Edit: that's not actually possible. We could
                 provide an attribute instead.  *)
            match arg with
            | `No_arg -> [%expr Jsont.Object.zero]
            | `Inline_record arg -> arg
            | `No_wrap arg -> arg
            | `Should_wrap arg ->
                let kind =
                  estring ~loc:real_name.loc
                  @@ Option.value kind ~default:real_name.txt
                in
                let args =
                  let open A in
                  let doc =
                    Option.bind doc (fun doc ->
                        labelled "doc" (estring ~loc doc))
                  in
                  make [ labelled "kind" kind; doc; no_label [%expr Fun.id] ]
                in
                let doc = estring ~loc ("Wrapper for " ^ real_name.txt) in
                let wrap_key = estring ~loc wrap_key in
                [%expr
                  [%e pexp_apply ~loc [%expr Jsont.Object.map] args]
                  |> Jsont.Object.mem [%e wrap_key] ~doc:[%e doc] [%e arg]
                       ~enc:Fun.id
                  |> Jsont.Object.finish]
          in
          let mk_fun =
            (* fun arg -> Circle arg *)
            let loc = real_name.loc in
            let pat, var =
              if arg = `No_arg then (punit ~loc, None)
              else
                let arg_name = "arg" in
                (pvar ~loc arg_name, Some (evar ~loc arg_name))
            in
            match arg with
            | `Inline_record _ -> [%expr Fun.id]
            | _ ->
                let construct = econstruct ~loc real_name var in
                pexp_fun ~loc Nolabel None pat construct
          in
          let result =
            (* Jsont.Object.Case.map "Circle" Circle.jsont ~dec:circle *)
            let name = estring ~loc:real_name.loc name in
            [%expr
              Jsont.Object.Case.map [%e name] [%e wrapped_arg] ~dec:[%e mk_fun]]
          in
          let binding_name = "jsont__" ^ real_name.txt in
          (binding_name, real_name, arg, result) :: acc)
        [] constrs
    in
    let bindings, cases =
      List.map
        (fun (binding_name, _, _, expr) ->
          ( value_binding ~loc ~pat:(pvar ~loc binding_name) ~expr,
            [%expr Jsont.Object.Case.make [%e evar ~loc binding_name]] ))
        constrs
      |> List.split
    in
    let enc_case =
      pexp_function_cases ~loc
      @@ List.map
           (fun (binding_name, real_name, arg, _) ->
             let loc = real_name.loc in
             let pat, var =
               if arg = `No_arg then (None, eunit ~loc)
               else (Some (pvar ~loc "t"), evar ~loc "t")
             in
             let wrapped_var =
               match arg with
               | `Inline_record _ -> econstruct ~loc real_name (Some var)
               | _ -> var
             in
             let rhs =
               [%expr
                 Jsont.Object.Case.value [%e evar ~loc binding_name]
                   [%e wrapped_var]]
             in
             let lhs = pconstruct ~loc real_name pat in
             case ~lhs ~guard:None ~rhs)
           constrs
    in
    let cases = elist ~loc cases in
    let map_args =
      let doc =
        Option.bind doc (fun doc -> A.labelled "doc" (estring ~loc doc))
      in
      [ A.labelled "kind" (estring ~loc kind); doc; A.no_label [%expr Fun.id] ]
      |> A.make
    in
    let doc = estring ~loc ("Cases for " ^ kind) in
    pexp_let ~loc Nonrecursive bindings
      [%expr
        [%e pexp_apply ~loc [%expr Jsont.Object.map] map_args]
        |> Jsont.Object.case_mem [%e estring ~loc type_key] ~doc:[%e doc]
             Jsont.string ~enc:Fun.id ~enc_case:[%e enc_case] [%e cases]
        |> Jsont.Object.finish]
  in
  if List.for_all (fun { args; _ } -> args = Pcstr_tuple []) constrs then
    as_enum constrs
  else as_object_cases constrs

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
(* Translates records to javascript objects *)
and of_record_type ~current_decls ~loc ~kind ?doc ?inlined_constr labels =
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
    let wrapped_record =
      match inlined_constr with
      | None -> record
      | Some lid -> pexp_construct ~loc:lid.loc lid (Some record)
    in
    List.fold_left
      (fun acc { pld_name = { txt = name; loc }; _ } ->
        pexp_fun ~loc Nolabel None (pvar ~loc name) acc)
      wrapped_record (List.rev labels)
  in
  let with_make_fun e =
    let pat = ppat_var ~loc { txt = "make"; loc } in
    let expr = make_fun in
    pexp_let ~loc Nonrecursive [ value_binding ~loc ~pat ~expr ] e
  in
  let map =
    let args =
      let doc =
        Option.bind doc (fun doc -> A.labelled "doc" (estring ~loc doc))
      in
      [
        doc;
        A.labelled "kind" (estring ~loc kind);
        A.no_label (pexp_ident ~loc { txt = lident "make"; loc });
      ]
      |> A.make
    in
    pexp_apply ~loc [%expr Jsont.Object.map] args
  in
  let mems =
    List.fold_left
      (fun acc
           ({ pld_name = { txt = default; loc = name_loc }; pld_type; _ } as ld)
         ->
        let jsont_name =
          Attribute.get Attributes.ld_key ld |> Option.value ~default
        in
        let doc = Attribute.get Attributes.ld_doc ld in
        let dec_absent, enc_omit =
          match Attribute.get Attributes.ld_option ld with
          | None -> (None, None)
          | Some () -> (Some [%expr None], Some [%expr Option.is_none])
        in
        let dec_absent =
          let absent_or_default =
            (* These have the same meaning. [default] is handled for
               compatibility with ppx_yojson_conv *)
            match Attribute.get Attributes.ld_absent ld with
            | None -> Attribute.get Attributes.ld_default ld
            | Some attr -> Some attr
          in
          match absent_or_default with None -> dec_absent | Some e -> Some e
        in
        let enc_omit =
          match Attribute.get Attributes.ld_omit ld with
          | None -> enc_omit
          | Some e -> Some e
        in
        let type_jsont = of_core_type ~current_decls pld_type in
        let field_access =
          let loc = pld_type.ptyp_loc in
          let arg =
            let var_t = ppat_var ~loc { txt = "t"; loc } in
            match inlined_constr with
            | None -> var_t
            | Some cstr -> ppat_construct ~loc cstr (Some var_t)
          in
          let pexp_attributes =
            match inlined_constr with
            | None -> []
            | Some _ ->
                let name = { txt = "ocaml.warning"; loc } in
                let payload = PStr [ pstr_eval ~loc (estring ~loc "-8") [] ] in
                [ attribute ~loc ~name ~payload ]
          in
          {
            ([%expr
               fun [%p arg] ->
                 [%e
                   pexp_field ~loc [%expr t] (Loc.make ~loc @@ lident default)]])
            with
            pexp_attributes;
          }
        in
        let loc = ld.pld_loc in
        let args =
          let open A in
          let name = estring ~loc:name_loc jsont_name in
          let doc =
            Option.bind doc (fun doc ->
                labelled "doc" (estring ~loc:Location.none doc))
          in
          let dec_absent = Option.bind dec_absent (labelled "dec_absent") in
          let enc_omit = Option.bind enc_omit (labelled "enc_omit") in
          [
            no_label name;
            doc;
            no_label type_jsont;
            labelled "enc" field_access;
            dec_absent;
            enc_omit;
          ]
          |> make
        in
        epipe ~loc acc (pexp_apply ~loc [%expr Jsont.Object.mem] args))
      map labels
  in
  with_make_fun (epipe ~loc mems [%expr Jsont.Object.finish])

type decl = { infos : decl_infos; jsont_expr : expression }

let of_type_declaration ~derived_item_loc ~current_decls
    ({ ast = { ptype_name; ptype_kind; ptype_manifest; _ }; _ } as infos) =
  (* TODO it would be better to have the loc of the annotation here *)
  let loc = derived_item_loc in
  let kind =
    Attribute.get Attributes.td_kind infos.ast
    |> Option.value ~default:(String.capitalize_ascii ptype_name.txt)
  in
  let doc = Attribute.get Attributes.td_doc infos.ast in
  let jsont_expr =
    match ptype_kind with
    | Ptype_variant constrs ->
        let constrs =
          List.map
            (fun ({ pcd_name; pcd_args; _ } as cd) ->
              let user_name = Attribute.get Attributes.cd_key cd in
              let kind = Attribute.get Attributes.cd_kind cd in
              let doc = Attribute.get Attributes.cd_doc cd in
              let nowrap =
                Attribute.get Attributes.cd_nowrap cd
                |> Option.fold ~none:false ~some:(fun () -> true)
              in
              {
                real_name = pcd_name;
                user_name;
                kind;
                doc;
                nowrap;
                args = pcd_args;
              })
            constrs
        in
        let type_key = Attribute.get Attributes.td_type_key infos.ast in
        let wrap_key = Attribute.get Attributes.td_wrap_key infos.ast in
        of_variant_type ~loc ~kind ?doc ?type_key ?wrap_key ~current_decls
          constrs
    | Ptype_record labels ->
        let expr = of_record_type ~current_decls ~loc ~kind ?doc labels in
        expr
    | Ptype_abstract -> (
        match ptype_manifest with
        | Some core_type ->
            let value = of_core_type ~kind ?doc ~current_decls core_type in
            value
        | _ -> failwith "ppx_deriving_jsont: not implemented: abstract types")
    | _ -> failwith "ppx_deriving_jsont: not implemented"
  in
  { infos; jsont_expr }

let jsont_value_binding ~loc rec_flag (decls : decl Map.t) =
  let open Ast_builder.Default in
  let bindings, values =
    List.map
      (fun (_, decl) ->
        let txt = jsont_name decl.infos.type_name.txt in
        let pat = ppat_var ~loc { decl.infos.type_name with txt } in
        let expr =
          if decl.infos.self_rec then [%expr lazy [%e decl.jsont_expr]]
          else decl.jsont_expr
        in
        let expr =
          List.fold_left
            (fun acc label ->
              pexp_fun ~loc:Location.none Nolabel None
                (ppat_var ~loc:label.loc label)
                acc)
            expr
            (List.rev decl.infos.type_params)
        in
        let value =
          let ident =
            pexp_ident ~loc { decl.infos.type_name with txt = Lident txt }
          in
          let with_args =
            match decl.infos.type_params with
            | _ :: _ ->
                Exp.apply ident
                  (List.map
                     (fun arg ->
                       ( Nolabel,
                         Exp.ident (Loc.make ~loc:arg.loc (Lident arg.txt)) ))
                     (List.rev decl.infos.type_params))
            | _ -> ident
          in
          let with_lazy =
            if decl.infos.self_rec then [%expr Lazy.force [%e with_args]]
            else with_args
          in
          List.fold_left
            (fun acc param ->
              pexp_fun ~loc Nolabel None (ppat_var ~loc param) acc)
            with_lazy decl.infos.type_params
        in
        (value_binding ~loc ~pat ~expr, value))
      (Map.to_list decls)
    |> List.split
  in
  match bindings with
  (* Special case for lone decls that are not recursive *)
  | [ binding ] when rec_flag = Nonrecursive -> binding
  | _ ->
      let expr = pexp_let ~loc rec_flag bindings (pexp_tuple ~loc values) in
      let names =
        ppat_tuple ~loc
        @@ List.map
             (fun (_, { infos = { type_name; _ }; _ }) ->
               pvar ~loc (jsont_name type_name.txt))
             (Map.to_list decls)
      in
      value_binding ~loc ~pat:names ~expr

let pp_rec_flag ppf = function
  | Nonrecursive -> ()
  | Recursive -> Format.fprintf ppf " rec"

let of_type_declarations ~derived_item_loc rec_flag tds =
  let open Ast_builder.Default in
  let non_rec = rec_flag = Nonrecursive in
  let current_decls = decl_infos ~non_rec tds in
  let () =
    if debug then begin
      List.iter
        (fun (rec_flag, decls) ->
          Format.eprintf "Group %a:\n%!" pp_rec_flag rec_flag;
          Map.iter
            (fun _ infos -> Format.eprintf "%a\n%!" pp_decl_infos infos)
            decls)
        current_decls;
      Format.eprintf "\n%!"
    end
  in
  let decls =
    List.map
      (fun (rec_flag, decls) ->
        let decls =
          Map.map
            (of_type_declaration ~derived_item_loc ~current_decls:decls)
            decls
        in
        (rec_flag, decls))
      current_decls
  in
  let bindings =
    List.map
      (fun (rec_flag', decls) ->
        let rec_flag =
          match rec_flag' with
          | Nonrecursive -> Nonrecursive
          | Recursive -> rec_flag
        in
        jsont_value_binding rec_flag ~loc:derived_item_loc decls)
      decls
  in
  List.map
    (fun b -> pstr_value ~loc:derived_item_loc Nonrecursive [ b ])
    bindings

let sig_of_type_decl ~derived_item_loc
    ({ ptype_name = { txt = name; _ }; _ } : Parsetree.type_declaration) =
  (* TODO it would be better to have the loc of the annotation here *)
  let loc = derived_item_loc in
  [
    jsont_sig_item ~loc ~name
      [%type: [%t Typ.constr (Loc.make ~loc @@ lident name) []] Jsont.t];
  ]

let generate_impl ~ctxt (rec_flag, type_declarations) =
  let derived_item_loc = Expansion_context.Deriver.derived_item_loc ctxt in
  of_type_declarations ~derived_item_loc rec_flag type_declarations

let generate_sig ~ctxt (_, type_declarations) =
  let derived_item_loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat_map (sig_of_type_decl ~derived_item_loc) type_declarations

let _jsont : Deriving.t =
  let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl in
  let sig_type_decl = Deriving.Generator.V2.make_noarg generate_sig in
  Deriving.add "jsont" ~str_type_decl ~sig_type_decl
