open Ppxlib
open! Ast_helper
open Names
module Set = Set.Make (String)
module Map = Map.Make (String)

let debug = false

type decl_infos = {
  ast : type_declaration;
  type_name : string with_loc;
  type_params : string with_loc list;
  requires : Set.t;
  self_rec : bool;
}

let pp_decl_infos fmt { type_name; requires; self_rec; _ } =
  let pp_sep fmt () = Format.fprintf fmt ";" in
  Format.fprintf fmt "%S requires [%a] (self_rec = %b)" type_name.txt
    (Format.pp_print_seq ~pp_sep Format.pp_print_string)
    (Set.to_seq requires) self_rec

let rec usages_of ~decls ~(in_type : Parsetree.core_type) (acc : Set.t) =
  let usages_of acc in_type = usages_of ~decls ~in_type acc in
  match in_type with
  | [%type: [%t? typ] option]
  | [%type: [%t? typ] list]
  | [%type: [%t? typ] array]
  | {
      ptyp_desc = Ptyp_alias (typ, _) | Ptyp_poly (_, typ) | Ptyp_open (_, typ);
      _;
    } ->
      usages_of acc typ
  | { ptyp_desc = Ptyp_arrow (_, typ1, typ2); _ } ->
      usages_of (usages_of acc typ1) typ2
  | { ptyp_desc = Ptyp_tuple cts | Ptyp_class (_, cts); _ } ->
      List.fold_left usages_of acc cts
  | { ptyp_desc = Ptyp_package (_, withs); _ } ->
      List.fold_left (fun acc (_, ct) -> usages_of acc ct) acc withs
  | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, args); _ } ->
      let acc =
        match lid with
        | Lident name when Map.mem name decls -> Set.add name acc
        | _ -> acc
      in
      List.fold_left usages_of acc args
  | { ptyp_desc = Ptyp_object (ofs, _); _ } ->
      List.fold_left
        (fun acc -> function
          | { pof_desc = Otag (_, ct) | Oinherit ct; _ } -> usages_of acc ct)
        acc ofs
  | _ -> acc

let usages_in_record_of ~decls labels acc =
  List.fold_left
    (fun acc { pld_type = in_type; _ } -> usages_of ~decls ~in_type acc)
    acc labels

let usages_of ~decls ~(in_type_decl : Parsetree.type_declaration) =
  let usages_of acc in_type = usages_of ~decls ~in_type acc in
  let acc =
    Option.fold ~none:Set.empty ~some:(usages_of Set.empty)
      in_type_decl.ptype_manifest
  in
  match in_type_decl.ptype_kind with
  | Ptype_variant constrs ->
      List.fold_left
        (fun acc { pcd_args; _ } ->
          match pcd_args with
          | Pcstr_tuple cts -> List.fold_left usages_of acc cts
          | Pcstr_record labels -> usages_in_record_of ~decls labels acc)
        acc constrs
  | Ptype_record labels -> usages_in_record_of ~decls labels acc
  | _ -> acc

let is_self_rec ~decls decl_name decl_requires =
  let rec aux decl_name ~already_checked requires =
    if Set.mem decl_name requires then true
    else
      (* This could be made more efficient if required *)
      Set.find_first_opt
        (fun required_decl_name ->
          if Set.mem required_decl_name already_checked then false
          else
            let _, _, _, requires = Map.find required_decl_name decls in
            let already_checked = Set.add required_decl_name already_checked in
            aux decl_name ~already_checked requires)
        requires
      |> Option.fold ~none:false ~some:(fun _ -> true)
  in
  aux decl_name.txt ~already_checked:Set.empty decl_requires

let decl_infos ~non_rec (decls : type_declaration list) =
  let decls =
    List.fold_left
      (fun acc decl -> Map.add decl.ptype_name.txt decl acc)
      Map.empty decls
  in
  let decls =
    let f decl =
      let type_name = decl.ptype_name in
      let mk_var ~loc lbl = { txt = jsont_type_var lbl; loc } in
      let type_params =
        List.filter_map
          (fun (core_type, _) ->
            let mk_var = mk_var ~loc:core_type.ptyp_loc in
            match core_type.ptyp_desc with
            | Ptyp_var label -> Some (mk_var label)
            | _ -> None)
          decl.ptype_params
      in
      let requires =
        (* References to the current decl or another in the group are not
           allowed when the type is marked as [nonrec]. *)
        if non_rec then Set.empty else usages_of ~decls ~in_type_decl:decl
      in
      (decl, type_name, type_params, requires)
    in
    Map.map f decls
  in
  let decls =
    Map.map
      (fun (ast, type_name, type_params, requires) ->
        let self_rec = is_self_rec ~decls type_name requires in
        { ast; type_name; type_params; requires; self_rec })
      decls
  in
  (* We group mutually recursive definitions together and isolate non-self
     recursive ones. This is required to prevent unauthorized uses of recursion
     such as:

     let rec t = u and u = 4;;
     Error: This kind of expression is not allowed as right-hand side of let rec
  *)
  (* TODO This part is due a refactor at some point. If a more performant
     algorithm is needed this looks like a good candidate for union find. *)
  let result =
    Map.fold
      (fun name decl acc ->
        if not decl.self_rec then
          Array.append acc [| (Nonrecursive, Map.singleton name decl) |]
        else
          match
            Array.find_index
              (function
                | Nonrecursive, _ -> false
                | Recursive, m ->
                    Map.exists
                      (fun _ decl ->
                        decl.self_rec && Set.mem name decl.requires)
                      m)
              acc
          with
          | None -> Array.append acc [| (Recursive, Map.singleton name decl) |]
          | Some i ->
              Array.set acc i
                (match acc.(i) with
                | Recursive, m -> (Recursive, Map.add name decl m)
                | Nonrecursive, _ -> assert false);
              acc)
      decls [||]
  in
  Array.sort
    (fun (_, ds1) (_, ds2) ->
      let ds2_requires_ds1 =
        Map.exists
          (fun _ d -> Set.exists (fun name -> Map.mem name ds1) d.requires)
          ds2
      in
      if ds2_requires_ds1 then -1 else 1)
    result;
  Array.to_list result
