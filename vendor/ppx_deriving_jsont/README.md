# [@@deriving jsont]

`ppx_deriving_jsont` is a [PPX deriver](https://ocaml-ppx.github.io/ppxlib/ppxlib/driver.html#def_derivers) that generates
[Jsont](https://erratique.ch/software/jsont) descriptions of OCaml types. Jsont
allows for a lot more flexibility and precision when writing mappings between
OCaml values and JSON. This PPX does not purposes to be a completely automatic
replacement for manual bindings but rather a tool to help generate tedious parts
of the bindings that can be mix-and-matched with carefully user-written
descriptions when that is necessary.

## 🚧🚧 Work in progress

This an early take on writing a deriver for
[Jsont](https://erratique.ch/software/jsont). It can already be used to skip
tedious mechanical work like describing large records and lists of variants, but
it does not do justice to Jsont's fine control and flexibily over the resulting
mappings (like choosing the way integers are mapped).

Any kind of contribution (bug-report, PR, suggestions) is welcomed! I am in no
way a PPX expert, so there might be a lot of non-idiomatic things here that I'd
be happy to improve.

## Todo / Roadmap / Wishlist

- [x] Variants without type parameters (enum)
- [x] Variants with one type parameter
- [x] Tuples
- [x] Variants with more than one type parameter (using tuples)
- [x] Inline records
- [x] Records-as-objects
- [x] Types with parameters
- [x] [Recursive types](https://erratique.ch/software/jsont/doc/cookbook.html#recursion)
- [x] Mutually recursive types
- [ ] Support for all meaningful base types
- [ ] Options (in the form of attributes)
    - [x] to pass custom Jsont values
    - [ ] for finer support of integers
    - [ ] for finer settings
    - [x] to provide `doc` comments
    - [ ] for other kinds of objects mappings (as sets for example)
    - [ ] for other kinds of variants mappings (as arrays for example)
- [ ] Also generate objects' Paths (lenses ?)
- [ ] Ensure locations make sense
- [ ] Comprehensive test-suite

## Installation

`ppx_deriving_jsont` is still experimental and has not been released to Opam
yet. Given how incomplete it is right now you might want to vendor it and
eventually contribute your improvements upstream. Alternatively, the development
version can be installed in a switch using Opam's `pin` command:

```shell
opam pin https://github.com/voodoos/ppx_deriving_jsont.git
```

## Configuration

Setup depends of your build system of choice. To enable the deriver in a Dune
library (or executable), one should add a dependency to `jsont` and use the
`ppx_deriving_jsont` preprocessor:

```sexp
(library
 ...
 (libraries jsont ...)
 (preprocess (pps ppx_deriving_jsont)))
```

## Usage

Generation is enabled by adding the `[@@deriving jsont]` attribute to type
declarations.

Generation can be tuned with the use of attributes like `[@key "Key"]` that are
often compatible with other derivers such as
[`ppx_yojson_conv`](https://github.com/janestreet/ppx_yojson_conv). These can
also be prefixed `[@jsont.key ...]` when that compatibility isn't desired.

The deriver follows the usual naming conventions. Types whose name is `t`
generates a value named `jsont`. Otherwise that value bears the name of the type
suffixed by `_jsont`.

### Declaration attributes

All type declarations can be annotated with the `[@@kind "Some kind"]` and
`[@@doc "Some doc"]` attributes to improve error messages. This has no effect
when used on base types.

The `kind` value usually defaults to the name of the type, the `doc` value to
`None`.

### Core types attribute

Users can overide any core type deriving byt providing their own `Jsont.t`
description using the `[@jsont <value>]` attribute.

### Basic types (with parameters)

#### Example

```ocaml
type 'a t_with_param = 'a [@@deriving jsont]

type u = int list t_with_param [@@deriving jsont]
```

<details><summary>See generated code</summary></h3>

```ocaml
let t_with_param_jsont jsont_type_var__a = jsont_type_var__a

let u_jsont = t_with_param_jsont (Jsont.list Jsont.int)
```

</details>

#### Json output:

```ocaml
# Jsont_bytesrw.encode_string u_jsont [3; 6; 4; 2];;
```

```json
[3,6,4,2]
```

### Enumerations

⚠️ Only variants whose constructors have no type parameters are translated as enumerations.

#### Type declaration attributes
- `@@kind <string>` and `@@doc <string>`

#### Constructor attributes
- `@key <string>` specifies the JSON name (otherwise the same as the
  constructor itself)

#### Example

```ocaml
type sort = A | X [@key "B"] | C
  [@@doc "A doc of sorts"] [@@deriving jsont]
```

<details><summary>See generated code</summary>

```ocaml
let sort_jsont =
  Jsont.enum ~doc:"A doc of sorts" ~kind:"Sort"
    [("A", A); ("B", X); ("C", C)]
```

</details>

#### Json output:

```ocaml
# Jsont_bytesrw.encode_string (Jsont.list u_jsont) [ A; X; C ];;
```

```json
["A","B","C"]
```

### Tuples

Tuples are encoded as json arrays.

#### Type declaration attributes
- `@@kind <string>` and `@@doc <string>`

#### Example

```ocaml
type tup = int * string t_with_param [@@doc "Tup doc"] [@@deriving jsont]
```

<details><summary>See generated code</summary>

```ocaml
let tup_jsont =
  let get_or_raise = function
    | Ok r -> r
    | Error err -> raise (Jsont.Error err)
  in
  let enc f acc (e0, e1) =
    let e0 = Jsont.Json.encode' Jsont.int e0 |> get_or_raise in
    let e1 =
      Jsont.Json.encode' (t_with_param_jsont Jsont.string) e1 |> get_or_raise
    in
    [ (0, e0); (1, e1) ] |> List.fold_left (fun acc (i, e) -> f acc i e) acc
  in
  let dec_empty () = (None, None) in
  let dec_add i elt (e0, e1) =
    match i with
    | 0 ->
        let e = Jsont.Json.decode' Jsont.int elt |> get_or_raise in
        (Some e, e1)
    | 1 ->
        let e =
          Jsont.Json.decode' (t_with_param_jsont Jsont.string) elt
          |> get_or_raise
        in
        (e0, Some e)
    | _ -> Jsont.Error.msgf Jsont.Meta.none "Too many elements for tuple."
  in
  let dec_finish meta _ (e0, e1) =
    let get_or_raise i o =
      match o with
      | Some v -> v
      | None -> Jsont.Error.msgf meta "Missing tuple member #%i" i
    in
    (get_or_raise 0 e0, get_or_raise 1 e1)
  in
  Jsont.Array.map ~kind:"Tup" ~doc:"Tup doc" ~enc:{ enc } ~dec_empty ~dec_add
    ~dec_finish Jsont.json
  |> Jsont.Array.array
```

</details>

#### Json output:

```ocaml
# Jsont_bytesrw.encode_string tup_jsont (42, "quarante-deux");;
```

```json
[42,"quarante-deux"]
```

### Records

Records are mapped using the ["objects-as-records"
technique](https://erratique.ch/software/jsont/doc/cookbook.html#objects_as_records).

#### Type declaration attributes
- `@@kind <string>` and `@@doc <string>`

#### Field attributes
- `@key <string>` specifies the JSON key (otherwise the same as the
  field)
- `@doc <string>` to document fields
- `@absent <expr>` / `@default <expr>` specifies the value to use when decoding
  if the field is absent (see [the cookbook](https://erratique.ch/software/jsont/doc/cookbook.html#optional_members))
- `@omit <expr: unit -> bool>` specifies when a value should be ommitted during encoding  [the cookbook](https://erratique.ch/software/jsont/doc/cookbook.html#optional_members)
- `@option` is a shorcut for `@absent None`  and `@omit Option.is_none`

#### Example

```ocaml
type t = {
  name : string; [@doc "Object name"]
  maybe_parent : t option; [@option]
  ids : string list; [@default []] [@omit List.is_empty]
  sort : sort; [@key "Sort"]
}
[@@doc "A t object"] [@@deriving jsont]
```

<details><summary>See generated code</summary>

```ocaml
let jsont =
  let rec jsont =
    lazy
      (let make name maybe_parent ids sort =
         { name; maybe_parent; ids; sort }
       in
       Jsont.Object.map ~doc:"A t object" ~kind:"T2" make
       |> Jsont.Object.mem "name" ~doc:"Object name" Jsont.string ~enc:(fun t ->
           t.name)
       |> Jsont.Object.mem "maybe_parent"
            (Jsont.option (Jsont.rec' jsont))
            ~enc:(fun t -> t.maybe_parent)
            ~dec_absent:None ~enc_omit:Option.is_none
       |> Jsont.Object.mem "ids" (Jsont.list Jsont.string)
            ~enc:(fun t -> t.ids)
            ~dec_absent:[] ~enc_omit:List.is_empty
       |> Jsont.Object.mem "Sort" sort_jsont ~enc:(fun t -> t.sort)
       |> Jsont.Object.finish)
  in
  Lazy.force jsont
```

</details>

#### Json output:

```ocaml
# Jsont_bytesrw.encode_string (Jsont.list u_jsont)
       {
         name = "Alice";
         maybe_parent = Some {
            name = "Bob";
            maybe_parent = None;
            ids = [ "X" ];
            sort = X };
         ids = [];
         sort = A;
       };;
```

```json
{
  "name":"Alice",
  "maybe_parent":
    {"name":"Bob", "ids":["X"], "Sort":"B"},
  "Sort":"A"
}
```

### Variants

Variants are encoded using ["object types" as described in the
cookbook](https://erratique.ch/software/jsont/doc/cookbook.html#cases).

The default `type_key` is `"type"`. Values that are not inlined-records are wrapped with the key `v`.

In the future we plan to also support the more traditional encoding variant as
arrays.

Polymorphic variant behaves similarly.

#### Type declaration attributes
- `@@kind <string>` and `@@doc <string>`
- `@@type_key <string>` specifies the name of the JSON field used to distinguish
  cases. This should not be `v` which is used as a wrapper for constructor
  arguments, or any of the member of an inlined record. Defaults to `type`.
- `@@wrap_key <string>` specifies the name of the JSON field used to wrap values
  other than inlined records or records with the `nowrap` attribute. Defaults to
  `v`.

#### Constructors attributes
- `@key <string>` specifies the JSON name (otherwise the same as the
  constructor itself)
- `@doc <string>` to document constructors
- `@kind <string>` to specify the kind of inlined-records
- `@nowrap` can be used on constructors whose only argument is the type of a record

For inlined record

#### Example

```ocaml
type v =
  | A of int [@key "Id"] [@kind "One of A kind"]
  | S of sort [@doc "Doc for S"]
  | R of { name : string [@doc "Doc for R.name"] }
    [@kind "Kind for R"] [@doc "Doc for R"]
  [@@doc "Doc for v"] [@@type_key "t"] [@@deriving jsont]
```

<details><summary>See generated code</summary>

```ocaml
let v_jsont =
  let jsont__R =
    Jsont.Object.Case.map "R"
      (let make name = R { name } in
       Jsont.Object.map ~doc:"Doc for R" ~kind:"Kind for R" make
       |> Jsont.Object.mem "name" ~doc:"Doc for R.name" Jsont.string
            ~enc:((fun (R t) -> t.name) [@ocaml.warning "-8"])
       |> Jsont.Object.finish)
      ~dec:Fun.id
  and jsont__S =
    Jsont.Object.Case.map "S"
      (Jsont.Object.map ~kind:"S" ~doc:"Doc for S" Fun.id
      |> Jsont.Object.mem "v" ~doc:"Wrapper for S" sort_jsont ~enc:Fun.id
      |> Jsont.Object.finish)
      ~dec:(fun arg -> S arg)
  and jsont__A =
    Jsont.Object.Case.map "Id"
      (Jsont.Object.map ~kind:"One of A kind" Fun.id
      |> Jsont.Object.mem "v" ~doc:"Wrapper for A" Jsont.int ~enc:Fun.id
      |> Jsont.Object.finish)
      ~dec:(fun arg -> A arg)
  in
  Jsont.Object.map ~kind:"V" ~doc:"Doc for v" Fun.id
  |> Jsont.Object.case_mem "t" ~doc:"Cases for V" Jsont.string ~enc:Fun.id
       ~enc_case:(function
         | R t -> Jsont.Object.Case.value jsont__R (R t)
         | S t -> Jsont.Object.Case.value jsont__S t
         | A t -> Jsont.Object.Case.value jsont__A t)
       [
         Jsont.Object.Case.make jsont__R;
         Jsont.Object.Case.make jsont__S;
         Jsont.Object.Case.make jsont__A;
       ]
  |> Jsont.Object.finish
```

</details>

#### Json output:

```ocaml
# Jsont_bytesrw.encode_string (Jsont.list v_jsont) [ S X; A 42 ];;
```

```json
[{"type":"S","v":"B"},{"type":"Id","v":42}]
```
