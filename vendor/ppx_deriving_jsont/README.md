# 🚧🚧 Work in progress

This a very early take on writing a deriver for
[Jsont](https://erratique.ch/software/jsont) that only support a very restricted
subset of features. It can already be used to skip tedious mechanical work like
describing large records and lists of variants, but it does not do justice to
Jsont's fine control and flexibily over the resulting mappings (like choosing
the way integers are mapped).

Any kind of contribution (bug-report, PR, suggestions) is welcomed! It's missing
a lot of features and I am in no way a PPX expert, so there might be a lot of
non-idiomatic things here that I'd be happy to improve.

## Todo / Roadmap / Wishlist

- [x] Basic support for variants without type parameters
- [x] Basic support for records-as-objects
- [ ] Use the quoter Luke
- [ ] Support for all base types
- [ ] Options (in the form of attributes)
    - [ ] for finer support of integers
    - [ ] for finer settings
    - [ ] to provide `doc` comments
    - [ ] for other kinds of objects mappings (as sets for example) 
- [ ] Handle some frequent pattern that do not map obvisousy from OCaml to JSON
  like variants with parameters (Yojson uses arrays).
- [ ] A2lso generate objects' Paths 
- [ ] Comprehensive testsuite

# [@@deriving jsont]

`ppx_deriving_jsont` is a deriver that generates
[Jsont](https://erratique.ch/software/jsont) descriptions of OCaml values. Jsont
allows for a lot of flexibility and precision when writing mappings between
OCaml values and JSON: this PPX does purpots to be a completely automatic
replacement for manual bindings but rather a tool to help generate tedious parts
of the bindings that can be mix-and-matched with carefully user-written
descriptions when that is necessary.

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
 (libraries  jsont ...)
 (preprocess (pps ppx_deriving_jsont)))
```

## Usage

Generation can be tuned with the use of attributes like `[@key "Key"]` that are
often compatible with other derivers such as
[`ppx_yojon_conv`](https://github.com/janestreet/ppx_yojson_conv). These can
also be prefixed `[@jsont.key ...]` when that compatibility isn't desired.

The deriver follows the usual naming conventions. Types whose name is `t`
generates a value named `jsont`. Otherwise that value bears the name of the type
suffixed by `_jsont`. 

### Enumerations

⚠️ Only constructors without type parameters are allowed.

#### Attributes
- `@key <string>` specifies the JSON name (otherwise the same as the
  constructor itself)

#### Example

```ocaml
type sort = A | X [@key "B"]  | C [@@deriving jsont]
``` 

Will generate:

```ocaml
let sort_jsont = Jsont.enum ~kind:"Sort" [ ("A", A); ("B", X); ("C", C) ]
``` 


### Records

Records are mapped using the  ["objects-as-records"
technique](https://erratique.ch/software/jsont/doc/cookbook.html#objects_as_records).

#### Attributes
- `@key <string>` specifies the JSON key (otherwise the same as the
  field)
- `@absent <expr>` / `@default <expr>` specifies the value to use when decoding
  if the field is absent (see [the cookbook](https://erratique.ch/software/jsont/doc/cookbook.html#optional_members))
- `@omit <expr: unit -> bool>` specifies when a value should be ommitted during encoding  [the cookbook](https://erratique.ch/software/jsont/doc/cookbook.html#optional_members)
- `@option` is a shorcut for `@absent None`  and `@omit Option.is_none`

#### Example

```ocaml
type t = {
    name: string;
    maybe_name: string option; [@option]
    ids : string list; [@default []] [@omit List.is_empty] 
    sort : sort; [@key "Sort"]
}
[@@deriving jsont]
``` 

Will generate:

```ocaml
let jsont =
  Jsont.Object.finish
    (Jsont.Object.mem "Sort" sort_jsont
      ~enc:(fun t -> t.sort)
      ?dec_absent:None ?enc_omit:None
      (Jsont.Object.mem "ids" (Jsont.list Jsont.string)
        ~enc:(fun t -> t.ids)
        ?dec_absent:(Some []) ?enc_omit:(Some List.is_empty)
        (Jsont.Object.mem "maybe_name"
          (Jsont.option Jsont.string)
          ~enc:(fun t -> t.maybe_name)
          ?dec_absent:(Some None) ?enc_omit:(Some Option.is_none)
          (Jsont.Object.mem "name" Jsont.string
            ~enc:(fun t -> t.name)
            ?dec_absent:None ?enc_omit:None
            (Jsont.Object.map ~kind:"T" 
              fun name maybe_name ids sort -> { name; maybe_name; ids; sort })))))
``` 
