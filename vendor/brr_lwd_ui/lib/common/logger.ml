open Import
open Brr

type section = ..
type active = All | Only of section list
type levels = Log | Error | Info | Debug

let logger = function
  | Log -> Console.log
  | Error -> Console.error
  | Info -> Console.info
  | Debug -> Console.debug

type t = {
  log : Console.log;
  error : Console.log;
  info : Console.log;
  debug : Console.log;
}

let for_section s =
  let log (l : Console.log) ?(fmt = "") msg =
    let fmt = Printf.sprintf "[%s] %s" s fmt in
    l (fmt :: msg)
  in
  {
    log = log Console.log;
    error = log Console.error;
    info = log Console.info;
    debug = log Console.debug;
  }
