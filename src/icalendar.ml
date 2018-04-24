open Angstrom
open Rresult.R.Infix

(* param data structure *)
type other = [
  | `Xname of string
  | `Ianatoken of string
]

type cutype = [ `Individual | `Group | `Resource | `Room | `Unknown | other ]
type encoding = [`Eightbit | `Base64 ]
type typename = string
type subtypename = string
type fbtype = [ `Free | `Busy | `Busyunavailable | `Busytentative | other ]
type partstat = [ `Needsaction | `Accepted | `Declined | `Tentative | `Delegated | `Completed | `Inprocess | other ]
type trigrel = [ `Start | `End ]
type reltype = [ `Parent | `Child | `Sibling | other ]
type role = [ `Chair | `Reqparticipant | `Optparticipant | `Nonparticipant | other ]
type caladdress = Uri.t
type languagetag = string
type valuetype = [ `Binary | `Boolean | `Caladdress | `Date | `Datetime | `Duration
 | `Float | `Integer | `Period | `Recur | `Text | `Time | `Uri | `Utcoffset | other ]

type icalparameter = [`Altrep of Uri.t | `Cn of string | `Cutype of cutype
 | `Delfrom of caladdress list | `Delto of caladdress list | `Dir of Uri.t
 | `Encoding of encoding | `Fmttype of typename * subtypename | `Fbtype of fbtype
 | `Language of languagetag | `Member of caladdress list | `Partstat of partstat
 | `Range | `Trigrel of trigrel | `Reltype of reltype | `Role of role | `Rsvp of bool
 | `Sentby of caladdress | `Tzid of bool * string | `Valuetype of valuetype | `Other ]

let pp_other fmt = function
  | `Xname xname -> Fmt.pf fmt "xname %s" xname
  | `Ianatoken token -> Fmt.pf fmt "ianatoken %s" token

let pp_cutype fmt = function
  | `Individual -> Fmt.string fmt "individual"
  | `Group -> Fmt.string fmt "group"
  | `Resource -> Fmt.string fmt "resource"
  | `Room -> Fmt.string fmt "room"
  | `Unknown -> Fmt.string fmt "unknown"
  | #other as e -> pp_other fmt e

let pp_caladdress = Uri.pp_hum

let pp_encoding fmt e =
  Fmt.string fmt @@
  match e with
  | `Eightbit -> "8bit"
  | `Base64 -> "base64"

let pp_fbtype fmt = function
  | `Free -> Fmt.string fmt "free"
  | `Busy -> Fmt.string fmt "busy"
  | `Busyunavailable -> Fmt.string fmt "busy-unavailable"
  | `Busytentative -> Fmt.string fmt "busy-tentative"
  | #other as e -> pp_other fmt e

let pp_partstat fmt = function
  | `Needsaction -> Fmt.string fmt "needs action"
  | `Accepted -> Fmt.string fmt "accepted"
  | `Declined -> Fmt.string fmt "declined"
  | `Tentative -> Fmt.string fmt "tentative"
  | `Delegated -> Fmt.string fmt "delegated"
  | `Completed -> Fmt.string fmt "completed"
  | `Inprocess -> Fmt.string fmt "in-process"
  | #other as e -> pp_other fmt e

let pp_trigrel fmt trigrel =
  Fmt.string fmt @@
  match trigrel with
  | `Start -> "start"
  | `End -> "end"

let pp_reltype fmt = function
  | `Parent -> Fmt.string fmt "parent"
  | `Child -> Fmt.string fmt "child"
  | `Sibling -> Fmt.string fmt "sibling"
  | #other as e -> pp_other fmt e

let pp_role fmt = function
  | `Chair -> Fmt.string fmt "chair"
  | `Reqparticipant -> Fmt.string fmt "required participant"
  | `Optparticipant -> Fmt.string fmt "optional participant"
  | `Nonparticipant -> Fmt.string fmt "non participant"
  | #other as e -> pp_other fmt e

let pp_valuetype fmt = function
  | `Binary -> Fmt.string fmt "binary"
  | `Boolean -> Fmt.string fmt "boolean"
  | `Caladdress -> Fmt.string fmt "calendar address"
  | `Date -> Fmt.string fmt "date"
  | `Datetime -> Fmt.string fmt "datetime"
  | `Duration -> Fmt.string fmt "duration"
  | `Float -> Fmt.string fmt "float"
  | `Integer -> Fmt.string fmt "integer"
  | `Period -> Fmt.string fmt "period"
  | `Recur -> Fmt.string fmt "recurrence"
  | `Text -> Fmt.string fmt "text"
  | `Time -> Fmt.string fmt "time"
  | `Uri -> Fmt.string fmt "uri"
  | `Utcoffset -> Fmt.string fmt "utc offset"
  | #other as e -> pp_other fmt e

let pp_icalparameter fmt = function
  | `Altrep uri -> Fmt.pf fmt "alternative text representation %a" Uri.pp_hum uri
  | `Cn name -> Fmt.pf fmt "common name %s" name
  | `Cutype typ -> Fmt.pf fmt "calendar user type %a" pp_cutype typ
  | `Delfrom addresses -> Fmt.pf fmt "delegation from %a" Fmt.(list ~sep:(unit ",@ ") pp_caladdress) addresses
  | `Delto addresses -> Fmt.pf fmt "delegation to %a" Fmt.(list ~sep:(unit ",@ ") pp_caladdress) addresses
  | `Dir uri -> Fmt.pf fmt "directory %a" Uri.pp_hum uri
  | `Encoding encoding -> Fmt.pf fmt "encoding %a" pp_encoding encoding
  | `Fmttype (typ, subtyp) -> Fmt.pf fmt "format type %s/%s" typ subtyp
  | `Fbtype fbtype -> Fmt.pf fmt "free-busy type: %a" pp_fbtype fbtype
  | `Language languagetag -> Fmt.pf fmt "language %s" languagetag
  | `Member addresses -> Fmt.pf fmt "members %a" Fmt.(list ~sep:(unit ",@ ") pp_caladdress) addresses
  | `Partstat partstat -> Fmt.pf fmt "participation status %a" pp_partstat partstat
  | `Range -> Fmt.string fmt "recurrence identifier range"
  | `Trigrel trigrel -> Fmt.pf fmt "alarm trigger relationship %a" pp_trigrel trigrel
  | `Reltype reltype -> Fmt.pf fmt "relationship status %a" pp_reltype reltype
  | `Role role -> Fmt.pf fmt "participation role %a" pp_role role
  | `Rsvp yes -> Fmt.pf fmt "rsvp %b" yes
  | `Sentby address -> Fmt.pf fmt "sent by %a" pp_caladdress address
  | `Tzid (prefix, paramtext) -> Fmt.pf fmt "time zone identifier (prefix %b) %s" prefix paramtext
  | `Valuetype valuetype -> Fmt.pf fmt "value type %a" pp_valuetype valuetype
  | `Other -> Fmt.string fmt "other"

let parse_other str =
  if Astring.String.is_prefix ~affix:"X-" str then
    `Xname str
  else
    `Ianatoken str

(* base data structure *)
let collect_param key value =
  match key, value with
  | "ALTREP", [ value ] -> `Altrep (Uri.of_string value)
  | "CN", [ value ] -> `Cn value
  | "CUTYPE", [ x ] ->
    let cutype = match x with
      | "INDIVIDUAL" ->`Individual
      | "GROUP" -> `Group
      | "RESOURCE" -> `Resource
      | "ROOM" -> `Room
      | "UNKNOWN" -> `Unknown
      | x -> parse_other x
    in
    `Cutype cutype
  | "DELEGATED-FROM", values -> `Delfrom (List.map Uri.of_string values)
  | "DELEGATED-TO", values -> `Delto (List.map Uri.of_string values)
  | "DIR", [ value ] -> `Dir (Uri.of_string value)
  | "ENCODING", [ value ] ->
    let enc = match value with
      | "8BIT" -> `Eightbit
      | "BASE64" -> `Base64
      | _ -> assert false
    in
    `Encoding enc
  | "FMTTYPE", [ value ] ->
    begin match Astring.String.cut ~sep:"/" value with
      | Some (typ, subtyp) -> `Fmttype (typ, subtyp)
      | None -> assert false
    end
  | "FBTYPE", [ value ] ->
    let typ = match value with
      | "FREE" -> `Free
      | "BUSY" -> `Busy
      | "BUSY-UNAVAILABLE" -> `Busyunavailable
      | "BUSY-TENTATIVE" -> `Busytentative
      | x -> parse_other x
    in
    `Fbtype typ
  | "LANGUAGE", [ value ] -> `Language value
  | "MEMBER", values -> `Member (List.map Uri.of_string values)
  | "PARTSTAT", [ value ] ->
    let stat = match value with
      | "NEEDS-ACTION" -> `Needsaction
      | "ACCEPTED" -> `Accepted
      | "DECLINED" -> `Declined
      | "TENTATIVE" -> `Tentative
      | "DELEGATED" -> `Delegated
      | "COMPLETED" -> `Completed
      | "IN-PROCESS" -> `Inprocess
      | x -> parse_other x
    in
    `Partstat stat
  | "RANGE", [ "THISANDFUTURE" ] -> `Range
  | "RELATED", [ value ] ->
    `Trigrel (match value with "START" -> `Start | "END" -> `End)
  | "RELTYPE", [ value ] ->
    let reltyp = match value with
      | "PARENT" -> `Parent
      | "CHILD" -> `Child
      | "SIBLING" -> `Sibling
      | x -> parse_other x
    in
    `Reltype reltyp
  | "ROLE", [ value ] ->
    let role = match value with
      | "CHAIR" -> `Chair
      | "REQ-PARTICIPANT" -> `Reqparticipant
      | "OPT-PARTICIPANT" -> `Optparticipant
      | "NON-PARTICIPANT" -> `Nonparticipant
      | x -> parse_other x
    in
    `Role role
  | "RSVP", [ value ] ->
    `Rsvp (match value with "TRUE" -> true | "FALSE" -> false | _ -> assert false)
  | "SENT-BY", [ value ] -> `Sentby (Uri.of_string value)
  | "TZID", [ value ] ->
    let prefix, string =
      match Astring.String.cut ~sep:"/" value with
      | Some (x, y) when x = "" -> true, y
      | _ -> false, value
    in
    `Tzid (prefix, string)
  | "VALUE", [ value ] ->
    let valtyp = match value with
      | "BINARY" -> `Binary
      | "BOOLEAN" -> `Boolean
      | "CAL-ADDRESS" -> `Caladdress
      | "DATE" -> `Date
      | "DATE-TIME" -> `Datetime
      | "DURATION" -> `Duration
      | "FLOAT" -> `Float
      | "INTEGER" -> `Integer
      | "PERIOD" -> `Period
      | "RECUR" -> `Recur
      | "TEXT" -> `Text
      | "TIME" -> `Time
      | "URI" -> `Uri
      | "UTC-OFFSET" -> `Utcoffset
      | x -> parse_other x
    in
    `Valuetype valtyp
  | _ -> assert false

(* value type dependent parsers *)
type value = [
  | `Text of string
  | `Binary of Cstruct.t
]

let pp_value fmt = function
  | `Text str -> Fmt.pf fmt "text %s" str
  | `Binary cs -> Fmt.pf fmt "binary %a" Cstruct.hexdump_pp cs

let binary encoding str =
  match encoding with
  | `Eightbit -> Ok (`Binary (Cstruct.of_string str))
  | `Base64 ->
    match Nocrypto.Base64.decode (Cstruct.of_string str) with
    | None -> Error "invalid base64 encoded input"
    | Some cs -> Ok (`Binary cs)

let collect_contentline key (params : icalparameter list) value =
  let t =
    let is_valuetype = function
      | `Valuetype _ -> true | _ -> false
    in
    match
      try Some (List.find is_valuetype params) with Not_found -> None
    with
    | Some (`Valuetype t) -> t
    | _ -> `Text
  in
  let v = match t with
    | `Text -> Ok (`Text value)
    | `Date -> Ok (`Text value)
    | `Binary ->
      let encoding =
        let is_encoding = function
          | `Encoding _ -> true | _ -> false
        in
        match
          try Some (List.find is_encoding params) with Not_found -> None
        with
        | Some (`Encoding e) -> e
        | _ -> `Eightbit
      in
      binary encoding value
  in
  match v with
  | Ok v -> Ok (key, params, v)
  | Error e -> Error e

let collect_contentlines parse_lines =
  List.fold_right (fun line lines ->
      lines >>= fun ls ->
      line >>= fun l ->
      Ok (l :: ls))
    parse_lines (Ok [])

(* base grammar *)
let is_alpha_digit_minus = function | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '-' -> true | _ -> false
let name = take_while1 is_alpha_digit_minus
let param_name = name
let is_control = function '\x00' .. '\x08' | '\x0a' .. '\x1f' | '\x7f' -> true | _ -> false
let is_qsafe_char = function x when is_control x -> false | '"' -> false | _ -> true
let quoted_string = char '"' *> take_while1 is_qsafe_char <* char '"'
let is_safe_char = function x when is_control x -> false | '"' | ';' | ':' | ',' -> false | _ -> true
let param_text = take_while1 is_safe_char
let param_value = param_text <|> quoted_string (* in contrast to rfc we require at least 1 char for param_value *)

let value_list = lift2 List.cons param_value (many (char ',' *> param_value))

let param = lift2 collect_param param_name (char '=' *> value_list)

let value = take_while (fun x -> not (is_control x)) (* in fact it is more complicated *)

let contentline = lift3 collect_contentline name (many ( char ';' *> param )) (char ':' *> value <* end_of_line)
let contentlines = many contentline <* end_of_input

(* processing *)

let normalize_lines s =
  let re = Re.compile ( Re.Perl.re ~opts:[`Multiline] "(\n|\r\n)^\\s" ) in
  Re.replace_string ~all:true re ~by:"" s

let parse (str:string) =
  parse_string contentlines (normalize_lines str) >>= fun lines ->
  collect_contentlines lines
