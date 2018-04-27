open Angstrom

exception Parse_error

type weekday = [ `Friday | `Monday | `Saturday | `Sunday | `Thursday | `Tuesday | `Wednesday ]

let pp_weekday fmt wd =
  Fmt.string fmt @@ match wd with
  | `Friday -> "friday"
  | `Monday -> "monday"
  | `Saturday -> "saturday"
  | `Sunday -> "sunday"
  | `Thursday -> "thursday"
  | `Tuesday -> "tuesday"
  | `Wednesday -> "wednesday"

type recur = [
  | `Byminute of int list
  | `Byday of (char * int * weekday) list
  | `Byhour of int list
  | `Bymonth of (char * int) list
  | `Bymonthday of (char * int) list
  | `Bysecond of int list
  | `Bysetposday of char * int
  | `Byweek of (char * int) list
  | `Byyearday of (char * int) list
  | `Count of int
  | `Frequency of [ `Daily | `Hourly | `Minutely | `Monthly | `Secondly | `Weekly | `Yearly ]
  | `Interval of int
  | `Until of Ptime.t * bool
  | `Weekday of weekday
]

let pp_recur fmt =
  let pp_list pp_e fmt xs = Fmt.(list ~sep:(unit ",@ ") pp_e) fmt xs
  and pp_triple pp_a pp_b pp_c fmt (a, b, c) =
    Fmt.pf fmt "%a, %a, %a" pp_a a pp_b b pp_c c
  and pp_frequency fmt f =
    Fmt.string fmt @@ match f with
    | `Daily -> "daily"
    | `Hourly -> "hourly"
    | `Minutely -> "minutely"
    | `Monthly -> "monthly"
    | `Secondly -> "secondly"
    | `Weekly -> "weekly"
    | `Yearly -> "yearly"
  in
  function
  | `Byminute ms -> Fmt.pf fmt "byminute %a" (pp_list Fmt.int) ms
  | `Byday days -> Fmt.pf fmt "byday %a" (pp_list (pp_triple Fmt.char Fmt.int pp_weekday)) days
  | `Byhour hours -> Fmt.pf fmt "byhour %a" (pp_list Fmt.int) hours
  | `Bymonth months -> Fmt.pf fmt "bymonth %a" (pp_list Fmt.(pair ~sep:(unit ", ") char int)) months
  | `Bymonthday monthdays -> Fmt.pf fmt "bymonthday %a" (pp_list Fmt.(pair ~sep:(unit ", ") char int)) monthdays
  | `Bysecond seconds -> Fmt.pf fmt "bysecond %a" (pp_list Fmt.int) seconds
  | `Bysetposday (s, i) -> Fmt.pf fmt "bysetposday %a %d" Fmt.char s i
  | `Byweek weeks -> Fmt.pf fmt "byweek %a" (pp_list Fmt.(pair ~sep:(unit ", ") char int)) weeks
  | `Byyearday days -> Fmt.pf fmt "byyearday %a" (pp_list Fmt.(pair ~sep:(unit ", ") char int)) days
  | `Count n -> Fmt.pf fmt "count %d" n
  | `Frequency f -> Fmt.pf fmt "frequency %a" pp_frequency f
  | `Interval i -> Fmt.pf fmt "interval %d" i
  | `Until (ts, utc) -> Fmt.pf fmt "until %a UTC? %b" Ptime.pp ts utc
  | `Weekday wd -> Fmt.pf fmt "weekday %a" pp_weekday wd

(* value data structures *)
type value = [
  | `Text of string
  | `Boolean of bool
  | `Binary of Cstruct.t
  | `Caladdress of Uri.t
  | `Date of Ptime.date list
  | `Datetime of Ptime.t * bool
  | `Duration of int
  | `Float of float
  | `Integer of int
  | `Period of Ptime.t * Ptime.t * bool
  | `Recur of recur list
]




let pp_value fmt = 
  let pp_date fmt (y, m, d) = Fmt.pf fmt "%04d-%02d-%02d" y m d in
  function
  | `Text str -> Fmt.pf fmt "text %s" str
  | `Boolean b -> Fmt.pf fmt "boolean %b" b
  | `Binary cs -> Fmt.pf fmt "binary %a" Cstruct.hexdump_pp cs
  | `Caladdress uri -> Fmt.pf fmt "caladdress %a" Uri.pp_hum uri
  | `Date date -> Fmt.pf fmt "date %a" Fmt.(list ~sep:(unit ",@ ") pp_date) date
  | `Datetime (date, utc) -> Fmt.pf fmt "datetime %a UTC? %b" Ptime.pp date utc
  | `Duration d -> Fmt.pf fmt "duration %d in seconds" d
  | `Float f -> Fmt.pf fmt "float %.10f" f
  | `Integer i -> Fmt.pf fmt "integer %d" i
  | `Period (s, e, u) -> Fmt.pf fmt "period %a - %a UTC? %b" Ptime.pp s Ptime.pp e u
  | `Recur recurs -> Fmt.pf fmt "recur %a" Fmt.(list ~sep:(unit "; ") pp_recur) recurs

let binary encoding str =
  let cs = Cstruct.of_string str in
  match encoding with
  | `Eightbit -> `Binary cs
  | `Base64 ->
    match Nocrypto.Base64.decode cs with
    | None -> raise Parse_error
    | Some cs -> `Binary cs

let raw_boolean = function
  | "TRUE" -> true
  | "FALSE" -> false
  | _ -> raise Parse_error

let boolean str = `Boolean (raw_boolean str)

let raw_caladdress = Uri.of_string

let caladdress str = `Caladdress (raw_caladdress str)

let ensure f x = try return (f x) with Failure _ -> fail "parse error"
let in_range min max v = if min <= v && v <= max then return v else fail "parse error"

let date_parser =
  let year = take 4 >>= ensure int_of_string
  and month = take 2 >>= ensure int_of_string >>= in_range 1 12
  and day = take 2 >>= ensure int_of_string >>= in_range 1 31
  and to_ptime_date y m d = (y, m, d) 
  in
  lift3 to_ptime_date year month day

(* TODO need calendar library to discover leap years *)
let date str = 
  match parse_string (sep_by1 (char ',') date_parser <* end_of_input) str with
  | Ok ds -> `Date ds
  | Error _ -> raise Parse_error

let time_parser =
  let hours = take 2 >>= ensure int_of_string >>= in_range 0 23
  and minutes = take 2 >>= ensure int_of_string >>= in_range 0 59
  and seconds = take 2 >>= ensure int_of_string >>= in_range 0 60
  and utc = option ' ' (char 'Z')
  in
  lift4 (fun h m s u -> ((h, m, s), u = 'Z'))
            hours minutes seconds utc

let datetime_parser =
  let ptime d (t, utc) = match Ptime.of_date_time (d, (t, 0)) with
  | Some p -> p, utc
  | None -> raise Parse_error in
  lift2 ptime date_parser (char 'T' *> time_parser) 
 
let datetime str =
  match parse_string (datetime_parser <* end_of_input) str with
  | Ok d -> `Datetime d
  | Error _ -> raise Parse_error

let digits = 
  let is_digit = function '0' .. '9' -> true | _ -> false in
  take_while1 is_digit

let sign = option '+' (char '+' <|> char '-')

let duration_parser =
  let to_seconds p factor = p >>= ensure int_of_string >>| ( * ) factor in
  let second = to_seconds (digits <* char 'S') 1 in
  let minute = lift2 (+) (to_seconds (digits <* char 'M') 60) (option 0 second) in
  let hour = lift2 (+) (to_seconds (digits <* char 'H') 3600) (option 0 minute) in
  let time = char 'T' *> (hour <|> minute <|> second)
  and day = to_seconds (digits <* char 'D') (24 * 3600) in
  let date = lift2 (+) day (option 0 time) 
  and week = to_seconds (digits <* char 'W') (7 * 24 * 3600)
  and apply_sign s n = if s = '+' then n else (- n) in
  lift2 apply_sign (sign <* char 'P') (date <|> time <|> week)

let duration str =
  match parse_string (duration_parser <* end_of_input) str with
  | Ok d -> `Duration d
  | Error _ -> raise Parse_error

let float str =
  let make_float s i f = 
    let n = try float_of_string (i ^ "." ^ f) with Failure _ -> raise Parse_error in
    if s = '+' then n else (-. n) in
  let float = lift3 make_float sign digits (option "" ((char '.') *> digits)) <* end_of_input
  in
  match parse_string float str with
  | Ok f -> `Float f
  | Error _ -> raise Parse_error

let signed_integer str =
  let apply_sign s n = if s = '+' then n else (- n) in
  let int = lift2 apply_sign sign (digits >>= ensure int_of_string >>= in_range (-2147483648) 2147483647) <* end_of_input
  in
  match parse_string int str with
  | Ok i -> `Integer i
  | Error _ -> raise Parse_error

let period str =
  let to_explicit (dt, utc) dur = match Ptime.add_span dt (Ptime.Span.of_int_s dur) with
  | Some t -> (dt, t, utc)
  | None -> raise Parse_error in
  let to_period (tstart, utc) (tend, utc') = if utc = utc' then (tstart, tend, utc) else raise Parse_error in
  let explicit = lift2 to_period datetime_parser (char '/' *> datetime_parser)
  and start = lift2 to_explicit datetime_parser (char '/' *> duration_parser) in
  let period = explicit <|> start <* end_of_input in
  match parse_string period str with
  | Ok p -> `Period p
  | Error _ -> raise Parse_error

let recur str =
  let up_to_two_digits = (take 2 >>= ensure int_of_string) <|> (take 1 >>= ensure int_of_string) in
  let up_to_three_digits = (take 3 >>= ensure int_of_string) <|> up_to_two_digits in
  let freq = ( string "SECONDLY" >>| fun _ -> `Secondly )
         <|> ( string "MINUTELY" >>| fun _ -> `Minutely )
         <|> ( string "HOURLY"   >>| fun _ -> `Hourly )
         <|> ( string "DAILY"    >>| fun _ -> `Daily )
         <|> ( string "WEEKLY"   >>| fun _ -> `Weekly )
         <|> ( string "MONTHLY"  >>| fun _ -> `Monthly )
         <|> ( string "YEARLY"   >>| fun _ -> `Yearly )
  and weekday = ( string "SU" >>| fun _ -> `Sunday ) 
            <|> ( string "MO" >>| fun _ -> `Monday ) 
            <|> ( string "TU" >>| fun _ -> `Tuesday )
            <|> ( string "WE" >>| fun _ -> `Wednesday )
            <|> ( string "TH" >>| fun _ -> `Thursday )
            <|> ( string "FR" >>| fun _ -> `Friday )
            <|> ( string "SA" >>| fun _ -> `Saturday ) in
  let triple a b c = (a, b, c) in
  let weekdaynum = lift3 triple sign (option 0 (up_to_two_digits >>= in_range 1 53) ) weekday in
  let pair a b = (a, b) in
  let monthdaynum = lift2 pair sign (up_to_two_digits >>= in_range 1 31) 
  and yeardaynum = lift2 pair sign (up_to_three_digits >>= in_range 1 366)
  and weeknum = lift2 pair sign (up_to_two_digits >>= in_range 1 53)
  and monthnum = lift2 pair sign (up_to_two_digits >>= in_range 1 12)
  and ptime = date_parser >>= fun d -> match Ptime.of_date d with None -> fail "Parse_error" | Some x -> return (x, true) in
  let recur_rule_part = 
       ( string "FREQ=" *> freq >>| fun f -> `Frequency f )
   <|> ( string "UNTIL=" *> (datetime_parser <|> ptime) >>| fun u -> `Until u )
   <|> ( string "COUNT=" *> digits >>= ensure int_of_string >>| fun c -> `Count c ) 
   <|> ( string "INTERVAL=" *> digits >>= ensure int_of_string >>| fun i -> `Interval i )
   <|> ( string "BYSECOND=" *> (sep_by1 (char ',') (up_to_two_digits >>= in_range 0 60)) >>| fun s -> `Bysecond s )
   <|> ( string "BYMINUTE=" *> (sep_by1 (char ',') (up_to_two_digits >>= in_range 0 59)) >>| fun m -> `Byminute m )
   <|> ( string "BYHOUR=" *> (sep_by1 (char ',') (up_to_two_digits >>= in_range 0 23)) >>| fun h -> `Byhour h )
   <|> ( string "BYDAY=" *> (sep_by1 (char ',') weekdaynum) >>| fun d -> `Byday d )
   <|> ( string "BYMONTHDAY=" *> (sep_by1 (char ',') monthdaynum) >>| fun d -> `Bymonthday d )
   <|> ( string "BYYEARDAY=" *> (sep_by1 (char ',') yeardaynum) >>| fun d -> `Byyearday d )
   <|> ( string "BYWEEKNO=" *> (sep_by1 (char ',') weeknum) >>| fun w -> `Byweek w )
   <|> ( string "BYMONTH=" *> (sep_by1 (char ',') monthnum) >>| fun m -> `Bymonth m )
   <|> ( string "BYSETPOS=" *> yeardaynum >>| fun d -> `Bysetposday d )
   <|> ( string "WKST=" *> weekday >>| fun d -> `Weekday d ) in
  let recur = sep_by1 (char ';') recur_rule_part in
  match parse_string (recur <* end_of_input) str with
  | Ok p -> `Recur p
  | Error _ -> raise Parse_error

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

  let cutype = function
    | "INDIVIDUAL" ->`Individual
    | "GROUP" -> `Group
    | "RESOURCE" -> `Resource
    | "ROOM" -> `Room
    | "UNKNOWN" -> `Unknown
    | x -> parse_other x

  and encoding = function
    | "8BIT" -> `Eightbit
    | "BASE64" -> `Base64
    | _ -> raise Parse_error

  and fbtype = function
    | "FREE" -> `Free
    | "BUSY" -> `Busy
    | "BUSY-UNAVAILABLE" -> `Busyunavailable
    | "BUSY-TENTATIVE" -> `Busytentative
    | x -> parse_other x

  and partstat = function
    | "NEEDS-ACTION" -> `Needsaction
    | "ACCEPTED" -> `Accepted
    | "DECLINED" -> `Declined
    | "TENTATIVE" -> `Tentative
    | "DELEGATED" -> `Delegated
    | "COMPLETED" -> `Completed
    | "IN-PROCESS" -> `Inprocess
    | x -> parse_other x

  and reltype = function
    | "PARENT" -> `Parent
    | "CHILD" -> `Child
    | "SIBLING" -> `Sibling
    | x -> parse_other x

  and role = function
    | "CHAIR" -> `Chair
    | "REQ-PARTICIPANT" -> `Reqparticipant
    | "OPT-PARTICIPANT" -> `Optparticipant
    | "NON-PARTICIPANT" -> `Nonparticipant
    | x -> parse_other x

  and valtype = function
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

  and trigrel = function
    | "START" -> `Start
    | "END" -> `End
    | _ -> raise Parse_error
  in

  match key, value with
  | "ALTREP", [ value ] -> `Altrep (raw_caladdress value)
  | "CN", [ value ] -> `Cn value
  | "CUTYPE", [ value ] -> `Cutype (cutype value)
  | "DELEGATED-FROM", values -> `Delfrom (List.map raw_caladdress values)
  | "DELEGATED-TO", values -> `Delto (List.map raw_caladdress values)
  | "DIR", [ value ] -> `Dir (raw_caladdress value)
  | "ENCODING", [ value ] -> `Encoding (encoding value)
  | "FMTTYPE", [ value ] ->
    begin match Astring.String.cut ~sep:"/" value with
      | Some (typ, subtyp) -> `Fmttype (typ, subtyp)
      | None -> raise Parse_error
    end
  | "FBTYPE", [ value ] -> `Fbtype (fbtype value)
  | "LANGUAGE", [ value ] -> `Language value
  | "MEMBER", values -> `Member (List.map raw_caladdress values)
  | "PARTSTAT", [ value ] -> `Partstat (partstat value)
  | "RANGE", [ "THISANDFUTURE" ] -> `Range
  | "RELATED", [ value ] -> `Trigrel (trigrel value)
  | "RELTYPE", [ value ] -> `Reltype (reltype value)
  | "ROLE", [ value ] -> `Role (role value)
  | "RSVP", [ value ] -> `Rsvp (raw_boolean value)
  | "SENT-BY", [ value ] -> `Sentby (raw_caladdress value)
  | "TZID", [ value ] ->
    let prefix, string =
      match Astring.String.cut ~sep:"/" value with
      | Some (x, y) when x = "" -> true, y
      | _ -> false, value
    in
    `Tzid (prefix, string)
  | "VALUE", [ value ] -> `Valuetype (valtype value)
  | _ -> raise Parse_error

(* value type dependent parsers *)

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
    | `Text -> `Text value
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
    | `Boolean -> boolean value
    | `Caladdress -> caladdress value
    | `Date -> date value
    | `Datetime -> datetime value
    | `Duration -> duration value
    | `Float -> float value
    | `Integer -> signed_integer value
    | `Period -> period value
    | `Recur -> recur value
  in
  (key, params, v)

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

let value_list = sep_by1 (char ',') param_value

let param = lift2 collect_param param_name (char '=' *> value_list)

let value = take_while (fun x -> not (is_control x)) (* in fact it is more complicated *)

let contentline = lift3 collect_contentline name (many ((char ';') *> param)) (char ':' *> value <* end_of_line)
let contentlines = many contentline <* end_of_input

(* processing *)

let normalize_lines s =
  let re = Re.compile ( Re.Perl.re ~opts:[`Multiline] "(\n|\r\n)^\\s" ) in
  Re.replace_string ~all:true re ~by:"" s

let parse (str:string) =
  try parse_string contentlines (normalize_lines str)
  with Parse_error -> Error "parse error"
