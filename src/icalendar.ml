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
  | `Text of string list
  | `Time of Ptime.time * bool
  | `Uri of Uri.t
  | `Utcoffset of Ptime.span
]

let pp_date fmt (y, m, d) = Fmt.pf fmt "%04d-%02d-%02d" y m d

let pp_value fmt = 
  function
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
  | `Text lst -> Fmt.pf fmt "text %a" Fmt.(list ~sep:(unit ",@ ") string) lst
  | `Time (((h, m, s), _), utc) -> Fmt.pf fmt "time %02d:%02d:%02d UTC? %b" h m s utc
  | `Uri uri -> Fmt.pf fmt "uri %a" Uri.pp_hum uri
  | `Utcoffset span -> Fmt.pf fmt "utcoffset %a" Ptime.Span.pp span

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

let time str =
  match parse_string (time_parser <* end_of_input) str with
  | Ok (t, utc) -> `Time ((t, 0), utc)
  | Error _ -> raise Parse_error

let datetime_parser =
  let ptime d (t, utc) = match Ptime.of_date_time (d, (t, 0)) with
  | Some p -> p, utc
  | None -> raise Parse_error in
  lift2 ptime date_parser (char 'T' *> time_parser) 
 
let datetime str =
  match parse_string (datetime_parser <* end_of_input) str with
  | Ok d -> `Datetime d
  | Error _ -> raise Parse_error

let is_digit = function '0' .. '9' -> true | _ -> false

let digits = take_while1 is_digit

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

let float_parser =
  let make_float s i f = 
    let n = try float_of_string (i ^ "." ^ f) with Failure _ -> raise Parse_error in
    if s = '+' then n else (-. n) in
  lift3 make_float sign digits (option "" ((char '.') *> digits))
 
let float str =
  match parse_string (float_parser <* end_of_input) str with
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

let recur_parser =
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
  sep_by1 (char ';') recur_rule_part

let recur str =
  match parse_string (recur_parser <* end_of_input) str with
  | Ok p -> `Recur p
  | Error _ -> raise Parse_error

let text_parser =
  let escaped_char =
    (string {_|\\|_} >>| fun _ -> {_|\|_})
    <|> (string "\\;" >>| fun _ -> ";")
    <|> (string "\\," >>| fun _ -> ",")
    <|> (string "\\N" >>| fun _ -> "\n")
    <|> (string "\\n" >>| fun _ -> "\n")
  in
  let is_control = function '\x00' .. '\x08' | '\x0a' .. '\x1f' | '\x7f' -> true | _ -> false in
  let is_tsafe_char =
    function x when is_control x -> false
           | '"' | ';' | ':' | '\\' | ',' -> false
           | _ -> true
  in
  let tsafe_char = take_while1 is_tsafe_char in
  many1 (tsafe_char <|> string ":" <|> string "\"" <|> escaped_char) >>| Astring.String.concat ~sep:""

let texts_parser = sep_by (char ',') text_parser

let text str =
  match parse_string (texts_parser <* end_of_input) str with
  | Ok p -> `Text p
  | Error _ -> raise Parse_error

let uri str = `Uri (Uri.of_string str)

let utcoffset str =
  let sign = char '+' <|> char '-'
  and hours = take 2 >>= ensure int_of_string >>= in_range 0 23
  and minutes = take 2 >>= ensure int_of_string >>= in_range 0 59
  and seconds = take 2 >>= ensure int_of_string >>= in_range 0 60
  in
  let to_span sign h m s =
    let factor = if sign = '+' then 1 else (-1)
    and seconds = (h * 60 + m) * 60 + s in
    if sign = '-' && seconds = 0
    then raise Parse_error
    else Ptime.Span.of_int_s (factor * seconds)
  in
  let offset = lift4 to_span sign hours minutes (option 0 seconds) in
  match parse_string (offset <* end_of_input) str with
  | Ok p -> `Utcoffset p
  | Error _ -> raise Parse_error

(* param data structure *)
type other = [
  | `Xname of string
  | `Ianatoken of string
]

type cutype' = [ `Individual | `Group | `Resource | `Room | `Unknown | other ]
type encoding = [`Eightbit | `Base64 ]
type typename = string
type subtypename = string
type fbtype = [ `Free | `Busy | `Busyunavailable | `Busytentative | other ]
type partstat' = [ `Needsaction | `Accepted | `Declined | `Tentative | `Delegated | `Completed | `Inprocess | other ]
type trigrel = [ `Start | `End ]
type reltype = [ `Parent | `Child | `Sibling | other ]
type role' = [ `Chair | `Reqparticipant | `Optparticipant | `Nonparticipant | other ]
type caladdress = Uri.t
type languagetag = string
type valuetype = [ `Binary | `Boolean | `Caladdress | `Date | `Datetime | `Duration
 | `Float | `Integer | `Period | `Recur | `Text | `Time | `Uri | `Utcoffset | other ]

type icalparameter = [`Altrep of Uri.t | `Cn of string | `Cutype of cutype'
 | `Delfrom of caladdress list | `Delto of caladdress list | `Dir of Uri.t
 | `Encoding of encoding | `Fmttype of typename * subtypename | `Fbtype of fbtype
 | `Language of languagetag | `Member of caladdress list | `Partstat of partstat'
 | `Range | `Trigrel of trigrel | `Reltype of reltype | `Role of role' | `Rsvp of bool
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
    | `Text -> text value
    | `Time -> time value
    | `Uri -> uri value
    | `Utcoffset -> utcoffset value
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
  try parse_string (* calobject *) contentlines (normalize_lines str)
  with Parse_error -> Error "parse error"



let pair a b = (a, b)
let triple a b c = (a, b, c)

(* from RFC 4288 Section 4.2 *)
let media_type_chars = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'
  | '!' | '#' | '$' | '&' | '.' | '+' | '-' | '^' | '_' -> true
  | _ -> false

let media_type_name =
  take_while1 media_type_chars >>= fun data ->
  if String.length data < 128
  then return data
  else fail "parse error"

let media_type =
  lift2 pair (media_type_name <* char '/') media_type_name

let iana_token = name

let iana_param = lift2 (fun k v -> `Iana_param (k, v))
    (iana_token <* (char '=')) value_list

let is_valid p str =
  if Astring.String.for_all p str then
    return str
  else
    fail "parse error"

let up_to_two p = (take 2 >>= is_valid p) <|> (take 1 >>= is_valid p)
let up_to_three p = (take 3 >>= is_valid p) <|> up_to_two p

let is_alpha_digit = function '0' .. '9' | 'a' .. 'z' -> true | _ -> false
let vendorid = up_to_three is_alpha_digit

let x_name = lift2 pair
    ((string "X-") *> (option "" (vendorid <* char '-')))
    (take_while1 is_alpha_digit_minus)

let x_param = lift2 (fun k v -> `Xparam (k, v))
    (x_name <* char '=') value_list

let other_param = iana_param <|> x_param

let other_params = many (char ';' *> other_param)

let propparser id pparser vparser lift =
  let params = many (char ';' *> pparser) in
  lift2 lift
    (string id *> params <* char ':')
    (vparser <* end_of_line)

(* NOTE grammar in RFC 3.7.3 regards pidvalue as text, thus it could be a list, but we forbid that *)
let prodid =
  propparser "PRODID" other_param text_parser (fun a b -> `Prodid (a, b))

let version =
  let vervalue = string "2.0" in
  propparser "VERSION" other_param vervalue (fun a b -> `Version (a, b))

let calscale =
  let calvalue = string "GREGORIAN" in
  propparser "CALSCALE" other_param calvalue (fun a b -> `Calscale (a, b))


let meth =
  let metvalue = iana_token in
  propparser "METHOD" other_param metvalue (fun a b -> `Method (a, b))

let calprops =
  many (prodid <|> version <|> calscale <|> meth)

let dtstamp =
  propparser "DTSTAMP" other_param datetime_parser (fun a b -> `Dtstamp (a, b))

let uid =
  propparser "UID" other_param text_parser (fun a b -> `Uid (a, b))

let tzidparam =
 lift2 (fun a b -> `Tzid (a = '/', b))
 (string "TZID=" *> option ' ' (char '/')) param_text

let time_or_date_param =
  string "VALUE=" *>
  (string "DATE-TIME" <|> string "DATE") >>| function
  | "DATE-TIME" -> `Valuetype `Datetime
  | "DATE" -> `Valuetype `Date
  | _ -> raise Parse_error

let build_time_or_date a b =
  let valuetype = try List.find (function `Valuetype _ -> true | _ -> false) a with Not_found -> `Valuetype `Datetime in
  match valuetype, b with
  | `Valuetype `Datetime, `Datetime dt -> (a, b)
  | `Valuetype `Date, `Date d -> (a, b)
  | _ -> raise Parse_error

let time_or_date_parser =
  (datetime_parser >>| fun dt -> `Datetime dt)
  <|> (date_parser >>| fun d -> `Date d)

let dtstart =
  let dtstparam = time_or_date_param <|> tzidparam <|> other_param in
  propparser "DTSTART" dtstparam time_or_date_parser
    (fun a b -> `Dtstart (build_time_or_date a b))

let class_ =
  let class_value =
       (string "PUBLIC" >>| fun _ -> `Public)
   <|> (string "PRIVATE" >>| fun _ -> `Private)
   <|> (string "CONFIDENTIAL" >>| fun _ -> `Confidential)
   <|> (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))
  in
  propparser "CLASS" other_param class_value (fun a b -> `Class (a, b))

let created =
  propparser "CREATED" other_param datetime_parser (fun a b -> `Created (a, b))

(* TODO use uri parser here *)
let altrepparam = (string "ALTREP=") *> quoted_string >>| fun uri -> `Altrep (Uri.of_string uri)

(* TODO use language tag rfc5646 parser *)
let languageparam = (string "LANGUAGE=") *> param_text >>| fun l -> `Language l 

let description =
  let desc_param = altrepparam <|> languageparam <|> other_param in
  propparser "DESCRIPTION" desc_param text_parser
    (fun a b -> `Description (a, b))

let geo =
  let latlon =
    lift2 pair (float_parser <* char ';') float_parser
  in
  propparser "GEO" other_param latlon (fun a b -> `Geo (a, b))

let last_mod =
  propparser "LAST-MODIFIED" other_param datetime_parser
    (fun a b -> `Lastmod (a, b))

let location =
  let loc_param = altrepparam <|> languageparam <|> other_param in
  propparser "LOCATION" loc_param text_parser (fun a b -> `Location (a, b))

let caladdress = take_while1 is_qsafe_char >>| Uri.of_string

let quoted_caladdress = char '"' *> caladdress <* char '"' 

let cnparam = string "CN=" *> param_value >>| fun cn -> `Cn cn
let dirparam = string "DIR=" *> quoted_string >>| fun s -> `Dir (Uri.of_string s)
let sentbyparam = string "SENT-BY=" *> quoted_caladdress >>| fun s -> `Sentby s

let organizer =
  let orgparam = cnparam <|> dirparam <|> sentbyparam <|> languageparam <|> other_param in
  propparser "ORGANIZER" orgparam caladdress (fun a b -> `Organizer (a, b))

let priority =
  let digit = satisfy is_digit >>= fun c -> ensure int_of_string @@ String.make 1 c in
  propparser "PRIORITY" other_param digit (fun a b -> `Priority (a, b))

let seq =
  let seqv = digits >>= ensure int_of_string >>= in_range 0 max_int in
  propparser "SEQUENCE" other_param seqv (fun a b -> `Seq (a, b))

let status =
  let statvalue_jour =
    (string "DRAFT" >>| fun _ -> `Draft) <|>
    (string "FINAL" >>| fun _ -> `Final) <|>
    (string "CANCELLED" >>| fun _ -> `Cancelled)
  and statvalue_todo =
    (string "NEEDS-ACTION" >>| fun _ -> `Needs_action) <|>
    (string "COMPLETED" >>| fun _ -> `Completed) <|>
    (string "IN-PROCESS" >>| fun _ -> `In_process) <|>
    (string "CANCELLED" >>| fun _ -> `Cancelled)
  and statvalue_event =
    (string "TENTATIVE" >>| fun _ -> `Tentative) <|>
    (string "CONFIRMED" >>| fun _ -> `Confirmed) <|>
    (string "CANCELLED" >>| fun _ -> `Cancelled)
  in
  let statvalue = statvalue_event <|> statvalue_todo <|> statvalue_jour in
  propparser "STATUS" other_param statvalue (fun a b -> `Status (a, b))

let summary =
  let summ_param = altrepparam <|> languageparam <|> other_param in
  propparser "SUMMARY" summ_param text_parser (fun a b -> `Summary (a, b))

let transp =
  let t_value =
    (string "TRANSPARENT" >>| fun _ -> `Transparent) <|>
    (string "OPAQUE" >>| fun _ -> `Opaque)
  in
  propparser "TRANSP" other_param t_value (fun a b -> `Transparency (a, b))

let url =
  propparser "URL" other_param caladdress (fun a b -> `Url (a, b))

let recurid =
  let range_param = string "RANGE=THISANDFUTURE" >>| fun _ -> `Range `Thisandfuture in
  let recur_param = tzidparam <|> time_or_date_param <|> range_param <|> other_param in
  propparser "RECURRENCE-ID" recur_param time_or_date_parser
    (fun a b -> `Recur_id (build_time_or_date a b))

let rrule =
  propparser "RRULE" other_param recur_parser (fun a b -> `Rrule (a, b))

let dtend =
  let dtend_param = tzidparam <|> time_or_date_param <|> other_param in
  propparser "DTEND" dtend_param time_or_date_parser
    (fun a b -> `Dtend (build_time_or_date a b))

let duration =
  propparser "DURATION" other_param duration_parser (fun a b -> `Duration (a, b))

let binary =
  let is_b_char = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '/' -> true | _ -> false in
  let b_end =
    (lift3 (fun a b c -> String.make 1 a ^ String.make 1 b ^ c)
       (satisfy is_b_char) (satisfy is_b_char) (string "==")) <|>
    (lift4 (fun a b c d -> String.make 1 a ^ String.make 1 b ^ String.make 1 c ^ d)
       (satisfy is_b_char) (satisfy is_b_char) (satisfy is_b_char) (string "="))
  in
  lift2 (fun a b -> String.concat "" a ^ b)
    (many (lift4
            (fun a b c d -> String.make 1 a ^ String.make 1 b ^ String.make 1 c ^ String.make 1 d)
            (satisfy is_b_char) (satisfy is_b_char) (satisfy is_b_char) (satisfy is_b_char)))
    b_end

let attach =
  let fmttype_param =
    string "FMTTYPE=" *> media_type >>| fun m -> `Media_type m
  and value_param =
    string "VALUE=BINARY" >>| fun _ -> `Valuetype `Binary
  and encoding_param =
    string "ENCODING=BASE64" >>| fun _ -> `Encoding `Base64
  in
  let attach_param = fmttype_param <|> value_param <|> encoding_param <|> other_param in
  let attach_value =
    (binary >>| fun b -> `Binary b) <|> (caladdress >>| fun a -> `Uri a)
  in
  propparser "ATTACH" attach_param attach_value
    (fun a b ->
       let valuetype = try Some (List.find (function `Valuetype _ -> true | _ -> false) a) with Not_found -> None
       and encoding = try Some (List.find (function `Encoding _ -> true | _ -> false) a) with Not_found -> None
       in
       match valuetype, encoding, b with
       | None, None, `Uri uri -> `Attach (a, `Uri uri)
       | Some (`Valuetype `Binary), Some (`Encoding `Base64), `Binary b -> `Attach (a,`Binary b)
       | _ -> raise Parse_error)

(* Default is INDIVIDUAL *)
let cutypeparam = lift (fun x -> `Cutype x) ((string "CUTYPE=") *> 
      ((string "INDIVIDUAL" >>| fun _ -> `Individual)
   <|> (string "GROUP" >>| fun _ -> `Group)
   <|> (string "RESOURCE" >>| fun _ -> `Resource)
   <|> (string "ROOM" >>| fun _ -> `Room)
   <|> (string "UNKNOWN" >>| fun _ -> `Unknown)
   <|> (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))))

let memberparam = lift (fun x -> `Member x)
  ((string "MEMBER=") *> sep_by1 (char ',') quoted_caladdress)

(* Default is REQ-PARTICIPANT *)
let roleparam = lift (fun x -> `Role x) ((string "ROLE=") *>
      ((string "CHAIR" >>| fun _ -> `Chair)  
   <|> (string "REQ-PARTICIPANT" >>| fun _ -> `Reqparticipant )  
   <|> (string "OPT-PARTICIPANT" >>| fun _ -> `Optparticipant )  
   <|> (string "NON-PARTICIPANT" >>| fun _ -> `Nonparticipant )  
   <|> (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))))

let partstatparam = 
  let statvalue_jour =
    (string "NEEDS-ACTION" >>| fun _ -> `Needs_action) <|>
    (string "ACCEPTED" >>| fun _ -> `Accepted) <|>
    (string "DECLINED" >>| fun _ -> `Declined)
  and statvalue_todo =
    (string "NEEDS-ACTION" >>| fun _ -> `Needs_action) <|>
    (string "ACCEPTED" >>| fun _ -> `Accepted) <|>
    (string "DECLINED" >>| fun _ -> `Declined) <|>
    (string "TENTATIVE" >>| fun _ -> `Tentative) <|>
    (string "DELEGATED" >>| fun _ -> `Delegated) <|>
    (string "COMPLETED" >>| fun _ -> `Completed) <|>
    (string "IN-PROCESS" >>| fun _ -> `In_process)
  and statvalue_event =
    (string "NEEDS-ACTION" >>| fun _ -> `Needs_action) <|>
    (string "ACCEPTED" >>| fun _ -> `Accepted) <|>
    (string "DECLINED" >>| fun _ -> `Declined) <|>
    (string "TENTATIVE" >>| fun _ -> `Tentative) <|>
    (string "DELEGATED" >>| fun _ -> `Delegated)
  and other =
       (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))
  in
  let statvalue = statvalue_event <|> statvalue_todo <|> statvalue_jour <|> other in
  lift (fun x -> `Partstat x) ((string "PARTSTAT=") *> statvalue)

let rsvpparam = lift (fun r -> `Rsvp r) (string "RSVP=" *> ((string "TRUE" >>| fun _ -> true) <|> (string "FALSE" >>| fun _ -> false )))

let deltoparam = lift (fun x -> `Delegated_to x)
  ((string "DELEGATED-TO=") *> sep_by1 (char ',') quoted_caladdress)

let delfromparam = lift (fun x -> `Delegated_from x)
  ((string "DELEGATED-FROM=") *> sep_by1 (char ',') quoted_caladdress)

let attendee =
  let attparam =
    cutypeparam <|> memberparam <|> roleparam <|> partstatparam <|>
    rsvpparam <|> deltoparam <|> delfromparam <|> sentbyparam <|>
    cnparam <|> dirparam <|> languageparam <|> other_param
  in
  propparser "ATTENDEE" attparam caladdress (fun a b -> `Attendee (a, b))

let categories =
  let catparam = languageparam <|> other_param in
  propparser "CATEGORIES" catparam texts_parser (fun a b -> `Categories (a, b))

let comment =
  let commparam = languageparam <|> altrepparam <|> other_param in
  propparser "COMMENT" commparam text_parser (fun a b -> `Comment (a, b))

let eventprop =
  dtstamp <|> uid <|>
  dtstart <|>
  class_ <|> created <|> description <|> geo <|>
  last_mod <|> location <|> organizer <|> priority <|>
  seq <|> status <|> summary <|> transp <|>
  url  <|> recurid <|>
  rrule <|>
  dtend <|> duration <|>
  attach <|> attendee <|> categories <|> comment (* <|>
  contact <|> exdate <|> rstatus <|> related <|>
  resources <|> rdate*)

let eventprops = many eventprop
(*let alarmc = *)

let eventc =
  string "BEGIN:VEVENT" *> end_of_line *> lift2 pair eventprops (*(many alarmc)*)
  (many_till contentline (string "END:VEVENT" <* end_of_line))

let component = many1 (eventc (* <|> todoc <|> journalc <|> freebusyc <|> timezonec *))

let icalbody = lift2 pair calprops component

let calobject =
  string "BEGIN:VCALENDAR" *> end_of_line *> icalbody <* string "END:VCALENDAR" <* end_of_line <* end_of_input

type other_param =
  [ `Iana_param of string * string list
  | `Xparam of (string * string) * string list ]

let pp_other_param fmt = function
  | `Iana_param (k, v) -> Fmt.pf fmt "key %s value %a" k (Fmt.list Fmt.string) v
  | `Xparam ((vendor, name), v) -> Fmt.pf fmt "vendor %s key %s value %a" vendor name (Fmt.list Fmt.string) v

type calprop =
  [ `Prodid of other_param list * string
  | `Version of other_param list * string
  | `Calscale of other_param list * string
  | `Method of other_param list * string
  ]

let pp_other_params = Fmt.list pp_other_param

let pp_calprop fmt = function
  | `Prodid (l, s) -> Fmt.pf fmt "product id %a %s" pp_other_params l s
  | `Version (l, s) -> Fmt.pf fmt "version %a %s" pp_other_params l s
  | `Calscale (l, s) -> Fmt.pf fmt "calscale %a %s" pp_other_params l s
  | `Method (l, s) -> Fmt.pf fmt "method %a %s" pp_other_params l s

type class_ = [ `Public | `Private | `Confidential | `Ianatoken of string | `Xname of string * string ]

let pp_class fmt = function
  | `Public -> Fmt.string fmt "public"
  | `Private -> Fmt.string fmt "private"
  | `Confidential -> Fmt.string fmt "confidential"
  | `Ianatoken t -> Fmt.pf fmt "ianatoken %s" t
  | `Xname (v, t) -> Fmt.pf fmt "xname vendor %s %s" v t

type status = [ `Draft | `Final | `Cancelled |
                `Needs_action | `Completed | `In_process | (* `Cancelled *)
                `Tentative | `Confirmed (* | `Cancelled *) ]

type cutype = [ `Group | `Individual | `Resource | `Room | `Unknown
              | `Ianatoken of string | `Xname of string * string ]

type partstat = [ `Accepted | `Completed | `Declined | `Delegated
                | `In_process | `Needs_action | `Tentative
                | `Ianatoken of string | `Xname of string * string ]

type role = [ `Chair | `Nonparticipant | `Optparticipant | `Reqparticipant
            | `Ianatoken of string | `Xname of string * string ]

type eventprop =
  [ `Dtstamp of other_param list * (Ptime.t * bool)
  | `Uid of other_param list * string
  | `Dtstart of [ other_param | `Valuetype of [`Datetime | `Date ] | `Tzid of bool * string ] list * 
    [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Class of other_param list * class_
  | `Created of other_param list * (Ptime.t * bool)
  | `Description of [other_param | `Altrep of Uri.t | `Language of string ] list * string
  | `Geo of other_param list * (float * float)
  | `Lastmod of other_param list * (Ptime.t * bool)
  | `Location of [other_param | `Altrep of Uri.t | `Language of string ] list * string
  | `Organizer of [other_param | `Cn of string | `Dir of Uri.t | `Sentby of Uri.t | `Language of string] list * Uri.t
  | `Priority of other_param list * int
  | `Seq of other_param list * int
  | `Status of other_param list * status
  | `Summary of [other_param | `Altrep of Uri.t | `Language of string ] list * string
  | `Transparency of other_param list * [ `Transparent | `Opaque ]
  | `Url of other_param list * Uri.t
  | `Recur_id of [ other_param | `Tzid of bool * string | `Valuetype of [ `Datetime | `Date ] | `Range of [ `Thisandfuture ] ] list *
                 [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Rrule of other_param list * recur list
  | `Dtend of [ other_param | `Valuetype of [`Datetime | `Date ] | `Tzid of bool * string ] list * 
              [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Duration of other_param list * int
  | `Attach of [`Media_type of string * string | `Encoding of [ `Base64 ] | `Valuetype of [ `Binary ] | other_param ] list *
               [ `Uri of Uri.t | `Binary of string ]
  | `Attendee of [ other_param
                 | `Cn of string
                 | `Cutype of cutype
                 | `Delegated_from of Uri.t list
                 | `Delegated_to of Uri.t list
                 | `Dir of Uri.t
                 | `Language of string
                 | `Member of Uri.t list
                 | `Partstat of partstat
                 | `Role of role
                 | `Rsvp of bool
                 | `Sentby of Uri.t ] list * Uri.t
  | `Categories of [ other_param | `Language of string ] list * string list
  | `Comment of [ other_param | `Language of string | `Altrep of Uri.t ] list * string
  ]

let pp_dtstart_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Valuetype `Datetime -> Fmt.string fmt "valuetype datetime"
  | `Valuetype `Date -> Fmt.string fmt "valuetype date"
  | `Tzid (prefix, name) -> Fmt.pf fmt "tzid prefix %b %s" prefix name

let pp_dtstart_value fmt = function
  | `Datetime (p, utc) -> Fmt.pf fmt "datetime %a Utc?%b" Ptime.pp p utc 
  | `Date d -> Fmt.pf fmt "date %a" pp_date d

let pp_categories_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Language l -> Fmt.pf fmt "language %s" l

let pp_desc_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Altrep uri -> Fmt.pf fmt "altrep uri %a" Uri.pp_hum uri
  | `Language l -> Fmt.pf fmt "language %s" l

let pp_organizer_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Language l -> Fmt.pf fmt "language %s" l
  | `Cn c -> Fmt.pf fmt "cn %s" c
  | `Dir d -> Fmt.pf fmt "dir %a" Uri.pp_hum d
  | `Sentby s -> Fmt.pf fmt "sent-by %a" Uri.pp_hum s

let pp_recur_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Valuetype `Datetime -> Fmt.string fmt "valuetype datetime"
  | `Valuetype `Date -> Fmt.string fmt "valuetype date"
  | `Tzid (prefix, name) -> Fmt.pf fmt "tzid prefix %b %s" prefix name
  | `Range `Thisandfuture -> Fmt.string fmt "range: thisandfuture"

let pp_status fmt s =
  Fmt.string fmt @@
  match s with
  | `Draft -> "draft"
  | `Final -> "final"
  | `Cancelled -> "cancelled"
  | `Needs_action -> "needs-action"
  | `Completed -> "completed"
  | `In_process -> "in-process"
  | `Tentative -> "tentative"
  | `Confirmed -> "confirmed"

let pp_attach_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Valuetype `Binary -> Fmt.string fmt "valuetype binary"
  | `Encoding `Base64 -> Fmt.string fmt "encoding base64"
  | `Media_type (typename, subtypename) -> Fmt.pf fmt "mediatype %s/%s" typename subtypename

let pp_attach_value fmt = function
  | `Binary b -> Fmt.string fmt b
  | `Uri u -> Uri.pp_hum fmt u

let pp_cutype fmt = function
  | `Individual -> Fmt.string fmt "individual"
  | `Group -> Fmt.string fmt "group"
  | `Resource -> Fmt.string fmt "resource"
  | `Room -> Fmt.string fmt "room"
  | `Unknown -> Fmt.string fmt "unknown"
  | `Ianatoken t -> Fmt.pf fmt "ianatoken %s" t
  | `Xname (x, y) -> Fmt.pf fmt "xname %s,%s" x y

let pp_role fmt = function
  | `Chair -> Fmt.string fmt "chair"
  | `Reqparticipant -> Fmt.string fmt "required participant"
  | `Optparticipant -> Fmt.string fmt "optional participant"
  | `Nonparticipant -> Fmt.string fmt "non participant"
  | `Ianatoken t -> Fmt.pf fmt "ianatoken %s" t
  | `Xname (x, y) -> Fmt.pf fmt "xname %s,%s" x y

let pp_partstat fmt = function
  | `Needs_action -> Fmt.string fmt "needs action"
  | `Accepted -> Fmt.string fmt "accepted"
  | `Declined -> Fmt.string fmt "declined"
  | `Tentative -> Fmt.string fmt "tentative"
  | `Delegated -> Fmt.string fmt "delegated"
  | `Completed -> Fmt.string fmt "completed"
  | `In_process -> Fmt.string fmt "in-process"
  | `Ianatoken t -> Fmt.pf fmt "ianatoken %s" t
  | `Xname (x, y) -> Fmt.pf fmt "xname %s,%s" x y

let pp_attendee_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Cn c -> Fmt.pf fmt "cn %s" c
  | `Cutype c -> Fmt.pf fmt "cutype %a" pp_cutype c
  | `Delegated_from l -> Fmt.pf fmt "delegated from %a" (Fmt.list Uri.pp_hum) l
  | `Delegated_to l -> Fmt.pf fmt "delegated to %a" (Fmt.list Uri.pp_hum) l
  | `Dir d -> Fmt.pf fmt "dir %a" Uri.pp_hum d
  | `Language l -> Fmt.pf fmt "language %s" l
  | `Member l -> Fmt.pf fmt "member %a" (Fmt.list Uri.pp_hum) l
  | `Partstat s -> Fmt.pf fmt "partstat %a" pp_partstat s
  | `Role r -> Fmt.pf fmt "role %a" pp_role r
  | `Rsvp b -> Fmt.pf fmt "rsvp %b" b
  | `Sentby s -> Fmt.pf fmt "sent-by %a" Uri.pp_hum s

let pp_eventprop fmt = function
  | `Dtstamp (l, (p, utc)) -> Fmt.pf fmt "dtstamp %a %a %b" pp_other_params l Ptime.pp p utc
  | `Uid (l, s) -> Fmt.pf fmt "uid %a %s" pp_other_params l s 
  | `Dtstart (l, v) -> Fmt.pf fmt "dtstart %a %a" (Fmt.list pp_dtstart_param) l pp_dtstart_value v
  | `Class (l, v) -> Fmt.pf fmt "class %a %a" pp_other_params l pp_class v
  | `Created (l, (p, utc)) -> Fmt.pf fmt "created %a %a %b" pp_other_params l Ptime.pp p utc
  | `Description (l, v) -> Fmt.pf fmt "description %a %s" (Fmt.list pp_desc_param) l v
  | `Geo (l, (lat, lon)) -> Fmt.pf fmt "geo %a lat %f lon %f" pp_other_params l lat lon
  | `Lastmod (l, (p, utc)) -> Fmt.pf fmt "last modified %a %a %b" pp_other_params l Ptime.pp p utc
  | `Location (l, v) -> Fmt.pf fmt "location %a %s" (Fmt.list pp_desc_param) l v
  | `Organizer (l, v) -> Fmt.pf fmt "organizer %a %a" (Fmt.list pp_organizer_param) l Uri.pp_hum v
  | `Priority (l, v) -> Fmt.pf fmt "priority %a %d" pp_other_params l v
  | `Seq (l, v) -> Fmt.pf fmt "seq %a %d" pp_other_params l v
  | `Status (l, v) -> Fmt.pf fmt "status %a %a" pp_other_params l pp_status v
  | `Summary (l, v) -> Fmt.pf fmt "summary %a %s" (Fmt.list pp_desc_param) l v
  | `Transparency (l, v) -> Fmt.pf fmt "transparency %a %s" pp_other_params l
                              (match v with `Transparent -> "transparent" | `Opaque -> "opaque")
  | `Url (l, v) -> Fmt.pf fmt "url %a %a" pp_other_params l Uri.pp_hum v
  | `Recur_id (l, v) -> Fmt.pf fmt "recur-id %a %a" (Fmt.list pp_recur_param) l pp_dtstart_value v
  | `Rrule (l, v) -> Fmt.pf fmt "rrule %a %a" pp_other_params l (Fmt.list pp_recur) v
  | `Dtend (l, v) -> Fmt.pf fmt "dtend %a %a" (Fmt.list pp_dtstart_param) l pp_dtstart_value v
  | `Duration (l, v) -> Fmt.pf fmt "duration %a %d seconds" pp_other_params l v
  | `Attach (l, v) -> Fmt.pf fmt "attach %a %a" (Fmt.list pp_attach_param) l pp_attach_value v 
  | `Attendee (l, v) -> Fmt.pf fmt "attendee %a %a" (Fmt.list pp_attendee_param) l Uri.pp_hum v
  | `Categories (l, v) -> Fmt.pf fmt "categories %a %a" (Fmt.list pp_categories_param) l (Fmt.list Fmt.string) v
  | `Comment (l, v) -> Fmt.pf fmt "comment %a %s" (Fmt.list pp_desc_param) l v

type component =
  eventprop list * 
  (string * icalparameter list * value) list

let pp_content_line fmt (k, params, v) = Fmt.pf fmt "key %s params %a value %a" k (Fmt.list pp_icalparameter) params pp_value v
let pp_component fmt (props, lines) = Fmt.pf fmt "props: %a @.lines:%a" (Fmt.list pp_eventprop) props (Fmt.list pp_content_line) lines

type calendar = calprop list * component list

let pp_calendar: calendar Fmt.t = fun fmt (props, comps) -> Fmt.pf fmt "properties %a components %a" (Fmt.list pp_calprop) props (Fmt.list pp_component) comps
(*
type calendar = {
  version : version ;
  productid : string ;
  calendarscale : foo option ;
  ... : .. option ;
  other_properties : properties list / map ;
  components : component list
}
*)

let parse_calobject (str : string) : (calendar, string) result =
  try parse_string calobject (normalize_lines str)
  with Parse_error -> Error "parse error"
