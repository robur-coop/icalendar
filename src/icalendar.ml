module Uri = struct 
  include Uri
  let pp = pp_hum
end

module Ptime = struct
  include Ptime
  let equal_date (y, m, d) (y', m', d') = y = y' && m = m' && d = d'
  let pp_date fmt (y, m, d) = Fmt.pf fmt "%04d-%02d-%02d" y m d 
end

type valuetype = [
    `Binary | `Boolean | `Caladdress | `Date | `Datetime | `Duration | `Float
  | `Integer | `Period | `Recur | `Text | `Time | `Uri | `Utcoffset
  | `Xname of (string * string) | `Ianatoken of string
] [@@deriving eq, show]

type valuetypeparam = [ `Valuetype of valuetype ] [@@deriving eq, show]

type other_param =
  [ `Iana_param of string * string list
  | `Xparam of (string * string) * string list ] [@@deriving eq, show]

type calprop =
  [ `Prodid of other_param list * string
  | `Version of other_param list * string
  | `Calscale of other_param list * string
  | `Method of other_param list * string
  ] [@@deriving eq, show]

type weekday = [ `Friday | `Monday | `Saturday | `Sunday | `Thursday | `Tuesday | `Wednesday ] [@@deriving eq, show]

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
] [@@deriving eq, show]

type class_ = [ `Public | `Private | `Confidential | `Ianatoken of string | `Xname of string * string ] [@@deriving eq, show]

type status = [ `Draft | `Final | `Cancelled |
                `Needs_action | `Completed | `In_process | (* `Cancelled *)
                `Tentative | `Confirmed (* | `Cancelled *) ] [@@deriving eq, show]

type cutype = [ `Group | `Individual | `Resource | `Room | `Unknown
              | `Ianatoken of string | `Xname of string * string ] [@@deriving eq, show]

type partstat = [ `Accepted | `Completed | `Declined | `Delegated
                | `In_process | `Needs_action | `Tentative
                | `Ianatoken of string | `Xname of string * string ] [@@deriving eq, show]

type role = [ `Chair | `Nonparticipant | `Optparticipant | `Reqparticipant
            | `Ianatoken of string | `Xname of string * string ] [@@deriving eq, show]

type relationship =
  [ `Parent | `Child | `Sibling |
    `Ianatoken of string | `Xname of string * string ] [@@deriving eq, show]

type eventprop =
  [ `Dtstamp of other_param list * (Ptime.t * bool)
  | `Uid of other_param list * string
  | `Dtstart of [ other_param | valuetypeparam | `Tzid of bool * string ] list *
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
  | `Recur_id of [ other_param | `Tzid of bool * string | valuetypeparam | `Range of [ `Thisandfuture ] ] list *
                 [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Rrule of other_param list * recur list
  | `Dtend of [ other_param | valuetypeparam | `Tzid of bool * string ] list * 
              [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Duration of other_param list * int
  | `Attach of [`Media_type of string * string | `Encoding of [ `Base64 ] | valuetypeparam | other_param ] list *
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
  | `Contact of [ other_param | `Language of string | `Altrep of Uri.t ] list * string
  | `Exdate of [ other_param | valuetypeparam | `Tzid of bool * string ] list *
               [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list ]
  | `Rstatus of [ other_param | `Language of string ] list * ((int * int * int option) * string * string option)
  | `Related of [ other_param | `Reltype of relationship ] list * string
  | `Resource of [ other_param | `Language of string | `Altrep of Uri.t ] list * string list
  | `Rdate of [ other_param | valuetypeparam | `Tzid of bool * string ] list *
              [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list | `Periods of (Ptime.t * Ptime.t * bool) list ]
  (* xprop and iana-prop not done yet *)
  ] [@@deriving eq, show]

type 'a alarm_struct = {
  trigger : [ other_param | valuetypeparam | `Related of [ `Start | `End ] ] list *
    [ `Duration of int | `Datetime of (Ptime.t * bool) ] ;
  duration_repeat: ((other_param list * int) * (other_param list * int )) option ;
  special: 'a ;
} [@@deriving eq, show] 

type audio_struct = { 
  attach: ([`Media_type of string * string | `Encoding of [ `Base64 ] | valuetypeparam | other_param ] list *
    [ `Uri of Uri.t | `Binary of string ]) option ;
  (* xprop: list ;
  iana_prop: list ; *)
} [@@deriving eq, show]

type display_struct = {
  description : [ other_param | `Altrep of Uri.t | `Language of string ] list * string ;
} [@@deriving eq, show]

type email_struct = {
  description : [ other_param | `Altrep of Uri.t | `Language of string ] list * string ;
  summary : [ other_param | `Altrep of Uri.t | `Language of string ] list * string ;
  attendees : ([ other_param
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
                 | `Sentby of Uri.t ] list * Uri.t) list ;
  attach: ([`Media_type of string * string | `Encoding of [ `Base64 ] | valuetypeparam | other_param ] list *
    [ `Uri of Uri.t | `Binary of string ]) option ;
} [@@deriving eq, show]

type alarm = [ `Audio of audio_struct alarm_struct | `Display of display_struct alarm_struct | `Email of email_struct alarm_struct ] [@@deriving eq, show]

type component = eventprop list * alarm list [@@deriving eq, show]

type calendar = calprop list * component list [@@deriving eq, show]

open Angstrom
exception Parse_error

(* pre-processing of the input: remove "\n " *)
let normalize_lines s =
  let re = Re.compile ( Re.Perl.re ~opts:[`Multiline] "(\n|\r\n)^\\s" ) in
  Re.replace_string ~all:true re ~by:"" s

(* Terminal parsers and helpers *)
let ensure f x = try return (f x) with Failure _ -> fail "parse error"
let in_range min max v = if min <= v && v <= max then return v else fail "parse error"

let is_digit = function '0' .. '9' -> true | _ -> false
let digits = take_while1 is_digit
let digit = satisfy is_digit >>= fun c -> ensure int_of_string @@ String.make 1 c

let sign = option '+' (char '+' <|> char '-')

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

let value = take_while (fun x -> not (is_control x)) (* in fact it is more complicated *)

let iana_token = name

let is_valid p str =
  if Astring.String.for_all p str then
    return str
  else
    fail "parse error"

let up_to_two p = (take 2 >>= is_valid p) <|> (take 1 >>= is_valid p)
let up_to_three p = (take 3 >>= is_valid p) <|> up_to_two p

let is_alpha_digit = function '0' .. '9' | 'a' .. 'z' -> true | _ -> false
let vendorid = up_to_three is_alpha_digit

let pair a b = (a, b)
let x_name = lift2 pair
    ((string "X-") *> (option "" (vendorid <* char '-')))
    (take_while1 is_alpha_digit_minus)

let caladdress = take_while1 is_qsafe_char >>| Uri.of_string

let quoted_caladdress = char '"' *> caladdress <* char '"' 

(* value parsers *)
let text =
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

let texts = sep_by (char ',') text

let date =
  let year = take 4 >>= ensure int_of_string
  and month = take 2 >>= ensure int_of_string >>= in_range 1 12
  and day = take 2 >>= ensure int_of_string >>= in_range 1 31
  and to_ptime_date y m d = (y, m, d)
  in
  lift3 to_ptime_date year month day

let time =
  let hours = take 2 >>= ensure int_of_string >>= in_range 0 23
  and minutes = take 2 >>= ensure int_of_string >>= in_range 0 59
  and seconds = take 2 >>= ensure int_of_string >>= in_range 0 60
  and utc = option ' ' (char 'Z')
  in
  lift4 (fun h m s u -> ((h, m, s), u = 'Z'))
            hours minutes seconds utc

let datetime =
  let ptime d (t, utc) = match Ptime.of_date_time (d, (t, 0)) with
  | Some p -> p, utc
  | None -> raise Parse_error in
  lift2 ptime date (char 'T' *> time)

let dur_value =
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

let float =
  let make_float s i f =
    let n = try float_of_string (i ^ "." ^ f) with Failure _ -> raise Parse_error in
    if s = '+' then n else (-. n) in
  lift3 make_float sign digits (option "" ((char '.') *> digits))

let period =
  let to_explicit (dt, utc) dur = match Ptime.add_span dt (Ptime.Span.of_int_s dur) with
  | Some t -> (dt, t, utc)
  | None -> raise Parse_error in
  let to_period (tstart, utc) (tend, utc') = if utc = utc' then (tstart, tend, utc) else raise Parse_error in
  let explicit = lift2 to_period datetime (char '/' *> datetime)
  and start = lift2 to_explicit datetime (char '/' *> dur_value) in
  explicit <|> start

let recur =
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
  and ptime = date >>= fun d -> match Ptime.of_date d with None -> fail "Parse_error" | Some x -> return (x, true) in
  let recur_rule_part =
       ( string "FREQ=" *> freq >>| fun f -> `Frequency f )
   <|> ( string "UNTIL=" *> (datetime <|> ptime) >>| fun u -> `Until u )
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

(* out in the wild *)
let utcoffset =
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
  lift4 to_span sign hours minutes (option 0 seconds)

(* processing *)
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


(* Parameters (PARAM1_KEY=PARAM1_VALUE) *)
let iana_param = lift2 (fun k v -> `Iana_param (k, v))
    (iana_token <* (char '=')) value_list

let x_param = lift2 (fun k v -> `Xparam (k, v))
    (x_name <* char '=') value_list

let other_param = iana_param <|> x_param

let tzidparam =
 lift2 (fun a b -> `Tzid (a = '/', b))
 (string "TZID=" *> option ' ' (char '/')) param_text

let valuetypeparam =
  lift (fun x -> `Valuetype x)
    (string "VALUE=" *>
     ((string "BINARY" >>| fun _ -> `Binary)
      <|> (string "BOOLEAN" >>| fun _ -> `Boolean)
      <|> (string "CAL-ADDRESS" >>| fun _ -> `Caladdress)
      <|> (string "DATE-TIME" >>| fun _ -> `Datetime)
      <|> (string "DATE" >>| fun _ -> `Date)
      <|> (string "DURATION" >>| fun _ -> `Duration)
      <|> (string "FLOAT" >>| fun _ -> `Float)
      <|> (string "INTEGER" >>| fun _ -> `Integer)
      <|> (string "PERIOD" >>| fun _ -> `Period)
      <|> (string "RECUR" >>| fun _ -> `Recur)
      <|> (string "TEXT" >>| fun _ -> `Text)
      <|> (string "TIME" >>| fun _ -> `Time)
      <|> (string "URI" >>| fun _ -> `Uri)
      <|> (string "UTC-OFFSET" >>| fun _ -> `Utcoffset)
      <|> (x_name >>| fun x -> `Xname x)
      <|> (iana_token >>| fun x -> `Ianatoken x)))

(* TODO use uri parser here *)
let altrepparam = (string "ALTREP=") *> quoted_string >>| fun uri -> `Altrep (Uri.of_string uri)

(* TODO use language tag rfc5646 parser *)
let languageparam = (string "LANGUAGE=") *> param_text >>| fun l -> `Language l 

let cnparam = string "CN=" *> param_value >>| fun cn -> `Cn cn
let dirparam = string "DIR=" *> quoted_string >>| fun s -> `Dir (Uri.of_string s)
let sentbyparam = string "SENT-BY=" *> quoted_caladdress >>| fun s -> `Sentby s

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

let reltypeparam =
  lift (fun x -> `Reltype x)
   (string "RELTYPE=" *>
      ((string "PARENT" >>| fun _ -> `Parent)
   <|> (string "CHILD" >>| fun _ -> `Child)
   <|> (string "SIBLING" >>| fun _ -> `Sibling)
   <|> (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))))

(* Properties *)
let propparser id pparser vparser lift =
  let params = many (char ';' *> pparser) in
  lift2 lift
    (string id *> params <* char ':')
    (vparser <* end_of_line)

let prodid =
  propparser "PRODID" other_param text (fun a b -> `Prodid (a, b))

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
  propparser "DTSTAMP" other_param datetime (fun a b -> `Dtstamp (a, b))

let uid =
  propparser "UID" other_param text (fun a b -> `Uid (a, b))

let check_date_datetime default a b =
  let valuetype =
    try List.find (function `Valuetype _ -> true | _ -> false) a
    with Not_found -> `Valuetype default
  in
  match valuetype, b with
  | `Valuetype `Datetime, `Datetime _ -> ()
  | `Valuetype `Date, `Date _ -> ()
  | _ -> raise Parse_error

let check_datetime_duration default a b =
  let valuetype =
    try List.find (function `Valuetype _ -> true | _ -> false) a
    with Not_found -> `Valuetype default
  in
  match valuetype, b with
  | `Valuetype `Datetime, `Datetime _ -> ()
  | `Valuetype `Duration, `Duration _ -> ()
  | _ -> raise Parse_error

let check_date_datetime_period default a b =
  let valuetype =
    try List.find (function `Valuetype _ -> true | _ -> false) a
    with Not_found -> `Valuetype default
  in
  match valuetype, b with
  | `Valuetype `Datetime, `Datetime _ -> ()
  | `Valuetype `Date, `Date _ -> ()
  | `Valuetype `Period, `Period _ -> ()
  | _ -> raise Parse_error

let check_binary_uri default a b =
  let valuetype =
    try List.find (function `Valuetype _ -> true | _ -> false) a
    with Not_found -> `Valuetype default
  in
  match valuetype, b with
  | `Valuetype `Binary, `Binary _ -> ()
  | `Valuetype `Uri, `Uri _ -> ()
  | _ -> raise Parse_error

let time_or_date =
  (datetime >>| fun dt -> `Datetime dt)
  <|> (date >>| fun d -> `Date d)

let dtstart =
  let dtstparam = valuetypeparam <|> tzidparam <|> other_param in
  propparser "DTSTART" dtstparam time_or_date
    (fun a b ->
       check_date_datetime `Datetime a b ;
       `Dtstart (a, b))

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
  propparser "CREATED" other_param datetime (fun a b -> `Created (a, b))

let description =
  let desc_param = altrepparam <|> languageparam <|> other_param in
  propparser "DESCRIPTION" desc_param text
    (fun a b -> `Description (a, b))

let geo =
  let latlon =
    lift2 pair (float <* char ';') float
  in
  propparser "GEO" other_param latlon (fun a b -> `Geo (a, b))

let last_mod =
  propparser "LAST-MODIFIED" other_param datetime
    (fun a b -> `Lastmod (a, b))

let location =
  let loc_param = altrepparam <|> languageparam <|> other_param in
  propparser "LOCATION" loc_param text (fun a b -> `Location (a, b))

let organizer =
  let orgparam = cnparam <|> dirparam <|> sentbyparam <|> languageparam <|> other_param in
  propparser "ORGANIZER" orgparam caladdress (fun a b -> `Organizer (a, b))

let priority =
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
  propparser "SUMMARY" summ_param text (fun a b -> `Summary (a, b))

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
  let recur_param = tzidparam <|> valuetypeparam <|> range_param <|> other_param in
  propparser "RECURRENCE-ID" recur_param time_or_date
    (fun a b ->
       check_date_datetime `Datetime a b ;
       `Recur_id (a, b))

let rrule =
  propparser "RRULE" other_param recur (fun a b -> `Rrule (a, b))

let dtend =
  let dtend_param = tzidparam <|> valuetypeparam <|> other_param in
  propparser "DTEND" dtend_param time_or_date
    (fun a b ->
       check_date_datetime `Datetime a b ;
       `Dtend (a, b))

let duration =
  propparser "DURATION" other_param dur_value (fun a b -> `Duration (a, b))

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
  and encoding_param =
    string "ENCODING=BASE64" >>| fun _ -> `Encoding `Base64
  in
  let attach_param = fmttype_param <|> valuetypeparam <|> encoding_param <|> other_param in
  let attach_value =
    (binary >>| fun b -> `Binary b) <|> (caladdress >>| fun a -> `Uri a)
  in
  propparser "ATTACH" attach_param attach_value
    (fun a b ->
       check_binary_uri `Uri a b ;
       let encoding = try Some (List.find (function `Encoding _ -> true | _ -> false) a) with Not_found -> None in
       match encoding, b with
       | None, `Uri _ -> `Attach (a, b)
       | Some (`Encoding `Base64), `Binary _ -> `Attach (a, b)
       | _ -> raise Parse_error)

let attendee =
  let attparam =
    cutypeparam <|> memberparam <|> roleparam <|> partstatparam <|>
    rsvpparam <|> deltoparam <|> delfromparam <|> sentbyparam <|>
    cnparam <|> dirparam <|> languageparam <|> other_param
  in
  propparser "ATTENDEE" attparam caladdress (fun a b -> `Attendee (a, b))

let categories =
  let catparam = languageparam <|> other_param in
  propparser "CATEGORIES" catparam texts (fun a b -> `Categories (a, b))

let comment =
  let commparam = languageparam <|> altrepparam <|> other_param in
  propparser "COMMENT" commparam text (fun a b -> `Comment (a, b))

let contact =
  let contactparam = languageparam <|> altrepparam <|> other_param in
  propparser "CONTACT" contactparam text (fun a b -> `Contact (a, b))

let exdate =
  let exdtparam = valuetypeparam <|> tzidparam <|> other_param in
  let exdtvalue = sep_by1 (char ',') time_or_date in
  propparser "EXDATE" exdtparam exdtvalue
    (fun a b ->
       List.iter (check_date_datetime `Datetime a) b ;
       let is_date = function `Date _ -> true | _ -> false
       and is_datetime = function `Datetime _ -> true | _ -> false
       in
       let date =
         if List.for_all is_date b then
           let extract = function `Date d -> d | _ -> raise Parse_error in
           `Dates (List.map extract b)
         else if List.for_all is_datetime b then
           let extract = function `Datetime d -> d | _ -> raise Parse_error in
           `Datetimes (List.map extract b)
         else raise Parse_error
       in
       `Exdate (a, date))

let rstatus =
  let rstatparam = languageparam <|> other_param in
  let statcode =
    lift3 triple
      digit
      (char '.' *> digit)
      (option None (char '.' *> (digit >>| fun x -> Some x)))
  in
  let rstatvalue =
    lift3 triple
      (statcode <* char ';')
      text
      (option None (char ';' *> (text >>| fun t -> Some t)))
  in
  propparser "REQUEST-STATUS" rstatparam rstatvalue
    (fun a b -> `Rstatus (a, b))

let related =
  let relparam = reltypeparam <|> other_param in
  propparser "RELATED-TO" relparam text
    (fun a b -> `Related (a, b))

let resources =
  let resrcparam = languageparam <|> altrepparam <|> other_param in
  propparser "RESOURCES" resrcparam texts
    (fun a b -> `Resource (a, b))

let time_or_date_or_period =
      (period >>| fun p -> `Period p)
  <|> (datetime >>| fun dt -> `Datetime dt)
  <|> (date >>| fun d -> `Date d)

let rdate =
  let rdtparam = tzidparam <|> valuetypeparam <|> other_param in
  let rdtvalue = sep_by1 (char ',') time_or_date_or_period in
  propparser "RDATE" rdtparam rdtvalue
    (fun a b ->
       List.iter (check_date_datetime_period `Datetime a) b ;
       let is_date = function `Date _ -> true | _ -> false
       and is_datetime = function `Datetime _ -> true | _ -> false
       and is_period = function `Period _ -> true | _ -> false
       in
       let date =
         if List.for_all is_date b then
           let extract = function `Date d -> d | _ -> raise Parse_error in
           `Dates (List.map extract b)
         else if List.for_all is_datetime b then
           let extract = function `Datetime d -> d | _ -> raise Parse_error in
           `Datetimes (List.map extract b)
         else if List.for_all is_period b then
           let extract = function `Period d -> d | _ -> raise Parse_error in
           `Periods (List.map extract b)
         else raise Parse_error
       in
       `Rdate (a, date))

let eventprop =
  dtstamp <|> uid <|>
  dtstart <|>
  class_ <|> created <|> description <|> geo <|>
  last_mod <|> location <|> organizer <|> priority <|>
  seq <|> status <|> summary <|> transp <|>
  url  <|> recurid <|>
  rrule <|>
  dtend <|> duration <|>
  attach <|> attendee <|> categories <|> comment <|>
  contact <|> exdate <|> rstatus <|> related <|>
  resources <|> rdate (* iana_prop <|> x_prop *)

let eventprops = many eventprop

let action =
  let actionvalue =
        (string "AUDIO" >>| fun _ -> `Audio)
    <|> (string "DISPLAY" >>| fun _ -> `Display)
    <|> (string "EMAIL" >>| fun _ -> `Email)
    <|> (iana_token >>| fun x -> `Ianatoken x)
    <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))
  in
  propparser "ACTION" other_param actionvalue (fun a b -> `Action (a, b))

let trigger =
  let trigrelparam =
    lift (fun x -> `Related x)
      (string "RELATED=" *>
       ((string "START" >>| fun _ -> `Start) <|>
        (string "END" >>| fun _ -> `End)))
  in
  let trigparam = trigrelparam <|> valuetypeparam <|> other_param in
  let trigvalue =
        (dur_value >>| fun d -> `Duration d)
    <|> (datetime >>| fun d -> `Datetime d)
  in
  propparser "TRIGGER" trigparam trigvalue
    (fun a b ->
       check_datetime_duration `Duration a b ;
       `Trigger (a, b))

let repeat =
  let rvalue = digits >>= ensure int_of_string in
  propparser "REPEAT" other_param rvalue
    (fun a b -> `Repeat (a, b))

let audioprop =
  action <|> trigger <|>
  duration <|> repeat <|>
  attach

(* missing cases already covered in audioprop *)
let dispprop =
  (* action <|> *) description (* <|> trigger <|>
   duration <|> repeat *)

(* missing cases already covered in audioprop *)
let emailprop =
  (* action <|> description <|> trigger <|> *) summary <|>
  attendee (* <|>
  duration <|> repeat <|>
  attach *)

(* let otherprop = x_prop <|> iana_prop *)

let build_alarm props =
  let actions, rest = List.partition (function `Action _ -> true | _ -> false) props in
  let action = match actions with 
   | [`Action x] -> x 
   | _ -> raise Parse_error in

  let triggers, rest' = List.partition (function `Trigger _ -> true | _ -> false ) rest in
  let trigger = match triggers with 
   | [`Trigger x] -> x 
   | _ -> raise Parse_error in

  (* check dur repeat *)
  let duration_repeat, rest''' = 
    let durations, rest'' = List.partition (function `Duration _ -> true | _ -> false ) rest' in
    let repeats, rest''' = List.partition (function `Repeat _ -> true | _ -> false ) rest'' in
    match durations, repeats with 
     | [`Duration x], [`Repeat y] -> Some (x, y), rest'''
     | [], [] -> None, rest'''
     | _, _ -> raise Parse_error in

  let build_audio rest =
    let attachs, rest' = List.partition (function `Attach _ -> true | _ -> false ) rest in
    let attach = match attachs with 
     | [`Attach x] -> Some x 
     | [] -> None
     | _ -> raise Parse_error in
    match rest' with 
     | [] -> `Audio { trigger ; duration_repeat ; special = { attach } } 
     | _ -> raise Parse_error in
  
  let build_display rest =
    let descriptions, rest' = List.partition (function `Description _ -> true | _ -> false ) rest in
    let description = match descriptions with 
     | [`Description x] -> x 
     | _ -> raise Parse_error in
    match rest' with 
     | [] -> `Display { trigger ; duration_repeat ; special = { description } } 
     | _ -> raise Parse_error in

  let build_email rest =
    let descriptions, rest' = List.partition (function `Description _ -> true | _ -> false ) rest in
    let description = match descriptions with 
     | [`Description x] -> x 
     | _ -> raise Parse_error in
    let summarys, rest'' = List.partition (function `Summary _ -> true | _ -> false ) rest' in
    let summary = match summarys with 
     | [`Summary x] -> x 
     | _ -> raise Parse_error in
    let raw_attendees, rest''' = List.partition (function `Attendee _ -> true | _ -> false ) rest'' in
    let attendees = List.map (function `Attendee x -> x | _ -> raise Parse_error) raw_attendees in
    if attendees = [] then raise Parse_error;
    let attachs, rest'''' = List.partition (function `Attach _ -> true | _ -> false ) rest''' in
    let attach = match attachs with 
     | [`Attach x] -> Some x 
     | [] -> None
     | _ -> raise Parse_error in
    match rest'''' with 
     | [] -> `Email { trigger ; duration_repeat ; special = { description ; summary ; attach ; attendees } } 
     | _ -> raise Parse_error in
    
  match action with
    | _, `Audio -> build_audio rest'''
    | _, `Display -> build_display rest'''
    | _, `Email -> build_email rest'''
    | _, _ -> raise Parse_error (*not supported yet*)

let alarmc =
  string "BEGIN:VALARM" *> end_of_line *>
  ( many (audioprop <|> dispprop <|> emailprop (* <|> otherprop *)) >>| build_alarm )
  <* string "END:VALARM" <* end_of_line

let eventc =
  string "BEGIN:VEVENT" *> end_of_line *>
  lift2 pair eventprops (many alarmc)
  <* string "END:VEVENT" <* end_of_line

let component = many1 (eventc (* <|> todoc <|> journalc <|> freebusyc <|> timezonec *))

let icalbody = lift2 pair calprops component

let calobject =
  string "BEGIN:VCALENDAR" *> end_of_line *>
  icalbody
  <* string "END:VCALENDAR" <* end_of_line <* end_of_input

let parse_calobject (str : string) : (calendar, string) result =
  try parse_string calobject (normalize_lines str)
  with Parse_error -> Error "parse error"
