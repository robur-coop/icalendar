module Uri = struct
  include Uri
  let pp = pp_hum
end

module Ptime = struct
  include Ptime
  let equal_date (y, m, d) (y', m', d') = y = y' && m = m' && d = d'
  let pp_date fmt (y, m, d) = Fmt.pf fmt "%04d-%02d-%02d" y m d
end

let positive = true

type timestamp_utc = Ptime.t [@@deriving eq, show]
type timestamp_local = Ptime.t [@@deriving eq, show]

type utc_or_timestamp_local = [
  | `Utc of timestamp_utc
  | `Local of timestamp_local
] [@@deriving eq, show]

type timestamp = [
  utc_or_timestamp_local
  | `With_tzid of timestamp_local * (bool * string)
] [@@deriving eq, show]

type date_or_datetime = [ `Datetime of timestamp | `Date of Ptime.date ] [@@deriving eq, show]

type valuetype = [
    `Binary | `Boolean | `Caladdress | `Date | `Datetime | `Duration | `Float
  | `Integer | `Period | `Recur | `Text | `Time | `Uri | `Utcoffset
  | `Xname of (string * string) | `Ianatoken of string
] [@@deriving eq, show]

type cutype = [ `Group | `Individual | `Resource | `Room | `Unknown
              | `Ianatoken of string | `Xname of string * string ] [@@deriving eq, show]

type partstat = [ `Accepted | `Completed | `Declined | `Delegated
                | `In_process | `Needs_action | `Tentative
                | `Ianatoken of string | `Xname of string * string ] [@@deriving eq, show]

type relationship =
  [ `Parent | `Child | `Sibling |
    `Ianatoken of string | `Xname of string * string ] [@@deriving eq, show]

type role = [ `Chair | `Nonparticipant | `Optparticipant | `Reqparticipant
            | `Ianatoken of string | `Xname of string * string ] [@@deriving eq, show]

type fbtype = [ `Free | `Busy | `Busy_Unavailable | `Busy_Tentative | `Ianatoken of string | `Xname of string * string ] [@@deriving eq, show]

type param_value = [ `Quoted of string | `String of string ] [@@deriving eq, show]

type _ icalparameter =
  | Altrep : Uri.t icalparameter
  | Cn : param_value icalparameter
  | Cutype : cutype icalparameter
  | Delegated_from : (Uri.t list) icalparameter
  | Delegated_to : (Uri.t list) icalparameter
  | Dir : Uri.t icalparameter
  | Encoding : [ `Base64 ] icalparameter
  | Media_type : (string * string) icalparameter
  | Fbtype : fbtype icalparameter
  | Language : string icalparameter
  | Member : (Uri.t list) icalparameter
  | Partstat : partstat icalparameter
  | Range : [ `Thisandfuture ] icalparameter
  | Related : [ `Start | `End ] icalparameter
  | Reltype : relationship icalparameter
  | Role : role icalparameter
  | Rsvp : bool icalparameter
  | Sentby : Uri.t icalparameter
  | Tzid : (bool * string) icalparameter
  | Valuetype : valuetype icalparameter
  | Iana_param : string -> param_value list icalparameter
  | Xparam : (string * string) -> param_value list icalparameter

let equal_icalparameter : type a. a icalparameter -> a -> a -> bool
  = fun k lhs_v rhs_v ->
  (* type system ensures that the values fit the constructors *)
    match k with
    | Altrep -> Uri.equal lhs_v rhs_v
    | Cn -> equal_param_value lhs_v rhs_v
    | Cutype -> equal_cutype lhs_v rhs_v
    | Delegated_from -> List.for_all2 Uri.equal lhs_v rhs_v
    | Delegated_to -> List.for_all2 Uri.equal lhs_v rhs_v
    | Dir -> Uri.equal lhs_v rhs_v
    | Encoding -> lhs_v = rhs_v
    | Media_type -> String.equal (fst lhs_v) (fst rhs_v) && String.equal (snd lhs_v) (snd rhs_v)
    | Fbtype -> equal_fbtype lhs_v rhs_v
    | Language -> String.equal lhs_v rhs_v
    | Member -> List.for_all2 Uri.equal lhs_v rhs_v
    | Partstat -> equal_partstat lhs_v rhs_v
    | Range -> lhs_v = rhs_v
    | Related -> lhs_v = rhs_v
    | Reltype -> equal_relationship lhs_v rhs_v
    | Role -> equal_role lhs_v rhs_v
    | Rsvp -> lhs_v = rhs_v
    | Sentby -> Uri.equal lhs_v rhs_v
    | Tzid -> fst lhs_v = fst rhs_v && String.equal (snd lhs_v) (snd rhs_v)
    | Valuetype -> equal_valuetype lhs_v rhs_v
    | Iana_param _ -> List.for_all2 equal_param_value lhs_v rhs_v
    | Xparam _ -> List.for_all2 equal_param_value lhs_v rhs_v

let pp_icalparameter : type a. Format.formatter -> a icalparameter -> a -> unit
  = fun fmt k v ->
    match k with
    | Altrep -> Format.fprintf fmt "Altrep %a" Uri.pp v
    | Cn -> Format.fprintf fmt "Cn %a" pp_param_value v
    | Cutype -> Format.fprintf fmt "Cutype %a" pp_cutype v
    | Delegated_from -> Format.fprintf fmt "Delegated_from %a" Fmt.(list Uri.pp) v
    | Delegated_to -> Format.fprintf fmt "Delegated_to %a" Fmt.(list Uri.pp) v
    | Dir -> Format.fprintf fmt "Dir %a" Uri.pp v
    | Encoding -> Format.fprintf fmt "Encoding base64"
    | Media_type -> Format.fprintf fmt "Media_type (%s/%s)" (fst v) (snd v)
    | Fbtype -> Format.fprintf fmt "Fbtype %a" pp_fbtype v
    | Language -> Format.fprintf fmt "Language %s" v
    | Member -> Format.fprintf fmt "Member %a" Fmt.(list Uri.pp) v
    | Partstat -> Format.fprintf fmt "Partstat %a" pp_partstat v
    | Range -> Format.fprintf fmt "Range thisandfuture"
    | Related -> Format.fprintf fmt "Related %s" (match v with `Start -> "start" | `End -> "end")
    | Reltype -> Format.fprintf fmt "Reltype %a" pp_relationship v
    | Role -> Format.fprintf fmt "Role %a" pp_role v
    | Rsvp -> Format.fprintf fmt "Rsvp %b" v
    | Sentby -> Format.fprintf fmt "Sentby %a" Uri.pp v
    | Tzid -> Format.fprintf fmt "Tzid (%b, %s)" (fst v) (snd v)
    | Valuetype -> Format.fprintf fmt "Valuetype %a" pp_valuetype v
    | Iana_param a -> Format.fprintf fmt "Iana_param (%s, %a)" a Fmt.(list pp_param_value) v
    | Xparam (a, b) -> Format.fprintf fmt "Xparam ((%s, %s), %a)" a b Fmt.(list pp_param_value) v

module K = struct
  type 'a t = 'a icalparameter

  let compare : type a b. a t -> b t -> (a, b) Gmap.Order.t = fun lhs rhs ->
    let open Gmap.Order in
          match (lhs, rhs) with
          | (Altrep, Altrep) -> Eq
          | (Cn, Cn) -> Eq
          | (Cutype, Cutype) -> Eq
          | (Delegated_from, Delegated_from) -> Eq
          | (Delegated_to, Delegated_to) -> Eq
          | (Dir, Dir) -> Eq
          | (Encoding, Encoding) -> Eq
          | (Media_type, Media_type) -> Eq
          | (Fbtype, Fbtype) -> Eq
          | (Language, Language) -> Eq
          | (Member, Member) -> Eq
          | (Partstat, Partstat) -> Eq
          | (Range, Range) -> Eq
          | (Related, Related) -> Eq
          | (Reltype, Reltype) -> Eq
          | (Role, Role) -> Eq
          | (Rsvp, Rsvp) -> Eq
          | (Sentby, Sentby) -> Eq
          | (Tzid, Tzid) -> Eq
          | (Valuetype, Valuetype) -> Eq
          | (Iana_param a, Iana_param a') ->
            begin match String.compare a a' with
              | 0 -> Eq
              | x when x < 0 -> Lt
              | _ -> Gt
            end
          | (Xparam (a, b), Xparam (a', b')) ->
            begin match String.compare a a' with
              | 0 ->
                begin match String.compare b b' with
                  | 0 -> Eq
                  | y when y < 0 -> Lt
                  | _ -> Gt
                end
              | x when x < 0 -> Lt
              | _ -> Gt
            end
          | _ ->
              let to_int : type a. a icalparameter -> int =
                function
                | Altrep -> 0
                | Cn -> 1
                | Cutype -> 2
                | Delegated_from -> 3
                | Delegated_to -> 4
                | Dir -> 5
                | Encoding -> 6
                | Media_type -> 7
                | Fbtype -> 8
                | Language -> 9
                | Member -> 10
                | Partstat -> 11
                | Range -> 12
                | Related -> 13
                | Reltype -> 14
                | Role -> 15
                | Rsvp -> 16
                | Sentby -> 17
                | Tzid -> 18
                | Valuetype -> 19
                | Iana_param _ -> 20
                | Xparam _ -> 21 in
              if Stdlib.compare (to_int lhs) (to_int rhs) < 0
              then Lt else Gt
end

module Params = Gmap.Make(K)

type params = Params.t

let equal_params m m' =
  Params.equal { f = equal_icalparameter } m m'

let pp_params ppf m = Params.iter
    (fun (Params.B (k, v)) ->
       pp_icalparameter ppf k v ;
       Format.pp_print_space ppf ()) m

type other_prop =
  [ `Iana_prop of string * params * string
  | `Xprop of (string * string) * params * string ] [@@deriving eq, show]

type cal_prop =
  [ `Prodid of params * string
  | `Version of params * string
  | `Calscale of params * string
  | `Method of params * string
  | other_prop
  ] [@@deriving eq, show]

type weekday = [ `Friday | `Monday | `Saturday | `Sunday | `Thursday | `Tuesday | `Wednesday ] [@@deriving eq, show]

type recur = [
  | `Byminute of int list
  | `Byday of (int * weekday) list
  | `Byhour of int list
  | `Bymonth of int list
  | `Bymonthday of int list
  | `Bysecond of int list
  | `Bysetposday of int list
  | `Byweek of int list
  | `Byyearday of int list
  | `Weekday of weekday
] [@@deriving eq, show]

type freq = [ `Daily | `Hourly | `Minutely | `Monthly | `Secondly | `Weekly | `Yearly ] [@@deriving eq, show]

type count_or_until = [
  | `Count of int
  | `Until of utc_or_timestamp_local
] [@@deriving eq, show]

type interval = int [@@deriving eq, show]

type recurrence = freq * count_or_until option * interval option * recur list [@@deriving eq, show]

type class_ = [ `Public | `Private | `Confidential | `Ianatoken of string | `Xname of string * string ] [@@deriving eq, show]

type status = [ `Draft | `Final | `Cancelled |
                `Needs_action | `Completed | `In_process | (* `Cancelled *)
                `Tentative | `Confirmed (* | `Cancelled *) ] [@@deriving eq, show]

type period = timestamp * Ptime.Span.t * bool [@@deriving eq, show]
type period_utc = timestamp_utc * Ptime.Span.t * bool [@@deriving eq, show]

type dates_or_datetimes = [ `Datetimes of timestamp list | `Dates of Ptime.date list ] [@@deriving eq, show]
type dates_or_datetimes_or_periods = [ dates_or_datetimes | `Periods of period list ] [@@deriving eq, show]

type general_prop = [
  | `Dtstamp of params * timestamp_utc
  | `Uid of params * string
  | `Dtstart of params * date_or_datetime
  | `Class of params * class_
  | `Created of params * timestamp_utc
  | `Description of params * string
  | `Geo of params * (float * float)
  | `Lastmod of params * timestamp_utc
  | `Location of params * string
  | `Organizer of params * Uri.t
  | `Priority of params * int
  | `Seq of params * int
  | `Status of params * status
  | `Summary of params * string
  | `Url of params * Uri.t
  | `Recur_id of params * date_or_datetime
   (* TODO: Furthermore, this property MUST be specified
      as a date with local time if and only if the "DTSTART" property
      contained within the recurring component is specified as a date
      with local time. *)
  | `Rrule of params * recurrence
  | `Duration of params * Ptime.Span.t
  | `Attach of params * [ `Uri of Uri.t | `Binary of string ]
  | `Attendee of params * Uri.t
  | `Categories of params * string list
  | `Comment of params * string
  | `Contact of params * string
  | `Exdate of params * dates_or_datetimes
  | `Rstatus of params * ((int * int * int option) * string * string option)
  | `Related of params * string
  | `Resource of params * string list
  | `Rdate of params * dates_or_datetimes_or_periods
] [@@deriving eq, show]

type event_prop = [
  | general_prop
  | `Transparency of params * [ `Transparent | `Opaque ]
  | `Dtend of params * date_or_datetime
  (* TODO: valuetype same as DTSTART *)
  | other_prop
] [@@deriving eq, show]

type 'a alarm_struct = {
  trigger : params * [ `Duration of Ptime.Span.t | `Datetime of timestamp_utc ] ;
  duration_repeat: ((params * Ptime.Span.t) * (params * int )) option ;
  other: other_prop list ;
  special: 'a ;
} [@@deriving eq, show]

type audio_struct = {
  attach: (params * [ `Uri of Uri.t | `Binary of string ]) option ;
} [@@deriving eq, show]

type display_struct = {
  description : params * string ;
} [@@deriving eq, show]

type email_struct = {
  description : params * string ;
  summary : params * string ;
  attendees : (params * Uri.t) list ;
  attach: (params * [ `Uri of Uri.t | `Binary of string ]) option ;
} [@@deriving eq, show]

type alarm = [
  | `Audio of audio_struct alarm_struct
  | `Display of display_struct alarm_struct
  | `Email of email_struct alarm_struct
  | `None of unit alarm_struct
] [@@deriving eq, show]

type tz_prop = [
  | `Dtstart_local of params * timestamp_local
  | `Tzoffset_to of params * Ptime.Span.t
  | `Tzoffset_from of params * Ptime.Span.t
  | `Rrule of params * recurrence
  | `Comment of params * string
  | `Rdate of params * dates_or_datetimes_or_periods
  | `Tzname of params * string
  | other_prop
] [@@deriving eq, show]

type timezone_prop = [
  | `Timezone_id of params * (bool * string)
  | `Lastmod of params * timestamp_utc
  | `Tzurl of params * Uri.t
  | `Standard of tz_prop list
  | `Daylight of tz_prop list
  | other_prop
] [@@deriving eq, show]

type todo_prop = [
  | general_prop
  | `Completed of params * timestamp_utc
  | `Percent of params * int
  | `Due of  params * date_or_datetime
  | other_prop
] [@@deriving eq, show]

type freebusy_prop = [
  | `Dtstamp of params * timestamp_utc
  | `Uid of params * string
  | `Contact of params * string
  | `Dtstart_utc of params * timestamp_utc
  | `Dtend_utc of params * timestamp_utc
  | `Organizer of params * Uri.t
  | `Url of params * Uri.t
  | `Attendee of params * Uri.t
  | `Comment of params * string
  | `Freebusy of params * period_utc list
  | `Rstatus of params * ((int * int * int option) * string * string option)
  | other_prop
] [@@deriving eq, show]

type event = {
  dtstamp : params * timestamp_utc ;
  uid : params * string ;
  dtstart : params * date_or_datetime ; (* NOTE: optional if METHOD present according to RFC 5545 *)
  dtend_or_duration : [ `Duration of params * Ptime.Span.t | `Dtend of params * date_or_datetime ] option ;
  rrule : (params * recurrence) option ; (* NOTE: RFC says SHOULD NOT occur more than once *)
  props : event_prop list ;
  alarms : alarm list ;
} [@@deriving eq, show]

type timezone = timezone_prop list [@@deriving eq, show]

type component = [
  | `Event of event
  | `Todo of todo_prop list * alarm list
  | `Freebusy of freebusy_prop list
  | `Timezone of timezone
] [@@deriving eq, show]

type calendar = cal_prop list * component list [@@deriving eq, show]

let component_to_ics_key = function
  | `Event _ -> "VEVENT"
  | `Todo _ -> "VTODO"
  | `Freebusy _ -> "VFREEBUSY"
  | `Timezone _ -> "VTIMEZONE"

let pp = pp_calendar

let weekday_strings = [
    (`Monday, "MO") ; (`Tuesday, "TU") ; (`Wednesday, "WE") ;
    (`Thursday, "TH") ; (`Friday, "FR") ; (`Saturday, "SA") ;
    (`Sunday, "SU")
  ]

let valuetype_strings = [
    (`Binary, "BINARY") ;
    (`Boolean, "BOOLEAN") ;
    (`Caladdress, "CAL-ADDRESS") ;
    (`Datetime, "DATE-TIME") ;
    (`Date, "DATE") ;
    (`Duration, "DURATION") ;
    (`Float, "FLOAT") ;
    (`Integer, "INTEGER") ;
    (`Period, "PERIOD") ;
    (`Recur, "RECUR") ;
    (`Text, "TEXT") ;
    (`Time, "TIME") ;
    (`Uri, "URI") ;
    (`Utcoffset, "UTC-OFFSET") ;
  ]

let cutype_strings = [
    (`Individual, "INDIVIDUAL") ;
    (`Group, "GROUP") ;
    (`Resource, "RESOURCE") ;
    (`Room, "ROOM") ;
    (`Unknown, "UNKNOWN") ;
  ]

let fbtype_strings = [
    (`Busy_Unavailable, "BUSY-UNAVAILABLE") ;
    (`Busy_Tentative, "BUSY-TENTATIVE") ;
    (`Free, "FREE") ;
    (`Busy, "BUSY") ;
  ]

let partstat_strings = [
    (`Needs_action, "NEEDS-ACTION") ;
    (`Accepted, "ACCEPTED") ;
    (`Declined, "DECLINED") ;
    (`Tentative, "TENTATIVE") ;
    (`Delegated, "DELEGATED") ;
    (`Completed, "COMPLETED") ;
    (`In_process, "IN-PROCESS") ;
  ]

let role_strings = [
    (`Chair, "CHAIR") ;
    (`Reqparticipant, "REQ-PARTICIPANT") ;
    (`Optparticipant, "OPT-PARTICIPANT") ;
    (`Nonparticipant, "NON-PARTICIPANT") ;
  ]

let status_strings = [
    (`Draft, "DRAFT") ;
    (`Final, "FINAL") ;
    (`Cancelled, "CANCELLED") ;
    (`Needs_action, "NEEDS-ACTION") ;
    (`Completed, "COMPLETED") ;
    (`In_process, "IN-PROCESS") ;
    (`Tentative, "TENTATIVE") ;
    (`Confirmed, "CONFIRMED") ;
  ]

let freq_strings = [
    (`Daily, "DAILY") ;
    (`Hourly, "HOURLY") ;
    (`Minutely, "MINUTELY") ;
    (`Monthly, "MONTHLY") ;
    (`Secondly, "SECONDLY") ;
    (`Weekly, "WEEKLY") ;
    (`Yearly, "YEARLY") ;
  ]

let relation_strings = [
    (`Parent, "PARENT") ;
    (`Child, "CHILD") ;
    (`Sibling, "SIBLING") ;
  ]

let class_strings = [
    (`Public, "PUBLIC") ;
    (`Private, "PRIVATE") ;
    (`Confidential, "CONFIDENTIAL") ;
  ]

let transp_strings = [
    (`Transparent, "TRANSPARENT") ;
    (`Opaque, "OPAQUE") ;
  ]

(* TODO this actually belongs to CalDAV! this is Webdav_xml module! *)
type comp = [ `Allcomp | `Comp of component_transform list ]
and prop = [ `Allprop | `Prop of (string * bool) list ]
and component_transform = string * prop * comp [@@deriving show, eq]

let add_span ts span = match Ptime.add_span ts span with
  | None -> assert false
  | Some ts' -> ts'

let add_span_to_ts ts span = match ts with
  | `Utc ts' -> `Utc (add_span ts' span)
  | `Local ts' -> `Local (add_span ts' span)
  | `With_tzid (ts', tzid) -> `With_tzid (add_span ts' span, tzid)

module Writer = struct
  let print_x vendor token = Printf.sprintf "X-%s%s%s" vendor (if String.length vendor = 0 then "" else "-") token

  let write_param_value = function
    | `String s -> s
    | `Quoted s -> "\"" ^ s ^ "\""

  let write_param : type a . Buffer.t -> a icalparameter -> a -> unit = fun buf k v ->
    let write_kv k v =
      Buffer.add_string buf k ;
      Buffer.add_char buf '=' ;
      Buffer.add_string buf v
    and quoted str =
      Printf.sprintf "%S" str
    in
    let quoted_uri uri = quoted (Uri.to_string uri) in
    match k, v with
    | Iana_param token, values -> write_kv token (String.concat "," (List.map write_param_value values))
    | Xparam (vendor, name), values -> write_kv (print_x vendor name) (String.concat "," (List.map write_param_value values))
    | Valuetype, v -> write_kv "VALUE" (List.assoc v valuetype_strings)
    | Tzid, (prefix, str) -> write_kv "TZID" (Printf.sprintf "%s%s" (if prefix then "/" else "") str)
    | Altrep, uri -> write_kv "ALTREP" (quoted_uri uri)
    | Language, lan -> write_kv "LANGUAGE" lan
    | Cn, str -> write_kv "CN" (write_param_value str)
    | Dir, uri -> write_kv "DIR" (quoted_uri uri)
    | Sentby, uri -> write_kv "SENT-BY" (quoted_uri uri)
    | Range, `Thisandfuture -> write_kv "RANGE" "THISANDFUTURE"
    | Media_type, (pre, post) -> write_kv "FMTTYPE" (Printf.sprintf "%s/%s" pre post)
    | Fbtype, fbtype -> write_kv "FBTYPE" (List.assoc fbtype fbtype_strings)
    | Encoding, `Base64 -> write_kv "ENCODING" "BASE64"
    | Cutype, cu -> write_kv "CUTYPE" (List.assoc cu cutype_strings)
    | Delegated_from, uris -> write_kv "DELEGATED-FROM" (String.concat "," (List.map quoted_uri uris))
    | Delegated_to, uris -> write_kv "DELEGATED-TO" (String.concat "," (List.map quoted_uri uris))
    | Member, uris -> write_kv "MEMBER" (String.concat "," (List.map quoted_uri uris))
    | Partstat, ps -> write_kv "PARTSTAT" (List.assoc ps partstat_strings)
    | Role, role -> write_kv "ROLE" (List.assoc role role_strings)
    | Rsvp, rsvp -> write_kv "RSVP" (if rsvp then "TRUE" else "FALSE")
    | Reltype, rel -> write_kv "RELTYPE" (List.assoc rel relation_strings)
    | Related, r ->
      let r = match r with `Start -> "START" | `End -> "END" in
      write_kv "RELATED" r

  let write_line cr buf name params ?(dont_write_value=false) value_writer =
    let write = Buffer.add_string buf in
    let write_char = Buffer.add_char buf in
    write name ;
    Params.iter (fun param ->
        let Params.B (paramk, paramv) = param in
        write_char ';' ;
        write_param buf paramk paramv)
      params ;
    write_char ':' ;
    if not dont_write_value then value_writer buf ;
    if cr then write_char '\r' ;
    write_char '\n'

  let escape_chars str =
    let replacements = [
      '\\', {|\\|} ; ';', {|\;|} ; ',', {|\,|} ; '\n', {|\n|}
    ]
    in
    List.fold_left (fun str (s, replacement) ->
        String.concat replacement (String.split_on_char s str))
      str replacements

  let write_string str buf = Buffer.add_string buf str

  let write_begin_end cr buf tag inside =
    write_line cr buf "BEGIN" Params.empty (write_string tag) ;
    inside (); (* delay because buffer is imperative *)
    write_line cr buf "END" Params.empty (write_string tag)

  let other_prop_to_ics_key (prop: other_prop) = match prop with
    | `Iana_prop (ianatoken, _, _) -> ianatoken
    | `Xprop ((vendor, token), _, _) -> print_x vendor token

  let cal_prop_to_ics_key (prop: cal_prop) = match prop with
    | `Prodid _ -> "PRODID"
    | `Version _ -> "VERSION"
    | `Calscale _ -> "CALSCALE"
    | `Method _ -> "METHOD"
    | #other_prop as x -> other_prop_to_ics_key x

  let other_prop_to_ics cr buf ?dont_write_value prop =
    let key = other_prop_to_ics_key prop in
    match prop with
    | `Iana_prop (_, params, value) -> write_line cr buf key params ?dont_write_value (write_string value)
    | `Xprop (_, params, value) -> write_line cr buf key params ?dont_write_value (write_string value)

  let write_prop_and_value name = function
    | `Allprop | `Prop [] -> true, false
    | `Prop ps -> match List.find_opt (fun (k, _) -> k = name) ps with
      | None -> false, true
      | Some (_, dont_print_value) -> true, dont_print_value

  let cal_prop_to_ics cr buf filter prop =
    let key = cal_prop_to_ics_key prop in
    let is_write_prop, dont_write_value = write_prop_and_value key filter in
    if not is_write_prop
    then ()
    else
      let output params value =
        write_line cr buf key params ~dont_write_value (write_string value)
      in
      match prop with
      | `Prodid (params, value) -> output params (escape_chars value)
      | `Version (params, value) -> output params value
      | `Calscale (params, value) -> output params value
      | `Method (params, value) -> output params value
      | #other_prop as x -> other_prop_to_ics cr buf ~dont_write_value x

  let cal_props_to_ics cr buf filter props =
    List.iter (cal_prop_to_ics cr buf filter) props

  let duration_to_ics span buf =
    let dur = match Ptime.Span.to_int_s span with
    | None -> assert false
    | Some x -> x in
    if dur < 0 then Buffer.add_char buf '-' ;
    Buffer.add_char buf 'P' ;
    let output_number i d c =
      Buffer.add_string buf (string_of_int (i / d)) ;
      Buffer.add_char buf c ;
      i mod d
    in
    let dur' = abs dur in
    let day = 24 * 60 * 60 in
    let week = 7 * day in
    if dur' mod week = 0 && dur' / week > 0
    then ignore (output_number dur' week 'W')
    else
      let rest =
        if dur' >= day
        then output_number dur' day 'D'
        else dur'
      in
      if dur' = 0 || rest > 0 then begin
        Buffer.add_char buf 'T' ;
        let hour = 60 * 60 in
        let rest' =
          if rest >= hour
          then output_number rest hour 'H'
          else rest
        in
        let rest'' =
          if (rest >= hour && rest' > 0) || rest' >= 60
          then output_number rest' 60 'M'
          else rest'
        in
        if dur' = 0 || rest'' > 0 then
          ignore (output_number rest'' 1 'S')
      end

  let date_to_str (y, m, d) =
    Printf.sprintf "%04d%02d%02d" y m d

  let date_to_ics buf date =
    Buffer.add_string buf (date_to_str date)

  let datetime_to_str ptime utc =
    let date, ((hh, mm, ss), _) = Ptime.to_date_time ptime in
    Printf.sprintf "%sT%02d%02d%02d%s" (date_to_str date) hh mm ss (if utc then "Z" else "")

  let timestamp_to_ics ts buf =
    Buffer.add_string buf @@ match ts with
    | `Utc ts -> datetime_to_str ts true
    | `Local ts -> datetime_to_str ts false
    | `With_tzid (ts, _str) -> (* TODO *) datetime_to_str ts false

  let date_or_time_to_ics dt buf = match dt with
    | `Date d -> date_to_ics buf d
    | `Datetime dt -> timestamp_to_ics dt buf

  let period_to_ics buf (start, span, was_explicit) =
    timestamp_to_ics start buf ;
    Buffer.add_char buf '/' ;
    if was_explicit then
      timestamp_to_ics (add_span_to_ts start span) buf
    else
      duration_to_ics span buf

  let dates_or_times_or_periods_to_ics dt buf =
    let swap f a b = f b a in
    match dt with
    | `Dates xs -> (match xs with
        | _ -> date_to_ics buf (List.hd xs);
          List.iter (fun x -> (Buffer.add_char buf ',';
                               date_to_ics buf x))
            (List.tl xs))
    | `Datetimes xs -> (match xs with
        | _ -> timestamp_to_ics (List.hd xs) buf;
          List.iter (fun x -> (Buffer.add_char buf ',';
                               swap timestamp_to_ics buf x))
            (List.tl xs))
    | `Periods xs -> List.iter (period_to_ics buf) xs

  let move_tzid_to_params timestamp params =
    match timestamp with
    | `Utc _ | `Local _ -> params
    | `With_tzid (_, tzid) -> Params.add Tzid tzid params

  let move_tzid_of_d_or_dt d_or_dt params =
    match d_or_dt with
    | `Date _ -> params
    | `Datetime x -> move_tzid_to_params x params

  let move_tzid_of_ds_or_dts_or_ps ds_or_dts_or_ps params =
    match ds_or_dts_or_ps with
    | `Datetimes (ts::_) -> move_tzid_to_params ts params (* head is sufficient, each element has same tzid *)
    | `Periods ((ts, _, _)::_) -> move_tzid_to_params ts params
    | `Dates _ -> params
    | _ -> params

  let recurs_to_ics (freq, count_or_until, interval, l) buf =
    let write_rulepart key value =
      Buffer.add_string buf key ;
      Buffer.add_char buf '=' ;
      Buffer.add_string buf value in
    let int_list l = String.concat "," @@ List.map string_of_int l in
    let recur_to_ics = function
      | `Byminute byminlist -> write_rulepart "BYMINUTE" (int_list byminlist)
      | `Byday bywdaylist ->
        let wday (weeknumber, weekday) =
          (if weeknumber = 0 then "" else string_of_int weeknumber) ^
            List.assoc weekday weekday_strings
        in
        write_rulepart "BYDAY" (String.concat "," @@ List.map wday bywdaylist)
      | `Byhour byhrlist -> write_rulepart "BYHOUR" (int_list byhrlist)
      | `Bymonth bymolist -> write_rulepart "BYMONTH" (int_list bymolist)
      | `Bymonthday bymodaylist -> write_rulepart "BYMONTHDAY" (int_list bymodaylist)
      | `Bysecond byseclist -> write_rulepart "BYSECOND" (int_list byseclist)
      | `Bysetposday bysplist -> write_rulepart "BYSETPOS" (int_list bysplist)
      | `Byweek bywknolist -> write_rulepart "BYWEEKNO" (int_list bywknolist)
      | `Byyearday byyrdaylist -> write_rulepart "BYYEARDAY" (int_list byyrdaylist)
      | `Weekday weekday -> write_rulepart "WKST" (List.assoc weekday weekday_strings)
    in
    write_rulepart "FREQ" (List.assoc freq freq_strings) ;
    ( match count_or_until with
      | None -> ()
      | Some x ->
        Buffer.add_char buf ';' ;
        match x with
        | `Count c -> write_rulepart "COUNT" (string_of_int c)
        | `Until enddate -> (* TODO cleanup *)
          Buffer.add_string buf "UNTIL=" ;
          timestamp_to_ics enddate buf) ;
    ( match interval with
      | None -> ()
      | Some i ->
        Buffer.add_char buf ';' ;
        write_rulepart "INTERVAL" (string_of_int i) ) ;
    List.iter (fun recur ->
        Buffer.add_char buf ';' ;
        recur_to_ics recur)
      l

  let general_prop_to_ics_key = function
    | `Dtstamp (_params, _ts) -> "DTSTAMP"
    | `Uid (_params, _str) -> "UID"
    | `Dtstart (_params, _date_or_time) -> "DTSTART"
    | `Class (_params, _class_) -> "CLASS"
    | `Created (_params, _ts) -> "CREATED"
    | `Description _desc -> "DESCRIPTION"
    | `Geo (_params, (_lat, _lon)) -> "GEO"
    | `Lastmod (_params, _ts) -> "LAST-MODIFIED"
    | `Location (_params, _name) -> "LOCATION"
    | `Organizer (_params, _uri) -> "ORGANIZER"
    | `Priority (_params, _prio) -> "PRIORITY"
    | `Seq (_params, _seq) -> "SEQUENCE"
    | `Status (_params, _status) -> "STATUS"
    | `Summary _summary -> "SUMMARY"
    | `Url (_params, _uri) -> "URL"
    | `Recur_id (_params, _date_or_time) -> "RECURRENCE-ID"
    | `Rrule (_params, _) -> "RRULE"
    | `Duration (_params, _dur) -> "DURATION"
    | `Attach _att -> "ATTACH"
    | `Attendee _att -> "ATTENDEE"
    | `Categories (_params, _cats) -> "CATEGORIES"
    | `Comment (_params, _comment) -> "COMMENT"
    | `Contact (_params, _contact) -> "CONTACT"
    | `Exdate (_params, _dates_or_times) -> "EXDATE"
    | `Rstatus (_params, (_statcode, _text, _comment)) -> "REQUEST-STATUS"
    | `Related (_params, _rel) -> "RELATED"
    | `Resource (_params, _res) -> "RESOURCE"
    | `Rdate (_params, _dates_or_times_or_periods) -> "RDATE"

  let general_prop_to_ics cr buf ?dont_write_value (prop : general_prop) =
    let key = general_prop_to_ics_key prop in
    let output params v = write_line cr buf key params ?dont_write_value v in
    match prop with
    | `Dtstamp (params, ts) -> output params (timestamp_to_ics (`Utc ts))
    | `Uid (params, str) -> output params (write_string (escape_chars str))
    | `Dtstart (params, date_or_time) ->
        output (move_tzid_of_d_or_dt date_or_time params) (date_or_time_to_ics date_or_time)
    | `Class (params, class_) -> output params (write_string (List.assoc class_ class_strings))
    | `Created (params, ts) -> output params (timestamp_to_ics (`Utc ts))
    | `Description (params, desc) -> output params (write_string (escape_chars desc))
    | `Geo (params, (lat, lon)) ->
      output params (fun buf ->
          Buffer.add_string buf (string_of_float lat) ;
          Buffer.add_char buf ';' ;
          Buffer.add_string buf (string_of_float lon))
    | `Lastmod (params, ts) -> output params (timestamp_to_ics (`Utc ts))
    | `Location (params, name) -> output params (write_string (escape_chars name))
    | `Organizer (params, uri) -> output params (write_string (Uri.to_string uri))
    | `Priority (params, prio) -> output params (write_string (string_of_int prio))
    | `Seq (params, seq) -> output params (write_string (string_of_int seq))
    | `Status (params, status) -> output params (write_string (List.assoc status status_strings))
    | `Summary (params, summary) -> output params (write_string (escape_chars summary))
    | `Url (params, uri) -> output params (write_string Uri.(pct_decode (to_string uri)))
    | `Recur_id (params, date_or_time) -> output (move_tzid_of_d_or_dt date_or_time params) (date_or_time_to_ics date_or_time)
    | `Rrule (params, recurs) -> output params (recurs_to_ics recurs)
    | `Duration (params, dur) -> output params (duration_to_ics dur)
    | `Attach (params, att) ->
      let value' = match att with
        | `Uri uri -> Uri.to_string uri
        | `Binary s -> s
      in
      output params (write_string value')
    | `Attendee (params, uri) -> output params (write_string (Uri.to_string uri))
    | `Categories (params,cats) ->
      let cat = String.concat "," (List.map escape_chars cats) in
      output params (write_string cat)
    | `Comment (params, comment) -> output params (write_string (escape_chars comment))
    | `Contact (params, contact) -> output params (write_string (escape_chars contact))
    | `Exdate (params, dates_or_times) ->
      let ds_or_ts_or_ps = (dates_or_times :> [ `Dates of Ptime.date list | `Datetimes of timestamp list | `Periods of period list ]) in
      output (move_tzid_of_ds_or_dts_or_ps ds_or_ts_or_ps params) (dates_or_times_or_periods_to_ics ds_or_ts_or_ps)
    | `Rstatus (params, (statcode, text, comment)) ->
      output params
        (fun buf ->
           let (major, minor, patch) = statcode in
           Buffer.add_string buf (string_of_int major) ;
           Buffer.add_char buf '.' ;
           Buffer.add_string buf (string_of_int minor) ;
           (match patch with
            | None -> ()
            | Some m ->
              Buffer.add_char buf '.' ;
              Buffer.add_string buf (string_of_int m)) ;
           Buffer.add_char buf ';' ;
           Buffer.add_string buf (escape_chars text) ;
           match comment with
           | None -> ()
           | Some x ->
             Buffer.add_char buf ';' ;
             Buffer.add_string buf (escape_chars x))
    | `Related (params, rel) -> output params (write_string (escape_chars rel))
    | `Resource (params, res) ->
      let r = String.concat "," (List.map escape_chars res) in
      output params (write_string r)
    | `Rdate (params, dates_or_times_or_periods) ->
      output (move_tzid_of_ds_or_dts_or_ps dates_or_times_or_periods params)
        (dates_or_times_or_periods_to_ics dates_or_times_or_periods)

  let event_prop_to_ics_key = function
    | #general_prop as x -> general_prop_to_ics_key x
    | #other_prop as x -> other_prop_to_ics_key x
    | `Transparency _ -> "TRANSP"
    | `Dtend _ -> "DTEND"

  let event_prop_to_ics cr buf filter (prop: event_prop) =
    let key = event_prop_to_ics_key prop in
    let is_write_prop, dont_write_value = write_prop_and_value key filter in
    if not is_write_prop then ()
    else
      let output params v = write_line cr buf key params ~dont_write_value v in
      match prop with
      | #general_prop as x -> general_prop_to_ics cr buf ~dont_write_value x
      | #other_prop as x -> other_prop_to_ics cr buf ~dont_write_value x
      | `Transparency (params, transp) -> output params (write_string (List.assoc transp transp_strings))
      | `Dtend (params, date_or_time) -> output (move_tzid_of_d_or_dt date_or_time params) (date_or_time_to_ics date_or_time)

  let event_props_to_ics cr buf filter props = List.iter (event_prop_to_ics cr buf filter) props

  let todo_prop_to_ics_key = function
    | #general_prop as x -> general_prop_to_ics_key x
    | #other_prop as x -> other_prop_to_ics_key x
    | `Completed _ -> "COMPLETED"
    | `Percent _ -> "PERCENT"
    | `Due _ -> "DUE"

  let todo_prop_to_ics cr buf filter prop =
    let key = todo_prop_to_ics_key prop in
    let is_write_prop, dont_write_value = write_prop_and_value key filter in
    if not is_write_prop then ()
    else
      let output params v = write_line cr buf key params ~dont_write_value v in
      match prop with
      | #general_prop as x -> general_prop_to_ics cr buf ~dont_write_value x
      | #other_prop as x -> other_prop_to_ics cr buf ~dont_write_value x
      | `Completed (params, ts) -> output params (timestamp_to_ics (`Utc ts))
      | `Percent (params, pct) -> output params (write_string (string_of_int pct))
      | `Due (params, date_or_time) -> output (move_tzid_of_d_or_dt date_or_time params) (date_or_time_to_ics date_or_time)

  let todo_props_to_ics cr buf filter props = List.iter (todo_prop_to_ics cr buf filter) props

  let filter_and_write_component component_writer name = function
    | `Allcomp | `Comp [] -> component_writer `Allprop `Allcomp
    | `Comp cs -> match List.find_opt (fun (k, _, _) -> k = name) cs with
      | None -> ()
      | Some (_, prop_filter, comp_filter) -> component_writer prop_filter comp_filter

  let alarm_to_ics cr buf filter alarm =
    let component_writer prop_filter _ =
      let prop_to_ics = function
        | `Action tag ->
          let is_write_prop, dont_write_value = write_prop_and_value "ACTION" prop_filter in
          if not is_write_prop then ()
          else write_line cr buf "ACTION" Params.empty ~dont_write_value (write_string tag)
        | `Trigger (params, value) ->
          let is_write_prop, dont_write_value = write_prop_and_value "TRIGGER" prop_filter in
          if not is_write_prop then ()
          else
            let print = match value with
              | `Duration d -> duration_to_ics d
              | `Datetime dt -> timestamp_to_ics (`Utc dt)
            in
            write_line cr buf "TRIGGER" params ~dont_write_value print
        | `Duration_repeat None -> ()
        | `Duration_repeat (Some ((durparams, dur), (repparams, rep))) ->
          let is_write_prop, dont_write_value = write_prop_and_value "DURATION" prop_filter in
          if not is_write_prop then ()
          else
            write_line cr buf "DURATION" durparams ~dont_write_value (duration_to_ics dur) ;
          let is_write_prop, dont_write_value = write_prop_and_value "REPEAT" prop_filter in
          if not is_write_prop then ()
          else
            write_line cr buf "REPEAT" repparams ~dont_write_value (write_string (string_of_int rep))
        | #general_prop as prop ->
          let key = event_prop_to_ics_key prop in
          let is_write_prop, dont_write_value = write_prop_and_value key prop_filter in
          if not is_write_prop then ()
          else general_prop_to_ics cr buf ~dont_write_value prop
        | #other_prop as prop ->
          let key = other_prop_to_ics_key prop in
          let is_write_prop, dont_write_value = write_prop_and_value key prop_filter in
          if not is_write_prop then ()
          else other_prop_to_ics cr buf ~dont_write_value prop
      in
      write_begin_end cr buf "VALARM" @@ fun () ->
      match alarm with
       | `Audio (audio : audio_struct alarm_struct) ->
         prop_to_ics (`Action "AUDIO") ;
         prop_to_ics (`Trigger audio.trigger) ;
         prop_to_ics (`Duration_repeat audio.duration_repeat) ;
         List.iter prop_to_ics audio.other;
         (match audio.special.attach with None -> () | Some attach -> prop_to_ics (`Attach attach))
       | `Display (display : display_struct alarm_struct) ->
         prop_to_ics (`Action "DISPLAY") ;
         prop_to_ics (`Trigger display.trigger) ;
         prop_to_ics (`Duration_repeat display.duration_repeat) ;
         prop_to_ics (`Description display.special.description) ;
         List.iter prop_to_ics display.other
       | `Email (email : email_struct alarm_struct) ->
         prop_to_ics (`Action "EMAIL") ;
         prop_to_ics (`Trigger email.trigger) ;
         prop_to_ics (`Duration_repeat email.duration_repeat) ;
         (match email.special.attach with None -> () | Some attach -> prop_to_ics (`Attach attach));
         prop_to_ics (`Description email.special.description) ;
         prop_to_ics (`Summary email.special.summary) ;
         List.iter (fun attendee -> prop_to_ics (`Attendee attendee)) email.special.attendees;
         List.iter prop_to_ics email.other
       | `None (alarm : unit alarm_struct) ->
         prop_to_ics (`Action "NONE") ;
         prop_to_ics (`Trigger alarm.trigger) ;
         prop_to_ics (`Duration_repeat alarm.duration_repeat) ;
         List.iter prop_to_ics alarm.other
    in
    filter_and_write_component component_writer "VALARM" filter

  let alarms_to_ics cr buf filter alarms =
    List.iter (alarm_to_ics cr buf filter) alarms

  let event_to_ics cr buf prop_filter comp_filter event =
    let prop_to_ics = event_prop_to_ics cr buf prop_filter in
    prop_to_ics (`Uid event.uid) ;
    prop_to_ics (`Dtstamp event.dtstamp) ;
    prop_to_ics (`Dtstart event.dtstart) ;
    (match event.dtend_or_duration with
     | Some x -> prop_to_ics (x :> event_prop)
     | None -> ()) ;
    (match event.rrule with
     | Some x -> prop_to_ics (`Rrule x)
     | None -> ()) ;
    event_props_to_ics cr buf prop_filter event.props ;
    alarms_to_ics cr buf comp_filter event.alarms

  let todo_to_ics cr buf prop_filter comp_filter todoprops alarms =
    todo_props_to_ics cr buf prop_filter todoprops ;
    alarms_to_ics cr buf comp_filter alarms

  let span_to_string span =
    match Ptime.Span.to_int_s span with
    | None -> assert false
    | Some seconds ->
      let sign = if seconds >= 0 then "+" else "-" in
      let abs_seconds = abs seconds in
      let hours, rest = abs_seconds / (60 * 60), abs_seconds mod (60 * 60) in
      let minutes, seconds = rest / 60, rest mod 60 in
      Printf.sprintf "%s%02d%02d%s" sign hours minutes
        (if seconds = 0 then "" else Printf.sprintf "%02d" seconds)

  let tz_prop_to_ics cr buf = function
    | `Dtstart_local (params, ts) -> write_line cr buf "DTSTART" params (timestamp_to_ics (`Local ts))
    | `Tzoffset_to (params, span) -> write_line cr buf "TZOFFSETTO" params (write_string (span_to_string span))
    | `Tzoffset_from (params, span) -> write_line cr buf "TZOFFSETFROM" params (write_string (span_to_string span))
    | `Rrule (params, recurs) -> write_line cr buf "RRULE" params (recurs_to_ics recurs)
    | `Comment (params, comment) -> write_line cr buf "COMMENT" params (write_string comment)
    | `Rdate (params, dates_or_times_or_periods) ->
      write_line cr buf "RDATE" params
        (dates_or_times_or_periods_to_ics dates_or_times_or_periods)
    | `Tzname (params, id) -> write_line cr buf "TZNAME" params (write_string (escape_chars id))
    | #other_prop as x -> other_prop_to_ics cr buf x

  let tz_props_to_ics cr buf tzprops = List.iter (tz_prop_to_ics cr buf) tzprops

  let timezone_prop_to_ics_key = function
    | `Timezone_id _ -> "TZID"
    | `Lastmod _ -> "LAST-MODIFIED"
    | `Tzurl _ -> "TZURL"
    | `Standard _tzprops -> "STANDARD" (* TODO preserve structure *)
    | `Daylight _tzprops -> "DAYLIGHT"
    | #other_prop as x -> other_prop_to_ics_key x

  let timezone_prop_to_ics cr buf prop_filter prop =
    let key = timezone_prop_to_ics_key prop in
    let is_write_prop, dont_write_value = write_prop_and_value key prop_filter in
    if not is_write_prop
    then ()
    else match prop with
      | `Timezone_id (params, (prefix, name)) ->
        let value = Printf.sprintf "%s%s" (if prefix then "/" else "") (escape_chars name) in
        write_line cr buf "TZID" params ~dont_write_value (write_string value)
      | `Lastmod (params, ts) -> write_line cr buf "LAST-MODIFIED" params ~dont_write_value (timestamp_to_ics (`Utc ts))
      | `Tzurl (params, uri) -> write_line cr buf "TZURL" params ~dont_write_value (write_string (Uri.to_string uri))
      | `Standard tzprops ->
        write_begin_end cr buf "STANDARD" @@ fun () ->
        tz_props_to_ics cr buf tzprops
      | `Daylight tzprops ->
        write_begin_end cr buf "DAYLIGHT" @@ fun () ->
        tz_props_to_ics cr buf tzprops
      | #other_prop as x -> other_prop_to_ics cr buf ~dont_write_value x

  let freebusy_prop_to_ics_key = function
    | `Dtend_utc _ -> "DTEND"
    | `Dtstart_utc _ -> "DTSTART"
    | `Freebusy _ -> "FREEBUSY"
    | #general_prop as x -> general_prop_to_ics_key x
    | #other_prop as x -> other_prop_to_ics_key x

  let freebusy_prop_to_ics cr buf filter prop =
    let key = freebusy_prop_to_ics_key prop in
    let is_write_prop, dont_write_value = write_prop_and_value key filter in
    if not is_write_prop
    then ()
    else
      match prop with
      | `Freebusy (params, periods) ->
        let periods' = List.map (fun (start, duration, was_explicit) -> (`Utc start, duration, was_explicit)) periods in
        write_line cr buf key params ~dont_write_value (fun buf -> List.iter (period_to_ics buf) periods')
      | `Dtend_utc (params, ts) -> write_line cr buf key params ~dont_write_value (timestamp_to_ics (`Utc ts))
      | `Dtstart_utc (params, ts) -> write_line cr buf key params ~dont_write_value (timestamp_to_ics (`Utc ts))
      | #general_prop as x -> general_prop_to_ics cr buf ~dont_write_value x
      | #other_prop as x -> other_prop_to_ics cr buf ~dont_write_value x

  let timezone_to_ics cr buf filter props =
    List.iter (timezone_prop_to_ics cr buf filter) props

  let freebusy_to_ics cr buf filter props =
    List.iter (freebusy_prop_to_ics cr buf filter) props

  let component_to_ics cr buf filter comp =
    let key = component_to_ics_key comp in
    let component_writer prop_filter comp_filter =
      write_begin_end cr buf key @@ fun () ->
      match comp with
       | `Event event -> event_to_ics cr buf prop_filter comp_filter event
       | `Timezone tzprops -> timezone_to_ics cr buf prop_filter tzprops
       | `Freebusy fbprops -> freebusy_to_ics cr buf prop_filter fbprops
       | `Todo (todoprops, alarms) -> todo_to_ics cr buf prop_filter comp_filter todoprops alarms
    in
    filter_and_write_component component_writer key filter

  let components_to_ics cr buf filter comps = List.iter (component_to_ics cr buf filter) comps

  let calendar_to_ics cr buf filter (props, comps) =
    let write_calendar prop_filter comp_filter =
      write_begin_end cr buf "VCALENDAR" @@ fun () ->
        cal_props_to_ics cr buf prop_filter props ;
        components_to_ics cr buf comp_filter comps
    in
    match filter with
    | None -> write_calendar `Allprop `Allcomp
    | Some (comp_name, prop_filter, comp_filter) ->
      if comp_name = "VCALENDAR"
      then write_calendar prop_filter comp_filter
      else ()
end


let to_ics ?(cr = true) ?(filter = None) (calendar : calendar) =
  let buf = Buffer.create 1023 in
  Writer.calendar_to_ics cr buf filter calendar ;
  Buffer.contents buf

open Angstrom
exception Parse_error of string

let string_parsers m =
  List.map (fun (t, str) -> string str >>| fun _ -> t) m

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

let sign = (char '+' >>| fun _ -> positive) <|> (char '-' >>| fun _ -> not positive)
let opt_sign = option positive sign

(* base grammar *)
let is_alpha_digit = function '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_alpha_digit_minus c = is_alpha_digit c || c = '-'
let name = take_while1 is_alpha_digit_minus

let is_control = function '\x00' .. '\x08' | '\x0a' .. '\x1f' | '\x7f' -> true | _ -> false
let is_qsafe_char = function x when is_control x -> false | '"' -> false | _ -> true

let quoted_string =
  lift (fun x -> `Quoted x)
    (char '"' *> take_while1 is_qsafe_char <* char '"')

let is_safe_char = function x when is_control x -> false | '"' | ';' | ':' | ',' -> false | _ -> true
let param_text = take_while1 is_safe_char

let param_value = (param_text >>| fun s -> `String s) <|> quoted_string (* in contrast to rfc we require at least 1 char for param_value *)

let value_list = sep_by1 (char ',') param_value

let value = take_while (fun x -> not (is_control x)) (* in fact it is more complicated *)

let iana_token = name

(* from OCaml 4.13 bytes.ml *)
let for_all p s =
  let n = String.length s in
  let rec loop i =
    if i = n then true
    else if p (String.unsafe_get s i) then loop (succ i)
    else false in
  loop 0

let is_valid p str =
  if for_all p str then
    return str
  else
    fail "parse error"

let up_to_two p = (take 2 >>= is_valid p) <|> (take 1 >>= is_valid p)
let up_to_three p = (take 3 >>= is_valid p) <|> up_to_two p

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
  many1 (tsafe_char <|> string ":" <|> string "\"" <|> escaped_char) >>| String.concat ""

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
    | Some p -> if utc then `Utc p else `Local p
    | None -> raise (Parse_error "datetime")
  in
  lift2 ptime date (char 'T' *> time)

let utc_only = function
  | `Utc ts -> return ts
  | `Local _ -> fail "timestamp must be in UTC"

let parse_datetime s =
  try parse_string ~consume:Consume.All datetime s with Parse_error s -> Error s

let dur_value =
  let to_seconds p factor = p >>= ensure int_of_string >>| ( * ) factor in
  let second = to_seconds (digits <* char 'S') 1 in
  let minute = lift2 (+) (to_seconds (digits <* char 'M') 60) (option 0 second) in
  let hour = lift2 (+) (to_seconds (digits <* char 'H') 3600) (option 0 minute) in
  (* Apple's CCS-caldavtester uses DURATION:P1DT, which does not conform to RFC 5545 Section 3.3.6 *)
  let time = (char 'T' *> (hour <|> minute <|> second)) <|> (char 'T' >>| fun _ -> 0)
  and day = to_seconds (digits <* char 'D') (24 * 3600) in
  let date = lift2 (+) day (option 0 time)
  and week = to_seconds (digits <* char 'W') (7 * 24 * 3600)
  and apply_sign s n = if s = positive then n else (- n) in
  let to_span s n = Ptime.Span.of_int_s (apply_sign s n) in
  lift2 to_span (opt_sign <* char 'P') (date <|> time <|> week)

let float =
  let make_float s i f =
    let n = try float_of_string (i ^ "." ^ f) with Failure _ -> raise (Parse_error "float") in
    if s = positive then n else (-. n) in
  lift3 make_float opt_sign digits (option "" ((char '.') *> digits))

let period =
  let to_period tstart tend = match tstart, tend with
    | `Utc s, `Utc e -> (`Utc s, Ptime.diff e s, true)
    | `Local s, `Local e -> (`Local s, Ptime.diff e s, true)
    | _ -> raise (Parse_error "period timestamps need to have the same format")
  in
  let explicit = lift2 to_period datetime (char '/' *> datetime)
  and duration = lift2 (fun a b -> (a, b, false)) datetime (char '/' *> dur_value) in
  explicit <|> duration

let recur =
  let up_to_two_digits = (take 2 >>= ensure int_of_string) <|> (take 1 >>= ensure int_of_string) in
  let up_to_three_digits = (take 3 >>= ensure int_of_string) <|> up_to_two_digits in
  let freq = choice (string_parsers freq_strings)
  and weekday = choice (string_parsers weekday_strings) in
  let apply_sign s i = (if s = positive then i else (-i)) in
  let apply_sign_triple s i c = (apply_sign s i, c) in
  let weekdaynum = lift3 apply_sign_triple opt_sign (option 0 (up_to_two_digits >>= in_range 1 53) ) weekday in
  let monthdaynum = lift2 apply_sign opt_sign (up_to_two_digits >>= in_range 1 31)
  and yeardaynum = lift2 apply_sign opt_sign (up_to_three_digits >>= in_range 1 366)
  and weeknum = lift2 apply_sign opt_sign (up_to_two_digits >>= in_range 1 53)
  and monthnum = up_to_two_digits >>= in_range 1 12
  and ts_of_date = date >>= fun d ->
    match Ptime.of_date d with
    | None -> fail "Parse_error recur: Ptime.of_date failed"
    | Some x -> return (`Utc x)
  in
  let recur_rule_part =
       ( string "FREQ=" *> freq >>| fun f -> `Frequency f )
   <|> ( string "UNTIL=" *> (datetime <|> ts_of_date) >>| fun u -> `Until u )
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
   <|> ( string "BYSETPOS=" *> (sep_by1 (char ',') yeardaynum) >>| fun d -> `Bysetposday d )
   <|> ( string "WKST=" *> weekday >>| fun d -> `Weekday d ) in
  lift (fun l ->
    let freqs, count_or_until, interval, rest =
      List.fold_left (fun (freqs, count_or_until, interval, rest) ->
        function | `Frequency f -> (f :: freqs), count_or_until, interval, rest
                 | `Count c -> freqs, `Count c :: count_or_until, interval, rest
                 | `Until u -> freqs, `Until u :: count_or_until, interval, rest
                 | `Interval i -> freqs, count_or_until, i :: interval, rest
                 | #recur as r -> freqs, count_or_until, interval, r :: rest) ([], [], [], []) l in
    let i' = match interval with
    | [] -> None
    | [ i ] -> Some i
    | _ -> raise (Parse_error "recur: interval") in
    let c' = match count_or_until with
    | [] -> None
    | [ c ] -> Some c
    | _ -> raise (Parse_error "recur: count_or_until") in
    match freqs with
    | [ f ] -> f, c', i', List.rev rest
    | _ -> raise (Parse_error "recur: frequency"))
  (sep_by1 (char ';') recur_rule_part)

(* out in the wild *)
let utcoffset =
  let hours = take 2 >>= ensure int_of_string >>= in_range 0 23
  and minutes = take 2 >>= ensure int_of_string >>= in_range 0 59
  and seconds = take 2 >>= ensure int_of_string >>= in_range 0 60
  in
  let to_span sign h m s =
    let factor = if sign = positive then 1 else (-1)
    and seconds = (h * 60 + m) * 60 + s in
    if sign = not positive && seconds = 0
    then raise (Parse_error "utcoffset: negative 0 seconds")
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

let param k v = Params.B (k, v)

(* Parameters (PARAM1_KEY=PARAM1_VALUE) *)
let iana_param = lift2 (fun k v -> param (Iana_param k) v)
    (iana_token <* (char '=')) value_list

let x_param = lift2 (fun (a, b) v -> param (Xparam (a, b)) v)
    (x_name <* char '=') value_list

let other_param = iana_param <|> x_param

let tzidparam =
 lift2 (fun a b -> param Tzid (a = '/', b))
 (string "TZID=" *> option ' ' (char '/')) param_text

let valuetypeparam =
  lift (fun x -> param Valuetype x)
    (string "VALUE=" *>
     (choice (string_parsers valuetype_strings)
      <|> (x_name >>| fun x -> `Xname x)
      <|> (iana_token >>| fun x -> `Ianatoken x)))

(* TODO use uri parser here *)
let altrepparam = (string "ALTREP=") *> quoted_caladdress >>| fun uri -> param Altrep uri

(* TODO use language tag rfc5646 parser *)
let languageparam = (string "LANGUAGE=") *> param_text >>| fun l -> param Language l

let cnparam = string "CN=" *> param_value >>| fun cn -> param Cn cn
let dirparam = string "DIR=" *> quoted_caladdress >>| fun s -> param Dir s
let sentbyparam = string "SENT-BY=" *> quoted_caladdress >>| fun s -> param Sentby s

(* Default is INDIVIDUAL *)
let cutypeparam =
  lift (fun x -> param Cutype x) ((string "CUTYPE=") *>
       (choice (string_parsers cutype_strings)
   <|> (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))))

let fbtypeparam =
  lift (fun x -> param Fbtype x) ((string "FBTYPE=") *>
       (choice (string_parsers fbtype_strings)
   <|> (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))))

let memberparam = lift (fun x -> param Member x)
  ((string "MEMBER=") *> sep_by1 (char ',') quoted_caladdress)

(* Default is REQ-PARTICIPANT *)
let roleparam = lift (fun x -> param Role x) ((string "ROLE=") *>
       (choice (string_parsers role_strings)
   <|> (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))))

let partstatparam =
  let statvalue =
    choice (string_parsers partstat_strings)
  and other =
       (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))
  in
  let statvalue = statvalue <|> other in
  lift (fun x -> param Partstat x) ((string "PARTSTAT=") *> statvalue)

let rsvpparam = lift (fun r -> param Rsvp r) (string "RSVP=" *> ((string "TRUE" >>| fun _ -> true) <|> (string "FALSE" >>| fun _ -> false )))

let deltoparam = lift (fun x -> param Delegated_to x)
  ((string "DELEGATED-TO=") *> sep_by1 (char ',') quoted_caladdress)

let delfromparam = lift (fun x -> param Delegated_from x)
  ((string "DELEGATED-FROM=") *> sep_by1 (char ',') quoted_caladdress)

let reltypeparam =
  lift (fun x -> param Reltype x)
   (string "RELTYPE=" *>
     (choice (string_parsers relation_strings)
   <|> (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))))

let fmttypeparam =
  string "FMTTYPE=" *> media_type >>| fun m -> param Media_type m

let encodingparam =
  string "ENCODING=BASE64" >>| fun _ -> param Encoding `Base64

let rangeparam = string "RANGE=THISANDFUTURE" >>| fun _ -> param Range `Thisandfuture

let icalparameter =
      altrepparam
  <|> cnparam
  <|> cutypeparam
  <|> delfromparam
  <|> deltoparam
  <|> dirparam
  <|> encodingparam
  <|> fmttypeparam
  <|> languageparam
  <|> memberparam
  <|> partstatparam
  <|> rangeparam
  <|> reltypeparam
  <|> roleparam
  <|> rsvpparam
  <|> sentbyparam
  <|> tzidparam
  <|> valuetypeparam
  <|> other_param

let list_to_map params =
  List.fold_right (fun (Params.B (k, v)) -> Params.add k v) params Params.empty

(* Properties *)
let propparser id pparser vparser lift =
  let params = many (char ';' *> pparser) >>| list_to_map in
  lift2 lift
    (string id *> params <* char ':')
    (vparser <* end_of_line)

let otherprop =
  let params = many (char ';' *> icalparameter) >>| list_to_map in
  let buildprop t p v = match t with
    | `Iana i -> `Iana_prop (i, p, v)
    | `Xname x -> `Xprop (x, p, v) in
  let my_iana_token =
    let not_legal s = s = "BEGIN" || String.sub s 0 3 = "END" in
    peek_string 5 >>= fun s -> if not_legal s then fail "Too eager" else iana_token in
  lift3 buildprop
    ((x_name >>| fun x -> `Xname x ) <|> (my_iana_token >>| fun i -> `Iana i))
    (params <* char ':')
    (value <* end_of_line)

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
  many (prodid <|> version <|> calscale <|> meth <|> otherprop)

let dtstamp =
  propparser "DTSTAMP" other_param (datetime >>= utc_only)
    (fun a b -> `Dtstamp (a, b))

let uid =
  propparser "UID" other_param text (fun a b -> `Uid (a, b))

let valuetype_or_default params default =
  match Params.find Valuetype params with
  | None -> default
  | Some v -> v

let check_date_datetime default a b =
  let valuetype = valuetype_or_default a default in
  match valuetype, b with
  | `Datetime, `Datetime _ -> ()
  | `Date, `Date _ -> ()
  | _ -> raise (Parse_error "check_date_datetime")

let check_datetime_duration default a b =
  let valuetype = valuetype_or_default a default in
  match valuetype, b with
  | `Datetime, `Datetime _ -> ()
  | `Duration, `Duration _ -> ()
  | _ -> raise (Parse_error "check_datetime_duration")

let check_date_datetime_period default a b =
  let valuetype = valuetype_or_default a default in
  match valuetype, b with
  | `Datetime, `Datetime _ -> ()
  | `Date, `Date _ -> ()
  | `Period, `Period _ -> ()
  | _ -> raise (Parse_error "check_date_datetime_period")

let check_binary_uri default a b =
  let valuetype = valuetype_or_default a default in
  match valuetype, b with
  | `Binary, `Binary _ -> ()
  | `Uri, `Uri _ -> ()
  | _ -> raise (Parse_error "check_binary_uri")

let time_or_date =
  (datetime >>| fun dt -> `Datetime dt)
  <|> (date >>| fun d -> `Date d)

let move_tzid params d_or_dt =
  match Params.find Tzid params, d_or_dt with
  | Some tzid, `Datetime (`Local ts) ->
    let params' = Params.remove Tzid params in
    (params', `Datetime (`With_tzid (ts, tzid)))
  | _, _ -> (params, (d_or_dt :> date_or_datetime))

let move_tzid_period params d_or_dt =
  match d_or_dt with
  | #date_or_datetime as d_or_dt -> (move_tzid params d_or_dt :> (params * [ date_or_datetime | `Period of period ]))
  | `Period (`Local ts, span, was_explicit) ->
    let timestamp = match Params.find Tzid params with
    | None -> `Local ts
    | Some tzid -> `With_tzid (ts, tzid)
    in
    let params' = Params.remove Tzid params in
    (params', `Period (timestamp, span, was_explicit))
  | `Period p -> (params, `Period p)

let dtstart =
  let dtstparam = valuetypeparam <|> tzidparam <|> other_param in
  propparser "DTSTART" dtstparam time_or_date
    (fun a b ->
       check_date_datetime `Datetime a b ;
       `Dtstart (move_tzid a b))

let completed =
  propparser "COMPLETED" other_param (datetime >>= utc_only)
    (fun a b -> `Completed (a, b))

let percent =
  let pv = digits >>= ensure int_of_string >>= in_range 0 100 in
  propparser "PERCENT" other_param pv
    (fun a b -> `Percent (a, b))

let due =
  let dueparam = valuetypeparam <|> tzidparam <|> other_param in
  propparser "DUE" dueparam time_or_date
    (fun a b ->
       check_date_datetime `Datetime a b ;
       `Due (move_tzid a b))

let class_ =
  let class_value = choice (string_parsers class_strings)
   <|> (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))
  in
  propparser "CLASS" other_param class_value (fun a b -> `Class (a, b))

let created =
  propparser "CREATED" other_param (datetime >>= utc_only)
    (fun a b -> `Created (a, b))

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
  propparser "LAST-MODIFIED" other_param (datetime >>= utc_only)
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
  let statvalue = choice (string_parsers status_strings) in
  propparser "STATUS" other_param statvalue (fun a b -> `Status (a, b))

let summary =
  let summ_param = altrepparam <|> languageparam <|> other_param in
  propparser "SUMMARY" summ_param text (fun a b -> `Summary (a, b))

let transp =
  let t_value = choice (string_parsers transp_strings) in
  propparser "TRANSP" other_param t_value (fun a b -> `Transparency (a, b))

let url =
  propparser "URL" other_param caladdress (fun a b -> `Url (a, b))

let recurid =
  let recur_param = tzidparam <|> valuetypeparam <|> rangeparam <|> other_param in
  propparser "RECURRENCE-ID" recur_param time_or_date
    (fun a b ->
       check_date_datetime `Datetime a b ;
       `Recur_id (move_tzid a b))

let rrule =
  propparser "RRULE" other_param recur (fun a b -> `Rrule (a, b))

let dtend =
  let dtend_param = tzidparam <|> valuetypeparam <|> other_param in
  propparser "DTEND" dtend_param time_or_date
    (fun a b ->
       check_date_datetime `Datetime a b ;
       `Dtend (move_tzid a b))

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
  let attach_param = fmttypeparam <|> valuetypeparam <|> encodingparam <|> other_param in
  let attach_value =
    (binary >>| fun b -> `Binary b) <|> (caladdress >>| fun a -> `Uri a)
  in
  propparser "ATTACH" attach_param attach_value
    (fun a b ->
       check_binary_uri `Uri a b ;
       let encoding = Params.find Encoding a in
       match encoding, b with
       | None, `Uri _ -> `Attach (a, b)
       | Some `Base64, `Binary _ -> `Attach (a, b)
       | _ -> raise (Parse_error "attach"))

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

(* collect dates and datetimes into tagged list *)
let move_tzid_and_collect_d_or_dt params (d_or_dts : [date_or_datetime | `Period of period] list) =
  let is_date = function `Date _ -> true | _ -> false
  and is_datetime = function `Datetime _ -> true | _ -> false
  in
  if List.for_all is_date d_or_dts then
    let extract = function `Date d -> d | _ -> raise (Parse_error "exdate: date") in
    Some (`Dates (List.map extract d_or_dts))
  else if List.for_all is_datetime d_or_dts then
    let datetimes = List.map (move_tzid_period params) d_or_dts in
    let extract = function (_, `Datetime d) -> d | _ -> raise (Parse_error "exdate: datetime") in
    Some (`Datetimes (List.map extract datetimes))
  else None

let exdate =
  let exdtparam = valuetypeparam <|> tzidparam <|> other_param in
  let exdtvalue = sep_by1 (char ',') time_or_date in
  propparser "EXDATE" exdtparam exdtvalue
    (fun a b ->
      List.iter (check_date_datetime `Datetime a) b ;
      match move_tzid_and_collect_d_or_dt a b with
      | None -> raise (Parse_error "exdate: value neither date nor datetime")
      | Some d_or_dts -> `Exdate (Params.remove Tzid a, d_or_dts))

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
       let is_period = function `Period _ -> true | _ -> false in
       let ds_or_dts_or_ps = match move_tzid_and_collect_d_or_dt a b with
         | Some ds_or_dts -> (ds_or_dts :> dates_or_datetimes_or_periods)
         | None ->
           if List.for_all is_period b then
             let periods = List.map (move_tzid_period a) b in
             let extract = function (_, `Period d) -> d | _ -> raise (Parse_error "rdate: period") in
             `Periods (List.map extract periods)
           else raise (Parse_error "rdate: value neither date nor datetime nor period")
       in
       `Rdate (Params.remove Tzid a, ds_or_dts_or_ps))

let event_prop =
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
  resources <|> rdate <|> otherprop

let event_props = many event_prop

let todoprop =
  dtstamp <|> uid <|>
  class_ <|> completed <|> created <|> description <|>
  dtstart <|> geo <|> last_mod <|> location <|> organizer <|>
  percent <|> priority <|> recurid <|> seq <|> status <|>
  summary <|> url <|>
  rrule <|>
  due <|> duration <|>
  attach <|> attendee <|> categories <|> comment <|> contact <|>
  exdate <|> rstatus <|> related <|> resources <|>
  rdate <|> otherprop

let todoprops = many todoprop

let action =
  let actionvalue =
        (string "AUDIO" >>| fun _ -> `Audio)
    <|> (string "DISPLAY" >>| fun _ -> `Display)
    <|> (string "EMAIL" >>| fun _ -> `Email)
    <|> (string "NONE" >>| fun _ -> `None)
    <|> (iana_token >>| fun x -> `Ianatoken x)
    <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))
  in
  propparser "ACTION" other_param actionvalue (fun a b -> `Action (a, b))

let trigger =
  let trigrelparam =
    lift (fun x -> param Related x)
      (string "RELATED=" *>
       ((string "START" >>| fun _ -> `Start) <|>
        (string "END" >>| fun _ -> `End)))
  in
  let trigparam = trigrelparam <|> valuetypeparam <|> other_param in
  let trigvalue =
        (dur_value >>| fun d -> `Duration d)
    <|> (datetime >>= utc_only >>| fun ts -> `Datetime ts)
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

let build_alarm props =
  let actions, rest = List.partition (function `Action _ -> true | _ -> false) props in
  let action = match actions with
   | [`Action x] -> x
   | _ -> raise (Parse_error "build_alarm props: action") in

  let triggers, rest' = List.partition (function `Trigger _ -> true | _ -> false ) rest in
  let trigger = match triggers with
   | [`Trigger x] -> x
   | _ -> raise (Parse_error "build_alarm props: trigger") in

  let (other: other_prop list), rest'' = List.fold_left (fun (other, rest) -> function
   | `Xprop v -> (`Xprop v :: other, rest)
   | `Iana_prop v -> (`Iana_prop v :: other, rest)
   | v -> other, v :: rest) ([], []) rest' in

  (* check dur repeat *)
  let duration_repeat, rest''' =
    let durations, rest''' = List.partition (function `Duration _ -> true | _ -> false ) rest'' in
    let repeats, rest'''' = List.partition (function `Repeat _ -> true | _ -> false ) rest''' in
    match durations, repeats with
     | [`Duration x], [`Repeat y] -> Some (x, y), rest''''
     | [], [] -> None, rest''''
     | _, _ -> raise (Parse_error "build_alarm props: duration_repeat") in

  let build_audio rest =
    let attachs, rest' = List.partition (function `Attach _ -> true | _ -> false ) rest in
    let attach = match attachs with
     | [`Attach x] -> Some x
     | [] -> None
     | _ -> raise (Parse_error "build_audio: attach") in
    match rest' with
     | [] -> `Audio { trigger ; duration_repeat ; other ; special = { attach } }
     | _ -> raise (Parse_error "build_audio: unkown input after attach") in

  let build_display rest =
    let descriptions, rest' = List.partition (function `Description _ -> true | _ -> false ) rest in
    let description = match descriptions with
     | [`Description x] -> x
     | _ -> raise (Parse_error "build_display: description") in
    match rest' with
     | [] -> `Display { trigger ; duration_repeat ; other ; special = { description } }
     | _ -> raise (Parse_error "build_display: unknown input after description") in

  let build_email rest =
    let descriptions, rest' = List.partition (function `Description _ -> true | _ -> false ) rest in
    let description = match descriptions with
     | [`Description x] -> x
     | _ -> raise (Parse_error "build_email: description") in
    let summarys, rest'' = List.partition (function `Summary _ -> true | _ -> false ) rest' in
    let summary = match summarys with
     | [`Summary x] -> x
     | _ -> raise (Parse_error "build_email: summary") in
    let raw_attendees, rest''' = List.partition (function `Attendee _ -> true | _ -> false ) rest'' in
    let attendees = List.map (function `Attendee x -> x | _ -> raise (Parse_error "build_email: attendee")) raw_attendees in
    if attendees = [] then raise (Parse_error "build_email: attendees");
    let attachs, rest'''' = List.partition (function `Attach _ -> true | _ -> false ) rest''' in
    let attach = match attachs with
     | [`Attach x] -> Some x
     | [] -> None
     | _ -> raise (Parse_error "build_email: attach") in
    match rest'''' with
     | [] -> `Email { trigger ; duration_repeat ; other ; special = { description ; summary ; attach ; attendees } }
     | _ -> raise (Parse_error "build_email: unknown input after attach") in

  let build_none _rest =
    (* TODO check if rest is empty *)
    `None { trigger ; duration_repeat ; other ; special = () }
  in

  match action with
  | _, `Audio -> build_audio rest'''
  | _, `Display -> build_display rest'''
  | _, `Email -> build_email rest'''
  | _, `None -> build_none rest'''
  | _, _ -> raise (Parse_error "build_alarm: unknown action, not supported")

let alarmc =
  string "BEGIN:VALARM" *> end_of_line *>
  ( many (audioprop <|> dispprop <|> emailprop <|> otherprop) >>| build_alarm )
  <* string "END:VALARM" <* end_of_line

let build_todo todoprops alarms =
  let f acc alarm = if List.exists (equal_alarm alarm) acc then acc else alarm :: acc in
  `Todo (todoprops, List.fold_left f [] alarms)

let todoc =
  string "BEGIN:VTODO" *> end_of_line *>
  lift2 build_todo todoprops (many alarmc)
  <* string "END:VTODO" <* end_of_line

let build_event eventprops alarms =
  let f acc alarm = if List.exists (equal_alarm alarm) acc then acc else alarm :: acc in
  let dtstamp, rest = List.partition (function `Dtstamp _ -> true | _ -> false) eventprops in
  let uid, rest' = List.partition (function `Uid _ -> true | _ -> false) rest in
  let dtstart, rest'' = List.partition (function `Dtstart _ -> true | _ -> false) rest' in
  let dtend_or_duration_list, rest''' = List.partition (function `Dtend _ | `Duration _ -> true | _ -> false) rest'' in
  let dtend_or_duration = match dtend_or_duration_list with
    | [ (`Duration _ as x) ] | [ (`Dtend _ as x) ] -> Some x
    | [] -> None
    | _ -> raise (Parse_error "only one Dtend or Duration allowed in event")
  in
  let rrule_list, rest'''' = List.partition (function `Rrule _ -> true | _ -> false) rest''' in
  let rrule = match rrule_list with
    | [ `Rrule r ] -> Some r
    | [] -> None
    | _ -> raise (Parse_error "only one Rrule allowed in event")
  in
  let alarms' = List.fold_left f [] alarms in
  match dtstamp, uid, dtstart with
  | [ `Dtstamp dtstamp ], [ `Uid uid ], [ `Dtstart dtstart ] ->
    `Event { dtstamp ; uid ; dtstart ; dtend_or_duration ; rrule ; props = rest'''' ; alarms = alarms' }
  | _, [ `Uid uid ], [ `Dtstart dtstart ] ->
    (* Firefox OS (g2b) kludge: tries to create event without dtstamp *)
    let dtstamp = Params.empty, match snd dtstart with
      | `Date d -> begin match Ptime.of_date_time (d, ((0, 0, 0), 0)) with
          | None -> raise (Parse_error "couldn't convert dtstart to timestamp")
          | Some x -> x end
      | `Datetime (`Utc ts) -> ts
      | `Datetime (`Local ts) -> ts
      | `Datetime (`With_tzid (ts, _)) -> ts
    in
    `Event { dtstamp ; uid ; dtstart ; dtend_or_duration ; rrule ; props = rest'''' ; alarms = alarms' }
  | _ -> raise (Parse_error "build_event: missing dtstamp, uid or dtstart")

let eventc =
  string "BEGIN:VEVENT" *> end_of_line *>
  lift2 build_event event_props (many alarmc)
  <* string "END:VEVENT" <* end_of_line

let tzid =
  propparser "TZID" other_param
    (lift2 (fun a b -> (a = '/', b)) (option ' ' (char '/')) text)
    (fun p v -> `Timezone_id (p, v))

let tzurl =
  propparser "TZURL" other_param caladdress
    (fun a b -> `Tzurl (a, b))

let tzoffsetto =
  propparser "TZOFFSETTO" other_param utcoffset
    (fun p v -> `Tzoffset_to (p, v))

let tzoffsetfrom =
  propparser "TZOFFSETFROM" other_param utcoffset
    (fun p v -> `Tzoffset_from (p, v))

let tzname =
  let tznparam = languageparam <|> other_param in
  propparser "TZNAME" tznparam text
    (fun p v -> `Tzname (p, v))

let tzprop =
  (dtstart >>= function
    | `Dtstart (params, `Datetime (`Local ts)) -> return (`Dtstart_local (params, ts))
    | `Dtstart _ -> fail "the dtstart time zone property needs to be in local time")
  <|> tzoffsetto <|> tzoffsetfrom <|>
  rrule <|>
  comment <|> rdate <|> tzname <|> otherprop

let freebusy =
  let fbparam = fbtypeparam <|> other_param in
  propparser "FREEBUSY" fbparam (sep_by1 (char ',') (period >>= function
    | `Utc ts, span, was_explicit -> return (ts, span, was_explicit)
    | `Local _, _, _ -> fail "freebusy prop may only contain UTC timestamps"))
    (fun p v -> `Freebusy (p, v))

let is_utc_datetime = function
  | `Datetime (`Utc ts) -> return ts
  | _ -> fail "is not a UTC timestamp"

let freebusyprop =
  dtstamp <|> uid <|> contact <|>
  (dtstart >>= fun (`Dtstart (params, v)) ->
   is_utc_datetime v >>| fun ts -> `Dtstart_utc (params, ts)) <|>
  (dtend >>= fun (`Dtend (params, v)) ->
   is_utc_datetime v >>| fun ts -> `Dtend_utc (params, ts)) <|>
  organizer <|> url <|> attendee <|> comment <|> freebusy <|> rstatus <|> otherprop

let standardc =
  string "BEGIN:STANDARD" *> end_of_line *>
  (many tzprop >>| fun props -> `Standard props)
  <* string "END:STANDARD" <* end_of_line

let daylightc =
  string "BEGIN:DAYLIGHT" *> end_of_line *>
  (many tzprop >>| fun props -> `Daylight props)
  <* string "END:DAYLIGHT" <* end_of_line

let timezonec =
  string "BEGIN:VTIMEZONE" *> end_of_line *>
  (many (tzid <|> last_mod <|> tzurl <|> standardc <|> daylightc <|> otherprop) >>| fun props -> `Timezone props)
  <* string "END:VTIMEZONE" <* end_of_line

let freebusyc =
  string "BEGIN:VFREEBUSY" *> end_of_line *>
  (many freebusyprop >>| fun props -> `Freebusy props)
  <* string "END:VFREEBUSY" <* end_of_line

let component =
  many1 (eventc <|> todoc (* <|> journalc *) <|> freebusyc <|> timezonec)

let icalbody =
  let to_pair props comps zapprops =
    props@zapprops, comps
  in
  lift3 to_pair calprops component calprops

let calobject =
  string "BEGIN:VCALENDAR" *> end_of_line *>
  icalbody
  <* string "END:VCALENDAR" <*
  (* "option ()" is a workaround for FirefoxOS, which does not end with a newline *)
  option () end_of_line <* end_of_input

let parse (str : string) : (calendar, string) result =
  try parse_string ~consume:Consume.All calobject (normalize_lines str)
  with Parse_error e -> Error ("parse error: " ^ e)

let recur_dates dtstart (rrule : recurrence) =
  Recurrence.new_gen dtstart rrule

let date_or_datetime_to_ptime = function
  | `Datetime (`Utc dtstart) -> dtstart
  | `Datetime (`Local dtstart) -> dtstart
  | `Datetime (`With_tzid (ts, _tzid)) -> ts
  | `Date start -> match Ptime.of_date_time (start, ((0, 0, 0), 0)) with
    | None -> assert false
    | Some dtstart -> dtstart

let date_or_datetime_with_ptime d_or_dt ts =
  match d_or_dt with
  | `Date _ -> `Date (fst @@ Ptime.to_date_time ts)
  | `Datetime (`Utc _) -> `Datetime (`Utc ts)
  | `Datetime (`Local _) -> `Datetime (`Local ts)
  | `Datetime (`With_tzid (_, tzid)) -> `Datetime (`With_tzid (ts, tzid))

(* TODO handle Exdate and Rdate *)
let recur_events event = match event.rrule with
  | None -> (fun () -> None)
  | Some (_, recur) ->
    let dtstart = date_or_datetime_to_ptime (snd event.dtstart) in
    let adjust_dtend ts = match event.dtend_or_duration with
      | None -> None
      | Some (`Duration d) -> Some (`Duration d)
      | Some (`Dtend (params, d_o_dt)) ->
        let dtend = date_or_datetime_to_ptime d_o_dt in
        let span = Ptime.diff dtend dtstart in
        let ts' = add_span ts span in
        let v = date_or_datetime_with_ptime d_o_dt ts' in
        Some (`Dtend (params, v))
    in
    let newdate = recur_dates dtstart recur in
    (fun () -> match newdate () with
       | None -> None
       | Some ts ->
         let dtstart = (fst event.dtstart, date_or_datetime_with_ptime (snd event.dtstart) ts)
         and dtend_or_duration = adjust_dtend ts
         in
         Some { event with dtstart ; dtend_or_duration })

let occurence_before_timestamp datetime (tzprops : tz_prop list) =
  let dtstart = List.find (function `Dtstart_local _ -> true | _ -> false) tzprops in
  let dtstart', dtstarty' = match dtstart with
    | `Dtstart_local (_, dtstart) ->
      let (y, _, _), _ = Ptime.to_date_time datetime in
      let (_, m, d), t = Ptime.to_date_time dtstart in
      dtstart, Option.get (Ptime.of_date_time ((y - 1, m, d), t))
    | _ -> assert false
  in
  (* dtstart in a vtimezone subcomponent may not contain a tzid property! *)
  let rrule = List.find_opt (function `Rrule _ -> true | _ -> false) tzprops in
  let next_event = match rrule with
    | None -> (fun () -> None)
    | Some (`Rrule (_, rrule)) ->
      let freq, _, _ , _ = rrule in
      if freq = `Yearly then
        recur_dates dtstarty' rrule
      else
        recur_dates dtstart' rrule
    | _ -> assert false
  in
  (* TODO handle RDATE in addition to rrule *)
  let rec in_timerange acc = function
    | Some dtstart'' ->
      if Ptime.is_earlier ~than:datetime dtstart''
      then in_timerange (Some dtstart'') (next_event ())
      else acc
   | None -> acc in
  in_timerange None (Some dtstart')

let calculate_offset (props : tz_prop list) ts datetime =
  match
    List.find (function `Tzoffset_to _ -> true | _ -> false) props,
    List.find (function `Tzoffset_from _ -> true | _ -> false) props
  with
  | `Tzoffset_to (_, to_span), `Tzoffset_from (_, from_span) ->
    begin
      let is_negative = Ptime.Span.compare to_span from_span = 1 in
      let delta = Ptime.Span.sub to_span from_span in
      let in_lost_span =
        let than = match Ptime.add_span ts delta with
          | None -> assert false
          | Some d -> d
        in
        Ptime.is_earlier ~than datetime
      in
      let datetime' =
        if is_negative && in_lost_span then
          match Ptime.add_span datetime delta with
          | None -> assert false
          | Some ts -> ts
        else
          datetime
      in
      match Ptime.sub_span datetime' to_span with
      | None -> assert false
      | Some timestamp -> timestamp
    end
  | _ -> assert false

let normalize_timezone datetime (is_unique, tzid) (timezones : timezone_prop list list) =
  (* TODO optimize timezone data structure *)
  match
    List.find_opt (fun tzprops ->
        List.exists (function
            | `Timezone_id (_, (is_unique', tzid')) ->
              is_unique = is_unique' && String.equal tzid tzid'
            | _ -> false) tzprops)
      timezones
  with
  | None -> None
  | Some timezoneprops ->
    let opt d = function
      | None -> None
      | Some ts -> Some (ts, d)
    in
    let relevant_offsets =
      List.map (function
          | `Standard props -> opt props (occurence_before_timestamp datetime props)
          | `Daylight props -> opt props (occurence_before_timestamp datetime props)
          | _ -> None)
        timezoneprops
    in
    let ts, props =
      List.fold_left
        (fun (a, props) -> function
           | None -> (a, props)
           | Some (ts, d) ->
             if Ptime.is_later ~than:a ts then (ts, d) else (a, props))
        (Ptime.min, []) relevant_offsets
    in
    Some (calculate_offset props ts datetime)
