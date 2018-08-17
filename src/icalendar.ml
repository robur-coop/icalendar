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

type _ icalparameter =
  | Altrep : Uri.t icalparameter
  | Cn : string icalparameter
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
  | Iana_param : (string * string list) icalparameter
  | Xparam : ((string * string) * string list) icalparameter

(*
let rec (equal_icalparameter
          _ icalparameter -> _ icalparameter -> Ppx_deriving_runtime.bool)
  =
  ((let open! Ppx_deriving_runtime in
      fun lhs ->
        fun rhs ->
          match (lhs, rhs) with
          | (Altrep, Altrep) -> true
          | (Cn, Cn) -> true
          | (Cutype, Cutype) -> true
          | (Delegated_from, Delegated_from) -> true
          | (Delegated_to, Delegated_to) -> true
          | (Dir, Dir) -> true
          | (Encoding, Encoding) -> true
          | (Media_type, Media_type) -> true
          | (Fbtype, Fbtype) -> true
          | (Language, Language) -> true
          | (Member, Member) -> true
          | (Partstat, Partstat) -> true
          | (Range, Range) -> true
          | (Related, Related) -> true
          | (Reltype, Reltype) -> true
          | (Role, Role) -> true
          | (Rsvp, Rsvp) -> true
          | (Sentby, Sentby) -> true
          | (Tzid, Tzid) -> true
          | (Valuetype, Valuetype) -> true
          | (Iana_param, Iana_param) -> true
          | (Xparam, Xparam) -> true
          | _ -> false)
  [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
*)

let rec equal_icalparameter : type a b.
          a icalparameter -> a -> b icalparameter -> b -> Ppx_deriving_runtime.bool
  =
  (* type system ensures that the values fit the constructors *)
  ((let open! Ppx_deriving_runtime in
      fun lhs_c lhs_v ->
        fun rhs_c rhs_v ->
          match (lhs_c, rhs_c) with
          | (Altrep, Altrep) -> Uri.equal lhs_v rhs_v
          | (Cn, Cn) -> String.equal lhs_v rhs_v
          | (Cutype, Cutype) -> equal_cutype lhs_v rhs_v
          | (Delegated_from, Delegated_from) -> List.for_all2 Uri.equal lhs_v rhs_v
          | (Delegated_to, Delegated_to) -> List.for_all2 Uri.equal lhs_v rhs_v
          | (Dir, Dir) -> Uri.equal lhs_v rhs_v
          | (Encoding, Encoding) -> lhs_v = rhs_v
          | (Media_type, Media_type) -> String.equal (fst lhs_v) (fst rhs_v) && String.equal (snd lhs_v) (snd rhs_v)
          | (Fbtype, Fbtype) -> equal_fbtype lhs_v rhs_v
          | (Language, Language) -> String.equal lhs_v rhs_v
          | (Member, Member) -> List.for_all2 Uri.equal lhs_v rhs_v
          | (Partstat, Partstat) -> equal_partstat lhs_v rhs_v
          | (Range, Range) -> lhs_v = rhs_v
          | (Related, Related) -> lhs_v = rhs_v
          | (Reltype, Reltype) -> equal_relationship lhs_v rhs_v
          | (Role, Role) -> equal_role lhs_v rhs_v
          | (Rsvp, Rsvp) -> lhs_v = rhs_v
          | (Sentby, Sentby) -> Uri.equal lhs_v rhs_v
          | (Tzid, Tzid) -> fst lhs_v = fst rhs_v && String.equal (snd lhs_v) (snd rhs_v)
          | (Valuetype, Valuetype) -> equal_valuetype lhs_v rhs_v
          | (Iana_param, Iana_param) -> String.equal (fst lhs_v) (fst rhs_v) && List.for_all2 String.equal (snd lhs_v) (snd rhs_v)
          | (Xparam, Xparam) -> String.equal (fst (fst lhs_v)) (fst (fst rhs_v)) && String.equal (snd (fst lhs_v)) (snd (fst rhs_v)) &&
                                List.for_all2 String.equal (snd lhs_v) (snd rhs_v)
          | _ -> false    )
  [@ocaml.warning "-A"])[@@ocaml.warning "-39"]

(* equal_icalparameter Altrep (http://foo.com) Xparam ("foo", "bar") *)


let rec (pp_icalparameter :
          Format.formatter -> _ icalparameter -> Ppx_deriving_runtime.unit)
  =
  ((let open! Ppx_deriving_runtime in
      fun fmt ->
        function
        | Altrep -> Format.pp_print_string fmt "Icalendar.Altrep"
        | Cn -> Format.pp_print_string fmt "Icalendar.Cn"
        | Cutype -> Format.pp_print_string fmt "Icalendar.Cutype"
        | Delegated_from ->
            Format.pp_print_string fmt "Icalendar.Delegated_from"
        | Delegated_to -> Format.pp_print_string fmt "Icalendar.Delegated_to"
        | Dir -> Format.pp_print_string fmt "Icalendar.Dir"
        | Encoding -> Format.pp_print_string fmt "Icalendar.Encoding"
        | Media_type -> Format.pp_print_string fmt "Icalendar.Media_type"
        | Fbtype -> Format.pp_print_string fmt "Icalendar.Fbtype"
        | Language -> Format.pp_print_string fmt "Icalendar.Language"
        | Member -> Format.pp_print_string fmt "Icalendar.Member"
        | Partstat -> Format.pp_print_string fmt "Icalendar.Partstat"
        | Range -> Format.pp_print_string fmt "Icalendar.Range"
        | Related -> Format.pp_print_string fmt "Icalendar.Related"
        | Reltype -> Format.pp_print_string fmt "Icalendar.Reltype"
        | Role -> Format.pp_print_string fmt "Icalendar.Role"
        | Rsvp -> Format.pp_print_string fmt "Icalendar.Rsvp"
        | Sentby -> Format.pp_print_string fmt "Icalendar.Sentby"
        | Tzid -> Format.pp_print_string fmt "Icalendar.Tzid"
        | Valuetype -> Format.pp_print_string fmt "Icalendar.Valuetype"
        | Iana_param -> Format.pp_print_string fmt "Icalendar.Iana_param"
        | Xparam -> Format.pp_print_string fmt "Icalendar.Xparam")
  [@ocaml.warning "-A"])
and show_icalparameter : _ icalparameter -> Ppx_deriving_runtime.string =
  fun x -> Format.asprintf "%a" pp_icalparameter x

type param = P : 'a icalparameter * 'a -> param

let equal_param  _ _ = false
  let pp_param _ _ = assert false

type other_prop =
  [ `Iana_prop of string * param list * string
  | `Xprop of (string * string) * param list * string ] [@@deriving eq, show]

type calprop =
  [ `Prodid of param list * string
  | `Version of param list * string
  | `Calscale of param list * string
  | `Method of param list * string
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
  | `Until of Ptime.t * bool
] [@@deriving eq, show]

type interval = int [@@deriving eq, show]

type recurrence = freq * count_or_until option * interval option * recur list [@@deriving eq, show]

type class_ = [ `Public | `Private | `Confidential | `Ianatoken of string | `Xname of string * string ] [@@deriving eq, show]

type status = [ `Draft | `Final | `Cancelled |
                `Needs_action | `Completed | `In_process | (* `Cancelled *)
                `Tentative | `Confirmed (* | `Cancelled *) ] [@@deriving eq, show]

type freebusyprop = [
  | `Dtstamp of param list * (Ptime.t * bool)
  | `Uid of param list * string
  | `Contact of param list * string
  | `Dtstart of param list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Dtend of param list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Organizer of param list * Uri.t
  | `Url of param list * Uri.t
  | `Attendee of param list * Uri.t
  | `Comment of param list * string
  | `Freebusy of param list * (Ptime.t * Ptime.t * bool) list 
  | `Rstatus of param list * ((int * int * int option) * string * string option)
  | other_prop 
] [@@deriving eq, show]

type generalprop = [
  | `Dtstamp of param list * (Ptime.t * bool)
  | `Uid of param list * string
  | `Dtstart of param list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Class of param list * class_
  | `Created of param list * (Ptime.t * bool)
  | `Description of param list * string
  | `Geo of param list * (float * float)
  | `Lastmod of param list * (Ptime.t * bool)
  | `Location of param list * string
  | `Organizer of param list * Uri.t
  | `Priority of param list * int
  | `Seq of param list * int
  | `Status of param list * status
  | `Summary of param list * string
  | `Url of param list * Uri.t
  | `Recur_id of param list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Rrule of param list * recurrence
  | `Duration of param list * int
  | `Attach of param list * [ `Uri of Uri.t | `Binary of string ]
  | `Attendee of param list * Uri.t
  | `Categories of param list * string list
  | `Comment of param list * string
  | `Contact of param list * string
  | `Exdate of param list *
    [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list ]
  | `Rstatus of param list * ((int * int * int option) * string * string option)
  | `Related of param list * string
  | `Resource of param list * string list
  | `Rdate of param list *
              [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list | `Periods of (Ptime.t * Ptime.t * bool) list ]
] [@@deriving eq, show]

type eventprop = [
  | generalprop
  | `Transparency of param list * [ `Transparent | `Opaque ]
  | `Dtend of param list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | other_prop
] [@@deriving eq, show]

type 'a alarm_struct = {
  trigger : param list * [ `Duration of int | `Datetime of (Ptime.t * bool) ] ;
  duration_repeat: ((param list * int) * (param list * int )) option ;
  other: other_prop list ;
  special: 'a ;
} [@@deriving eq, show]

type audio_struct = {
  attach: (param list * [ `Uri of Uri.t | `Binary of string ]) option ;
} [@@deriving eq, show]

type display_struct = {
  description : param list * string ;
} [@@deriving eq, show]

type email_struct = {
  description : param list * string ;
  summary : param list * string ;
  attendees : (param list * Uri.t) list ;
  attach: (param list * [ `Uri of Uri.t | `Binary of string ]) option ;
} [@@deriving eq, show]

type alarm = [ `Audio of audio_struct alarm_struct | `Display of display_struct alarm_struct | `Email of email_struct alarm_struct ] [@@deriving eq, show]

type tzprop = [
  | `Dtstart of param list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Tzoffset_to of param list * Ptime.Span.t
  | `Tzoffset_from of param list * Ptime.Span.t
  | `Rrule of param list * recurrence
  | `Comment of param list * string
  | `Rdate of param list * [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list | `Periods of (Ptime.t * Ptime.t * bool) list ]
  | `Tzname of param list * string
  | other_prop
] [@@deriving eq, show]

type timezoneprop = [
  | `Timezone_id of param list * (bool * string)
  | `Lastmod of param list * (Ptime.t * bool)
  | `Tzurl of param list * Uri.t
  | `Standard of tzprop list
  | `Daylight of tzprop list
  | other_prop
] [@@deriving eq, show]

type todoprop = [
  | generalprop
  | `Completed of param list * (Ptime.t * bool)
  | `Percent of param list * int
  | `Due of  param list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | other_prop
] [@@deriving eq, show]

type component = [
  | `Event of eventprop list * alarm list
  | `Todo of todoprop list * alarm list
  | `Freebusy of freebusyprop list 
  | `Timezone of timezoneprop list
] [@@deriving eq, show]

type calendar = calprop list * component list [@@deriving eq, show]

let next start freq = 
  let (y, m, d), ((hh, mm, ss), offset) = Ptime.to_date_time start in
  let dt = match freq with
  | `Daily -> (y, m, d + 1), ((hh, mm, ss), offset)
  | `Hourly -> (y, m, d), ((hh + 1, mm, ss), offset) 
  | `Minutely -> (y, m, d), ((hh, mm + 1, ss), offset) 
  | `Monthly -> (y, m + 1, d), ((hh, mm, ss), offset) 
  | `Secondly -> (y, m, d), ((hh, mm, ss + 1), offset) 
  | `Weekly -> (y, m, d + 7), ((hh, mm, ss), offset) 
  | `Yearly -> (y + 1, m, d), ((hh, mm, ss), offset) 
  in Ptime.of_date_time dt

let other_prop_to_params = function 
  | `Iana_prop (_, params, value)
  | `Xprop (_, params, value) -> params

let generalprop_to_params = function 
  | `Dtstamp (params, _)
  | `Uid (params, _)
  | `Dtstart (params, _)
  | `Class (params, _)
  | `Created (params, _)
  | `Description (params, _)
  | `Geo (params, _)
  | `Lastmod (params, _)
  | `Location (params, _)
  | `Organizer (params, _)
  | `Priority (params, _)
  | `Seq (params, _)
  | `Status (params, _)
  | `Summary (params, _)
  | `Url (params, _)
  | `Recur_id (params, _)
  | `Rrule (params, _)
  | `Duration (params, _)
  | `Attach (params, _)
  | `Attendee (params, _)
  | `Categories (params, _)
  | `Comment (params, _)
  | `Contact (params, _)
  | `Exdate (params, _)
  | `Rstatus (params, _)
  | `Related (params, _)
  | `Resource (params, _)
  | `Rdate (params, _) -> params

let eventprop_to_params = function
  | #generalprop as x -> generalprop_to_params x
  | #other_prop as x  -> other_prop_to_params x
  | `Transparency (params, _)
  | `Dtend (params, _) -> params

let todoprop_to_params = function
  | #generalprop as x -> generalprop_to_params x
  | #other_prop as x  -> other_prop_to_params x
  | `Completed (params, _)
  | `Percent (params, _)
  | `Due (params, _) -> params

let freebusyprop_to_params = function
  | #generalprop as x -> generalprop_to_params x
  | #other_prop as x  -> other_prop_to_params x
  | `Freebusy (params, _)
  | `Dtend (params, _) -> params

let params_to_tzid (params : param list) =
  let find_tzid_param acc = function
     | P (Tzid, (_, tzid)) -> Astring.String.Set.add tzid acc
     | _ -> acc in
  List.fold_left find_tzid_param Astring.String.Set.empty params

let collect_tzids (comp: component) = 
  let params = match comp with
  | `Event (props, alarms) -> List.map eventprop_to_params props
  | `Todo (props, alarms) -> List.map todoprop_to_params props
  | `Freebusy props -> List.map freebusyprop_to_params props
  | `Timezone _ -> []
  in
  params_to_tzid (List.flatten params)

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
    (`Free, "FREE") ;
    (`Busy, "BUSY") ;
    (`Busy_Unavailable, "BUSY-UNAVAILABLE") ;
    (`Busy_Tentative, "BUSY-TENTATIVE") ;
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


module Writer = struct
  let print_x vendor token = Printf.sprintf "X-%s%s%s" vendor (if String.length vendor = 0 then "" else "-") token

  let write_param buf =
    let write_kv k v =
      Buffer.add_string buf k ;
      Buffer.add_char buf '=' ;
      Buffer.add_string buf v
    and quoted str =
      Printf.sprintf "%S" str
    in
    let quoted_uri uri = quoted (Uri.to_string uri) in
    function
    | P (Iana_param, (token, values)) -> write_kv token (String.concat "," values)
    | P (Xparam, ((vendor, name), values)) -> write_kv (print_x vendor name) (String.concat "," values)
    | P (Valuetype, v) -> write_kv "VALUE" (List.assoc v valuetype_strings)
    | P (Tzid, (prefix, str)) -> write_kv "TZID" (Printf.sprintf "%s%s" (if prefix then "/" else "") str)
    | P (Altrep, uri) -> write_kv "ALTREP" (quoted_uri uri)
    | P (Language, lan) -> write_kv "LANGUAGE" lan
    | P (Cn, str) -> write_kv "CN" str
    | P (Dir, uri) -> write_kv "DIR" (quoted_uri uri)
    | P (Sentby, uri) -> write_kv "SENT-BY" (quoted_uri uri)
    | P (Range, `Thisandfuture) -> write_kv "RANGE" "THISANDFUTURE"
    | P (Media_type, (pre, post)) -> write_kv "FMTTYPE" (Printf.sprintf "%s/%s" pre post)
    | P (Fbtype, fbtype) -> write_kv "FBTYPE" (List.assoc fbtype fbtype_strings)
    | P (Encoding, `Base64) -> write_kv "ENCODING" "BASE64"
    | P (Cutype, cu) -> write_kv "CUTYPE" (List.assoc cu cutype_strings)
    | P (Delegated_from, uris) -> write_kv "DELEGATED-FROM" (String.concat "," (List.map quoted_uri uris))
    | P (Delegated_to, uris) -> write_kv "DELEGATED-TO" (String.concat "," (List.map quoted_uri uris))
    | P (Member, uris) -> write_kv "MEMBER" (String.concat "," (List.map quoted_uri uris))
    | P (Partstat, ps) -> write_kv "PARTSTAT" (List.assoc ps partstat_strings)
    | P (Role, role) -> write_kv "ROLE" (List.assoc role role_strings)
    | P (Rsvp, rsvp) -> write_kv "RSVP" (if rsvp then "TRUE" else "FALSE")
    | P (Reltype, rel) -> write_kv "RELTYPE" (List.assoc rel relation_strings)
    | P (Related, r) ->
      let r = match r with `Start -> "START" | `End -> "END" in
      write_kv "RELATED" r

  let write_line cr buf name params write_value =
    let write = Buffer.add_string buf in
    let write_char = Buffer.add_char buf in
    write name ;
    List.iteri (fun idx param ->
        write_char ';' ;
        write_param buf param)
      params ;
    write_char ':' ;
    write_value buf ;
    if cr then
      write_char '\r' ;
    write_char '\n'

  let write_string str buf = Buffer.add_string buf str

  let other_prop_to_ics_key (prop: other_prop) = match prop with
    | `Iana_prop (ianatoken, _, _) -> ianatoken
    | `Xprop ((vendor, token), _, _) -> print_x vendor token

  let calprop_to_ics_key (prop: calprop) = match prop with
    | `Prodid _ -> "PRODID"
    | `Version _ -> "VERSION"
    | `Calscale _ -> "CALSCALE"
    | `Method _ -> "METHOD"
    | #other_prop as x -> other_prop_to_ics_key x

  let other_prop_to_ics cr buf prop = 
    let key = other_prop_to_ics_key prop in
    match prop with
    | `Iana_prop (_, params, value) -> write_line cr buf key params (write_string value)
    | `Xprop (_, params, value) -> write_line cr buf key params (write_string value)

  let calprop_to_ics cr buf prop =
    let key = calprop_to_ics_key prop in
    let output = write_line cr buf key in
    match prop with
    | `Prodid (params, value) -> output params (write_string value)
    | `Version (params, value) -> output params (write_string value)
    | `Calscale (params, value) -> output params (write_string value)
    | `Method (params, value) -> output params (write_string value)
    | #other_prop as x -> other_prop_to_ics cr buf x

  let calprops_to_ics cr buf props = List.iter (calprop_to_ics cr buf) props

  let duration_to_ics dur buf =
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

  let datetime_to_str (ptime, utc) =
    let date, ((hh, mm, ss), _) = Ptime.to_date_time ptime in
    Printf.sprintf "%sT%02d%02d%02d%s" (date_to_str date) hh mm ss (if utc then "Z" else "")

  let datetime_to_ics datetime buf =
    Buffer.add_string buf (datetime_to_str datetime)

  let duration_repeat_to_ics cr buf = function
    | None -> ()
    | Some ((durparams, dur), (repparams, rep)) ->
      write_line cr buf "DURATION" durparams (duration_to_ics dur) ;
      write_line cr buf "REPEAT" repparams (write_string (string_of_int rep))

  let attach_to_ics cr buf = function
    | None -> ()
    | Some (params, value) ->
      let value' = match value with
        | `Uri uri -> Uri.to_string uri
        | `Binary s -> s
      in
      write_line cr buf "ATTACH" params (write_string value')

  let description_to_ics cr buf (params, desc) =
    write_line cr buf "DESCRIPTION" params (write_string desc)

  let summary_to_ics cr buf (params, summary) =
    write_line cr buf "SUMMARY" params (write_string summary)

  let date_or_time_to_ics dt buf = match dt with
    | `Date d -> date_to_ics buf d
    | `Datetime dt -> datetime_to_ics dt buf

  let dates_or_times_to_ics dt buf =
    let swap f a b = f b a in
    match dt with
    | `Dates xs -> List.iter (date_to_ics buf) xs
    | `Datetimes xs -> List.iter (swap datetime_to_ics buf) xs

  let period_to_ics buf (start, until, utc) =
    datetime_to_ics (start, utc) buf ;
    Buffer.add_char buf '/' ;
    datetime_to_ics (until, utc) buf

  let dates_or_times_or_periods_to_ics dt buf =
    let swap f a b = f b a in
    match dt with
    | `Dates xs -> List.iter (date_to_ics buf) xs
    | `Datetimes xs -> List.iter (swap datetime_to_ics buf) xs
    | `Periods xs -> List.iter (period_to_ics buf) xs


  let recurs_to_ics (freq, count_or_until, interval, l) buf =
    let write_rulepart key value =
      Buffer.add_string buf key ;
      Buffer.add_char buf '=' ;
      Buffer.add_string buf value in
    let recur_to_ics buf =
      let int_list l = String.concat "," @@ List.map string_of_int l in
      function
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
        | `Until enddate -> write_rulepart "UNTIL" (datetime_to_str enddate) ) ;
    ( match interval with
      | None -> ()
      | Some i -> 
        Buffer.add_char buf ';' ;
        write_rulepart "INTERVAL" (string_of_int i) ) ;
    List.iter (fun recur ->
        Buffer.add_char buf ';' ;
        recur_to_ics buf recur)
      l

  let attendee_to_ics cr buf (params, uri) =
    write_line cr buf "ATTENDEE" params (write_string (Uri.to_string uri))

  let generalprop_to_ics_key = function
    | `Dtstamp (params, ts) -> "DTSTAMP"
    | `Uid (params, str) -> "UID"
    | `Dtstart (params, date_or_time) -> "DTSTART"
    | `Class (params, class_) -> "CLASS"
    | `Created (params, ts) -> "CREATED"
    | `Description desc -> "DESCRIPTION" 
    | `Geo (params, (lat, lon)) -> "GEO" 
    | `Lastmod (params, ts) -> "LAST-MODIFIED"
    | `Location (params, name) -> "LOCATION"
    | `Organizer (params, uri) -> "ORGANIZER"
    | `Priority (params, prio) -> "PRIORITY"
    | `Seq (params, seq) -> "SEQUENCE"
    | `Status (params, status) -> "STATUS"
    | `Summary summary -> "SUMMARY"
    | `Url (params, uri) -> "URL"
    | `Recur_id (params, date_or_time) -> "RECURRENCE-ID"
    | `Rrule (params, _) -> "RRULE"
    | `Duration (params, dur) -> "DURATION"
    | `Attach att -> "ATTACH" 
    | `Attendee att -> "ATTENDEE"
    | `Categories (params, cats) -> "CATEGORIES"
    | `Comment (params, comment) -> "COMMENT"
    | `Contact (params, contact) -> "CONTACT"
    | `Exdate (params, dates_or_times) -> "EXDATE"
    | `Rstatus (params, (statcode, text, comment)) -> "REQUEST-STATUS"
    | `Related (params, rel) -> "RELATED"
    | `Resource (params, res) -> "RESOURCE"
    | `Rdate (params, dates_or_times_or_periods) -> "RDATE"

  let generalprop_to_ics cr buf prop = 
    let key = generalprop_to_ics_key prop in
    let output params v = write_line cr buf key params v in
    match prop with
    | `Dtstamp (params, ts) -> output params (datetime_to_ics ts)
    | `Uid (params, str) -> output params (write_string str)
    | `Dtstart (params, date_or_time) -> output params (date_or_time_to_ics date_or_time)
    | `Class (params, class_) -> output params (write_string (List.assoc class_ class_strings))
    | `Created (params, ts) -> output params (datetime_to_ics ts)
    | `Description desc -> description_to_ics cr buf desc
    | `Geo (params, (lat, lon)) ->
      output params (fun buf ->
          Buffer.add_string buf (string_of_float lat) ;
          Buffer.add_char buf ';' ;
          Buffer.add_string buf (string_of_float lon))
    | `Lastmod (params, ts) -> output params (datetime_to_ics ts)
    | `Location (params, name) -> output params (write_string name)
    | `Organizer (params, uri) -> output params (write_string (Uri.to_string uri))
    | `Priority (params, prio) -> output params (write_string (string_of_int prio))
    | `Seq (params, seq) -> output params (write_string (string_of_int seq))
    | `Status (params, status) -> output params (write_string (List.assoc status status_strings))
    | `Summary summary -> summary_to_ics cr buf summary
    | `Url (params, uri) -> output params (write_string Uri.(pct_decode (to_string uri)))
    | `Recur_id (params, date_or_time) -> output params (date_or_time_to_ics date_or_time)
    | `Rrule (params, recurs) -> output params (recurs_to_ics recurs)
    | `Duration (params, dur) -> output params (duration_to_ics dur)
    | `Attach att -> attach_to_ics cr buf (Some att)
    | `Attendee att -> attendee_to_ics cr buf att
    | `Categories (params, cats) ->
      let cat = String.concat "," cats in
      output params (write_string cat)
    | `Comment (params, comment) -> output params (write_string comment)
    | `Contact (params, contact) -> output params (write_string contact)
    | `Exdate (params, dates_or_times) -> output params (dates_or_times_to_ics dates_or_times)
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
           Buffer.add_string buf text ;
           match comment with
           | None -> ()
           | Some x ->
             Buffer.add_char buf ';' ;
             Buffer.add_string buf x)
    | `Related (params, rel) -> output params (write_string rel)
    | `Resource (params, res) ->
      let r = String.concat "," res in
      output params (write_string r)
    | `Rdate (params, dates_or_times_or_periods) ->
      output params
        (dates_or_times_or_periods_to_ics dates_or_times_or_periods)

  let eventprop_to_ics_key = function
    | #generalprop as x -> generalprop_to_ics_key x
    | #other_prop as x -> other_prop_to_ics_key x
    | `Transparency _ -> "TRANSP"
    | `Dtend _ -> "DTEND"

  let eventprop_to_ics cr buf (prop: eventprop) =
    let key = eventprop_to_ics_key prop in
    match prop with
    | #generalprop as x -> generalprop_to_ics cr buf x
    | #other_prop as x -> other_prop_to_ics cr buf x
    | `Transparency (params, transp) -> write_line cr buf key params (write_string (List.assoc transp transp_strings))
    | `Dtend (params, date_or_time) -> write_line cr buf key params (date_or_time_to_ics date_or_time)

  let eventprops_to_ics cr buf props = List.iter (eventprop_to_ics cr buf) props

  let attendees_to_ics cr buf xs = List.iter (attendee_to_ics cr buf) xs

  let todoprop_to_ics_key = function
    | #generalprop as x -> generalprop_to_ics_key x
    | #other_prop as x -> other_prop_to_ics_key x
    | `Completed _ -> "COMPLETED"
    | `Percent _ -> "PERCENT"
    | `Due _ -> "DUE"

  let todoprop_to_ics cr buf prop =
    let key = todoprop_to_ics_key prop in
    match prop with
    | #generalprop as x -> generalprop_to_ics cr buf x
    | #other_prop as x -> other_prop_to_ics cr buf x
    | `Completed (params, ts) -> write_line cr buf key params (datetime_to_ics ts)
    | `Percent (params, pct) -> write_line cr buf key params (write_string (string_of_int pct))
    | `Due (params, date_or_time) -> write_line cr buf key params (date_or_time_to_ics date_or_time)

  let todoprops_to_ics cr buf props = List.iter (todoprop_to_ics cr buf) props

  let alarm_to_ics cr buf alarm =
    (* TODO: output alarm.other field *)
    write_line cr buf "BEGIN" [] (write_string "VALARM") ;
    let write_trigger buf trig =
      let params, print = match trig with
        | (params, `Duration d) ->
          (params, (duration_to_ics d))
        | (params, `Datetime dt) ->
          (params, (datetime_to_ics dt))
      in
      write_line cr buf "TRIGGER" params print
    in
    (match alarm with
     | `Audio (audio : audio_struct alarm_struct) ->
       write_line cr buf "ACTION" [] (write_string "AUDIO") ;
       write_trigger buf audio.trigger ;
       duration_repeat_to_ics cr buf audio.duration_repeat ;
       attach_to_ics cr buf audio.special.attach
     | `Display (display : display_struct alarm_struct) ->
       write_line cr buf "ACTION" [] (write_string "DISPLAY") ;
       write_trigger buf display.trigger ;
       duration_repeat_to_ics cr buf display.duration_repeat ;
       description_to_ics cr buf display.special.description
     | `Email email ->
       write_line cr buf "ACTION" [] (write_string "EMAIL") ;
       write_trigger buf email.trigger ;
       duration_repeat_to_ics cr buf email.duration_repeat ;
       attach_to_ics cr buf email.special.attach ;
       description_to_ics cr buf email.special.description ;
       summary_to_ics cr buf email.special.summary ;
       attendees_to_ics cr buf email.special.attendees ) ;
    write_line cr buf "END" [] (write_string "VALARM")

  let alarms_to_ics cr buf alarms = List.iter (alarm_to_ics cr buf) alarms

  let event_to_ics cr buf eventprops alarms =
    eventprops_to_ics cr buf eventprops ;
    alarms_to_ics cr buf alarms

  let todo_to_ics cr buf todoprops alarms =
    todoprops_to_ics cr buf todoprops ;
    alarms_to_ics cr buf alarms

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

  let tzprop_to_ics cr buf = function
    | `Dtstart (params, date_or_time) -> write_line cr buf "DTSTART" params (date_or_time_to_ics date_or_time)
    | `Tzoffset_to (params, span) -> write_line cr buf "TZOFFSETTO" params (write_string (span_to_string span))
    | `Tzoffset_from (params, span) -> write_line cr buf "TZOFFSETFROM" params (write_string (span_to_string span))
    | `Rrule (params, recurs) -> write_line cr buf "RRULE" params (recurs_to_ics recurs)
    | `Comment (params, comment) -> write_line cr buf "COMMENT" params (write_string comment)
    | `Rdate (params, dates_or_times_or_periods) ->
      write_line cr buf "RDATE" params
        (dates_or_times_or_periods_to_ics dates_or_times_or_periods)
    | `Tzname (params, id) -> write_line cr buf "TZNAME" params (write_string id)
    | #other_prop as x -> other_prop_to_ics cr buf x

  let tzprops_to_ics cr buf tzprops = List.iter (tzprop_to_ics cr buf) tzprops

  let timezoneprop_to_ics_key = function
    | `Timezone_id _ -> "TZID"
    | `Lastmod _ -> "LAST-MODIFIED"
    | `Tzurl _ -> "TZURL"
    | `Standard tzprops -> "STANDARD" (* TODO preserve structure *)
    | `Daylight tzprops -> "DAYLIGHT"
    | #other_prop as x -> other_prop_to_ics_key x

  let timezone_prop_to_ics cr buf = function
    | `Timezone_id (params, (prefix, name)) ->
      let value = Printf.sprintf "%s%s" (if prefix then "/" else "") name in
      write_line cr buf "TZID" params (write_string value)
    | `Lastmod (params, ts) -> write_line cr buf "LAST-MODIFIED" params (datetime_to_ics ts)
    | `Tzurl (params, uri) -> write_line cr buf "TZURL" params (write_string (Uri.to_string uri))
    | `Standard tzprops ->
      write_line cr buf "BEGIN" [] (write_string "STANDARD") ;
      tzprops_to_ics cr buf tzprops ;
      write_line cr buf "END" [] (write_string "STANDARD")
    | `Daylight tzprops ->
      write_line cr buf "BEGIN" [] (write_string "DAYLIGHT") ;
      tzprops_to_ics cr buf tzprops ;
      write_line cr buf "END" [] (write_string "DAYLIGHT")
    | #other_prop as x -> other_prop_to_ics cr buf x

  let freebusyprop_to_ics_key = function
    | #generalprop as x -> generalprop_to_ics_key x
    | #other_prop as x -> other_prop_to_ics_key x
    | `Dtend _ -> "DTEND"
    | `Freebusy _ -> "FREEBUSY"
 
  let freebusy_prop_to_ics cr buf prop = 
    let key = freebusyprop_to_ics_key prop in
    match prop with
    | `Freebusy (params, periods) -> write_line cr buf key params (fun buf -> List.iter (period_to_ics buf) periods)
    | `Dtend (params, date_or_time) -> write_line cr buf key params (date_or_time_to_ics date_or_time)
    | #generalprop as x -> generalprop_to_ics cr buf x
    | #other_prop as x -> other_prop_to_ics cr buf x

  let timezone_to_ics cr buf props = List.iter (timezone_prop_to_ics cr buf) props

  let freebusy_to_ics cr buf props = List.iter (freebusy_prop_to_ics cr buf) props

  let component_to_ics cr buf comp =
    let key = component_to_ics_key comp in
    write_line cr buf "BEGIN" [] (write_string key) ;
    (match comp with
     | `Event (eventprops, alarms) -> event_to_ics cr buf eventprops alarms
     | `Timezone tzprops -> timezone_to_ics cr buf tzprops
     | `Freebusy fbprops -> freebusy_to_ics cr buf fbprops
     | `Todo (todoprops, alarms) -> todo_to_ics cr buf todoprops alarms) ;
    write_line cr buf "END" [] (write_string key)

  let components_to_ics cr buf comps = List.iter (component_to_ics cr buf) comps

  let calendar_to_ics cr buf (props, comps) =
    write_line cr buf "BEGIN" [] (write_string "VCALENDAR") ;
    calprops_to_ics cr buf props ;
    components_to_ics cr buf comps ;
    write_line cr buf "END" [] (write_string "VCALENDAR")
end

let to_ics ?(cr = true) calendar =
  let buf = Buffer.create 1023 in
  Writer.calendar_to_ics cr buf calendar ;
  Buffer.contents buf

open Angstrom
exception Parse_error

let string_parsers m = List.map (fun (t, str) -> string str >>| fun _ -> t) m

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

let parse_datetime s =
  try parse_string datetime s with Parse_error -> Error "Invalid datetime."
 
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
  lift2 apply_sign (opt_sign <* char 'P') (date <|> time <|> week)

let float =
  let make_float s i f =
    let n = try float_of_string (i ^ "." ^ f) with Failure _ -> raise Parse_error in
    if s = positive then n else (-. n) in
  lift3 make_float opt_sign digits (option "" ((char '.') *> digits))

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
  let freq = choice (string_parsers freq_strings)
  and weekday = choice (string_parsers weekday_strings) in
  let apply_sign s i = (if s = positive then i else (-i)) in
  let apply_sign_triple s i c = (apply_sign s i, c) in
  let weekdaynum = lift3 apply_sign_triple opt_sign (option 0 (up_to_two_digits >>= in_range 1 53) ) weekday in
  let monthdaynum = lift2 apply_sign opt_sign (up_to_two_digits >>= in_range 1 31)
  and yeardaynum = lift2 apply_sign opt_sign (up_to_three_digits >>= in_range 1 366)
  and weeknum = lift2 apply_sign opt_sign (up_to_two_digits >>= in_range 1 53)
  and monthnum = up_to_two_digits >>= in_range 1 12
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
    | _ -> raise Parse_error in
    let c' = match count_or_until with
    | [] -> None
    | [ c ] -> Some c
    | _ -> raise Parse_error in
    match freqs with
    | [ f ] -> f, c', i', List.rev rest
    | _ -> raise Parse_error)
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
let iana_param = lift2 (fun k v -> P (Iana_param, (k, v)))
    (iana_token <* (char '=')) value_list


let x_param = lift2 (fun k v -> P (Xparam, (k, v)))
    (x_name <* char '=') value_list

let other_param = iana_param <|> x_param

let tzidparam =
 lift2 (fun a b -> P (Tzid, (a = '/', b)))
 (string "TZID=" *> option ' ' (char '/')) param_text

let valuetypeparam =
  lift (fun x -> P (Valuetype, x))
    (string "VALUE=" *>
     (choice (string_parsers valuetype_strings)
      <|> (x_name >>| fun x -> `Xname x)
      <|> (iana_token >>| fun x -> `Ianatoken x)))

(* TODO use uri parser here *)
let altrepparam = (string "ALTREP=") *> quoted_caladdress >>| fun uri -> P (Altrep, uri)

(* TODO use language tag rfc5646 parser *)
let languageparam = (string "LANGUAGE=") *> param_text >>| fun l -> P (Language, l)

let cnparam = string "CN=" *> param_value >>| fun cn -> P (Cn, cn)
let dirparam = string "DIR=" *> quoted_caladdress >>| fun s -> P (Dir, s)
let sentbyparam = string "SENT-BY=" *> quoted_caladdress >>| fun s -> P (Sentby, s)

(* Default is INDIVIDUAL *)
let cutypeparam =
  lift (fun x -> P (Cutype, x)) ((string "CUTYPE=") *>
       (choice (string_parsers cutype_strings)
   <|> (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))))

let fbtypeparam =
  lift (fun x -> P (Fbtype, x)) ((string "FBTYPE=") *>
       (choice (string_parsers fbtype_strings)
   <|> (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))))

let memberparam = lift (fun x -> P (Member, x))
  ((string "MEMBER=") *> sep_by1 (char ',') quoted_caladdress)

(* Default is REQ-PARTICIPANT *)
let roleparam = lift (fun x -> P (Role, x)) ((string "ROLE=") *>
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
  lift (fun x -> P (Partstat, x)) ((string "PARTSTAT=") *> statvalue)

let rsvpparam = lift (fun r -> P (Rsvp, r)) (string "RSVP=" *> ((string "TRUE" >>| fun _ -> true) <|> (string "FALSE" >>| fun _ -> false )))

let deltoparam = lift (fun x -> P (Delegated_to, x))
  ((string "DELEGATED-TO=") *> sep_by1 (char ',') quoted_caladdress)

let delfromparam = lift (fun x -> P (Delegated_from, x))
  ((string "DELEGATED-FROM=") *> sep_by1 (char ',') quoted_caladdress)

let reltypeparam =
  lift (fun x -> P (Reltype, x))
   (string "RELTYPE=" *>
     (choice (string_parsers relation_strings)
   <|> (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))))

let fmttypeparam =
  string "FMTTYPE=" *> media_type >>| fun m -> P (Media_type, m)

let encodingparam =
  string "ENCODING=BASE64" >>| fun _ -> P (Encoding, `Base64)

let rangeparam = string "RANGE=THISANDFUTURE" >>| fun _ -> P (Range, `Thisandfuture)

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

(* Properties *)
let propparser id pparser vparser lift =
  let params = many (char ';' *> pparser) in
  lift2 lift
    (string id *> params <* char ':')
    (vparser <* end_of_line)

let otherprop =
  let params = many (char ';' *> icalparameter) in
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
  propparser "DTSTAMP" other_param datetime (fun a b -> `Dtstamp (a, b))

let uid =
  propparser "UID" other_param text (fun a b -> `Uid (a, b))

let check_date_datetime default a b =
  let valuetype =
    try List.find (function P (Valuetype, _) -> true | _ -> false) a
    with Not_found -> P (Valuetype, default)
  in
  match valuetype, b with
  | P (Valuetype, `Datetime), `Datetime _ -> ()
  | P (Valuetype, `Date), `Date _ -> ()
  | _ -> raise Parse_error

let check_datetime_duration default a b =
  let valuetype =
    try List.find (function P (Valuetype, _) -> true | _ -> false) a
    with Not_found -> P (Valuetype, default)
  in
  match valuetype, b with
  | P (Valuetype, `Datetime), `Datetime _ -> ()
  | P (Valuetype, `Duration), `Duration _ -> ()
  | _ -> raise Parse_error

let check_date_datetime_period default a b =
  let valuetype =
    try List.find (function P (Valuetype, _) -> true | _ -> false) a
    with Not_found -> P (Valuetype, default)
  in
  match valuetype, b with
  | P (Valuetype, `Datetime), `Datetime _ -> ()
  | P (Valuetype, `Date), `Date _ -> ()
  | P (Valuetype, `Period), `Period _ -> ()
  | _ -> raise Parse_error

let check_binary_uri default a b =
  let valuetype =
    try List.find (function P (Valuetype, _) -> true | _ -> false) a
    with Not_found -> P (Valuetype, default)
  in
  match valuetype, b with
  | P (Valuetype, `Binary), `Binary _ -> ()
  | P (Valuetype, `Uri), `Uri _ -> ()
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

let completed =
  propparser "COMPLETED" other_param datetime
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
       `Due (a, b))

let class_ =
  let class_value = choice (string_parsers class_strings)
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
  let attach_param = fmttypeparam <|> valuetypeparam <|> encodingparam <|> other_param in
  let attach_value =
    (binary >>| fun b -> `Binary b) <|> (caladdress >>| fun a -> `Uri a)
  in
  propparser "ATTACH" attach_param attach_value
    (fun a b ->
       check_binary_uri `Uri a b ;
       let encoding = try Some (List.find (function P (Encoding, _) -> true | _ -> false) a) with Not_found -> None in
       match encoding, b with
       | None, `Uri _ -> `Attach (a, b)
       | Some (P (Encoding, `Base64)), `Binary _ -> `Attach (a, b)
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
  resources <|> rdate <|> otherprop

let eventprops = many eventprop

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
    <|> (iana_token >>| fun x -> `Ianatoken x)
    <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))
  in
  propparser "ACTION" other_param actionvalue (fun a b -> `Action (a, b))

let trigger =
  let trigrelparam =
    lift (fun x -> P (Related, x))
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
     | [] -> `Audio { trigger ; duration_repeat ; other = [] ; special = { attach } }
     | _ -> raise Parse_error in

  let build_display rest =
    let descriptions, rest' = List.partition (function `Description _ -> true | _ -> false ) rest in
    let description = match descriptions with
     | [`Description x] -> x
     | _ -> raise Parse_error in
    match rest' with
     | [] -> `Display { trigger ; duration_repeat ; other = [] ; special = { description } }
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
     | [] -> `Email { trigger ; duration_repeat ; other = [] ; special = { description ; summary ; attach ; attendees } }
     | _ -> raise Parse_error in

  match action with
    | _, `Audio -> build_audio rest'''
    | _, `Display -> build_display rest'''
    | _, `Email -> build_email rest'''
    | _, _ -> raise Parse_error (*not supported yet*)

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
  `Event (eventprops, List.fold_left f [] alarms)

let eventc =
  string "BEGIN:VEVENT" *> end_of_line *>
  lift2 build_event eventprops (many alarmc)
  <* string "END:VEVENT" <* end_of_line

let tzid =
  propparser "TZID" other_param
    (lift2 (fun a b -> (a = '/', b)) (option ' ' (char '/')) text)
    (fun p v -> `Timezone_id (p, v))

let tzurl =
  propparser "TZURL" other_param text
    (fun a b -> `Tzurl (a, Uri.of_string b))

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
  dtstart <|> tzoffsetto <|> tzoffsetfrom <|>
  rrule <|>
  comment <|> rdate <|> tzname <|> otherprop

let freebusy =
  let fbparam = fbtypeparam <|> other_param in
  propparser "FREEBUSY" fbparam (sep_by1 (char ',') period)
    (fun p v -> `Freebusy (p, v))

let freebusyprop =
  dtstamp <|> uid <|> contact <|> dtstart <|> dtend <|>
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
  
let component = many1 (eventc <|> todoc (* <|> journalc *) <|> freebusyc <|> timezonec)

let icalbody = lift2 pair calprops component

let calobject =
  string "BEGIN:VCALENDAR" *> end_of_line *>
  icalbody
  <* string "END:VCALENDAR" <* end_of_line <* end_of_input

let parse (str : string) : (calendar, string) result =
  try parse_string calobject (normalize_lines str)
  with Parse_error -> Error "parse error"

let recur_events dtstart (rrule : recurrence) =
  Recurrence.new_gen dtstart rrule 

let occurence_before_timestamp datetime (tzprops : tzprop list) =
  let dtstart = List.find (function `Dtstart _ -> true | _ -> false) tzprops in
  let dtstart' = match dtstart with
    | `Dtstart (_, `Datetime (dtstart, utc)) -> dtstart
    | `Dtstart (_, `Date start) ->  begin match Ptime.of_date_time (start, ((0, 0, 0), 0)) with
      | None -> assert false
      | Some dtstart -> dtstart
      end
    | _ -> assert false
  in
  (* dtstart in a vtimezone subcomponent may not contain a tzid property! *)
  let rrule = List.find_opt (function `Rrule _ -> true | _ -> false) tzprops in
  let next_event = match rrule with
  | None -> (fun () -> None)
  | Some (`Rrule (_, rrule)) -> recur_events dtstart' rrule
  in
  (* TODO handle RDATE in addition to rrule *)
  let rec in_timerange acc = function
    | Some dtstart'' ->
      if Ptime.is_earlier ~than:datetime dtstart''
      then in_timerange (Some dtstart'') (next_event ())
      else acc
   | None -> acc in
  in_timerange None (Some dtstart')

let calculate_offset (props : tzprop list) ts datetime =
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

let normalize_timezone datetime (`Tzid (is_unique, tzid)) (timezones : timezoneprop list list) =
  (* TODO optimize timezone data structure *)
  let timezoneprops =
    List.find (fun tzprops ->
        List.exists (function
            | `Timezone_id (_, (is_unique', tzid')) ->
              is_unique = is_unique' && String.equal tzid tzid'
            | _ -> false) tzprops)
      timezones
  in
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
  calculate_offset props ts datetime
