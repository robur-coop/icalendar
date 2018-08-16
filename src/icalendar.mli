
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

type interval = int

type recurrence = freq * count_or_until option * interval option * recur list [@@deriving eq, show]

type other_param =
  [ `Iana_param of string * string list
  | `Xparam of (string * string) * string list ]

type valuetype = [
    `Binary | `Boolean | `Caladdress | `Date | `Datetime | `Duration | `Float
  | `Integer | `Period | `Recur | `Text | `Time | `Uri | `Utcoffset
  | `Xname of (string * string) | `Ianatoken of string
]

type valuetypeparam = [ `Valuetype of valuetype ]

type cutype = [ `Group | `Individual | `Resource | `Room | `Unknown
              | `Ianatoken of string | `Xname of string * string ]

type partstat = [ `Accepted | `Completed | `Declined | `Delegated
                | `In_process | `Needs_action | `Tentative
                | `Ianatoken of string | `Xname of string * string ]

type role = [ `Chair | `Nonparticipant | `Optparticipant | `Reqparticipant
            | `Ianatoken of string | `Xname of string * string ]

type relationship =
  [ `Parent | `Child | `Sibling |
    `Ianatoken of string | `Xname of string * string ]

type fbtype = [ `Free | `Busy | `Busy_Unavailable | `Busy_Tentative | `Ianatoken of string | `Xname of string * string ] [@@deriving eq, show]

type icalparameter =
  [ `Altrep of Uri.t
  | `Cn of string
  | `Cutype of cutype
  | `Delegated_from of Uri.t list
  | `Delegated_to of Uri.t list
  | `Dir of Uri.t
  | `Encoding of [ `Base64 ]
  | `Media_type of string * string
  | `Fbtype of fbtype
  | `Language of string
  | `Member of Uri.t list
  | `Partstat of partstat
  | `Range of [ `Thisandfuture ]
  | `Related of [ `Start | `End ]
  | `Reltype of relationship
  | `Role of role
  | `Rsvp of bool
  | `Sentby of Uri.t
  | `Tzid of bool * string
  | valuetypeparam
  | other_param
  ]

type other_prop =
  [ `Iana_prop of string * icalparameter list * string
  | `Xprop of (string * string) * icalparameter list * string ] [@@deriving eq, show]

type calprop =
  [ `Prodid of other_param list * string
  | `Version of other_param list * string
  | `Calscale of other_param list * string
  | `Method of other_param list * string
  | other_prop
  ]

type class_ = [ `Public | `Private | `Confidential | `Ianatoken of string | `Xname of string * string ]

type status = [ `Draft | `Final | `Cancelled |
                `Needs_action | `Completed | `In_process | (* `Cancelled *)
                `Tentative | `Confirmed (* | `Cancelled *) ]

type generalprop = [
  | `Dtstamp of other_param list * (Ptime.t * bool)
  | `Uid of other_param list * string
  | `Dtstart of [ other_param | valuetypeparam | `Tzid of bool * string ] list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
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
  | `Url of other_param list * Uri.t
  | `Recur_id of [ other_param | `Tzid of bool * string | valuetypeparam | `Range of [ `Thisandfuture ] ] list *
                 [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Rrule of other_param list * recurrence
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
]

type eventprop = [
  | generalprop
  | `Transparency of other_param list * [ `Transparent | `Opaque ]
  | `Dtend of [ other_param | valuetypeparam | `Tzid of bool * string ] list *
              [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | other_prop
]

type 'a alarm_struct = {
  trigger : [ other_param | valuetypeparam | `Related of [ `Start | `End ] ] list *
    [ `Duration of int | `Datetime of (Ptime.t * bool) ] ;
  duration_repeat: ((other_param list * int) * (other_param list * int )) option ;
  other: other_prop list ;
  special: 'a ;
}

type audio_struct = {
  attach: ([`Media_type of string * string | `Encoding of [ `Base64 ] | valuetypeparam | other_param ] list *
    [ `Uri of Uri.t | `Binary of string ]) option ;
  (* xprop: list ;
  iana_prop: list ; *)
}

type display_struct = {
  description : [ other_param | `Altrep of Uri.t | `Language of string ] list * string ;
}

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
}

type alarm = [ `Audio of audio_struct alarm_struct | `Display of display_struct alarm_struct | `Email of email_struct alarm_struct ]

type tzprop = [
  | `Dtstart of [ other_param | valuetypeparam | `Tzid of bool * string ] list *
    [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Tzoffset_to of other_param list * Ptime.Span.t
  | `Tzoffset_from of other_param list * Ptime.Span.t
  | `Rrule of other_param list * recurrence
  | `Comment of [ other_param | `Language of string | `Altrep of Uri.t ] list * string
  | `Rdate of [ other_param | valuetypeparam | `Tzid of bool * string ] list *
              [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list | `Periods of (Ptime.t * Ptime.t * bool) list ]
  | `Tzname of [ other_param | `Language of string ] list * string
  | other_prop
]

type timezoneprop = [
  | `Timezone_id of other_param list * (bool * string)
  | `Lastmod of other_param list * (Ptime.t * bool)
  | `Tzurl of other_param list * Uri.t
  | `Standard of tzprop list
  | `Daylight of tzprop list
  | other_prop
]

type todoprop = [
  | generalprop
  | `Completed of other_param list * (Ptime.t * bool)
  | `Percent of other_param list * int
  | `Due of  [ other_param | valuetypeparam | `Tzid of bool * string ] list *
             [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | other_prop
]

type freebusyprop = [
  | `Dtstamp of other_param list * (Ptime.t * bool)
  | `Uid of other_param list * string
  | `Contact of [ other_param | `Language of string | `Altrep of Uri.t ] list * string
  | `Dtstart of [ other_param | valuetypeparam | `Tzid of bool * string ] list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Dtend of [ other_param | valuetypeparam | `Tzid of bool * string ] list *
              [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Organizer of [other_param | `Cn of string | `Dir of Uri.t | `Sentby of Uri.t | `Language of string] list * Uri.t
  | `Url of other_param list * Uri.t
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
  | `Comment of [ other_param | `Language of string | `Altrep of Uri.t ] list * string
  | `Freebusy of [ other_param | `Fbtype of fbtype ] list * (Ptime.t * Ptime.t * bool) list 
  | `Rstatus of [ other_param | `Language of string ] list * ((int * int * int option) * string * string option)
  | other_prop 
]

type component = [
  | `Event of eventprop list * alarm list
  | `Todo of todoprop list * alarm list
  | `Freebusy of freebusyprop list 
  | `Timezone of timezoneprop list
] [@@deriving show]

(*
val in_timerange : component -> (Ptime.t * bool) * (Ptime.t * bool) -> bool
*)
val component_to_ics_key : component -> string

type calendar = calprop list * component list

val collect_tzids: component -> Astring.String.set

val parse_datetime: string -> (Ptime.t * bool, string) result
val parse : string -> (calendar, string) result
val pp : calendar Fmt.t

val to_ics : ?cr:bool -> calendar -> string

module Writer : sig
  val duration_to_ics : int -> Buffer.t -> unit
  val calprop_to_ics_key : calprop -> string
  val eventprop_to_ics_key : eventprop -> string
  val todoprop_to_ics_key : todoprop -> string
  val freebusyprop_to_ics_key : freebusyprop -> string
  val timezoneprop_to_ics_key : timezoneprop -> string
end

val recur_events : Ptime.t -> recurrence -> (unit -> Ptime.t option)

val normalize_timezone : Ptime.t -> [< `Tzid of bool * String.t ] ->
  timezoneprop list list ->
  Ptime.t 
