
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

type valuetype = [
    `Binary | `Boolean | `Caladdress | `Date | `Datetime | `Duration | `Float
  | `Integer | `Period | `Recur | `Text | `Time | `Uri | `Utcoffset
  | `Xname of (string * string) | `Ianatoken of string
]

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

type param = P : 'a icalparameter * 'a -> param

type other_prop =
  [ `Iana_prop of string * param list * string
  | `Xprop of (string * string) * param list * string ] [@@deriving eq, show]

type calprop =
  [ `Prodid of param list * string
  | `Version of param list * string
  | `Calscale of param list * string
  | `Method of param list * string
  | other_prop
  ] 

type class_ = [ `Public | `Private | `Confidential | `Ianatoken of string | `Xname of string * string ]

type status = [ `Draft | `Final | `Cancelled |
                `Needs_action | `Completed | `In_process | (* `Cancelled *)
                `Tentative | `Confirmed (* | `Cancelled *) ]

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
] 

type eventprop = [
  | generalprop
  | `Transparency of param list * [ `Transparent | `Opaque ]
  | `Dtend of param list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | other_prop
] 

type 'a alarm_struct = {
  trigger : param list * [ `Duration of int | `Datetime of (Ptime.t * bool) ] ;
  duration_repeat: ((param list * int) * (param list * int )) option ;
  other: other_prop list ;
  special: 'a ;
}

type audio_struct = {
  attach: (param list * [ `Uri of Uri.t | `Binary of string ]) option ;
}

type display_struct = {
  description : param list * string ;
}

type email_struct = {
  description : param list * string ;
  summary : param list * string ;
  attendees : (param list * Uri.t) list ;
  attach: (param list * [ `Uri of Uri.t | `Binary of string ]) option ;
}

type alarm = [ `Audio of audio_struct alarm_struct | `Display of display_struct alarm_struct | `Email of email_struct alarm_struct ]

type tzprop = [
  | `Dtstart of param list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Tzoffset_to of param list * Ptime.Span.t
  | `Tzoffset_from of param list * Ptime.Span.t
  | `Rrule of param list * recurrence
  | `Comment of param list * string
  | `Rdate of param list * [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list | `Periods of (Ptime.t * Ptime.t * bool) list ]
  | `Tzname of param list * string
  | other_prop
]

type timezoneprop = [
  | `Timezone_id of param list * (bool * string)
  | `Lastmod of param list * (Ptime.t * bool)
  | `Tzurl of param list * Uri.t
  | `Standard of tzprop list
  | `Daylight of tzprop list
  | other_prop
]

type todoprop = [
  | generalprop
  | `Completed of param list * (Ptime.t * bool)
  | `Percent of param list * int
  | `Due of  param list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | other_prop
]

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
