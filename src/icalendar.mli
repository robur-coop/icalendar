
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

val equal_icalparameter : 'a icalparameter -> 'a -> 'b icalparameter -> 'b -> bool

module Params : sig include Gmap.S with type 'a key = 'a icalparameter end

type params = Params.t

type other_prop =
  [ `Iana_prop of string * params * string
  | `Xprop of (string * string) * params * string ] [@@deriving eq, show]

type calprop =
  [ `Prodid of params * string
  | `Version of params * string
  | `Calscale of params * string
  | `Method of params * string
  | other_prop
  ] 

type class_ = [ `Public | `Private | `Confidential | `Ianatoken of string | `Xname of string * string ]

type status = [ `Draft | `Final | `Cancelled |
                `Needs_action | `Completed | `In_process | (* `Cancelled *)
                `Tentative | `Confirmed (* | `Cancelled *) ]

type generalprop = [
  | `Dtstamp of params * (Ptime.t * bool)
  | `Uid of params * string
  | `Dtstart of params * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Class of params * class_
  | `Created of params * (Ptime.t * bool)
  | `Description of params * string
  | `Geo of params * (float * float)
  | `Lastmod of params * (Ptime.t * bool)
  | `Location of params * string
  | `Organizer of params * Uri.t
  | `Priority of params * int
  | `Seq of params * int
  | `Status of params * status
  | `Summary of params * string
  | `Url of params * Uri.t
  | `Recur_id of params * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Rrule of params * recurrence
  | `Duration of params * int
  | `Attach of params * [ `Uri of Uri.t | `Binary of string ]
  | `Attendee of params * Uri.t
  | `Categories of params * string list
  | `Comment of params * string
  | `Contact of params * string
  | `Exdate of params *
    [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list ]
  | `Rstatus of params * ((int * int * int option) * string * string option)
  | `Related of params * string
  | `Resource of params * string list
  | `Rdate of params *
              [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list | `Periods of (Ptime.t * Ptime.t * bool) list ]
] 

type eventprop = [
  | generalprop
  | `Transparency of params * [ `Transparent | `Opaque ]
  | `Dtend of params * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | other_prop
] 

type 'a alarm_struct = {
  trigger : params * [ `Duration of int | `Datetime of (Ptime.t * bool) ] ;
  duration_repeat: ((params * int) * (params * int )) option ;
  other: other_prop list ;
  special: 'a ;
}

type audio_struct = {
  attach: (params * [ `Uri of Uri.t | `Binary of string ]) option ;
}

type display_struct = {
  description : params * string ;
}

type email_struct = {
  description : params * string ;
  summary : params * string ;
  attendees : (params * Uri.t) list ;
  attach: (params * [ `Uri of Uri.t | `Binary of string ]) option ;
}

type alarm = [ `Audio of audio_struct alarm_struct | `Display of display_struct alarm_struct | `Email of email_struct alarm_struct ]

type tzprop = [
  | `Dtstart of params * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Tzoffset_to of params * Ptime.Span.t
  | `Tzoffset_from of params * Ptime.Span.t
  | `Rrule of params * recurrence
  | `Comment of params * string
  | `Rdate of params * [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list | `Periods of (Ptime.t * Ptime.t * bool) list ]
  | `Tzname of params * string
  | other_prop
]

type timezoneprop = [
  | `Timezone_id of params * (bool * string)
  | `Lastmod of params * (Ptime.t * bool)
  | `Tzurl of params * Uri.t
  | `Standard of tzprop list
  | `Daylight of tzprop list
  | other_prop
]

type todoprop = [
  | generalprop
  | `Completed of params * (Ptime.t * bool)
  | `Percent of params * int
  | `Due of  params * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | other_prop
]

type freebusyprop = [
  | `Dtstamp of params * (Ptime.t * bool)
  | `Uid of params * string
  | `Contact of params * string
  | `Dtstart of params * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Dtend of params * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Organizer of params * Uri.t
  | `Url of params * Uri.t
  | `Attendee of params * Uri.t
  | `Comment of params * string
  | `Freebusy of params * (Ptime.t * Ptime.t * bool) list 
  | `Rstatus of params * ((int * int * int option) * string * string option)
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

val normalize_timezone : Ptime.t -> bool * String.t ->
  timezoneprop list list ->
  Ptime.t 
