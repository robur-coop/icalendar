
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
  | `Valuetype of valuetype
  | `Iana_param of string * string list
  | `Xparam of (string * string) * string list 
  ] 

type other_prop =
  [ `Iana_prop of string * icalparameter list * string
  | `Xprop of (string * string) * icalparameter list * string ] [@@deriving eq, show]

type calprop =
  [ `Prodid of icalparameter list * string
  | `Version of icalparameter list * string
  | `Calscale of icalparameter list * string
  | `Method of icalparameter list * string
  | other_prop
  ] 

type class_ = [ `Public | `Private | `Confidential | `Ianatoken of string | `Xname of string * string ]

type status = [ `Draft | `Final | `Cancelled |
                `Needs_action | `Completed | `In_process | (* `Cancelled *)
                `Tentative | `Confirmed (* | `Cancelled *) ]

type generalprop = [
  | `Dtstamp of icalparameter list * (Ptime.t * bool)
  | `Uid of icalparameter list * string
  | `Dtstart of icalparameter list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Class of icalparameter list * class_
  | `Created of icalparameter list * (Ptime.t * bool)
  | `Description of icalparameter list * string
  | `Geo of icalparameter list * (float * float)
  | `Lastmod of icalparameter list * (Ptime.t * bool)
  | `Location of icalparameter list * string
  | `Organizer of icalparameter list * Uri.t
  | `Priority of icalparameter list * int
  | `Seq of icalparameter list * int
  | `Status of icalparameter list * status
  | `Summary of icalparameter list * string
  | `Url of icalparameter list * Uri.t
  | `Recur_id of icalparameter list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Rrule of icalparameter list * recurrence
  | `Duration of icalparameter list * int
  | `Attach of icalparameter list * [ `Uri of Uri.t | `Binary of string ]
  | `Attendee of icalparameter list * Uri.t
  | `Categories of icalparameter list * string list
  | `Comment of icalparameter list * string
  | `Contact of icalparameter list * string
  | `Exdate of icalparameter list *
    [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list ]
  | `Rstatus of icalparameter list * ((int * int * int option) * string * string option)
  | `Related of icalparameter list * string
  | `Resource of icalparameter list * string list
  | `Rdate of icalparameter list *
              [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list | `Periods of (Ptime.t * Ptime.t * bool) list ]
] 

type eventprop = [
  | generalprop
  | `Transparency of icalparameter list * [ `Transparent | `Opaque ]
  | `Dtend of icalparameter list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | other_prop
] 

type 'a alarm_struct = {
  trigger : icalparameter list * [ `Duration of int | `Datetime of (Ptime.t * bool) ] ;
  duration_repeat: ((icalparameter list * int) * (icalparameter list * int )) option ;
  other: other_prop list ;
  special: 'a ;
}

type audio_struct = {
  attach: (icalparameter list * [ `Uri of Uri.t | `Binary of string ]) option ;
}

type display_struct = {
  description : icalparameter list * string ;
}

type email_struct = {
  description : icalparameter list * string ;
  summary : icalparameter list * string ;
  attendees : (icalparameter list * Uri.t) list ;
  attach: (icalparameter list * [ `Uri of Uri.t | `Binary of string ]) option ;
}

type alarm = [ `Audio of audio_struct alarm_struct | `Display of display_struct alarm_struct | `Email of email_struct alarm_struct ]

type tzprop = [
  | `Dtstart of icalparameter list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Tzoffset_to of icalparameter list * Ptime.Span.t
  | `Tzoffset_from of icalparameter list * Ptime.Span.t
  | `Rrule of icalparameter list * recurrence
  | `Comment of icalparameter list * string
  | `Rdate of icalparameter list * [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list | `Periods of (Ptime.t * Ptime.t * bool) list ]
  | `Tzname of icalparameter list * string
  | other_prop
]

type timezoneprop = [
  | `Timezone_id of icalparameter list * (bool * string)
  | `Lastmod of icalparameter list * (Ptime.t * bool)
  | `Tzurl of icalparameter list * Uri.t
  | `Standard of tzprop list
  | `Daylight of tzprop list
  | other_prop
]

type todoprop = [
  | generalprop
  | `Completed of icalparameter list * (Ptime.t * bool)
  | `Percent of icalparameter list * int
  | `Due of  icalparameter list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | other_prop
]

type freebusyprop = [
  | `Dtstamp of icalparameter list * (Ptime.t * bool)
  | `Uid of icalparameter list * string
  | `Contact of icalparameter list * string
  | `Dtstart of icalparameter list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Dtend of icalparameter list * [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Organizer of icalparameter list * Uri.t
  | `Url of icalparameter list * Uri.t
  | `Attendee of icalparameter list * Uri.t
  | `Comment of icalparameter list * string
  | `Freebusy of icalparameter list * (Ptime.t * Ptime.t * bool) list 
  | `Rstatus of icalparameter list * ((int * int * int option) * string * string option)
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
