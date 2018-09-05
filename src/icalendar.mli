
(* TODO: tag these with `Utc | `Local *)
type utc_timestamp = Ptime.t
type local_timestamp = Ptime.t

type utc_or_local_timestamp = [
  | `Utc of utc_timestamp
  | `Local of local_timestamp
] [@@deriving eq, show]

type timestamp = [
  utc_or_local_timestamp
  | `With_tzid of local_timestamp * string
] [@@deriving eq, show]

type date_or_datetime = [ `Datetime of timestamp | `Date of Ptime.date ]

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
  | `Until of utc_or_local_timestamp (* TODO date or datetime *)
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

type param_value = [ `Quoted of string | `String of string ]

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
  | Iana_param : (string * param_value list) icalparameter (* TODO need to allow Iana_param "foo" and Iana_param "bar" in the same map! *)
  | Xparam : ((string * string) * param_value list) icalparameter

val equal_icalparameter : 'a icalparameter -> 'a -> 'b icalparameter -> 'b -> bool

module Params : sig include Gmap.S with type 'a key = 'a icalparameter end

type params = Params.t

type other_prop =
  [ `Iana_prop of string * params * string
  | `Xprop of (string * string) * params * string ] [@@deriving eq, show]

type cal_prop =
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

type general_prop = [
  | `Dtstamp of params * utc_timestamp
  | `Uid of params * string
  | `Dtstart of params * date_or_datetime
  | `Class of params * class_
  | `Created of params * utc_timestamp
  | `Description of params * string
  | `Geo of params * (float * float)
  | `Lastmod of params * utc_timestamp
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
  | `Exdate of params *
    [ `Datetimes of timestamp list | `Dates of Ptime.date list ]
  | `Rstatus of params * ((int * int * int option) * string * string option)
  | `Related of params * string
  | `Resource of params * string list
  | `Rdate of params *
              [ `Datetimes of timestamp list | `Dates of Ptime.date list | `Periods of (timestamp * Ptime.Span.t) list ]
]

type event_prop = [
  | general_prop
  | `Transparency of params * [ `Transparent | `Opaque ]
  | `Dtend of params * date_or_datetime
  (* TODO: valuetype same as DTSTART *)
  | other_prop
]

type 'a alarm_struct = {
  trigger : params * [ `Duration of Ptime.Span.t | `Datetime of utc_timestamp ] ;
  duration_repeat: ((params * Ptime.Span.t) * (params * int )) option ;
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

type tz_prop = [
  | `Dtstart_local of params * local_timestamp
  | `Tzoffset_to of params * Ptime.Span.t
  | `Tzoffset_from of params * Ptime.Span.t
  | `Rrule of params * recurrence
  | `Comment of params * string
  | `Rdate of params * [ `Datetimes of timestamp list | `Dates of Ptime.date list | `Periods of (timestamp * Ptime.Span.t) list ]
  | `Tzname of params * string
  | other_prop
]

type timezone_prop = [
  | `Timezone_id of params * (bool * string)
  | `Lastmod of params * utc_timestamp
  | `Tzurl of params * Uri.t
  | `Standard of tz_prop list
  | `Daylight of tz_prop list
  | other_prop
]

type todo_prop = [
  | general_prop
  | `Completed of params * utc_timestamp
  | `Percent of params * int
  | `Due of  params * date_or_datetime
  | other_prop
]

type freebusy_prop = [
  | `Dtstamp of params * utc_timestamp
  | `Uid of params * string
  | `Contact of params * string
  | `Dtstart_utc of params * utc_timestamp
  | `Dtend_utc of params * utc_timestamp
  | `Organizer of params * Uri.t
  | `Url of params * Uri.t
  | `Attendee of params * Uri.t
  | `Comment of params * string
  | `Freebusy of params * (utc_timestamp * Ptime.Span.t) list
  | `Rstatus of params * ((int * int * int option) * string * string option)
  | other_prop
]

type event = {
  dtstamp : params * utc_timestamp ;
  uid : params * string ;
  dtstart : params * date_or_datetime ; (* NOTE: optional if METHOD present according to RFC 5545 *)
  dtend_or_duration : [ `Duration of params * Ptime.Span.t | `Dtend of params * date_or_datetime ] option ;
  rrule : (params * recurrence) option ; (* NOTE: RFC says SHOULD NOT occur more than once *)
  props : event_prop list ;
  alarms : alarm list ;
}

type timezone = timezone_prop list

type component = [
  | `Event of event
  | `Todo of todo_prop list * alarm list
  | `Freebusy of freebusy_prop list
  | `Timezone of timezone
] [@@deriving show]

(*
val in_timerange : component -> (Ptime.t * bool) * (Ptime.t * bool) -> bool
*)
val component_to_ics_key : component -> string

type calendar = cal_prop list * component list

val parse_datetime: string -> (timestamp, string) result
val parse : string -> (calendar, string) result
val pp : calendar Fmt.t

(* TODO this actually belongs to CalDAV! this is Webdav_xml module! *)
type comp = [ `Allcomp | `Comp of component_transform list ]
and prop = [ `Allprop | `Prop of (string * bool) list ]
and component_transform = string * prop * comp [@@deriving show, eq]

val to_ics : ?cr:bool -> ?filter:component_transform option -> calendar -> string

module Writer : sig
  val duration_to_ics : Ptime.Span.t -> Buffer.t -> unit
end

val recur_dates : Ptime.t -> recurrence -> (unit -> Ptime.t option)

val recur_events : event -> (unit -> event option)

val normalize_timezone : Ptime.t -> bool * String.t ->
  timezone_prop list list ->
  Ptime.t

(*
val add_tzid_offset : local_timestamp -> string -> timezone list -> utc_timestamp
val remove_tzid_offset : utc_timestamp -> string -> timezone list -> local_timestamp
                                                                     *)
