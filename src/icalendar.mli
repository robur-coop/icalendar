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

val pp_icalparameter : icalparameter Fmt.t

type weekday = [ `Friday | `Monday | `Saturday | `Sunday | `Thursday | `Tuesday | `Wednesday ]

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

val pp_value : value Fmt.t

val parse : string -> ((string * icalparameter list * value) list, string) result
