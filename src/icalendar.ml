open Angstrom

(* param data structure *)
type cutype = [`Individual | `Group | `Resource | `Room | `Unknown | `Xname | `Ianatoken ]
type encoding = [`Eightbit | `Base64 ]
type typename = []
type subtypename = []
type fbtype = [ `Free | `Busy | `Busyunavailable | `Busytentative | `Xname | `Ianatoken ]
type partstat = [ `Needsaction | `Accepted | `Declined | `Tentative | `Delegated | `Completed | `Inprocess | `Xname | `Ianatoken ]
type trigrel = [ `Start | `End ]
type reltype = [ `Parent | `Child | `Sibling | `Ianatoken | `Xname ]
type role = [ `Chair | `Reqparticipant | `Optparticipant | `Nonparticipant | `Xname | `Ianatoken ]
type uri = string
type caladdress = string
type languagetag = string
type valuetype = [ `Binary | `Boolean | `Caladdress | `Date | `Datetime | `Duration 
 | `Float | `Integer | `Period | `Recur | `Text | `Time | `Uri | `Utcoffset | `Xname | `Ianatoken ]
type icalparameter = [`Altrep of uri | `Cn of string | `Cutype of cutype 
 | `Delfrom of caladdress list | `Delto of caladdress list | `Dir of uri 
 | `Encoding of encoding | `Fmttype of typename * subtypename | `Fbtype of fbtype
 | `Language of languagetag | `Member of caladdress list | `Partstat of partstat 
 | `Range | `Trigrel of trigrel | `Reltype of reltype | `Role of role | `Rsvp of bool 
 | `Sentby of caladdress list | `Tzid of bool * string | `Valuetype of valuetype | `Other ]

(* base data structure *)
let collect_param key value = (key, value)
let collect_contentline key params value = (key, params, value) 

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
let param = lift2 collect_param param_name (char '=' *> param_value)

let value = take_while (fun x -> not (is_control x)) (*in fact it is more complicated*)
let contentline = lift3 collect_contentline name (many ( char ';' *> param )) (char ':' *> value <* end_of_line) 
let contentlines = many contentline <* end_of_input

(* processing *)

let normalize_lines s = 
  let re = Re.compile ( Re.Perl.re ~opts:[`Multiline] "(\n|\r\n)^\\s" ) in
  Re.replace_string ~all:true re ~by:"" s

let parse (str:string) = parse_string contentlines (normalize_lines str)
