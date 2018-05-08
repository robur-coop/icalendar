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

(* value parser *)
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

let time_or_date_param =
  lift (fun x -> `Valuetype x)
    (string "VALUE=" *>
     ((string "DATE-TIME" >>| fun _ -> `Datetime)
      <|> (string "DATE" >>| fun _ -> `Date)))

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

let time_or_date_or_period_param =
  string "VALUE=" *>
  (string "DATE-TIME" <|> string "DATE" <|> string "PERIOD") >>| function
  | "DATE-TIME" -> `Valuetype `Datetime
  | "DATE" -> `Valuetype `Date
  | "PERIOD" -> `Valuetype `Period
  | _ -> raise Parse_error

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

let build_time_or_date a b =
  let valuetype = try List.find (function `Valuetype _ -> true | _ -> false) a with Not_found -> `Valuetype `Datetime in
  match valuetype, b with
  | `Valuetype `Datetime, `Datetime dt -> b
  | `Valuetype `Date, `Date d -> b
  | _ -> raise Parse_error

let time_or_date =
  (datetime >>| fun dt -> `Datetime dt)
  <|> (date >>| fun d -> `Date d)

let dtstart =
  let dtstparam = time_or_date_param <|> tzidparam <|> other_param in
  propparser "DTSTART" dtstparam time_or_date
    (fun a b -> `Dtstart (a, build_time_or_date a b))

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
  let recur_param = tzidparam <|> time_or_date_param <|> range_param <|> other_param in
  propparser "RECURRENCE-ID" recur_param time_or_date
    (fun a b -> `Recur_id (a, build_time_or_date a b))

let rrule =
  propparser "RRULE" other_param recur (fun a b -> `Rrule (a, b))

let dtend =
  let dtend_param = tzidparam <|> time_or_date_param <|> other_param in
  propparser "DTEND" dtend_param time_or_date
    (fun a b -> `Dtend (a, build_time_or_date a b))

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
  and value_param =
    string "VALUE=BINARY" >>| fun _ -> `Valuetype `Binary
  and encoding_param =
    string "ENCODING=BASE64" >>| fun _ -> `Encoding `Base64
  in
  let attach_param = fmttype_param <|> value_param <|> encoding_param <|> other_param in
  let attach_value =
    (binary >>| fun b -> `Binary b) <|> (caladdress >>| fun a -> `Uri a)
  in
  propparser "ATTACH" attach_param attach_value
    (fun a b ->
       let valuetype = try Some (List.find (function `Valuetype _ -> true | _ -> false) a) with Not_found -> None
       and encoding = try Some (List.find (function `Encoding _ -> true | _ -> false) a) with Not_found -> None
       in
       match valuetype, encoding, b with
       | None, None, `Uri uri -> `Attach (a, `Uri uri)
       | Some (`Valuetype `Binary), Some (`Encoding `Base64), `Binary b -> `Attach (a,`Binary b)
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
  let exdtparam = time_or_date_param <|> tzidparam <|> other_param in
  let exdtvalue = sep_by1 (char ',') time_or_date in
  propparser "EXDATE" exdtparam exdtvalue
    (fun a b ->
       let dates = List.map (build_time_or_date a) b in
       let date =
         if List.for_all (function `Date _ -> true | _ -> false) dates then
           `Dates (List.map
                     (function `Date d -> d | _ -> raise Parse_error)
                     dates)
         else if List.for_all (function `Datetime _ -> true | _ -> false) dates then
           `Datetimes (List.map
                         (function `Datetime d -> d | _ -> raise Parse_error)
                         dates)
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

let reltypeparam =
  lift (fun x -> `Reltype x)
   (string "RELTYPE=" *>
      ((string "PARENT" >>| fun _ -> `Parent)
   <|> (string "CHILD" >>| fun _ -> `Child)
   <|> (string "SIBLING" >>| fun _ -> `Sibling)
   <|> (iana_token >>| fun x -> `Ianatoken x)
   <|> (x_name >>| fun (vendor, name) -> `Xname (vendor, name))))

let related =
  let relparam = reltypeparam <|> other_param in
  propparser "RELATED-TO" relparam text
    (fun a b -> `Related (a, b))

let resources =
  let resrcparam = languageparam <|> altrepparam <|> other_param in
  propparser "RESOURCES" resrcparam texts
    (fun a b -> `Resource (a, b))

let build_time_or_date_or_period a b =
  let valuetype = try List.find (function `Valuetype _ -> true | _ -> false) a with Not_found -> `Valuetype `Datetime in
  match valuetype, b with
  | `Valuetype `Datetime, `Datetime dt -> b
  | `Valuetype `Date, `Date d -> b
  | `Valuetype `Period, `Period p -> b
  | _ -> raise Parse_error

let time_or_date_or_period =
      (period >>| fun p -> `Period p)
  <|> (datetime >>| fun dt -> `Datetime dt)
  <|> (date >>| fun d -> `Date d)

let rdate =
  let rdtparam = tzidparam <|> time_or_date_or_period_param <|> other_param in
  let rdtvalue = sep_by1 (char ',') time_or_date_or_period in
  propparser "RDATE" rdtparam rdtvalue
    (fun a b ->
       let dates = List.map (build_time_or_date_or_period a) b in
       let date =
         if List.for_all (function `Date _ -> true | _ -> false) dates then
           `Dates (List.map
                     (function `Date d -> d | _ -> raise Parse_error)
                     dates)
         else if List.for_all (function `Datetime _ -> true | _ -> false) dates then
           `Datetimes (List.map
                         (function `Datetime d -> d | _ -> raise Parse_error)
                         dates)
         else if List.for_all (function `Period _ -> true | _ -> false) dates then
           `Periods (List.map
                         (function `Period d -> d | _ -> raise Parse_error)
                         dates)
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
  and valueparam =
    lift (fun x -> `Valuetype x)
      (string "VALUE=" *>
       ((string "DURATION" >>| fun _ -> `Duration)
        <|> (string "DATE-TIME" >>| fun _ -> `Datetime)))
  in
  let trigparam = trigrelparam <|> valueparam <|> other_param in
  let trigvalue =
        (dur_value >>| fun d -> `Duration d)
    <|> (datetime >>| fun d -> `Datetime d)
  in
  propparser "TRIGGER" trigparam trigvalue
    (fun a b ->
       let vtype = try List.find (function `Valuetype _ -> true | _ -> false) a with Not_found -> `Valuetype `Duration in
       (match vtype, b with
        | `Valuetype `Duration, `Duration _ -> ()
        | `Valuetype `Datetime, `Datetime _ -> ()
        | _ -> raise Parse_error) ;
       `Trigger (a, b))

let repeat =
  let rvalue = digits >>= ensure int_of_string in
  propparser "REPEAT" other_param rvalue
    (fun a b -> `Repeat (a, b))

let audioprop =
  action <|> trigger <|>
  duration <|> repeat <|>
  attach

let dispprop =
  (* action <|> *) description (* <|> trigger <|>
   duration <|> repeat *)

let emailprop =
  (* action <|> description <|> trigger <|> *) summary <|>
  attendee (* <|>
  duration <|> repeat <|>
  attach *)

(* let otherprop = x_prop <|> iana_prop *)

let alarmc =
  string "BEGIN:VALARM" *> end_of_line *>
  many (audioprop <|> dispprop <|> emailprop (* <|> otherprop *))
  <* string "END:VALARM" <* end_of_line

let eventc =
  string "BEGIN:VEVENT" *> end_of_line *>
  lift2 pair eventprops (many alarmc)
  <* string "END:VEVENT" <* end_of_line

let component = many1 (eventc (* <|> todoc <|> journalc <|> freebusyc <|> timezonec *))

let icalbody = lift2 pair calprops component

let calobject =
  string "BEGIN:VCALENDAR" *> end_of_line *> icalbody <* string "END:VCALENDAR" <* end_of_line <* end_of_input

type other_param =
  [ `Iana_param of string * string list
  | `Xparam of (string * string) * string list ]

let pp_other_param fmt = function
  | `Iana_param (k, v) -> Fmt.pf fmt "key %s value %a" k (Fmt.list Fmt.string) v
  | `Xparam ((vendor, name), v) -> Fmt.pf fmt "vendor %s key %s value %a" vendor name (Fmt.list Fmt.string) v

type calprop =
  [ `Prodid of other_param list * string
  | `Version of other_param list * string
  | `Calscale of other_param list * string
  | `Method of other_param list * string
  ]

type weekday = [ `Friday | `Monday | `Saturday | `Sunday | `Thursday | `Tuesday | `Wednesday ]

let pp_weekday fmt wd =
  Fmt.string fmt @@ match wd with
  | `Friday -> "friday"
  | `Monday -> "monday"
  | `Saturday -> "saturday"
  | `Sunday -> "sunday"
  | `Thursday -> "thursday"
  | `Tuesday -> "tuesday"
  | `Wednesday -> "wednesday"

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

let pp_recur fmt =
  let pp_list pp_e fmt xs = Fmt.(list ~sep:(unit ",@ ") pp_e) fmt xs
  and pp_triple pp_a pp_b pp_c fmt (a, b, c) =
    Fmt.pf fmt "%a, %a, %a" pp_a a pp_b b pp_c c
  and pp_frequency fmt f =
    Fmt.string fmt @@ match f with
    | `Daily -> "daily"
    | `Hourly -> "hourly"
    | `Minutely -> "minutely"
    | `Monthly -> "monthly"
    | `Secondly -> "secondly"
    | `Weekly -> "weekly"
    | `Yearly -> "yearly"
  in
  function
  | `Byminute ms -> Fmt.pf fmt "byminute %a" (pp_list Fmt.int) ms
  | `Byday days -> Fmt.pf fmt "byday %a" (pp_list (pp_triple Fmt.char Fmt.int pp_weekday)) days
  | `Byhour hours -> Fmt.pf fmt "byhour %a" (pp_list Fmt.int) hours
  | `Bymonth months -> Fmt.pf fmt "bymonth %a" (pp_list Fmt.(pair ~sep:(unit ", ") char int)) months
  | `Bymonthday monthdays -> Fmt.pf fmt "bymonthday %a" (pp_list Fmt.(pair ~sep:(unit ", ") char int)) monthdays
  | `Bysecond seconds -> Fmt.pf fmt "bysecond %a" (pp_list Fmt.int) seconds
  | `Bysetposday (s, i) -> Fmt.pf fmt "bysetposday %a %d" Fmt.char s i
  | `Byweek weeks -> Fmt.pf fmt "byweek %a" (pp_list Fmt.(pair ~sep:(unit ", ") char int)) weeks
  | `Byyearday days -> Fmt.pf fmt "byyearday %a" (pp_list Fmt.(pair ~sep:(unit ", ") char int)) days
  | `Count n -> Fmt.pf fmt "count %d" n
  | `Frequency f -> Fmt.pf fmt "frequency %a" pp_frequency f
  | `Interval i -> Fmt.pf fmt "interval %d" i
  | `Until (ts, utc) -> Fmt.pf fmt "until %a UTC? %b" Ptime.pp ts utc
  | `Weekday wd -> Fmt.pf fmt "weekday %a" pp_weekday wd

let pp_date fmt (y, m, d) = Fmt.pf fmt "%04d-%02d-%02d" y m d

let pp_other_params = Fmt.list pp_other_param

let pp_calprop fmt = function
  | `Prodid (l, s) -> Fmt.pf fmt "product id %a %s" pp_other_params l s
  | `Version (l, s) -> Fmt.pf fmt "version %a %s" pp_other_params l s
  | `Calscale (l, s) -> Fmt.pf fmt "calscale %a %s" pp_other_params l s
  | `Method (l, s) -> Fmt.pf fmt "method %a %s" pp_other_params l s

type class_ = [ `Public | `Private | `Confidential | `Ianatoken of string | `Xname of string * string ]

let pp_class fmt = function
  | `Public -> Fmt.string fmt "public"
  | `Private -> Fmt.string fmt "private"
  | `Confidential -> Fmt.string fmt "confidential"
  | `Ianatoken t -> Fmt.pf fmt "ianatoken %s" t
  | `Xname (v, t) -> Fmt.pf fmt "xname vendor %s %s" v t

type status = [ `Draft | `Final | `Cancelled |
                `Needs_action | `Completed | `In_process | (* `Cancelled *)
                `Tentative | `Confirmed (* | `Cancelled *) ]

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

type eventprop =
  [ `Dtstamp of other_param list * (Ptime.t * bool)
  | `Uid of other_param list * string
  | `Dtstart of [ other_param | `Valuetype of [`Datetime | `Date ] | `Tzid of bool * string ] list * 
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
  | `Recur_id of [ other_param | `Tzid of bool * string | `Valuetype of [ `Datetime | `Date ] | `Range of [ `Thisandfuture ] ] list *
                 [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Rrule of other_param list * recur list
  | `Dtend of [ other_param | `Valuetype of [`Datetime | `Date ] | `Tzid of bool * string ] list * 
              [ `Datetime of Ptime.t * bool | `Date of Ptime.date ]
  | `Duration of other_param list * int
  | `Attach of [`Media_type of string * string | `Encoding of [ `Base64 ] | `Valuetype of [ `Binary ] | other_param ] list *
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
  | `Exdate of [ other_param | `Valuetype of [ `Datetime | `Date ] | `Tzid of bool * string ] list *
               [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list ]
  | `Rstatus of [ other_param | `Language of string ] list * ((int * int * int option) * string * string option)
  | `Related of [ other_param | `Reltype of relationship ] list * string
  | `Resource of [ other_param | `Language of string | `Altrep of Uri.t ] list * string list
  | `Rdate of [ other_param | `Valuetype of [ `Datetime | `Date | `Period ] | `Tzid of bool * string ] list *
              [ `Datetimes of (Ptime.t * bool) list | `Dates of Ptime.date list | `Periods of (Ptime.t * Ptime.t * bool) list ]
  ]

let pp_dtstart_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Valuetype `Datetime -> Fmt.string fmt "valuetype datetime"
  | `Valuetype `Date -> Fmt.string fmt "valuetype date"
  | `Tzid (prefix, name) -> Fmt.pf fmt "tzid prefix %b %s" prefix name

let pp_dtstart_value fmt = function
  | `Datetime (p, utc) -> Fmt.pf fmt "datetime %a Utc?%b" Ptime.pp p utc
  | `Date d -> Fmt.pf fmt "date %a" pp_date d

let pp_categories_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Language l -> Fmt.pf fmt "language %s" l

let pp_desc_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Altrep uri -> Fmt.pf fmt "altrep uri %a" Uri.pp_hum uri
  | `Language l -> Fmt.pf fmt "language %s" l

let pp_organizer_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Language l -> Fmt.pf fmt "language %s" l
  | `Cn c -> Fmt.pf fmt "cn %s" c
  | `Dir d -> Fmt.pf fmt "dir %a" Uri.pp_hum d
  | `Sentby s -> Fmt.pf fmt "sent-by %a" Uri.pp_hum s

let pp_recur_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Valuetype `Datetime -> Fmt.string fmt "valuetype datetime"
  | `Valuetype `Date -> Fmt.string fmt "valuetype date"
  | `Tzid (prefix, name) -> Fmt.pf fmt "tzid prefix %b %s" prefix name
  | `Range `Thisandfuture -> Fmt.string fmt "range: thisandfuture"

let pp_status fmt s =
  Fmt.string fmt @@
  match s with
  | `Draft -> "draft"
  | `Final -> "final"
  | `Cancelled -> "cancelled"
  | `Needs_action -> "needs-action"
  | `Completed -> "completed"
  | `In_process -> "in-process"
  | `Tentative -> "tentative"
  | `Confirmed -> "confirmed"

let pp_attach_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Valuetype `Binary -> Fmt.string fmt "valuetype binary"
  | `Encoding `Base64 -> Fmt.string fmt "encoding base64"
  | `Media_type (typename, subtypename) -> Fmt.pf fmt "mediatype %s/%s" typename subtypename

let pp_attach_value fmt = function
  | `Binary b -> Fmt.string fmt b
  | `Uri u -> Uri.pp_hum fmt u

let pp_cutype fmt = function
  | `Individual -> Fmt.string fmt "individual"
  | `Group -> Fmt.string fmt "group"
  | `Resource -> Fmt.string fmt "resource"
  | `Room -> Fmt.string fmt "room"
  | `Unknown -> Fmt.string fmt "unknown"
  | `Ianatoken t -> Fmt.pf fmt "ianatoken %s" t
  | `Xname (x, y) -> Fmt.pf fmt "xname %s,%s" x y

let pp_role fmt = function
  | `Chair -> Fmt.string fmt "chair"
  | `Reqparticipant -> Fmt.string fmt "required participant"
  | `Optparticipant -> Fmt.string fmt "optional participant"
  | `Nonparticipant -> Fmt.string fmt "non participant"
  | `Ianatoken t -> Fmt.pf fmt "ianatoken %s" t
  | `Xname (x, y) -> Fmt.pf fmt "xname %s,%s" x y

let pp_partstat fmt = function
  | `Needs_action -> Fmt.string fmt "needs action"
  | `Accepted -> Fmt.string fmt "accepted"
  | `Declined -> Fmt.string fmt "declined"
  | `Tentative -> Fmt.string fmt "tentative"
  | `Delegated -> Fmt.string fmt "delegated"
  | `Completed -> Fmt.string fmt "completed"
  | `In_process -> Fmt.string fmt "in-process"
  | `Ianatoken t -> Fmt.pf fmt "ianatoken %s" t
  | `Xname (x, y) -> Fmt.pf fmt "xname %s,%s" x y

let pp_attendee_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Cn c -> Fmt.pf fmt "cn %s" c
  | `Cutype c -> Fmt.pf fmt "cutype %a" pp_cutype c
  | `Delegated_from l -> Fmt.pf fmt "delegated from %a" (Fmt.list Uri.pp_hum) l
  | `Delegated_to l -> Fmt.pf fmt "delegated to %a" (Fmt.list Uri.pp_hum) l
  | `Dir d -> Fmt.pf fmt "dir %a" Uri.pp_hum d
  | `Language l -> Fmt.pf fmt "language %s" l
  | `Member l -> Fmt.pf fmt "member %a" (Fmt.list Uri.pp_hum) l
  | `Partstat s -> Fmt.pf fmt "partstat %a" pp_partstat s
  | `Role r -> Fmt.pf fmt "role %a" pp_role r
  | `Rsvp b -> Fmt.pf fmt "rsvp %b" b
  | `Sentby s -> Fmt.pf fmt "sent-by %a" Uri.pp_hum s

let pp_exdate_value fmt = function
  | `Datetimes dates -> Fmt.pf fmt "%a" Fmt.(list (pair Ptime.pp bool)) dates
  | `Dates dates -> Fmt.pf fmt "%a" Fmt.(list pp_date) dates

let pp_relationship fmt = function
  | `Parent -> Fmt.string fmt "parent"
  | `Child -> Fmt.string fmt "child"
  | `Sibling -> Fmt.string fmt "sibling"
  | `Ianatoken t -> Fmt.pf fmt "ianatoken %s" t
  | `Xname (x, y) -> Fmt.pf fmt "xname %s,%s" x y

let pp_related_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Reltype c -> Fmt.pf fmt "reltype %a" pp_relationship c

let pp_rdate_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Tzid (prefix, name) -> Fmt.pf fmt "tzid prefix %b %s" prefix name
  | `Valuetype `Datetime -> Fmt.string fmt "valuetype datetime"
  | `Valuetype `Date -> Fmt.string fmt "valuetype date"
  | `Valuetype `Period -> Fmt.string fmt "valuetype period"

let pp_rdate_value fmt = function
  | `Datetimes xs -> Fmt.pf fmt "datetimes %a" Fmt.(list (pair Ptime.pp bool)) xs
  | `Dates ds -> Fmt.pf fmt "dates %a" (Fmt.list pp_date) ds
  | `Periods ps ->
    let pp_period fmt (start, stop, utc) =
      Fmt.pf fmt "%a - %a utc? %b" Ptime.pp start Ptime.pp stop utc
    in
    Fmt.pf fmt "periods %a" (Fmt.list pp_period) ps

let pp_eventprop fmt = function
  | `Dtstamp (l, (p, utc)) -> Fmt.pf fmt "dtstamp %a %a %b" pp_other_params l Ptime.pp p utc
  | `Uid (l, s) -> Fmt.pf fmt "uid %a %s" pp_other_params l s 
  | `Dtstart (l, v) -> Fmt.pf fmt "dtstart %a %a" (Fmt.list pp_dtstart_param) l pp_dtstart_value v
  | `Class (l, v) -> Fmt.pf fmt "class %a %a" pp_other_params l pp_class v
  | `Created (l, (p, utc)) -> Fmt.pf fmt "created %a %a %b" pp_other_params l Ptime.pp p utc
  | `Description (l, v) -> Fmt.pf fmt "description %a %s" (Fmt.list pp_desc_param) l v
  | `Geo (l, (lat, lon)) -> Fmt.pf fmt "geo %a lat %f lon %f" pp_other_params l lat lon
  | `Lastmod (l, (p, utc)) -> Fmt.pf fmt "last modified %a %a %b" pp_other_params l Ptime.pp p utc
  | `Location (l, v) -> Fmt.pf fmt "location %a %s" (Fmt.list pp_desc_param) l v
  | `Organizer (l, v) -> Fmt.pf fmt "organizer %a %a" (Fmt.list pp_organizer_param) l Uri.pp_hum v
  | `Priority (l, v) -> Fmt.pf fmt "priority %a %d" pp_other_params l v
  | `Seq (l, v) -> Fmt.pf fmt "seq %a %d" pp_other_params l v
  | `Status (l, v) -> Fmt.pf fmt "status %a %a" pp_other_params l pp_status v
  | `Summary (l, v) -> Fmt.pf fmt "summary %a %s" (Fmt.list pp_desc_param) l v
  | `Transparency (l, v) -> Fmt.pf fmt "transparency %a %s" pp_other_params l
                              (match v with `Transparent -> "transparent" | `Opaque -> "opaque")
  | `Url (l, v) -> Fmt.pf fmt "url %a %a" pp_other_params l Uri.pp_hum v
  | `Recur_id (l, v) -> Fmt.pf fmt "recur-id %a %a" (Fmt.list pp_recur_param) l pp_dtstart_value v
  | `Rrule (l, v) -> Fmt.pf fmt "rrule %a %a" pp_other_params l (Fmt.list pp_recur) v
  | `Dtend (l, v) -> Fmt.pf fmt "dtend %a %a" (Fmt.list pp_dtstart_param) l pp_dtstart_value v
  | `Duration (l, v) -> Fmt.pf fmt "duration %a %d seconds" pp_other_params l v
  | `Attach (l, v) -> Fmt.pf fmt "attach %a %a" (Fmt.list pp_attach_param) l pp_attach_value v 
  | `Attendee (l, v) -> Fmt.pf fmt "attendee %a %a" (Fmt.list pp_attendee_param) l Uri.pp_hum v
  | `Categories (l, v) -> Fmt.pf fmt "categories %a %a" (Fmt.list pp_categories_param) l (Fmt.list Fmt.string) v
  | `Comment (l, v) -> Fmt.pf fmt "comment %a %s" (Fmt.list pp_desc_param) l v
  | `Contact (l, v) -> Fmt.pf fmt "contact %a %s" (Fmt.list pp_desc_param) l v
  | `Exdate (l, v) -> Fmt.pf fmt "exdate %a %a" (Fmt.list pp_dtstart_param) l pp_exdate_value v
  | `Rstatus (l, ((one, two, three), desc, extdata)) ->
    Fmt.pf fmt "rstatus %a %d.%d.%a %s %a" (Fmt.list pp_categories_param) l
      one two Fmt.(option int) three desc Fmt.(option string) extdata
  | `Related (l, v) -> Fmt.pf fmt "related to %a %s" (Fmt.list pp_related_param) l v
  | `Resource (l, v) -> Fmt.pf fmt "resource %a %a" (Fmt.list pp_desc_param) l Fmt.(list string) v
  | `Rdate (l, v) -> Fmt.pf fmt "rdate %a %a" (Fmt.list pp_rdate_param) l pp_rdate_value v

type alarm = [
  | `Action of other_param list * [ `Audio | `Display | `Email | `Ianatoken of string | `Xname of string * string ]
  | `Trigger of [ other_param | `Valuetype of [ `Datetime | `Duration ] | `Related of [ `Start | `End ] ] list *
                [ `Duration of int | `Datetime of (Ptime.t * bool) ]
  | `Duration of other_param list * int
  | `Repeat of other_param list * int
  | `Attach of [`Media_type of string * string | `Encoding of [ `Base64 ] | `Valuetype of [ `Binary ] | other_param ] list *
               [ `Uri of Uri.t | `Binary of string ]
  | `Description of [other_param | `Altrep of Uri.t | `Language of string ] list * string
  | `Summary of [other_param | `Altrep of Uri.t | `Language of string ] list * string
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
] list

let pp_action fmt = function
  | `Audio -> Fmt.string fmt "audio"
  | `Display -> Fmt.string fmt "display"
  | `Email -> Fmt.string fmt "email"
  | `Ianatoken a -> Fmt.pf fmt "ianatoken %s" a
  | `Xname (a, b) -> Fmt.pf fmt "xname %s %s" a b

let pp_trigger_param fmt = function
  | #other_param as p -> pp_other_param fmt p
  | `Valuetype `Datetime -> Fmt.string fmt "valuetype datetime"
  | `Valuetype `Duration -> Fmt.string fmt "valuetype duration"
  | `Related `Start -> Fmt.string fmt "related to start"
  | `Related `End -> Fmt.string fmt "related to end"

let pp_trigger_value fmt = function
  | `Datetime (p, utc) -> Fmt.pf fmt "datetime %a utc %b" Ptime.pp p utc
  | `Duration s -> Fmt.pf fmt "period %ds" s

let pp_alarm_element fmt = function
  | `Action (params, action) -> Fmt.pf fmt "action %a %a" pp_other_params params pp_action action
  | `Trigger (params, value) -> Fmt.pf fmt "trigger %a %a" (Fmt.list pp_trigger_param) params pp_trigger_value value
  | `Duration (params, value) -> Fmt.pf fmt "duration %a %ds" pp_other_params params value
  | `Repeat (params, value) -> Fmt.pf fmt "repeat %a %ds" pp_other_params params value
  | `Attach (l, v) -> Fmt.pf fmt "attach %a %a" (Fmt.list pp_attach_param) l pp_attach_value v 
  | `Description (l, v) -> Fmt.pf fmt "description %a %s" (Fmt.list pp_desc_param) l v
  | `Summary (l, v) -> Fmt.pf fmt "summary %a %s" (Fmt.list pp_desc_param) l v
  | `Attendee (l, v) -> Fmt.pf fmt "attendee %a %a" (Fmt.list pp_attendee_param) l Uri.pp_hum v

let pp_alarm fmt data =
  (Fmt.list pp_alarm_element) fmt data

type component = eventprop list * alarm list

let pp_component fmt (props, alarms) =
  Fmt.pf fmt "props: %a @.alarms:%a"
    (Fmt.list pp_eventprop) props
    (Fmt.list pp_alarm) alarms

type calendar = calprop list * component list

let pp_calendar: calendar Fmt.t = fun fmt (props, comps) -> Fmt.pf fmt "properties %a components %a" (Fmt.list pp_calprop) props (Fmt.list pp_component) comps
(*
type calendar = {
  version : version ;
  productid : string ;
  calendarscale : foo option ;
  ... : .. option ;
  other_properties : properties list / map ;
  components : component list
}
*)

let parse_calobject (str : string) : (calendar, string) result =
  try parse_string calobject (normalize_lines str)
  with Parse_error -> Error "parse error"
