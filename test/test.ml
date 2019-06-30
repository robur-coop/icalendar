let compare_calendar =
  let module M = struct
    type t = Icalendar.calendar
    let pp = Icalendar.pp
    let equal = Icalendar.equal_calendar
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let result_c = Alcotest.(result compare_calendar string)

open Icalendar

let empty = Params.empty
let singleton k v = Params.add k v empty
let list_to_map xs =
  List.fold_right (fun (Params.B (k, v)) -> Params.add k v) xs empty

let to_ptime date time =
  match Ptime.of_date_time (date, (time, 0)) with
  | None -> Alcotest.fail "invalid date time"
  | Some p -> p

let test_line () =
  let line =
    {_|BEGIN:VCALENDAR
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DESCRIPTION:This is a long description that exists on a long line.
END:VEVENT
END:VCALENDAR
|_}
  in
  let expected =
    let event =
      { uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
        dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
        dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
        dtend_or_duration = None ; rrule = None ;
        props = [ `Description (empty, "This is a long description that exists on a long line.") ] ;
        alarms = [] }
    in
    Ok ([], [ `Event event ])
  in
  Alcotest.check result_c "test short line" expected (parse line)

let test_multiline () =
  let multiline = {_|BEGIN:VCALENDAR
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DESCRIPTION:This is a lo
 ng description
  that exists on a long line.
END:VEVENT
END:VCALENDAR
|_} in
  let expected =
    let event =
      { uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
        dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
        dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
        dtend_or_duration = None ; rrule = None ;
        props = [ `Description (empty, "This is a long description that exists on a long line.") ] ;
        alarms = [] }
    in
    Ok ([], [ `Event event ])
  in
  Alcotest.check result_c "test short line" expected (parse multiline)

let calendar_object () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event =
       { uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
         dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
         dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
         dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
         rrule = None ;
         props = [ `Summary (empty, "Bastille Day Party") ] ;
         alarms = []
       }
    in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_tzid () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART;TZID=America/New_York:19970714T170000
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45) ) ;
      dtstart = (empty, `Datetime (`With_tzid (to_ptime (1997, 07, 14) (17, 00, 00), (false, "America/New_York")))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_class () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
CLASS:PUBLIC
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Class (empty, `Public) ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_created () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
CREATED:19960329T133000Z
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Created (empty, to_ptime (1996, 03, 29) (13, 30, 00)) ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_description () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
DESCRIPTION;ALTREP="CID:part3.msg970930T083000SILVER@example.com":Meeting to provide technical review for "Phoenix"
  design.\nHappy Face Conference Room. Phoenix design team
  MUST attend this meeting.\nRSVP to team leader.
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [
        `Description (singleton Altrep (Uri.of_string "CID:part3.msg970930T083000SILVER@example.com"), "Meeting to provide technical review for \"Phoenix\" design.\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\nRSVP to team leader.") ;
        `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_geo () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
GEO:37.386013;-122.082932
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Geo (empty, (37.386013, -122.082932) ) ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_last_mod () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
LAST-MODIFIED:19960817T133000Z
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Lastmod (empty, to_ptime (1996, 08, 17) (13, 30, 00)) ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_location () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
LOCATION;ALTREP="http://xyzcorp.com/conf-rooms/f123.vcf":
 Conference Room - F123\, Bldg. 002
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props =
        [ `Location (singleton Altrep (Uri.of_string "http://xyzcorp.com/conf-rooms/f123.vcf"), "Conference Room - F123, Bldg. 002") ;
          `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_organizer () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
ORGANIZER;SENT-BY="mailto:sray@example.com":mailto:
 jsmith@example.com
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Organizer(singleton Sentby (Uri.of_string "mailto:sray@example.com"), Uri.of_string "mailto:jsmith@example.com") ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_priority () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
PRIORITY:2
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Priority (empty, 2) ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_seq () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
SEQUENCE:1234
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Seq (empty, 1234) ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_status () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
STATUS:TENTATIVE
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Status (empty, `Tentative) ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_summary () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
SUMMARY:Department Party
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Summary (empty, "Department Party") ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_transp () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
TRANSP:TRANSPARENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Transparency (empty, `Transparent) ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_url () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
URL:http://example.com/pub/busy/jpublic-01.ifb
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Url (empty, Uri.of_string "http://example.com/pub/busy/jpublic-01.ifb") ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_recur_id () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
RECURRENCE-ID;RANGE=THISANDFUTURE:19960120T120000Z
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Recur_id (singleton Range `Thisandfuture, `Datetime (`Utc (to_ptime (1996, 01, 20) (12,00,00)))) ;
                `Summary (empty, "Bastille Day Party") ] ;
        alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_rrule () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
RRULE:FREQ=DAILY;UNTIL=19971224T000000Z
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = Some (empty, (`Daily, Some( `Until (`Utc (to_ptime (1997, 12, 24) (00, 00, 00)))), None, [])) ;
      props = [ `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_duration () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
DURATION:PT1H0M0S
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Duration (empty, Ptime.Span.of_int_s 3600)) ;
      rrule = None ;
      props = [ `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_attach () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:TG9yZW
 0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW
 5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IG
 xhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbm
 ltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIG
 xhYm9yaXMgbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdW
 F0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbi
 B2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdC
 BudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaW
 RhdGF0IG5vbiBwcm9pZGVudCwgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYS
 BkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4=
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00)))));
      rrule = None ;
props = [ `Attach (list_to_map [ B (Media_type, ("text", "plain")) ; B (Encoding, `Base64) ; B (Valuetype, `Binary)], `Binary "TG9yZW\
0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW\
5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IG\
xhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbm\
ltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIG\
xhYm9yaXMgbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdW\
F0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbi\
B2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdC\
BudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaW\
RhdGF0IG5vbiBwcm9pZGVudCwgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYS\
BkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4=") ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_attendee () =
  let input l =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
ATTENDEE|_} ^ l ^ {_|
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected l =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ l ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  let inputs = List.map input [
    {_|;MEMBER="mailto:DEV-GROUP@example.com":mailto:joecool@example.com|_} ;
    {_|;ROLE=REQ-PARTICIPANT:mailto:hcabot@example.com|_} ;
    {_|;DELEGATED-FROM="mailto:immud@example.com":mailto:ildoit@example.com|_} ;
    {_|;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;CN=Henry Cabot:mailto:hcabot@example.com|_} ;
    {_|;ROLE=REQ-PARTICIPANT;DELEGATED-FROM="mailto:bob@example.com";PARTSTAT=ACCEPTED;CN=Jane Doe:mailto:jdoe@example.com|_} ;
    (*{_|;CN=John Smith;DIR="ldap://example.com:6666/o=ABC%20Industries,c=US???(cn=Jim%20Dolittle)":mailto:jimdo@example.com|_} ;*)
    {_|;CN=John Smith;DIR="ldap://example.com:6666":mailto:jimdo@example.com|_} ;
    {_|;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;DELEGATED-FROM="mailto:iamboss@example.com";CN=Henry Cabot:mailto:hcabot@example.com|_} ;
    {_|;ROLE=NON-PARTICIPANT;PARTSTAT=DELEGATED;DELEGATED-TO="mailto:hcabot@example.com";CN=The Big Cheese:mailto:iamboss@example.com|_} ;
    {_|;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;CN=Jane Doe:mailto:jdoe@example.com|_} ;
  ]
  and expecteds = List.map expected [
    `Attendee (singleton Member [Uri.of_string "mailto:DEV-GROUP@example.com"], Uri.of_string "mailto:joecool@example.com") ;
    `Attendee (singleton Role `Reqparticipant, Uri.of_string "mailto:hcabot@example.com") ;
    `Attendee (singleton Delegated_from [Uri.of_string "mailto:immud@example.com"], Uri.of_string "mailto:ildoit@example.com") ;
    `Attendee (list_to_map [B (Role, `Reqparticipant) ; B (Partstat, `Tentative) ; B (Cn, `String "Henry Cabot")], Uri.of_string "mailto:hcabot@example.com") ;
    `Attendee (list_to_map [B (Role, `Reqparticipant) ; B (Delegated_from, [Uri.of_string "mailto:bob@example.com"]) ; B (Partstat, `Accepted) ; B (Cn, `String "Jane Doe")], Uri.of_string "mailto:jdoe@example.com") ;
    (*`Attendee ([`Cn "John Smith" ; `Dir (Uri.of_string "ldap://example.com:6666/o=ABC%20Industries,c=US???(cn=Jim%20Dolittle)")], Uri.of_string "mailto:jimdo@example.com") ;*)
    `Attendee (list_to_map [B (Cn, `String "John Smith") ; B (Dir, (Uri.of_string "ldap://example.com:6666"))], Uri.of_string "mailto:jimdo@example.com") ;
    `Attendee (list_to_map [B (Role, `Reqparticipant) ; B (Partstat, `Tentative) ; B (Delegated_from, [Uri.of_string "mailto:iamboss@example.com"]) ; B (Cn, `String "Henry Cabot")], Uri.of_string "mailto:hcabot@example.com") ;
    `Attendee (list_to_map [B (Role, `Nonparticipant) ; B (Partstat, `Delegated) ; B (Delegated_to, [Uri.of_string "mailto:hcabot@example.com"]) ; B (Cn, `String "The Big Cheese")], Uri.of_string "mailto:iamboss@example.com") ;
    `Attendee (list_to_map [B (Role, `Reqparticipant) ; B (Partstat, `Accepted) ; B (Cn, `String "Jane Doe")], Uri.of_string "mailto:jdoe@example.com") ;
  ] in
  List.iter2 (fun i e -> Alcotest.check result_c __LOC__ e (parse i)) inputs expecteds

let calendar_object_with_categories () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
CATEGORIES:APPOINTMENT,EDUCATION
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00)))));
      rrule = None ;
      props = [ `Categories (empty, ["APPOINTMENT" ; "EDUCATION"]) ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_comment () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
COMMENT:The meeting really needs to include both ourselves
  and the customer. We can't hold this meeting without them.
  As a matter of fact\, the venue for the meeting ought to be at
  their site. - - John
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Comment (empty, "The meeting really needs to include both ourselves and the customer. We can't hold this meeting without them. As a matter of fact, the venue for the meeting ought to be at their site. - - John") ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_contact () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
CONTACT;ALTREP="CID:part3.msg970930T083000SILVER@example.com":
 Jim Dolittle\, ABC Industries\, +1-919-555-1234
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Contact (singleton Altrep (Uri.of_string "CID:part3.msg970930T083000SILVER@example.com"), "Jim Dolittle, ABC Industries, +1-919-555-1234") ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_exdate () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
EXDATE:19960402T010000Z,19960403T010000Z,19960404T010000Z
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Exdate (empty, `Datetimes [ (`Utc (to_ptime (1996, 04, 02) (01, 00, 00))) ;
                                             (`Utc (to_ptime (1996, 04, 03) (01, 00, 00))) ;
                                             (`Utc (to_ptime (1996, 04, 04) (01, 00, 00))) ]) ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_rstatus () =
  let input s =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
REQUEST-STATUS:|_} ^ s ^ {_|
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected s =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ s ; `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  let inputs = List.map input [
      "2.0;Success" ;
      "3.1;Invalid property value;DTSTART:96-Apr-01" ;
      "2.8; Success\\, repeating event ignored. Scheduled as a single event.;RRULE:FREQ=WEEKLY\\;INTERVAL=2" ;
      "4.1;Event conflict.  Date-time is busy." ;
      "3.7;Invalid calendar user;ATTENDEE:mailto:jsmith@example.com" ;
    ]
  and expecteds = List.map expected [
      `Rstatus (empty, ((2, 0, None), "Success", None)) ;
      `Rstatus (empty, ((3, 1, None), "Invalid property value", Some "DTSTART:96-Apr-01")) ;
      `Rstatus (empty, ((2, 8, None), " Success, repeating event ignored. Scheduled as a single event.", Some "RRULE:FREQ=WEEKLY;INTERVAL=2")) ;
      `Rstatus (empty, ((4, 1, None), "Event conflict.  Date-time is busy.", None)) ;
      `Rstatus (empty, ((3, 7, None), "Invalid calendar user", Some "ATTENDEE:mailto:jsmith@example.com")) ;
    ]
  in
  List.iter2 (fun i e -> Alcotest.check result_c __LOC__ e (parse i)) inputs expecteds

let calendar_object_with_related () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
RELATED-TO:jsmith.part7.19960817T083000.xyzMail@example.com
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Related (empty, "jsmith.part7.19960817T083000.xyzMail@example.com") ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_related2 () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
RELATED-TO:19960401-080045-4000F192713-0052@example.com
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Related (empty, "19960401-080045-4000F192713-0052@example.com") ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_resource () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
RESOURCES:EASEL,PROJECTOR,VCR
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Resource (empty, [ "EASEL" ; "PROJECTOR" ; "VCR" ]) ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_resource2 () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
RESOURCES;LANGUAGE=fr:Nettoyeur haute pression
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Resource (singleton Language "fr", [ "Nettoyeur haute pression" ]) ;
                `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_rdate () =
  let input s =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
RDATE|_} ^ s ^ {_|
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected s =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ s ; `Summary (empty, "Bastille Day Party") ] ;
      alarms = []
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  let inputs = List.map input [
      ":19970714T123000Z" ;
      ";TZID=America/New_York:19970714T083000" ;
      ";VALUE=PERIOD:19960403T020000Z/19960403T040000Z,19960404T010000Z/PT3H" ;
      ";VALUE=DATE:19970101,19970120,19970217,19970421,19970526,19970704,19970901,19971014,19971128,19971129,19971225"
    ]
  and expecteds = List.map expected [
      `Rdate (empty, `Datetimes [ `Utc (to_ptime (1997, 07, 14) (12, 30, 00)) ]) ;
      `Rdate (empty, `Datetimes [ `With_tzid (to_ptime (1997, 07, 14) (08, 30, 00), (false, "America/New_York")) ]) ;
      `Rdate (singleton Valuetype `Period, `Periods [
          (`Utc (to_ptime (1996, 04, 03) (02, 00, 00)), Ptime.Span.of_int_s (2 * 60 * 60), true) ;
          (`Utc (to_ptime (1996, 04, 04) (01, 00, 00)), Ptime.Span.of_int_s (3 * 60 * 60), false)
        ]) ;
      `Rdate (singleton Valuetype `Date, `Dates [ (1997, 01, 01) ; (1997, 01, 20) ; (1997, 02, 17) ;
                                             (1997, 04, 21) ; (1997, 05, 26) ; (1997, 07, 04) ;
                                             (1997, 09, 01) ; (1997, 10, 14) ; (1997, 11, 28) ;
                                             (1997, 11, 29) ; (1997, 12, 25) ])
    ]
  in
  List.iter2 (fun i e -> Alcotest.check result_c __LOC__ e (parse i)) inputs expecteds

let calendar_object_with_illegal_valarm () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
BEGIN:VALARM
ACTION:AUDIO
END:VALARM
END:VEVENT
END:VCALENDAR
|_}
  and expected = Error "parse error: build_alarm props: trigger"
  in
  Alcotest.check result_c __LOC__ expected (parse input)


let calendar_object_with_valarm_action () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
BEGIN:VALARM
ACTION:AUDIO
TRIGGER;VALUE=DATE-TIME:19970317T133000Z
END:VALARM
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Summary (empty, "Bastille Day Party") ] ;
      alarms =
        [ `Audio { Icalendar.trigger = (singleton Valuetype `Datetime, `Datetime (to_ptime (1997, 03, 17) (13, 30, 00))) ; duration_repeat = None ; other = [] ; special = {Icalendar.attach = None } } ]
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_valarm_trigger () =
  let input s =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
BEGIN:VALARM
ACTION:AUDIO
TRIGGER|_} ^ s ^ {_|
END:VALARM
END:VEVENT
END:VCALENDAR
|_}
  and expected s =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Summary (empty, "Bastille Day Party") ] ;
      alarms = [ `Audio { Icalendar.trigger = s ; duration_repeat = None ; other = [] ; special = {Icalendar.attach = None } } ]
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  let inputs = List.map input [
      ":-PT15M" ;
      ";RELATED=END:PT5M" ;
      ";VALUE=DATE-TIME:19980101T050000Z" ;
    ]
  and expecteds = List.map expected [
      (empty, `Duration (Ptime.Span.of_int_s (- (15 * 60)))) ;
      (singleton Related `End, `Duration (Ptime.Span.of_int_s (5 * 60))) ;
      (singleton Valuetype `Datetime, `Datetime (to_ptime (1998, 01, 01) (05, 00, 00)))
    ]
  in
  List.iter2 (fun i e -> Alcotest.check result_c __LOC__ e (parse i)) inputs expecteds

let calendar_object_with_valarm_duration () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
BEGIN:VALARM
ACTION:AUDIO
TRIGGER:-PT30M
DURATION:PT1H
REPEAT:2
END:VALARM
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Summary (empty, "Bastille Day Party") ] ;
      alarms = [
        `Audio { Icalendar.trigger = (empty, `Duration (Ptime.Span.of_int_s (-1800))) ;
                 duration_repeat = Some ((empty, Ptime.Span.of_int_s 3600), (empty, 2)) ;
                 other = [] ;
                 special = { Icalendar.attach = None }}
      ]
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_valarm_repeat () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
BEGIN:VALARM
TRIGGER:-PT30M
ACTION:AUDIO
DURATION:PT1H
REPEAT:4
END:VALARM
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Summary (empty, "Bastille Day Party") ] ;
      alarms = [
        `Audio { Icalendar.trigger = (empty, `Duration (Ptime.Span.of_int_s (-1800))) ; 
                 duration_repeat = Some ((empty, Ptime.Span.of_int_s 3600), (empty, 4)) ;
                 other = [] ;
                 special = { Icalendar.attach = None } }
      ]
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_valarm_attach () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
BEGIN:VALARM
ACTION:AUDIO
TRIGGER:-PT30M
ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:TG9yZW
 0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW
 5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IG
 xhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbm
 ltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIG
 xhYm9yaXMgbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdW
 F0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbi
 B2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdC
 BudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaW
 RhdGF0IG5vbiBwcm9pZGVudCwgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYS
 BkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4=
END:VALARM
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Summary (empty, "Bastille Day Party") ] ;
      alarms = [
        `Audio { Icalendar.trigger = (empty, `Duration (Ptime.Span.of_int_s (-1800))) ;
                 duration_repeat = None;
                 other = [] ;
                 special = { Icalendar.attach = Some
                                 (list_to_map [B (Media_type, ("text", "plain")) ; B (Encoding, `Base64) ; B (Valuetype, `Binary)], `Binary "TG9yZW\
0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW\
5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IG\
xhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbm\
ltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIG\
xhYm9yaXMgbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdW\
F0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbi\
B2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdC\
BudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaW\
RhdGF0IG5vbiBwcm9pZGVudCwgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYS\
BkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4=")
                           }
               }
      ]
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_valarm_description () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
BEGIN:VALARM
ACTION:DISPLAY
TRIGGER:-PT30M
DESCRIPTION;ALTREP="CID:part3.msg970930T083000SILVER@example.com":Meeting to provide technical review for "Phoenix"
  design.\nHappy Face Conference Room. Phoenix design team
  MUST attend this meeting.\nRSVP to team leader.
END:VALARM
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Summary (empty, "Bastille Day Party") ] ;
      alarms = [
        `Display { Icalendar.trigger = (empty, `Duration (Ptime.Span.of_int_s (-1800))) ;
                   duration_repeat = None ;
                   other = [] ;
                   special = { Icalendar.description = (singleton Altrep (Uri.of_string "CID:part3.msg970930T083000SILVER@example.com"), "Meeting to provide technical review for \"Phoenix\" design.\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\nRSVP to team leader.") } }
      ]
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_valarm_summary () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
BEGIN:VALARM
ACTION:EMAIL
TRIGGER:-PT30M
SUMMARY:*** REMINDER: SEND AGENDA FOR WEEKLY STAFF MEETING ***
ATTENDEE:mailto:john_doe@example.com
DESCRIPTION:A draft agenda needs to be sent out to the attendees
  to the weekly managers meeting (MGR-LIST). Attached is a
  pointer the document template for the agenda file.
END:VALARM
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Summary (empty, "Bastille Day Party") ] ;
      alarms = [
        `Email { Icalendar.trigger = (empty, `Duration (Ptime.Span.of_int_s (-1800))) ;
                 duration_repeat = None ;
                 other = [] ;
                 special = { Icalendar.summary = (empty, "*** REMINDER: SEND AGENDA FOR WEEKLY STAFF MEETING ***") ;
                             description = (empty, "A draft agenda needs to be sent out to the attendees to the weekly managers meeting (MGR-LIST). Attached is a pointer the document template for the agenda file.") ;
                             attendees = [(empty, Uri.of_string "mailto:john_doe@example.com")] ;
                             attach = None ;
                           }
               }
      ]
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_display_alarm_relative () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
BEGIN:VALARM
TRIGGER:-PT30M
REPEAT:2
DURATION:PT15M
ACTION:DISPLAY
DESCRIPTION:Breakfast meeting with executive\n
 team at 8:30 AM EST.
END:VALARM
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Summary (empty, "Bastille Day Party") ] ;
      alarms = [
        `Display { Icalendar.trigger = (empty, `Duration (Ptime.Span.of_int_s (-1800)));
                   duration_repeat = Some ((empty, Ptime.Span.of_int_s(15 * 60)), (empty, 2)) ;
                   other = [] ;
                   special = { Icalendar.description = (empty, "Breakfast meeting with executive\nteam at 8:30 AM EST."); }
                 }
      ]
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)


let calendar_object_with_audio_alarm_precise () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
BEGIN:VALARM
TRIGGER;VALUE=DATE-TIME:19970317T133000Z
REPEAT:4
DURATION:PT15M
ACTION:AUDIO
ATTACH;FMTTYPE=audio/basic:ftp://example.com/pub/
 sounds/bell-01.aud
END:VALARM
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Summary (empty, "Bastille Day Party") ] ;
      alarms = [
        `Audio { Icalendar.trigger = (singleton Valuetype `Datetime, `Datetime (to_ptime (1997,03,17) (13,30,00))) ;
                 duration_repeat = Some ((empty, Ptime.Span.of_int_s(15 * 60)), (empty, 4)) ;
                 other = [] ;
                 special = { Icalendar.attach = Some (singleton Media_type ("audio", "basic"), `Uri(Uri.of_string "ftp://example.com/pub/sounds/bell-01.aud"));
                           }
               }
      ]
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let calendar_object_with_email_alarm () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
BEGIN:VALARM
TRIGGER;RELATED=END:-P2D
ACTION:EMAIL
ATTENDEE:mailto:john_doe@example.com
SUMMARY:*** REMINDER: SEND AGENDA FOR WEEKLY STAFF MEETING ***
DESCRIPTION:A draft agenda needs to be sent out to the attendees
  to the weekly managers meeting (MGR-LIST). Attached is a
  pointer the document template for the agenda file.
ATTACH;FMTTYPE=application/msword:http://example.com/
 templates/agenda.doc
END:VALARM
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "19970610T172345Z-AF23B2@example.com") ;
      dtstamp = (empty, to_ptime (1997, 06, 10) (17, 23, 45)) ;
      dtstart = (empty, `Datetime (`Utc (to_ptime (1997, 07, 14) (17, 00, 00)))) ;
      dtend_or_duration = Some (`Dtend (empty, `Datetime (`Utc (to_ptime (1997, 07, 15) (04, 00, 00))))) ;
      rrule = None ;
      props = [ `Summary (empty, "Bastille Day Party") ] ;
      alarms = [
        `Email { Icalendar.trigger = (singleton Related `End, `Duration (Ptime.Span.of_int_s (-2*24*60*60))) ;
                 duration_repeat = None ;
                 other = [] ;
                 special = { Icalendar.attach = Some (singleton Media_type ("application", "msword"), `Uri(Uri.of_string "http://example.com/templates/agenda.doc")) ; attendees = [(empty, Uri.of_string "mailto:john_doe@example.com")]; 
                             summary = (empty, "*** REMINDER: SEND AGENDA FOR WEEKLY STAFF MEETING ***");
                             description = (empty, "A draft agenda needs to be sent out to the attendees to the weekly managers meeting (MGR-LIST). Attached is a pointer the document template for the agenda file.")
                           }
               }
      ]
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)



let calendar_object_with_duplicate_alarm () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//PYVOBJECT//NONSGML Version 1//EN
BEGIN:VEVENT
UID:put-8@example.com
DURATION:P1DT
DTSTART;VALUE=DATE:20180427
DTSTAMP:20051222T205953Z
SUMMARY:event 8
BEGIN:VALARM
ACTION:DISPLAY
DESCRIPTION:Test
TRIGGER;RELATED=START:-PT10M
END:VALARM
BEGIN:VALARM
ACTION:DISPLAY
DESCRIPTION:Test
TRIGGER;RELATED=START:-PT5M
END:VALARM
BEGIN:VALARM
ACTION:DISPLAY
DESCRIPTION:Test
TRIGGER;RELATED=START:-PT10M
END:VALARM
END:VEVENT
END:VCALENDAR
|_}
  and expected =
    let event = {
      uid = (empty, "put-8@example.com") ;
      dtstamp = (empty, to_ptime (2005, 12, 22) (20, 59,53)) ;
      dtstart = (singleton Valuetype `Date, `Date (2018, 04, 27)) ;
      dtend_or_duration = Some (`Duration (empty, Ptime.Span.of_int_s(1*24*60*60))) ;
      rrule = None ;
      props = [ `Summary (empty, "event 8") ] ;
      alarms = [
        `Display { Icalendar.trigger = (singleton Related `Start, `Duration (Ptime.Span.of_int_s(- 5 * 60))) ; duration_repeat = None ; other = [] ; special = { Icalendar.description = (empty, "Test") } } ;
        `Display { Icalendar.trigger = (singleton Related `Start, `Duration (Ptime.Span.of_int_s(- 10 * 60))) ; duration_repeat = None ; other = [] ; special = { Icalendar.description = (empty, "Test") } } ;
      ]
    } in
    Ok
      ( [ `Version (empty, "2.0") ;
          `Prodid (empty, "-//PYVOBJECT//NONSGML Version 1//EN") ],
        [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

(*
It is not very clear whether an URI may contain a \ character, but
ocaml-uri throws away content in hostname after a \ (in the Uri_re.authority
regular expression) -- once Uri.of_string may return either success or failure,
this should explicitly return a failure!

let apple_put_relaxed_url () =
  let input = {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//PYVOBJECT//NONSGML Version 1//EN
BEGIN:VEVENT
UID:put-i2@example.com
DTSTART;VALUE=DATE:20180427
DURATION:P1DT
DTSTAMP:20051222T205953Z
SUMMARY:event 1
URL:http://www.example.com$abc\,def
END:VEVENT
END:VCALENDAR
|} and expected =
     Ok ([ `Version (empty, "2.0") ; `Prodid (empty, "-//PYVOBJECT//NONSGML Version 1//EN") ],
         [ `Event ([
               uid = (empty, "put-i2@example.com") ;
               `Dtstart ([`Valuetype `Date], `Date (2018, 04, 27)) ;
               `Duration (empty, 1 * 24 * 60 * 60) ;
               dtstamp = (empty, (`Utc (to_ptime (2005, 12, 22) (20, 59, 53)))) ;
               `Summary (empty, "event 1") ;
               `Url (empty, Uri.of_string "http://www.example.com$abc\\,def")
             ], [])
         ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)
*)

let apple_test_with_x_not_text () =
  let input = {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//PYVOBJECT//NONSGML Version 1//EN
BEGIN:VEVENT
UID:put-3X-@example.com
DTSTART;VALUE=DATE:20180427
DURATION:P1DT
DTSTAMP:20051222T205953Z
SUMMARY:event 1
X-APPLE-STRUCTURED-LOCATION;VALUE=URI:geo:123.123,123.123
X-Test:Just some text\, <- here.
X-Test:geo:123.123,123.123
END:VEVENT
END:VCALENDAR
|} and expected =
     let event = {
       uid = (empty, "put-3X-@example.com") ;
       dtstamp = (empty, to_ptime (2005, 12, 22) (20, 59, 53)) ;
       dtstart = (singleton Valuetype `Date, `Date (2018, 04, 27)) ;
       dtend_or_duration = Some (`Duration (empty, Ptime.Span.of_int_s(1 * 24 * 60 * 60))) ;
       rrule = None ;
       props = [
         `Summary (empty, "event 1") ;
         `Xprop (("", "APPLE-STRUCTURED-LOCATION"),
                 singleton Valuetype `Uri,
                 "geo:123.123,123.123") ;
         `Xprop (("", "Test"), empty, "Just some text\\, <- here.") ;
         `Xprop (("", "Test"), empty, "geo:123.123,123.123")
       ] ;
       alarms = []
     } in
     Ok ([ `Version (empty, "2.0") ; `Prodid (empty, "-//PYVOBJECT//NONSGML Version 1//EN") ],
         [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let object_tests = [
  "test single long line", `Quick, test_line ;
  "test multiline", `Quick, test_multiline ;

  "calendar object parsing", `Quick, calendar_object ;
  "calendar object parsing with tzid", `Quick, calendar_object_with_tzid ;
  "calendar object parsing with class", `Quick, calendar_object_with_class ;
  "calendar object parsing with created", `Quick, calendar_object_with_created ;
  "calendar object parsing with description", `Quick, calendar_object_with_description ;
  "calendar object parsing with geo", `Quick, calendar_object_with_geo ;
  "calendar object parsing with last modified", `Quick, calendar_object_with_last_mod ;
  "calendar object parsing with location", `Quick, calendar_object_with_location ;
  "calendar object parsing with organizer", `Quick, calendar_object_with_organizer ;
  "calendar object parsing with priority", `Quick, calendar_object_with_priority ;
  "calendar object parsing with seq", `Quick, calendar_object_with_seq ;
  "calendar object parsing with status", `Quick, calendar_object_with_status ;
  "calendar object parsing with summary", `Quick, calendar_object_with_summary ;
  "calendar object parsing with transp", `Quick, calendar_object_with_transp ;
  "calendar object parsing with url", `Quick, calendar_object_with_url ;
  "calendar object parsing with recur_id", `Quick, calendar_object_with_recur_id ;
  "calendar object parsing with rrule", `Quick, calendar_object_with_rrule ;
  "calendar object parsing with duration", `Quick, calendar_object_with_duration ;
  "calendar object parsing with attach", `Quick, calendar_object_with_attach ;
  "calendar object parsing with attendee", `Quick, calendar_object_with_attendee ;
  "calendar object parsing with categories", `Quick, calendar_object_with_categories ;
  "calendar object parsing with comment", `Quick, calendar_object_with_comment ;
  "calendar object parsing with contact", `Quick, calendar_object_with_contact ;
  "calendar object parsing with exdate", `Quick, calendar_object_with_exdate ;
  "calendar object parsing with rstatus", `Quick, calendar_object_with_rstatus ;
  "calendar object parsing with related", `Quick, calendar_object_with_related ;
  "calendar object parsing with related2", `Quick, calendar_object_with_related2 ;
  "calendar object parsing with resource", `Quick, calendar_object_with_resource ;
  "calendar object parsing with resource2", `Quick, calendar_object_with_resource2 ;
  "calendar object parsing with rdate", `Quick, calendar_object_with_rdate ;
  "calendar object parsing with illegal valarm action", `Quick, calendar_object_with_illegal_valarm ;
  "calendar object parsing with valarm and action", `Quick, calendar_object_with_valarm_action ;
  "calendar object parsing with valarm and trigger", `Quick, calendar_object_with_valarm_trigger ;
  "calendar object parsing with valarm and duration", `Quick, calendar_object_with_valarm_duration ;
  "calendar object parsing with valarm and repeat", `Quick, calendar_object_with_valarm_repeat ;
  "calendar object parsing with valarm and attach", `Quick, calendar_object_with_valarm_attach ;
  "calendar object parsing with valarm and description", `Quick, calendar_object_with_valarm_description ;
  "calendar object parsing with valarm and summary", `Quick, calendar_object_with_valarm_summary ;
  "calendar object parsing with audio alarm precise", `Quick, calendar_object_with_audio_alarm_precise ;
  "calendar object parsing with display alarm relative", `Quick, calendar_object_with_display_alarm_relative ;
  "calendar object parsing with email alarm", `Quick, calendar_object_with_email_alarm ;
  "calendar object parsing with duplicate alarm", `Quick, calendar_object_with_duplicate_alarm ;
  (*  "calendar object with unencoded url", `Quick, apple_put_relaxed_url ; *)
  "calendar object with X- and not text value", `Quick, apple_test_with_x_not_text ;
]

let timezone_new_york_dtstart () =
  let input =
    {|BEGIN:VCALENDAR
BEGIN:VTIMEZONE
TZID:America/New_York
LAST-MODIFIED:20050809T050000Z
BEGIN:STANDARD
DTSTART:20071104T020000
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:20070311T020000
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE
END:VCALENDAR
|}
  and expected = Ok ([], [
      `Timezone [
        `Timezone_id (empty, (false, "America/New_York")) ;
        `Lastmod (empty, to_ptime (2005, 08, 09) (05, 00, 00)) ;
        `Standard [
          `Dtstart_local (empty, to_ptime (2007, 11, 04) (02, 00, 00)) ;
          `Tzoffset_from (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
          `Tzoffset_to (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
          `Tzname (empty, "EST")
        ] ;
        `Daylight [
          `Dtstart_local (empty, to_ptime (2007, 03, 11) (02, 00, 00)) ;
          `Tzoffset_from (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
          `Tzoffset_to (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
          `Tzname (empty, "EDT")
        ]
      ]
    ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let timezone_new_york_since_april_1967 () =
  let input = {|BEGIN:VCALENDAR
BEGIN:VTIMEZONE
TZID:America/New_York
LAST-MODIFIED:20050809T050000Z
BEGIN:DAYLIGHT
DTSTART:19670430T020000
RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19730429T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:19671029T020000
RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU;UNTIL=20061029T060000Z
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:19740106T020000
RDATE:19750223T020000
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:19760425T020000
RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19860427T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:19870405T020000
RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=1SU;UNTIL=20060402T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:20070311T020000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20071104T020000
RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
END:VTIMEZONE
END:VCALENDAR
|}
  and expected =
    Ok ([], [
        `Timezone [
          `Timezone_id (empty, (false, "America/New_York")) ;
          `Lastmod (empty, to_ptime (2005, 08, 09) (05, 00, 00)) ;
          `Daylight [
            `Dtstart_local (empty, to_ptime (1967, 04, 30) (02, 00, 00)) ;
            `Rrule (empty, (`Yearly, Some ( `Until (`Utc (to_ptime (1973, 04, 29) (07, 00, 00))) ), None,
[
                          `Bymonth [4] ;
                          `Byday [(-1, `Sunday)] ;
                          ])) ;
            `Tzoffset_from (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
            `Tzoffset_to (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
            `Tzname (empty, "EDT") ] ;
          `Standard [
            `Dtstart_local (empty, to_ptime (1967, 10, 29) (02, 00, 00)) ;
            `Rrule (empty, (`Yearly, Some ( `Until (`Utc (to_ptime (2006, 10, 29) (06, 00, 00))) ), None,
[
                          `Bymonth [10] ;
                          `Byday [(-1, `Sunday)] ;
                          ])) ;
            `Tzoffset_from (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
            `Tzoffset_to (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
            `Tzname (empty, "EST") ] ;
          `Daylight [
            `Dtstart_local (empty, to_ptime (1974, 01, 06) (02, 00, 00)) ;
            `Rdate (empty, `Datetimes [ (`Local (to_ptime (1975, 02, 23) (02, 00, 00))) ]) ;
            `Tzoffset_from (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
            `Tzoffset_to (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
            `Tzname (empty, "EDT") ] ;
          `Daylight [
            `Dtstart_local (empty, to_ptime (1976, 04, 25) (02, 00, 00)) ;
            `Rrule (empty, (`Yearly, Some ( `Until (`Utc (to_ptime (1986, 04, 27) (07, 00, 00))) ), None,
[
                          `Bymonth [4] ;
                          `Byday [(-1, `Sunday)] ;
                          ]) ) ;
            `Tzoffset_from (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
            `Tzoffset_to (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
            `Tzname (empty, "EDT") ] ;
          `Daylight [
            `Dtstart_local (empty, to_ptime (1987, 04, 05) (02, 00, 00)) ;
            `Rrule (empty, (`Yearly, Some ( `Until (`Utc (to_ptime (2006, 04, 02) (07, 00, 00))) ), None,
[
                          `Bymonth [4] ;
                          `Byday [(1, `Sunday)] ;
                          ]) ) ;
            `Tzoffset_from (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
            `Tzoffset_to (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
            `Tzname (empty, "EDT") ] ;
          `Daylight [
            `Dtstart_local (empty, to_ptime (2007, 03, 11) (02, 00, 00)) ;
            `Rrule (empty, (`Yearly, None, None,
[
                          `Bymonth [3] ;
                          `Byday [(2, `Sunday)] ]) ) ;
            `Tzoffset_from (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
            `Tzoffset_to (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
            `Tzname (empty, "EDT") ] ;
          `Standard [
            `Dtstart_local (empty, to_ptime (2007, 11, 04) (02, 00, 00)) ;
            `Rrule (empty, (`Yearly, None, None,
[
                          `Bymonth [11] ;
                          `Byday [(1, `Sunday)] ]) ) ;
            `Tzoffset_from (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
            `Tzoffset_to (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
            `Tzname (empty, "EST") ]
        ] ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let timezone_new_york_since_2007 () =
  let input = {|BEGIN:VCALENDAR
BEGIN:VTIMEZONE
TZID:America/New_York
LAST-MODIFIED:20050809T050000Z
TZURL:http://zones.example.com/tz/America-New_York.ics
BEGIN:STANDARD
DTSTART:20071104T020000
RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:20070311T020000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE
END:VCALENDAR
|} and expected =
     Ok ([], [
         `Timezone [
           `Timezone_id (empty, (false, "America/New_York")) ;
           `Lastmod (empty, to_ptime (2005, 08, 09) (05, 00, 00)) ;
           `Tzurl (empty, Uri.of_string "http://zones.example.com/tz/America-New_York.ics") ;
           `Standard [
             `Dtstart_local (empty, to_ptime (2007, 11, 04) (02, 00, 00)) ;
             `Rrule (empty, (`Yearly, None, None, [
                           `Bymonth [11] ;
                           `Byday [(1, `Sunday)] ])) ;
             `Tzoffset_from (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
             `Tzoffset_to (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
             `Tzname (empty, "EST") ] ;
           `Daylight [
             `Dtstart_local (empty, to_ptime (2007, 03, 11) (02, 00, 00)) ;
             `Rrule (empty,
                 (`Yearly, None, None, [
                 `Bymonth [3] ;
                 `Byday [(2, `Sunday)] ])) ;
             `Tzoffset_from (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
             `Tzoffset_to (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
             `Tzname (empty, "EDT") ] ;
         ] ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let timezone_fictitious_end_date () =
  let input = {|BEGIN:VCALENDAR
BEGIN:VTIMEZONE
TZID:Fictitious
LAST-MODIFIED:19870101T000000Z
BEGIN:STANDARD
DTSTART:19671029T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:19870405T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4;UNTIL=19980404T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE
END:VCALENDAR
|} and expected =
     Ok ([],
         [ `Timezone [
               `Timezone_id (empty, (false, "Fictitious")) ;
               `Lastmod (empty, to_ptime (1987, 01, 01) (00, 00, 00)) ;
               `Standard [
                 `Dtstart_local (empty, to_ptime (1967, 10, 29) (02, 00, 00)) ;
                 `Rrule (empty, (`Yearly, None, None, [ `Byday [(-1, `Sunday)] ; `Bymonth [10]])) ;
                 `Tzoffset_from (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
                 `Tzoffset_to (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
                 `Tzname (empty, "EST") ] ;
               `Daylight [
                 `Dtstart_local (empty, to_ptime (1987, 04, 05) (02, 00, 00)) ;
                 `Rrule (empty, (`Yearly, Some (`Until (`Utc (to_ptime (1998, 04, 04) (07, 00, 00)))), None, [ `Byday [(1, `Sunday)] ; `Bymonth [4]  ])) ;
                 `Tzoffset_from (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
                 `Tzoffset_to (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
                 `Tzname (empty, "EDT") ] ;
             ] ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let timezone_fictitious_two_daylight () =
  let input = {|BEGIN:VCALENDAR
BEGIN:VTIMEZONE
TZID:Fictitious
LAST-MODIFIED:19870101T000000Z
BEGIN:STANDARD
DTSTART:19671029T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:19870405T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4;UNTIL=19980404T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:19990424T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=4
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE
END:VCALENDAR
|} and expected =
     Ok ([], [
         `Timezone [
           `Timezone_id (empty, (false, "Fictitious")) ;
           `Lastmod (empty, to_ptime (1987, 01, 01) (00, 00, 00)) ;
           `Standard [
             `Dtstart_local (empty, to_ptime (1967, 10, 29) (02, 00, 00)) ;
             `Rrule (empty, (`Yearly, None, None, [ `Byday [(-1, `Sunday)] ; `Bymonth [10] ])) ;
             `Tzoffset_from (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
             `Tzoffset_to (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
             `Tzname (empty, "EST") ] ;
           `Daylight [
             `Dtstart_local (empty, to_ptime (1987, 04, 05) (02, 00, 00)) ;
             `Rrule (empty, (`Yearly, Some (`Until (`Utc (to_ptime (1998, 04, 04) (07, 00, 00))) ), None, [ `Byday [(1, `Sunday)] ; `Bymonth [4] ])) ;
             `Tzoffset_from (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
             `Tzoffset_to (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
             `Tzname (empty, "EDT") ] ;
           `Daylight [
             `Dtstart_local (empty, to_ptime (1999, 04, 24) (02, 00, 00)) ;
             `Rrule (empty, (`Yearly, None, None, [ `Byday [(-1, `Sunday)] ; `Bymonth [4] ])) ;
             `Tzoffset_from (empty, Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
             `Tzoffset_to (empty, Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
             `Tzname (empty, "EDT") ] ;
         ] ])
  in
  Alcotest.check result_c __LOC__ expected (parse input)

let timezone_tests = [
  "New York timezone with dtstart only", `Quick, timezone_new_york_dtstart ;
  "New York timezone since 30 April 1967", `Quick, timezone_new_york_since_april_1967 ;
  "New York timezone since 2007", `Quick, timezone_new_york_since_2007 ;
  "Fictitious timezone with until", `Quick, timezone_fictitious_end_date ;
  "Fictitious timezone with two daylight", `Quick, timezone_fictitious_two_daylight ;
]

let encode_durations () =
  let values = [
    0 ;
    7 * 24 * 60 * 60 ;
    7 * 24 * 60 * 60 + 1 ;
    8 * 24 * 60 * 60 ;
    24 * 60 * 60 ;
    25 * 60 * 60 + 30 * 60 + 25 ;
    25 * 60 * 60 + 25 ;
    5
  ]
  and expecteds = [
    "PT0S" ;
    "P1W" ;
    "P7DT1S" ;
    "P8D" ;
    "P1D" ;
    "P1DT1H30M25S" ;
    "P1DT1H0M25S" ;
    "PT5S"
  ]
  in
  List.iter2 (fun v e ->
      let to_string v =
        let buf = Buffer.create 10 in
        Icalendar.Writer.duration_to_ics v buf ;
        Buffer.contents buf
      in
      Alcotest.(check string __LOC__ e (to_string v)))
    (List.map Ptime.Span.of_int_s values) expecteds

let decode_encode () =
  let input = String.concat "\r\n" [
    "BEGIN:VCALENDAR" ;
    "VERSION:2.0" ;
    "PRODID:-//hacksw/handcal//NONSGML v1.0//EN" ;
    "BEGIN:VEVENT" ;
    "UID:19970610T172345Z-AF23B2@example.com" ;
    "DTSTAMP:19970610T172345Z" ;
    "DTSTART:19970714T170000Z" ;
    "DTEND:19970715T040000Z" ;
    "SUMMARY:Bastille Day Party" ;
    "BEGIN:VALARM" ;
    "ACTION:EMAIL" ;
    "TRIGGER;RELATED=END:-P2D" ;
    "ATTACH;FMTTYPE=application/msword:http://example.com/templates/agenda.doc" ;
    "DESCRIPTION:A draft agenda needs to be sent out to the attendees to the weekly managers meeting (MGR-LIST). Attached is a pointer the document template for the agenda file." ;
    "SUMMARY:*** REMINDER: SEND AGENDA FOR WEEKLY STAFF MEETING ***" ;
    "ATTENDEE:mailto:john_doe@example.com" ;
    "END:VALARM" ;
    "END:VEVENT" ;
    "END:VCALENDAR\r\n" ;
  ] in
  let c = match Icalendar.parse input with
    | Ok c -> c
    | Error e -> Alcotest.fail e
  in
  Alcotest.(check string __LOC__ input (Icalendar.to_ics c))

let x_apple_put () =
  let input = {|BEGIN:VCALENDAR
CALSCALE:GREGORIAN
PRODID:-//Example Inc.//Example Calendar//EN
VERSION:2.0
X-WR-CALNAME:CalDAV tests
BEGIN:VEVENT
DTSTAMP:20060202T205536Z
DTSTART:20180101T120000
DURATION:PT1H
SUMMARY:event 1
UID:event1@example.local
END:VEVENT
END:VCALENDAR
|}
  and expected =
    let event = {
      dtstamp = (empty, to_ptime (2006, 02, 02) (20, 55, 36)) ;
      uid = (empty, "event1@example.local") ;
      dtstart = (empty, `Datetime (`Local (to_ptime (2018, 01, 01) (12, 0, 0 )))) ;
      dtend_or_duration = Some (`Duration (empty, Ptime.Span.of_int_s(60 * 60) )) ;
      rrule = None ;
      props = [ `Summary (empty, "event 1") ] ;
      alarms = []
    } in
    Ok
      ([ `Calscale (empty, "GREGORIAN") ; `Prodid (empty, "-//Example Inc.//Example Calendar//EN") ; `Version (empty, "2.0") ; `Xprop (("WR", "CALNAME"), empty, "CalDAV tests") ],
       [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (Icalendar.parse input)

let apple_reminder_todos () =
  let input = {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Apple Inc.//Mac OS X 10.13.5//EN
CALSCALE:GREGORIAN
BEGIN:VTIMEZONE
TZID:Europe/Paris
BEGIN:DAYLIGHT
TZOFFSETFROM:+0100
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU
DTSTART:19810329T020000
TZNAME:CEST
TZOFFSETTO:+0200
END:DAYLIGHT
BEGIN:STANDARD
TZOFFSETFROM:+0200
RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU
DTSTART:19961027T030000
TZNAME:CET
TZOFFSETTO:+0100
END:STANDARD
END:VTIMEZONE
BEGIN:VTODO
CREATED:20180904T112426Z
UID:F41EFFCF-2483-43CE-A10E-91C484419193
SUMMARY:tomorrow buy milk
DTSTART;TZID=Europe/Paris:20180905T120000
DTSTAMP:20180904T112432Z
SEQUENCE:0
DUE;TZID=Europe/Paris:20180905T120000
BEGIN:VALARM
X-WR-ALARMUID:C5EF8EEC-546B-462A-BE1E-5F4CB06661AB
UID:C5EF8EEC-546B-462A-BE1E-5F4CB06661AB
TRIGGER;VALUE=DATE-TIME:20180905T100000Z
DESCRIPTION:Event reminder
ACTION:DISPLAY
END:VALARM
END:VTODO
END:VCALENDAR
|}
  and expected = Ok
  ([`Version ((empty, "2.0"));
     `Prodid ((empty, "-//Apple Inc.//Mac OS X 10.13.5//EN"));
     `Calscale ((empty, "GREGORIAN"))],
   [`Timezone ([`Timezone_id ((empty, (false, "Europe/Paris")));
                 `Daylight ([`Tzoffset_from ((empty, Ptime.Span.of_int_s (60*60)));
                              `Rrule ((empty,
                                       (`Yearly, None, None,
                                        [`Bymonth ([3]);
                                          `Byday ([(-1, `Sunday)])])));
                              `Dtstart_local ((empty,
                                         to_ptime (1981,03,29) (02,00,00) ));
                              `Tzname ((empty, "CEST")); `Tzoffset_to ((empty, Ptime.Span.of_int_s (60*60*2)))]);
                 `Standard ([`Tzoffset_from ((empty, Ptime.Span.of_int_s (60*60*2)));
                              `Rrule ((empty,
                                       (`Yearly, None, None,
                                        [`Bymonth ([10]);
                                          `Byday ([(-1, `Sunday)])])));
                              `Dtstart_local ((empty,
                                         to_ptime (1996,10,27) (03,00,00))) ;
                              `Tzname ((empty, "CET")); `Tzoffset_to ((empty, Ptime.Span.of_int_s (60*60)))])
                 ]);
     `Todo (([`Created ((empty, to_ptime (2018,09,04) (11,24,26)));
               `Uid ((empty, "F41EFFCF-2483-43CE-A10E-91C484419193"));
               `Summary ((empty, "tomorrow buy milk"));
               `Dtstart ((empty,
                          `Datetime ((`With_tzid (to_ptime (2018,09,05) (12,00,00), (false, "Europe/Paris"))))));
               `Dtstamp ((empty, to_ptime (2018,09,04) (11,24,32)));
               `Seq ((empty, 0));
               `Due ((empty,
                      `Datetime ((`With_tzid (to_ptime (2018,09,05) (12,00,00), (false, "Europe/Paris"))))))
               ],
             [`Display ({ Icalendar.trigger =
                          (singleton Valuetype `Datetime,
                           `Datetime (to_ptime (2018,09,05) (10,00,00)));
                          duration_repeat = None;
                          other =
                          [`Iana_prop (("UID", empty,
                                        "C5EF8EEC-546B-462A-BE1E-5F4CB06661AB"));
                            `Xprop ((("WR", "ALARMUID"), empty,
                                     "C5EF8EEC-546B-462A-BE1E-5F4CB06661AB"))
                            ];
                          special =
                          { Icalendar.description = (empty, "Event reminder") } })
               ]))
     ]) in
  Alcotest.check result_c __LOC__ expected (Icalendar.parse input)

let apple_event () =
  let input = {|BEGIN:VCALENDAR
CALSCALE:GREGORIAN
PRODID:-//Apple Inc.//iOS 11.4.1//EN
VERSION:2.0
BEGIN:VTIMEZONE
TZID:Europe/Berlin
BEGIN:DAYLIGHT
DTSTART:19810329T020000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU
TZNAME:CEST
TZOFFSETFROM:+0100
TZOFFSETTO:+0200
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:19961027T030000
RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU
TZNAME:CET
TZOFFSETFROM:+0200
TZOFFSETTO:+0100
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
CREATED:20180930T150537Z
DTEND;TZID=Europe/Berlin:20180930T180000
DTSTAMP:20180930T150538Z
DTSTART;TZID=Europe/Berlin:20180930T170000
LAST-MODIFIED:20180930T150537Z
SEQUENCE:0
SUMMARY:hhhhhhhh
TRANSP:OPAQUE
UID:C0A734CB-5A7B-40E1-A49D-018083F5B469
URL;VALUE=URI:
BEGIN:VALARM
ACTION:NONE
TRIGGER;VALUE=DATE-TIME:19760401T005545Z
END:VALARM
END:VEVENT
END:VCALENDAR
|}
  and expected =
    let timezone = [
      `Timezone_id (empty, (false, "Europe/Berlin"));
      `Daylight ([
          `Dtstart_local (empty, to_ptime (1981, 03, 29) (02, 00, 00));
          `Rrule (empty, (`Yearly, None, None, [`Bymonth ([3]); `Byday ([(-1, `Sunday)])]));
          `Tzname (empty, "CEST");
          `Tzoffset_from (empty, Ptime.Span.of_int_s @@ 60*60);
          `Tzoffset_to (empty, Ptime.Span.of_int_s @@ 2*60*60)
        ]);
      `Standard ([
          `Dtstart_local (empty, to_ptime (1996, 10, 27) (03, 00, 00));
          `Rrule (empty, (`Yearly, None, None, [`Bymonth ([10]); `Byday ([(-1, `Sunday)])]));
          `Tzname (empty, "CET");
          `Tzoffset_from (empty, Ptime.Span.of_int_s @@ 2*60*60);
          `Tzoffset_to (empty, Ptime.Span.of_int_s @@ 60*60)])
    ]
    in
    let event = {
      dtstamp = (empty, to_ptime (2018, 09, 30) (15, 05, 38)) ;
      uid = (empty, "C0A734CB-5A7B-40E1-A49D-018083F5B469");
      dtstart =
        (empty, `Datetime (`With_tzid (to_ptime (2018, 09, 30) (17, 00, 00),
                                       (false, "Europe/Berlin"))));
      dtend_or_duration =
        (Some (`Dtend (empty, `Datetime (`With_tzid (to_ptime (2018, 09, 30) (18, 00, 00),
                                                    (false, "Europe/Berlin"))))));
      rrule = None;
      props = [
        `Created (empty, to_ptime (2018, 09, 30) (15, 05, 37));
        `Lastmod (empty, to_ptime (2018, 09, 30) (15, 05, 37));
        `Seq (empty, 0);
        `Summary (empty, "hhhhhhhh");
        `Transparency (empty, `Opaque);
        `Iana_prop ("URL", singleton Valuetype `Uri, "")
      ];
      alarms =
        [`None ({ trigger = (singleton Valuetype `Datetime,
                             `Datetime (to_ptime (1976, 04, 01) (00,55,45)));
                  duration_repeat = None; other = []; special = () })
        ]
    } in
    Ok
      ([ `Calscale (empty, "GREGORIAN") ; `Prodid (empty, "-//Apple Inc.//iOS 11.4.1//EN") ; `Version (empty, "2.0") ],
       [ `Timezone timezone ; `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (Icalendar.parse input)

let firefox_os_put () =
  let input = {|BEGIN:VCALENDAR
PRODID:-//Mozilla//FirefoxOS
VERSION:2.0
BEGIN:VEVENT
UID:36bfc385-c19e-4533-8629-329e0e7d1e55
SUMMARY:adgjmptw
DESCRIPTION:\n
LOCATION:
SEQUENCE:1
DTSTART;VALUE=DATE:20180924
DTEND;VALUE=DATE:20180925
BEGIN:VALARM
ACTION:DISPLAY
DESCRIPTION:This is an event reminder
TRIGGER;RELATIVE=START:-PT15H
END:VALARM
END:VEVENT
END:VCALENDAR|}
  in
  let expected =
    let event = {
      dtstamp = (empty, to_ptime (2018, 09, 24) (0, 0, 0)) ;
      uid = (empty, "36bfc385-c19e-4533-8629-329e0e7d1e55");
      dtstart = (singleton Valuetype `Date, `Date (2018, 09, 24));
      dtend_or_duration = Some (`Dtend (singleton Valuetype `Date, `Date (2018, 09, 25)));
      rrule = None ;
      props = [
        `Summary (empty, "adgjmptw") ;
        `Description (empty, "\n") ;
        `Iana_prop ("LOCATION", empty, "");
        `Seq (empty, 1)
      ] ;
      alarms = [
        `Display {
          trigger = (singleton (Iana_param "RELATIVE") [ `String "START" ],
                     `Duration (Ptime.Span.of_int_s (- 15 * 60* 60))) ;
          duration_repeat = None ;
          other = [] ;
          special = { description = (empty, "This is an event reminder") }
        }
      ]
    } in
    Ok
      ([ `Prodid (empty, "-//Mozilla//FirefoxOS") ; `Version (empty, "2.0") ],
       [ `Event event ])
  in
  Alcotest.check result_c __LOC__ expected (Icalendar.parse input)

let google_invitation () =
  let input = {|BEGIN:VCALENDAR
PRODID:-//Google Inc//Google Calendar 70.9054//EN
VERSION:2.0
CALSCALE:GREGORIAN
METHOD:REQUEST
BEGIN:VTIMEZONE
TZID:Europe/Madrid
X-LIC-LOCATION:Europe/Madrid
BEGIN:DAYLIGHT
TZOFFSETFROM:+0100
TZOFFSETTO:+0200
TZNAME:CEST
DTSTART:19700329T020000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU
END:DAYLIGHT
BEGIN:STANDARD
TZOFFSETFROM:+0200
TZOFFSETTO:+0100
TZNAME:CET
DTSTART:19701025T030000
RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
DTSTART;TZID=Europe/Madrid:20181203T173000
DTEND;TZID=Europe/Madrid:20181203T180000
RRULE:FREQ=WEEKLY;UNTIL=20190113T225959Z;BYDAY=MO
DTSTAMP:20190311T153122Z
ORGANIZER;CN=Shared Calendar:mailto:a3l8fdvmovb4lou7nvdntnj
 hoc@group.calendar.google.com
UID:2kur9onu5uusl8rtss0t9joqu3@google.com
ATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;RSVP=TRUE
 ;CN=a@a.com;X-NUM-GUESTS=0:mailto:a@a.com
ATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;RSVP=TRUE
 ;CN=b@b.com;X-NUM-GUESTS=0:mailto:b@b.com
CREATED:20190114T174224Z
DESCRIPTION:-::~:~::~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~
 :~:~:~:~:~:~:~:~::~:~::-\nPlease do not edit this section of the descriptio
 n.\n\nView your event at https://www.google.com/calendar/event?action=VIEW&
 eid=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&ctz=America%2FL
 os_Angeles&hl=en&es=0.\n-::~:~::~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~
 :~:~:~:~:~:~:~:~:~:~:~:~:~:~::~:~::-
LAST-MODIFIED:20190311T153116Z
LOCATION:https://a.com/somewhere_else
SEQUENCE:0
STATUS:CONFIRMED
SUMMARY:weekly meeting
TRANSP:OPAQUE
END:VEVENT
END:VCALENDAR
|}
  and expected =
    [`Prodid (empty, "-//Google Inc//Google Calendar 70.9054//EN");
     `Version (empty, "2.0");
     `Calscale (empty, "GREGORIAN");
     `Method (empty, "REQUEST")],
    [`Timezone ([`Timezone_id (empty, (false, "Europe/Madrid"));
                 `Xprop ((("LIC", "LOCATION"), empty, "Europe/Madrid"));
                 `Daylight ([`Tzoffset_from (empty, Ptime.Span.of_int_s (60 * 60));
                             `Tzoffset_to (empty, Ptime.Span.of_int_s (2 * 60 * 60));
                             `Tzname (empty, "CEST");
                             `Dtstart_local (empty, to_ptime (1970, 03, 29) (02, 00, 00));
                             `Rrule (empty,
                                       (`Yearly, None, None,
                                        [`Bymonth ([3]);
                                          `Byday ([(-1, `Sunday)])]))
                              ]);
                 `Standard ([`Tzoffset_from (empty, Ptime.Span.of_int_s (2 * 60 * 60));
                             `Tzoffset_to (empty, Ptime.Span.of_int_s (60 * 60));
                              `Tzname (empty, "CET");
                              `Dtstart_local (empty, to_ptime (1970, 10, 25) (03, 00, 00));
                              `Rrule (empty,
                                      (`Yearly, None, None,
                                       [`Bymonth ([10]);
                                        `Byday ([(-1, `Sunday)])]))
                            ])
                ]) ;
     `Event ({ Icalendar.dtstamp = (empty, to_ptime (2019, 03, 11) (15, 31, 22));
               uid = (empty, "2kur9onu5uusl8rtss0t9joqu3@google.com");
               dtstart =
               (empty,
                `Datetime (`With_tzid (to_ptime (2018, 12, 03) (17, 30, 00),
                                        (false, "Europe/Madrid"))));
               dtend_or_duration =
                 (Some (`Dtend (empty,
                               `Datetime (`With_tzid (to_ptime (2018, 12, 03) (18, 00, 00),
                                                      (false, "Europe/Madrid"))))));
               rrule =
                 (Some (empty,
                      (`Weekly,
                       (Some (`Until (`Utc (to_ptime (2019, 01, 13) (22, 59, 59))))),
                       None, [`Byday ([(0, `Monday)])])));
               props =
               [`Organizer (singleton Cn (`String "Shared Calendar"),
                            Uri.of_string "mailto:a3l8fdvmovb4lou7nvdntnjhoc@group.calendar.google.com");
                `Attendee (list_to_map [ B (Cn, `String "a@a.com") ;
                                         B (Cutype, `Individual) ;
                                         B (Partstat, `Accepted) ;
                                         B (Role, `Reqparticipant) ;
                                         B (Rsvp, true) ;
                                         B (Iana_param "X-NUM-GUESTS", [ `String "0" ]) ],
                           Uri.of_string "mailto:a@a.com");
                `Attendee (list_to_map [ B (Cn, `String "b@b.com") ;
                                         B (Cutype, `Individual) ;
                                         B (Partstat, `Accepted) ;
                                         B (Role, `Reqparticipant) ;
                                         B (Rsvp, true) ;
                                         B (Iana_param "X-NUM-GUESTS", [ `String "0" ]) ],
                           Uri.of_string "mailto:b@b.com");
                `Created (empty, to_ptime (2019, 01, 14) (17, 42, 24));
                `Description (empty,
                              "-::~:~::~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~::~:~::-\nPlease do not edit this section of the description.\n\nView your event at https://www.google.com/calendar/event?action=VIEW&eid=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&ctz=America%2FLos_Angeles&hl=en&es=0.\n-::~:~::~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~:~::~:~::-");
                `Lastmod (empty, to_ptime (2019, 03, 11) (15, 31, 16));
                `Location (empty, "https://a.com/somewhere_else");
                `Seq (empty, 0);
                `Status (empty, `Confirmed);
                `Summary (empty, "weekly meeting");
                `Transparency (empty, `Opaque)
                 ];
               alarms = [] })
    ]
  in
  Alcotest.check result_c __LOC__ (Ok expected) (Icalendar.parse input) ;
  let txt = Icalendar.to_ics expected in
  Alcotest.check result_c __LOC__ (Ok expected) (Icalendar.parse txt)

let iana_params () =
  let input = {|BEGIN:VCALENDAR
PRODID:-//Google Inc//Google Calendar 70.9054//EN
VERSION:2.0
BEGIN:VEVENT
DTSTART;TZID=Europe/Madrid:20181203T173000
DTEND;TZID=Europe/Madrid:20181203T180000
DTSTAMP:20190311T153122Z
UID:2kur9onu5uusl8rtss0t9joqu3@google.com
ATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;RSVP=TRUE
 ;CN=a@a.com;X-NUM-GUESTS=0;X-BLA=1;X-BLUBB=2:mailto:a@a.com
END:VEVENT
END:VCALENDAR
|}
  and expected =
    [`Prodid (empty, "-//Google Inc//Google Calendar 70.9054//EN");
     `Version (empty, "2.0")],
    [ `Event ({ Icalendar.dtstamp = (empty, to_ptime (2019, 03, 11) (15, 31, 22));
                uid = (empty, "2kur9onu5uusl8rtss0t9joqu3@google.com");
                dtstart =
                  (empty,
                   `Datetime (`With_tzid (to_ptime (2018, 12, 03) (17, 30, 00),
                                          (false, "Europe/Madrid"))));
                dtend_or_duration =
                  (Some (`Dtend (empty,
                                 `Datetime (`With_tzid (to_ptime (2018, 12, 03) (18, 00, 00),
                                                        (false, "Europe/Madrid"))))));
                rrule = None ; alarms = [] ;
                props =
                  [ `Attendee (list_to_map [ B (Cn, `String "a@a.com") ;
                                             B (Cutype, `Individual) ;
                                             B (Partstat, `Accepted) ;
                                             B (Role, `Reqparticipant) ;
                                             B (Rsvp, true) ;
                                             B (Iana_param "X-NUM-GUESTS", [ `String "0" ]) ;
                                             B (Iana_param "X-BLA", [ `String "1" ]) ;
                                             B (Iana_param "X-BLUBB", [ `String "2" ])],
                               Uri.of_string "mailto:a@a.com"); ] }) ]
  in
  Alcotest.check result_c __LOC__ (Ok expected) (Icalendar.parse input)

let decode_encode_tests = [
  "encode durations", `Quick, encode_durations ;
  "decode and encode is identity", `Quick, decode_encode ;
  "apple calendar tester case for put", `Quick, x_apple_put ;
  "apple reminders app todos", `Quick, apple_reminder_todos ;
  "apple calendar.app event", `Quick, apple_event ;
  "firefox OS put event", `Quick, firefox_os_put ;
  "google invitation", `Quick, google_invitation ;
  "iana parameters", `Quick, iana_params ;
]

let reply_busy_time () =
  let input = {|BEGIN:VCALENDAR
PRODID:-//Example Inc.//Example Calendar//EN
VERSION:2.0
BEGIN:VFREEBUSY
UID:19970901T095957Z-76A912@example.com
ORGANIZER:mailto:jane_doe@example.com
ATTENDEE:mailto:john_public@example.com
DTSTAMP:19970901T100000Z
FREEBUSY:19971015T050000Z/PT8H30M,
 19971015T160000Z/PT5H30M,19971015T223000Z/PT6H30M
URL:http://example.com/pub/busy/jpublic-01.ifb
COMMENT:This iCalendar file contains busy time information for
  the next three months.
END:VFREEBUSY
END:VCALENDAR
|}
  and expected =
  [`Prodid (empty, "-//Example Inc.//Example Calendar//EN");
     `Version (empty, "2.0")],
   [`Freebusy ([ `Uid ((empty, "19970901T095957Z-76A912@example.com"));
                 `Organizer ((empty, Uri.of_string "mailto:jane_doe@example.com"));
                 `Attendee ((empty, Uri.of_string "mailto:john_public@example.com"));
                 `Dtstamp ((empty, to_ptime (1997,09,01) (10,00,00)));
                 `Freebusy ((empty,
                             [ to_ptime (1997,10,15) (05,00,00), Ptime.Span.of_int_s (8 * 60 * 60 + 30 * 60), false ;
                               to_ptime (1997,10,15) (16,00,00), Ptime.Span.of_int_s (5 * 60 * 60 + 30 * 60), false;
                               to_ptime (1997,10,15) (22,30,00), Ptime.Span.of_int_s (6 * 60 * 60 + 30 * 60), false
                               ]));
                 `Url ((empty, Uri.of_string "http://example.com/pub/busy/jpublic-01.ifb"));
                 `Comment ((empty,
                            "This iCalendar file contains busy time information for the next three months."))
                 ])
     ]
  in
  Alcotest.check result_c __LOC__ (Ok expected) (parse input) 

let publish_busy_time () = 
  let input = {|BEGIN:VCALENDAR
PRODID:-//Example Inc.//Example Calendar//EN
VERSION:2.0
BEGIN:VFREEBUSY
UID:19970901T115957Z-76A912@example.com
DTSTAMP:19970901T120000Z
ORGANIZER:jsmith@example.com
DTSTART:19980313T141711Z
DTEND:19980410T141711Z
FREEBUSY:19980314T233000Z/19980315T003000Z
FREEBUSY:19980316T153000Z/19980316T163000Z
FREEBUSY:19980318T030000Z/19980318T040000Z
URL:http://www.example.com/calendar/busytime/jsmith.ifb
END:VFREEBUSY
END:VCALENDAR
|}
  and expected = [`Prodid ((empty, "-//Example Inc.//Example Calendar//EN"));
     `Version ((empty, "2.0"))],
   [`Freebusy ([`Uid ((empty, "19970901T115957Z-76A912@example.com"));
                 `Dtstamp ((empty, to_ptime (1997,09,01) (12,00,00)));
                 `Organizer ((empty, Uri.of_string "jsmith@example.com"));
                 `Dtstart_utc ((empty,
                            (to_ptime (1998,03,13) (14,17,11))));
                 `Dtend_utc ((empty, (to_ptime (1998,04,10) (14,17,11))));
                 `Freebusy ((empty,
                             [to_ptime (1998,03,14) (23,30,00), Ptime.Span.of_int_s (60 * 60), true ])) ;
                 `Freebusy ((empty,
                             [to_ptime (1998,03,16) (15,30,00), Ptime.Span.of_int_s (60 * 60), true]));
                 `Freebusy ((empty,
                             [to_ptime (1998,03,18) (03,00,00), Ptime.Span.of_int_s (60 * 60), true]));
                 `Url ((empty,
                        Uri.of_string "http://www.example.com/calendar/busytime/jsmith.ifb"))
                 ])
     ]
  in
  Alcotest.check result_c __LOC__ (Ok expected) (Icalendar.parse input) 

let freebusy_tests = [
  "reply busy time", `Quick, reply_busy_time ;
  "publish busy time", `Quick, publish_busy_time ;
]

let compare_ptime =
  let module M = struct
    type t = Ptime.t
    let pp = Ptime.pp
    let equal = Ptime.equal
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let timezone =
  [ `Lastmod (empty, to_ptime (2004, 01, 10) (03, 28, 45));
    `Timezone_id (empty, (false, "America/New_York"));
    `Daylight
      [`Dtstart_local (empty, to_ptime (2007, 03, 11) (02, 00, 00));
       `Rrule
         (empty,
          (`Yearly, None, None, [`Byday [(2, `Sunday)]; `Bymonth [3]]));
       `Tzname (empty, "EDT");
       `Tzoffset_from (empty, Ptime.Span.of_int_s (- 5*60*60));
       `Tzoffset_to (empty, Ptime.Span.of_int_s (- 4*60*60))];
    `Standard
      [`Dtstart_local (empty, to_ptime (2007, 11, 04) (02, 00, 00));
       `Rrule
         (empty,
          (`Yearly, None, None, [`Byday [(1, `Sunday)]; `Bymonth [11]]));
       `Tzname (empty, "EST");
       `Tzoffset_from (empty, Ptime.Span.of_int_s (- 4*60*60));
       `Tzoffset_to (empty, Ptime.Span.of_int_s (- 5*60*60))]]

let normal_tz_test () =
  let datetime = to_ptime (2018, 08, 12) (00, 30, 00)
  and tzid = (false, "America/New_York")
  in
  let expected = to_ptime (2018, 08, 12) (04, 30, 00) in
  Alcotest.(check (option compare_ptime) __LOC__ (Some expected)
              (Icalendar.normalize_timezone datetime tzid [ timezone ]))

let ts_exists_twice () =
  let datetime = to_ptime (2007, 11, 04) (01, 30, 00)
  and tzid = (false, "America/New_York")
  in
  let expected = to_ptime (2007, 11, 04) (05, 30, 00) in
  Alcotest.(check (option compare_ptime) __LOC__ (Some expected)
              (Icalendar.normalize_timezone datetime tzid [ timezone ]))

let ts_non_existing () =
  let datetime = to_ptime (2007, 03, 11) (02, 30, 00)
  and tzid = (false, "America/New_York")
  in
  let expected = to_ptime (2007, 03, 11) (07, 30, 00) in
  Alcotest.(check (option compare_ptime) __LOC__ (Some expected)
              (Icalendar.normalize_timezone datetime tzid [ timezone ]))

let tz_normalisation_tests = [
  "normal timezone normalisation", `Quick, normal_tz_test ;
  "timestamp exists twice (DST -> standard)", `Quick, ts_exists_twice ;
  "timestamp doesn't exist (standard -> DST)", `Quick, ts_non_existing ;
]

let tests = [
  "Object tests", object_tests ;
  "Timezone tests", timezone_tests ;
  "Decode-Encode tests", decode_encode_tests ;
  "Freebusy tests", freebusy_tests ;
  "Recurrence tests", Test_recur.tests ;
  "Timezone normalization tests", tz_normalisation_tests ;
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "" tests
