let compare_calendar =
  let module M = struct
    type t = Icalendar.calendar
    let pp = Icalendar.pp
    let equal a b = compare a b = 0
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let result_c = Alcotest.(result compare_calendar string)

let test_line () =
  let line =
    {_|BEGIN:VCALENDAR
BEGIN:VEVENT
DESCRIPTION:This is a long description that exists on a long line.
END:VEVENT
END:VCALENDAR
|_}
  in
  let expected =
    Ok ([], [
        `Event ([ `Description ([], "This is a long description that exists on a long line.") ], [])
      ])
  in
  let f = Icalendar.parse line in
  Alcotest.check result_c "test short line" expected f

let test_multiline () =
  let multiline = {_|BEGIN:VCALENDAR
BEGIN:VEVENT
DESCRIPTION:This is a lo
 ng description
  that exists on a long line.
END:VEVENT
END:VCALENDAR
|_} in
  let expected =
    Ok ([], [
        `Event ([ `Description ([], "This is a long description that exists on a long line.")], [])
      ])
  in
  let f = Icalendar.parse multiline in
  Alcotest.check result_c "test short line" expected f

let to_ptime date time =
  match Ptime.of_date_time (date, (time, 0)) with
  | None -> Alcotest.fail "invalid date time"
  | Some p -> p

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event ([ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
                    `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
                    `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
                    `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
                    `Summary ([], "Bastille Day Party")
                  ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

let calendar_object_with_tzid () =
  let input =
{_|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART;TZID=America/New_York:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([`Tzid (false, "America/New_York")], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Class ([], `Public) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Created ([], (to_ptime (1996, 03, 29) (13, 30, 00), true)) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Description ([`Altrep (Uri.of_string "CID:part3.msg970930T083000SILVER@example.com")], "Meeting to provide technical review for \"Phoenix\" design.\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\nRSVP to team leader.") ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Geo ([], (37.386013, -122.082932) ) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Lastmod ([], (to_ptime (1996, 08, 17) (13, 30, 00), true) ) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Location ([`Altrep (Uri.of_string "http://xyzcorp.com/conf-rooms/f123.vcf")], "Conference Room - F123, Bldg. 002") ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Organizer([`Sentby (Uri.of_string "mailto:sray@example.com")], Uri.of_string "mailto:jsmith@example.com") ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Priority ([], 2) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Seq ([], 1234) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Status ([], `Tentative) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Summary ([], "Department Party") ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Transparency ([], `Transparent) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Url ([], Uri.of_string "http://example.com/pub/busy/jpublic-01.ifb") ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Recur_id ([ `Range `Thisandfuture ], `Datetime (to_ptime (1996, 01, 20) (12,00,00), true)) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Rrule ([], [ `Frequency `Daily ; `Until (to_ptime (1997, 12, 24) (00, 00, 00), true)]) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
|_}
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Duration ([], 3600) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Attach ([`Media_type ("text", "plain") ; `Encoding `Base64 ; `Valuetype `Binary], `Binary "TG9yZW\
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
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected l = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ l ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
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
    `Attendee ([`Member [Uri.of_string "mailto:DEV-GROUP@example.com"]], Uri.of_string "mailto:joecool@example.com") ;
    `Attendee ([`Role `Reqparticipant  ], Uri.of_string "mailto:hcabot@example.com") ;
    `Attendee ([`Delegated_from [Uri.of_string "mailto:immud@example.com"]], Uri.of_string "mailto:ildoit@example.com") ;
    `Attendee ([`Role `Reqparticipant ; `Partstat `Tentative ; `Cn "Henry Cabot"], Uri.of_string "mailto:hcabot@example.com") ;
    `Attendee ([`Role `Reqparticipant ; `Delegated_from [Uri.of_string "mailto:bob@example.com"] ; `Partstat `Accepted ; `Cn "Jane Doe"], Uri.of_string "mailto:jdoe@example.com") ;
    (*`Attendee ([`Cn "John Smith" ; `Dir (Uri.of_string "ldap://example.com:6666/o=ABC%20Industries,c=US???(cn=Jim%20Dolittle)")], Uri.of_string "mailto:jimdo@example.com") ;*)
    `Attendee ([`Cn "John Smith" ; `Dir (Uri.of_string "ldap://example.com:6666")], Uri.of_string "mailto:jimdo@example.com") ;
    `Attendee ([`Role `Reqparticipant ; `Partstat `Tentative ; `Delegated_from [Uri.of_string "mailto:iamboss@example.com"] ;`Cn "Henry Cabot"], Uri.of_string "mailto:hcabot@example.com") ;
    `Attendee ([`Role `Nonparticipant ; `Partstat `Delegated ; `Delegated_to [Uri.of_string "mailto:hcabot@example.com"] ; `Cn "The Big Cheese"], Uri.of_string "mailto:iamboss@example.com") ;
    `Attendee ([`Role `Reqparticipant ; `Partstat `Accepted ; `Cn "Jane Doe"], Uri.of_string "mailto:jdoe@example.com") ;
  ] in
  List.iter2 (fun i e -> let f = Icalendar.parse i in
  Alcotest.check result_c __LOC__ e f) inputs expecteds

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Categories ([], ["APPOINTMENT" ; "EDUCATION"]) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Comment ([], "The meeting really needs to include both ourselves and the customer. We can't hold this meeting without them. As a matter of fact, the venue for the meeting ought to be at their site. - - John") ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Contact ([ `Altrep (Uri.of_string "CID:part3.msg970930T083000SILVER@example.com") ], "Jim Dolittle, ABC Industries, +1-919-555-1234") ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Exdate ([], `Datetimes [ (to_ptime (1996, 04, 02) (01, 00, 00), true) ;
                                         (to_ptime (1996, 04, 03) (01, 00, 00), true) ;
                                         (to_ptime (1996, 04, 04) (01, 00, 00), true) ]) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected s = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ s ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let inputs = List.map input [
      "2.0;Success" ;
      "3.1;Invalid property value;DTSTART:96-Apr-01" ;
      "2.8; Success\, repeating event ignored. Scheduled as a single event.;RRULE:FREQ=WEEKLY\;INTERVAL=2" ;
      "4.1;Event conflict.  Date-time is busy." ;
      "3.7;Invalid calendar user;ATTENDEE:mailto:jsmith@example.com" ;
    ]
  and expecteds = List.map expected [
      `Rstatus ([], ((2, 0, None), "Success", None)) ;
      `Rstatus ([], ((3, 1, None), "Invalid property value", Some "DTSTART:96-Apr-01")) ;
      `Rstatus ([], ((2, 8, None), " Success, repeating event ignored. Scheduled as a single event.", Some "RRULE:FREQ=WEEKLY;INTERVAL=2")) ;
      `Rstatus ([], ((4, 1, None), "Event conflict.  Date-time is busy.", None)) ;
      `Rstatus ([], ((3, 7, None), "Invalid calendar user", Some "ATTENDEE:mailto:jsmith@example.com")) ;
    ]
  in
  List.iter2 (fun i e ->
      let f = Icalendar.parse i in
      Alcotest.check result_c __LOC__ e f)
    inputs expecteds

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Related ([], "jsmith.part7.19960817T083000.xyzMail@example.com") ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Related ([], "19960401-080045-4000F192713-0052@example.com") ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Resource ([], [ "EASEL" ; "PROJECTOR" ; "VCR" ]) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Resource ([ `Language "fr" ], [ "Nettoyeur haute pression" ]) ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected s = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ s ;
               `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [])
        ])
  in
  let inputs = List.map input [
      ":19970714T123000Z" ;
      ";TZID=America/New_York:19970714T083000" ;
      ";VALUE=PERIOD:19960403T020000Z/19960403T040000Z,19960404T010000Z/PT3H" ;
      ";VALUE=DATE:19970101,19970120,19970217,19970421,19970526,19970704,19970901,19971014,19971128,19971129,19971225"
    ]
  and expecteds = List.map expected [
      `Rdate ([], `Datetimes [ to_ptime (1997, 07, 14) (12, 30, 00), true ]) ;
      `Rdate ([ `Tzid (false, "America/New_York") ], `Datetimes [ to_ptime (1997, 07, 14) (08, 30, 00), false ]) ;
      `Rdate ([ `Valuetype `Period ], `Periods [
          (to_ptime (1996, 04, 03) (02, 00, 00), to_ptime (1996, 04, 03) (04, 00, 00), true) ;
          (to_ptime (1996, 04, 04) (01, 00, 00), to_ptime (1996, 04, 04) (04, 00, 00), true)
        ]) ;
      `Rdate ([ `Valuetype `Date ], `Dates [ (1997, 01, 01) ; (1997, 01, 20) ; (1997, 02, 17) ;
                                             (1997, 04, 21) ; (1997, 05, 26) ; (1997, 07, 04) ;
                                             (1997, 09, 01) ; (1997, 10, 14) ; (1997, 11, 28) ;
                                             (1997, 11, 29) ; (1997, 12, 25) ])
    ]
  in
  List.iter2 (fun i e ->
      let f = Icalendar.parse i in
      Alcotest.check result_c __LOC__ e f)
    inputs expecteds

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
  and expected = Error "parse error"
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f


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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ],
             [ `Audio { Icalendar.trigger = ([`Valuetype `Datetime], `Datetime (to_ptime (1997, 03, 17) (13, 30, 00), true)) ; duration_repeat = None ; other = [] ; special = {Icalendar.attach = None } } ])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected s = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ],
             [ `Audio { Icalendar.trigger = s ; duration_repeat = None ; other = [] ; special = {Icalendar.attach = None } } ])
        ])
  in
  let inputs = List.map input [
      ":-PT15M" ;
      ";RELATED=END:PT5M" ;
      ";VALUE=DATE-TIME:19980101T050000Z" ;
    ]
  and expecteds = List.map expected [
      ([], `Duration (- (15 * 60))) ;
      ([ `Related `End ], `Duration (5 * 60)) ;
      ([ `Valuetype `Datetime ], `Datetime (to_ptime (1998, 01, 01) (05, 00, 00), true))
    ]
  in
  List.iter2 (fun i e ->
      let f = Icalendar.parse i in
      Alcotest.check result_c __LOC__ e f)
    inputs expecteds

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [
               `Audio { Icalendar.trigger = ([], `Duration (-1800)) ; 
                        duration_repeat = Some (([], 3600), ([], 2)) ; 
                        other = [] ;
                        special = { Icalendar.attach = None }}
             ])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [
               `Audio { Icalendar.trigger = ([], `Duration (-1800)) ; 
                        duration_repeat = Some (([], 3600), ([], 4)) ;
                        other = [] ;
                        special = { Icalendar.attach = None } }
             ])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [
               `Audio { Icalendar.trigger = ([], `Duration (-1800)) ; 
                        duration_repeat = None;
                        other = [] ;
                        special = { Icalendar.attach = Some
               ([`Media_type ("text", "plain") ; `Encoding `Base64 ; `Valuetype `Binary], `Binary "TG9yZW\
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
             ])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [
               `Display { Icalendar.trigger = ([], `Duration (-1800)) ;
                          duration_repeat = None ;
                          other = [] ;
                          special = { Icalendar.description = ([`Altrep (Uri.of_string "CID:part3.msg970930T083000SILVER@example.com")], "Meeting to provide technical review for \"Phoenix\" design.\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\nRSVP to team leader.") } }
             ])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [
               `Email { Icalendar.trigger = ([], `Duration (-1800)) ;
                        duration_repeat = None ;
                        other = [] ;
                        special = { Icalendar.summary = ([], "*** REMINDER: SEND AGENDA FOR WEEKLY STAFF MEETING ***") ;
                                    description = ([], "A draft agenda needs to be sent out to the attendees to the weekly managers meeting (MGR-LIST). Attached is a pointer the document template for the agenda file.") ;
                                    attendees = [([], Uri.of_string "mailto:john_doe@example.com")] ;
                                    attach = None ;
                                  }
                      }
             ])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [
               `Display { Icalendar.trigger = ([], `Duration (-1800));
                          duration_repeat = Some (([], 15 * 60), ([], 2)) ;
                          other = [] ;
                          special = { Icalendar.description = ([], "Breakfast meeting with executive\nteam at 8:30 AM EST."); }
                        }
             ])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f


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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [
               `Audio { Icalendar.trigger = ([`Valuetype `Datetime], `Datetime (to_ptime (1997,03,17) (13,30,00), true)) ;
                        duration_repeat = Some (([], 15 * 60), ([], 4)) ;
                        other = [] ;
                        special = { Icalendar.attach = Some ([`Media_type ("audio", "basic")], `Uri(Uri.of_string "ftp://example.com/pub/sounds/bell-01.aud"));
                                  }
                      }
             ])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//hacksw/handcal//NONSGML v1.0//EN") ],
        [
          `Event
            ([ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
               `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
               `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
               `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
               `Summary ([], "Bastille Day Party")
             ], [
               `Email { Icalendar.trigger = ([`Related `End], `Duration (-2*24*60*60)) ;
                        duration_repeat = None ;
                        other = [] ;
                        special = { Icalendar.attach = Some ([`Media_type ("application", "msword")], `Uri(Uri.of_string "http://example.com/templates/agenda.doc")) ; attendees = [([], Uri.of_string "mailto:john_doe@example.com")]; 
                                    summary = ([], "*** REMINDER: SEND AGENDA FOR WEEKLY STAFF MEETING ***");
                                    description = ([], "A draft agenda needs to be sent out to the attendees to the weekly managers meeting (MGR-LIST). Attached is a pointer the document template for the agenda file.")
                                  }
                      }
             ])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f



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
  and expected = Ok
      ( [ `Version ([], "2.0") ;
          `Prodid ([], "-//PYVOBJECT//NONSGML Version 1//EN") ],
        [
          `Event
            ([ `Uid ([], "put-8@example.com") ;
               `Duration ([], 1*24*60*60) ;
               `Dtstart ([`Valuetype `Date], `Date (2018, 04, 27)) ;
               `Dtstamp ([], (to_ptime (2005, 12, 22) (20, 59,53), true) ) ;
               `Summary ([], "event 8")
             ], [
               `Display { Icalendar.trigger = ([`Related `Start], `Duration (- 5 * 60)) ; duration_repeat = None ; other = [] ; special = { Icalendar.description = ([], "Test") } } ;
               `Display { Icalendar.trigger = ([`Related `Start], `Duration (- 10 * 60)) ; duration_repeat = None ; other = [] ; special = { Icalendar.description = ([], "Test") } } ;
             ])
        ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
     Ok ([ `Version ([], "2.0") ; `Prodid ([], "-//PYVOBJECT//NONSGML Version 1//EN") ],
         [ `Event ([
               `Uid ([], "put-i2@example.com") ;
               `Dtstart ([`Valuetype `Date], `Date (2018, 04, 27)) ;
               `Duration ([], 1 * 24 * 60 * 60) ;
               `Dtstamp ([], (to_ptime (2005, 12, 22) (20, 59, 53), true)) ;
               `Summary ([], "event 1") ;
               `Url ([], Uri.of_string "http://www.example.com$abc\\,def")
             ], [])
         ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f
*)

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
        `Tzid ([], (false, "America/New_York")) ;
        `Lastmod ([], (to_ptime (2005, 08, 09) (05, 00, 00), true)) ;
        `Standard [
          `Dtstart ([], `Datetime (to_ptime (2007, 11, 04) (02, 00, 00), false)) ;
          `Tzoffset_from ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
          `Tzoffset_to ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
          `Tzname ([], "EST")
        ] ;
        `Daylight [
          `Dtstart ([], `Datetime (to_ptime (2007, 03, 11) (02, 00, 00), false)) ;
          `Tzoffset_from ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
          `Tzoffset_to ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
          `Tzname ([], "EDT")
        ]
      ]
    ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
          `Tzid ([], (false, "America/New_York")) ;
          `Lastmod ([], (to_ptime (2005, 08, 09) (05, 00, 00), true)) ;
          `Daylight [
            `Dtstart ([], `Datetime (to_ptime (1967, 04, 30) (02, 00, 00), false)) ;
            `Rrule ([], [ `Frequency `Yearly ;
                          `Bymonth [4] ;
                          `Byday [(-1, `Sunday)] ;
                          `Until (to_ptime (1973, 04, 29) (07, 00, 00), true) ]) ;
            `Tzoffset_from ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
            `Tzoffset_to ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
            `Tzname ([], "EDT") ] ;
          `Standard [
            `Dtstart ([], `Datetime (to_ptime (1967, 10, 29) (02, 00, 00), false)) ;
            `Rrule ([], [ `Frequency `Yearly ;
                          `Bymonth [10] ;
                          `Byday [(-1, `Sunday)] ;
                          `Until (to_ptime (2006, 10, 29) (06, 00, 00), true) ]) ;
            `Tzoffset_from ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
            `Tzoffset_to ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
            `Tzname ([], "EST") ] ;
          `Daylight [
            `Dtstart ([], `Datetime (to_ptime (1974, 01, 06) (02, 00, 00), false)) ;
            `Rdate ([], `Datetimes [ (to_ptime (1975, 02, 23) (02, 00, 00), false) ]) ;
            `Tzoffset_from ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
            `Tzoffset_to ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
            `Tzname ([], "EDT") ] ;
          `Daylight [
            `Dtstart ([], `Datetime (to_ptime (1976, 04, 25) (02, 00, 00), false)) ;
            `Rrule ([], [ `Frequency `Yearly ;
                          `Bymonth [4] ;
                          `Byday [(-1, `Sunday)] ;
                          `Until (to_ptime (1986, 04, 27) (07, 00, 00), true) ]) ;
            `Tzoffset_from ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
            `Tzoffset_to ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
            `Tzname ([], "EDT") ] ;
          `Daylight [
            `Dtstart ([], `Datetime (to_ptime (1987, 04, 05) (02, 00, 00), false)) ;
            `Rrule ([], [ `Frequency `Yearly ;
                          `Bymonth [4] ;
                          `Byday [(1, `Sunday)] ;
                          `Until (to_ptime (2006, 04, 02) (07, 00, 00), true) ]) ;
            `Tzoffset_from ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
            `Tzoffset_to ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
            `Tzname ([], "EDT") ] ;
          `Daylight [
            `Dtstart ([], `Datetime (to_ptime (2007, 03, 11) (02, 00, 00), false)) ;
            `Rrule ([], [ `Frequency `Yearly ;
                          `Bymonth [3] ;
                          `Byday [(2, `Sunday)] ]) ;
            `Tzoffset_from ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
            `Tzoffset_to ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
            `Tzname ([], "EDT") ] ;
          `Standard [
            `Dtstart ([], `Datetime (to_ptime (2007, 11, 04) (02, 00, 00), false)) ;
            `Rrule ([], [ `Frequency `Yearly ;
                          `Bymonth [11] ;
                          `Byday [(1, `Sunday)] ]) ;
            `Tzoffset_from ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
            `Tzoffset_to ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
            `Tzname ([], "EST") ]
        ] ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
           `Tzid ([], (false, "America/New_York")) ;
           `Lastmod ([], (to_ptime (2005, 08, 09) (05, 00, 00), true)) ;
           `Tzurl ([], Uri.of_string "http://zones.example.com/tz/America-New_York.ics") ;
           `Standard [
             `Dtstart ([], `Datetime (to_ptime (2007, 11, 04) (02, 00, 00), false)) ;
             `Rrule ([], [ `Frequency `Yearly ;
                           `Bymonth [11] ;
                           `Byday [(1, `Sunday)] ]) ;
             `Tzoffset_from ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
             `Tzoffset_to ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
             `Tzname ([], "EST") ] ;
           `Daylight [
             `Dtstart ([], `Datetime (to_ptime (2007, 03, 11) (02, 00, 00), false)) ;
             `Rrule ([], [
                 `Frequency `Yearly ;
                 `Bymonth [3] ;
                 `Byday [(2, `Sunday)] ]) ;
             `Tzoffset_from ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
             `Tzoffset_to ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
             `Tzname ([], "EDT") ] ;
         ] ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
               `Tzid ([], (false, "Fictitious")) ;
               `Lastmod ([], (to_ptime (1987, 01, 01) (00, 00, 00), true)) ;
               `Standard [
                 `Dtstart ([], `Datetime (to_ptime (1967, 10, 29) (02, 00, 00), false)) ;
                 `Rrule ([], [ `Frequency `Yearly ; `Byday [(-1, `Sunday)] ; `Bymonth [10]]) ;
                 `Tzoffset_from ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
                 `Tzoffset_to ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
                 `Tzname ([], "EST") ] ;
               `Daylight [
                 `Dtstart ([], `Datetime (to_ptime (1987, 04, 05) (02, 00, 00), false)) ;
                 `Rrule ([], [ `Frequency `Yearly ; `Byday [(1, `Sunday)] ; `Bymonth [4] ;
                               `Until (to_ptime (1998, 04, 04) (07, 00, 00), true) ]) ;
                 `Tzoffset_from ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
                 `Tzoffset_to ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
                 `Tzname ([], "EDT") ] ;
             ] ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
           `Tzid ([], (false, "Fictitious")) ;
           `Lastmod ([], (to_ptime (1987, 01, 01) (00, 00, 00), true)) ;
           `Standard [
             `Dtstart ([], `Datetime (to_ptime (1967, 10, 29) (02, 00, 00), false)) ;
             `Rrule ([], [ `Frequency `Yearly ; `Byday [(-1, `Sunday)] ; `Bymonth [10] ]) ;
             `Tzoffset_from ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
             `Tzoffset_to ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
             `Tzname ([], "EST") ] ;
           `Daylight [
             `Dtstart ([], `Datetime (to_ptime (1987, 04, 05) (02, 00, 00), false)) ;
             `Rrule ([], [ `Frequency `Yearly ; `Byday [(1, `Sunday)] ; `Bymonth [4]; `Until (to_ptime (1998, 04, 04) (07, 00, 00), true) ]) ;
             `Tzoffset_from ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
             `Tzoffset_to ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
             `Tzname ([], "EDT") ] ;
           `Daylight [
             `Dtstart ([], `Datetime (to_ptime (1999, 04, 24) (02, 00, 00), false)) ;
             `Rrule ([], [ `Frequency `Yearly ; `Byday [(-1, `Sunday)] ; `Bymonth [4] ]) ;
             `Tzoffset_from ([], Ptime.Span.of_int_s ((-5) * 60 * 60)) ;
             `Tzoffset_to ([], Ptime.Span.of_int_s ((-4) * 60 * 60)) ;
             `Tzname ([], "EDT") ] ;
         ] ])
  in
  let f = Icalendar.parse input in
  Alcotest.check result_c __LOC__ expected f

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
    values expecteds

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
  and expected = [ `Calscale ([], "GREGORIAN") ; `Prodid ([], "-//Example Inc.//Example Calendar//EN") ; `Version ([], "2.0") ; `Xprop (("", "WR-CALNAME"), [], "CalDAV tests") ], 
    [ `Event ([ `Dtstamp ([], (to_ptime (2006, 02, 02) (20, 55, 36), true)) ;
                `Dtstart ([], `Datetime (to_ptime (2018, 01, 01) (12, 0, 0 ), false)) ;
                `Duration ([], 60 * 60 ) ;
                `Summary ([], "event 1") ;
                `Uid ([], "event1@example.local") ;
              ], [])
    ]
  in
  Alcotest.check result_c __LOC__ (Ok expected) (Icalendar.parse input) 

let decode_encode_tests = [
  "encode durations", `Quick, encode_durations ;
  "decode and encode is identity", `Quick, decode_encode ;
  "apple calendar tester case for put", `Quick, x_apple_put ;
]

let tests = [
  "Object tests", object_tests ;
  "Timezone tests", timezone_tests ;
  "Decode Encode tests", decode_encode_tests ;
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "" tests
