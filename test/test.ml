let compare_calendar =
  let module M = struct
    type t = Icalendar.calendar
    let pp = Icalendar.pp_calendar
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
  let expected = Ok ([], [
      [ `Description ([], "This is a long description that exists on a long line.") ],
      []
    ]) in
  let f = Icalendar.parse_calobject line in
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
  let expected = Ok ([], [
      [ `Description ([], "This is a long description that exists on a long line.")],
      []
    ])
  in
  let f = Icalendar.parse_calobject multiline in
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
          [ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([`Tzid (false, "America/New_York")], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Class ([], `Public) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Created ([], (to_ptime (1996, 03, 29) (13, 30, 00), true)) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Description ([`Altrep (Uri.of_string "CID:part3.msg970930T083000SILVER@example.com")], "Meeting to provide technical review for \"Phoenix\" design.\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\nRSVP to team leader.") ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Geo ([], (37.386013, -122.082932) ) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Lastmod ([], (to_ptime (1996, 08, 17) (13, 30, 00), true) ) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Location ([`Altrep (Uri.of_string "http://xyzcorp.com/conf-rooms/f123.vcf")], "Conference Room - F123, Bldg. 002") ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Organizer([`Sentby (Uri.of_string "mailto:sray@example.com")], Uri.of_string "mailto:jsmith@example.com") ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Priority ([], 2) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Seq ([], 1234) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Status ([], `Tentative) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Summary ([], "Department Party") ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Transparency ([], `Transparent) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Url ([], Uri.of_string "http://example.com/pub/busy/jpublic-01.ifb") ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Recur_id ([ `Range `Thisandfuture ], `Datetime (to_ptime (1996, 01, 20) (12,00,00), true)) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Rrule ([], [ `Frequency `Daily ; `Until (to_ptime (1997, 12, 24) (00, 00, 00), true)]) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Duration ([], 3600) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Attach ([`Media_type ("text", "plain") ; `Encoding `Base64 ; `Valuetype `Binary], `Binary "TG9yZW\
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
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ l ; 
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
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
  List.iter2 (fun i e -> let f = Icalendar.parse_calobject i in
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
          [ `Categories ([], ["APPOINTMENT" ; "EDUCATION"]) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Comment ([], "The meeting really needs to include both ourselves and the customer. We can't hold this meeting without them. As a matter of fact, the venue for the meeting ought to be at their site. - - John") ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Contact ([ `Altrep (Uri.of_string "CID:part3.msg970930T083000SILVER@example.com") ], "Jim Dolittle, ABC Industries, +1-919-555-1234") ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Exdate ([], `Datetimes [ (to_ptime (1996, 04, 02) (01, 00, 00), true) ;
                                      (to_ptime (1996, 04, 03) (01, 00, 00), true) ;
                                      (to_ptime (1996, 04, 04) (01, 00, 00), true) ]) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ s ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
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
      let f = Icalendar.parse_calobject i in
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
          [ `Related ([], "jsmith.part7.19960817T083000.xyzMail@example.com") ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Related ([], "19960401-080045-4000F192713-0052@example.com") ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Resource ([], [ "EASEL" ; "PROJECTOR" ; "VCR" ]) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Resource ([ `Language "fr" ], [ "Nettoyeur haute pression" ]) ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ s ;
            `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], []
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
      let f = Icalendar.parse_calobject i in
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
  let f = Icalendar.parse_calobject input in
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
          [ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ],
          [ `Audio { Icalendar.trigger = ([`Valuetype `Datetime], `Datetime (to_ptime (1997, 03, 17) (13, 30, 00), true)) ; duration_repeat = None ; special = {Icalendar.attach = None } } ]
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ],
          [ `Audio { Icalendar.trigger = s ; duration_repeat = None ; special = {Icalendar.attach = None } } ]
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
      let f = Icalendar.parse_calobject i in
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
          [ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], [
            `Audio { Icalendar.trigger = ([], `Duration (-1800)) ; 
                     duration_repeat = Some (([], 3600), ([], 2)) ; 
                     special = { Icalendar.attach = None }}
          ]
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], [
             `Audio { Icalendar.trigger = ([], `Duration (-1800)) ; 
                      duration_repeat = Some (([], 3600), ([], 4)) ;
                      special = { Icalendar.attach = None } }
          ]
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], [
             `Audio { Icalendar.trigger = ([], `Duration (-1800)) ; 
                      duration_repeat = None;
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
          ]
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], [
            `Display { Icalendar.trigger = ([], `Duration (-1800)) ;
                       duration_repeat = None ;
                       special = { Icalendar.description = ([`Altrep (Uri.of_string "CID:part3.msg970930T083000SILVER@example.com")], "Meeting to provide technical review for \"Phoenix\" design.\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\nRSVP to team leader.") } }
          ]
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], [
            `Email { Icalendar.trigger = ([], `Duration (-1800)) ;
                     duration_repeat = None ;
                     special = { Icalendar.summary = ([], "*** REMINDER: SEND AGENDA FOR WEEKLY STAFF MEETING ***") ;
                                 description = ([], "A draft agenda needs to be sent out to the attendees to the weekly managers meeting (MGR-LIST). Attached is a pointer the document template for the agenda file.") ;
                                 attendees = [([], Uri.of_string "mailto:john_doe@example.com")] ;
                                 attach = None ;
                     }
             }
          ]
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], [
            `Display { Icalendar.trigger = ([], `Duration (-1800));
                     duration_repeat = Some (([], 15 * 60), ([], 2)) ;
                     special = { Icalendar.description = ([], "Breakfast meeting with executive\nteam at 8:30 AM EST.");
                     }
             }
          ]
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], [
            `Audio { Icalendar.trigger = ([`Valuetype `Datetime], `Datetime (to_ptime (1997,03,17) (13,30,00), true)) ;
                     duration_repeat = Some (([], 15 * 60), ([], 4)) ;
                     special = { Icalendar.attach = Some ([`Media_type ("audio", "basic")], `Uri(Uri.of_string "ftp://example.com/pub/sounds/bell-01.aud"));
                     }
             }
          ]
        ])
  in
  let f = Icalendar.parse_calobject input in
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
          [ `Uid ([], "19970610T172345Z-AF23B2@example.com") ;
            `Dtstamp ([], (to_ptime (1997, 06, 10) (17, 23, 45), true) ) ;
            `Dtstart ([], `Datetime (to_ptime (1997, 07, 14) (17, 00, 00), true)) ;
            `Dtend ([], `Datetime (to_ptime (1997, 07, 15) (04, 00, 00), true)) ;
            `Summary ([], "Bastille Day Party")
          ], [
            `Email { Icalendar.trigger = ([`Related `End], `Duration (-2*24*60*60)) ;
                     duration_repeat = None ;
                     special = { Icalendar.attach = Some ([`Media_type ("application", "msword")], `Uri(Uri.of_string "http://example.com/templates/agenda.doc")) ; attendees = [([], Uri.of_string "mailto:john_doe@example.com")]; 
                                 summary = ([], "*** REMINDER: SEND AGENDA FOR WEEKLY STAFF MEETING ***");
                                 description = ([], "A draft agenda needs to be sent out to the attendees to the weekly managers meeting (MGR-LIST). Attached is a pointer the document template for the agenda file.")
                     }
             }
          ]
        ])
  in
  let f = Icalendar.parse_calobject input in
  Alcotest.check result_c __LOC__ expected f


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
]

let tests = [
  "Object tests", object_tests ;
]

let () = 
  Printexc.record_backtrace true;
  Alcotest.run "" tests
