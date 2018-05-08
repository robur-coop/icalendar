let compare_param =
  let module M = struct
    type t = Icalendar.icalparameter
    let pp = Icalendar.pp_icalparameter
    let equal a b = compare a b = 0
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let compare_value =
  let module M = struct
    type t = Icalendar.value
    let pp = Icalendar.pp_value
    let equal a b = compare a b = 0
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let compare_t =
  let module M = struct
    type t = (string * Icalendar.icalparameter list * Icalendar.value)
    let pp f (a, b, c) = Fmt.pf f "(%s, %a, %a)" a (Fmt.list ~sep:(Fmt.unit ";") Icalendar.pp_icalparameter) b Icalendar.pp_value c
    let equal (a, b, c) (a', b', c') = String.compare a a' = 0
      && List.for_all2 (fun x x' -> compare x x' = 0) b b'
      && compare c c' = 0
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let result = Alcotest.(result (list compare_t) string)

let test_line () =
  let line = "DESCRIPTION:This is a long description that exists on a long line.\n" in
  let expected = Ok [("DESCRIPTION", [], `Text [ "This is a long description that exists on a long line." ])] in
  let f = Icalendar.parse line in
  Alcotest.check result "test short line" expected f

let test_multiline () =
  let multiline = {__|DESCRIPTION:This is a lo
 ng description
  that exists on a long line.
|__} in
  let expected = Ok [("DESCRIPTION", [], `Text [ "This is a long description that exists on a long line." ])] in
  let f = Icalendar.parse multiline in
  Alcotest.check result "test short line" expected f

let line_with_parameters () =
  let line = "ATTENDEE;RSVP=TRUE;ROLE=REQ-PARTICIPANT:mailto:jsmith@example.com\n"
  and expected = Ok [("ATTENDEE", [`Rsvp true ; `Role `Reqparticipant], `Text [ "mailto:jsmith@example.com" ])] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let line_with_parameter () =
  let line = "RDATE;VALUE=DATE:19970304,19970504,19970704,19970904\n"
  and expected = Ok [("RDATE", [`Valuetype `Date], `Date [(1997, 03, 04) ; (1997, 05, 04) ; (1997, 07, 04) ; (1997, 09, 04) ])] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let attach () =
  let line = "ATTACH:http://example.com/public/quarterly-report.doc\n"
  and expected = Ok [("ATTACH", [], `Text [ "http://example.com/public/quarterly-report.doc" ])] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let attach_inline_binary_error () =
  let line = "ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4\n"
  and expected = Error "parse error" in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let attach_inline_binary () =
  (* from RFC 5545 Section 3.1.3, but has wrong padding (here added a =)! *)
  let b64 = match Nocrypto.Base64.decode (Cstruct.of_string "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4=") with
    | None -> Alcotest.fail "invalid base64 encoding"
    | Some x -> `Binary x
  in
  let line = "ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4=\n"
  and expected = Ok [("ATTACH",[`Fmttype ("text", "plain"); `Encoding `Base64 ; `Valuetype `Binary], b64)] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let quoted_prop () =
  let line = "DESCRIPTION;ALTREP=\"cid:part1.0001@example.org\":The Fall'98 Wild Wizards Conference - - Las Vegas\\, NV\\, USA\n"
  and expected = Ok [("DESCRIPTION",[`Altrep (Uri.of_string "cid:part1.0001@example.org")], `Text [ "The Fall'98 Wild Wizards Conference - - Las Vegas, NV, USA" ])] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let altprep () =
  let line = {__|DESCRIPTION;ALTREP="CID:part3.msg.970415T083000@example.com":
 Project XYZ Review Meeting will include the following agenda
  items: (a) Market Overview\, (b) Finances\, (c) Project Man
 agement
|__}
  and expected = Ok [("DESCRIPTION", [`Altrep (Uri.of_string "CID:part3.msg.970415T083000@example.com") ],
                      `Text [ "Project XYZ Review Meeting will include the following agenda items: (a) Market Overview, (b) Finances, (c) Project Management" ]) ]
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let delegated_from () =
  let line = "ATTENDEE;DELEGATED-FROM=\"mailto:jsmith@example.com\":mailto:jdoe@example.com\n"
  and expected = Ok [("ATTENDEE", [`Delfrom [ Uri.of_string "mailto:jsmith@example.com" ] ], `Text [ "mailto:jdoe@example.com" ]) ] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let multiple_delegated_to () =
  let line = "ATTENDEE;DELEGATED-TO=\"mailto:jdoe@example.com\",\"mailto:jqpublic@example.com\":mailto:jsmith@example.com\n"
  and expected = Ok [("ATTENDEE", [`Delto [ Uri.of_string "mailto:jdoe@example.com" ; Uri.of_string "mailto:jqpublic@example.com" ] ],
                      `Text [ "mailto:jsmith@example.com" ]) ] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let line_tests = [
  "Parse line", `Quick, test_line ;
  "Parse multiline", `Quick, test_multiline ;
  "Parse parameters", `Quick, line_with_parameters ;
  "Parse parameter", `Quick, line_with_parameter ;
  "Parse attachment", `Quick, attach ;
  "Parse errored inline attachment", `Quick, attach_inline_binary_error ;
  "Parse inline attachment", `Quick, attach_inline_binary ;
  "Parse quoted prop", `Quick, quoted_prop ;
  "Parse altrep", `Quick, altprep ;
  "Parse delegated from", `Quick, delegated_from ;
  "Parse multiple delegated to", `Quick, multiple_delegated_to ;
]


(*     ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:TG9yZW
      0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW
      5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IG
      xhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbm
      ltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIG
      xhYm9yaXMgbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdW
      F0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbi
      B2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdC
      BudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaW
      RhdGF0IG5vbiBwcm9pZGVudCwgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYS
       BkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4= *)

let binary_b64_value () =
  let decoded =
    let b64 = "AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAgIAAAICAgADAwMAA////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMwAAAAAAABNEMQAAAAAAAkQgAAAAAAJEREQgAAACECQ0QgEgAAQxQzM0E0AABERCRCREQAADRDJEJEQwAAAhA0QwEQAAAAAEREAAAAAAAAREQAAAAAAAAkQgAAAAAAAAMgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" in
    match Nocrypto.Base64.decode (Cstruct.of_string b64) with
    | None -> Alcotest.fail "bad base64 encoding"
    | Some x -> `Binary x
  in
  let line = {__|ATTACH;FMTTYPE=image/vnd.microsoft.icon;ENCODING=BASE64;VALUE
 =BINARY:AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAA
 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAgIAAAICAgADAwMAA////AAAA
 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
 AAAAAAAAAAAAAAAAAAAAAAMwAAAAAAABNEMQAAAAAAAkQgAAAAAAJEREQgAA
 ACECQ0QgEgAAQxQzM0E0AABERCRCREQAADRDJEJEQwAAAhA0QwEQAAAAAERE
 AAAAAAAAREQAAAAAAAAkQgAAAAAAAAMgAAAAAAAAAAAAAAAAAAAAAAAAAAAA
 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
 AAAAAAAAAAAA
|__}
  and expected = Ok [ ("ATTACH", [ `Fmttype ("image", "vnd.microsoft.icon") ; `Encoding `Base64 ; `Valuetype `Binary ], decoded) ]
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let bool_value () =
  let line = "KEY;VALUE=BOOLEAN:TRUE\n"
  and expected = Ok [ ("KEY", [`Valuetype `Boolean], `Boolean true)]
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let date_value () =
  let line = "KEY;VALUE=DATE:19970714\n"
  and expected = Ok [ ("KEY", [`Valuetype `Date], `Date [(1997, 07, 14)]) ]
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let to_ptime date time =
  match Ptime.of_date_time (date, (time, 0)) with
  | None -> Alcotest.fail "invalid date time"
  | Some p -> p

let datetime_value () =
  let fake_key s = "KEY;VALUE=DATE-TIME:" ^ s ^ "\n" in
  let lines = List.map fake_key [ "19980118T230000" ; "19980119T070000Z" ; "19970630T235960Z" ]
  and expected = [
    Ok [ ("KEY", [`Valuetype `Datetime], `Datetime (to_ptime (1998, 01, 18) (23, 00, 00), false)) ] ;
    Ok [ ("KEY", [`Valuetype `Datetime], `Datetime (to_ptime (1998, 01, 19) (07, 00, 00), true)) ] ;
    Ok [ ("KEY", [`Valuetype `Datetime], `Datetime (to_ptime (1997, 06, 30) (23, 59, 60), true)) ] ;
  ]
  in
  List.iter2 (fun l e -> 
      let f = Icalendar.parse l in
      Alcotest.check result __LOC__ e f
    ) lines expected

let datetime_with_timezone_value () =
  let line = "KEY;VALUE=DATE-TIME;TZID=America/New_York:19980119T020000\n" in
  let expected = Ok [ ("KEY", [`Valuetype `Datetime; `Tzid (false, "America/New_York")], `Datetime (to_ptime (1998, 01, 19) (02, 00, 00), false)) ]
  in 
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

(*timezone normalization tests at end of 3.3.5 still TODO 

Example:  The following represents July 14, 1997, at 1:30 PM in New
      York City in each of the three time formats, using the "DTSTART"
      property.

       DTSTART:19970714T133000                   ; Local time
       DTSTART:19970714T173000Z                  ; UTC time
       DTSTART;TZID=America/New_York:19970714T133000
                                                 ; Local time and time
                                                 ; zone reference
*)

let duration_value () =
  let line = "KEY;VALUE=DURATION:P15DT5H0M20S\n"
  and expected = Ok [ ("KEY", [`Valuetype `Duration], `Duration 1314020) ]
  in 
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let float_value () =
  let fake_key s = "KEY;VALUE=FLOAT:" ^ s ^ "\n" in
  let lines = List.map fake_key ["1000000.0000001" ; "1.333" ; "-3.14" ]
  and expected = [
   Ok [ ("KEY", [`Valuetype `Float], `Float 1000000.0000001) ] ;
   Ok [ ("KEY", [`Valuetype `Float], `Float 1.333) ] ;
   Ok [ ("KEY", [`Valuetype `Float], `Float (-3.14)) ] ;
  ]
  in
  List.iter2 (fun l e -> 
    let f = Icalendar.parse l in
    Alcotest.check result __LOC__ e f
  ) lines expected

let integer_value () =
  let fake_key s = "KEY;VALUE=INTEGER:" ^ s ^ "\n" in
  let lines = List.map fake_key ["1234567890" ; "-1234567890" ; "+1234567890" ; "432109876" ]
  and expected = [
   Ok [ ("KEY", [`Valuetype `Integer], `Integer 1234567890) ] ;
   Ok [ ("KEY", [`Valuetype `Integer], `Integer (-1234567890)) ] ;
   Ok [ ("KEY", [`Valuetype `Integer], `Integer 1234567890) ] ;
   Ok [ ("KEY", [`Valuetype `Integer], `Integer 432109876) ] ;
  ]
  in
  List.iter2 (fun l e -> 
    let f = Icalendar.parse l in
    Alcotest.check result __LOC__ e f
  ) lines expected

let period_value () =
  let fake_key s = "KEY;VALUE=PERIOD:" ^ s ^ "\n" in
  let lines = List.map fake_key ["19970101T180000Z/PT5H30M" ; "19970101T180000Z/19970102T070000Z" ] in
  let expected = [
   Ok [ ("KEY", [`Valuetype `Period], `Period (to_ptime (1997, 01, 01) (18, 0, 0), to_ptime (1997, 01, 01) (23, 30, 0), true)) ] ;
   Ok [ ("KEY", [`Valuetype `Period], `Period (to_ptime (1997, 01, 01) (18, 0, 0), to_ptime (1997, 01, 02) (07, 00, 0), true)) ] ;
  ]
  in
  List.iter2 (fun l e -> 
    let f = Icalendar.parse l in
    Alcotest.check result __LOC__ e f
  ) lines expected

let recur_value () =
  let line = "KEY;VALUE=RECUR:FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1\n"
  and expected =
    Ok [ ("KEY", [`Valuetype `Recur],
          `Recur [`Frequency `Monthly;
                  `Byday [('+', 0, `Monday) ; ('+', 0, `Tuesday) ; ('+', 0, `Wednesday) ; ('+', 0, `Thursday) ; ('+', 0, `Friday) ] ;
                  `Bysetposday ('-', 1) ]) ]
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let recur_and_datetime_value () =
  (* TODO example from rfc, but added a VALUE=DATE-TIME and VALUE=RECUR to force recur parsing *)
  let line = "DTSTART;TZID=America/New_York;VALUE=DATE-TIME:19970105T083000\nRRULE;VALUE=RECUR:FREQ=YEARLY;INTERVAL=2;BYMONTH=1;BYDAY=SU;BYHOUR=8,9;BYMINUTE=30\n"
  and expected =
    Ok [ ("DTSTART", [`Tzid (false, "America/New_York"); `Valuetype `Datetime],
          `Datetime (to_ptime (1997, 01, 05) (08, 30, 00), false)) ;
         ("RRULE", [`Valuetype `Recur],
          `Recur [`Frequency `Yearly ;
                  `Interval 2 ;
                  `Bymonth [('+', 1)] ;
                  `Byday [('+', 0, `Sunday)] ;
                  `Byhour [8;9] ;
                  `Byminute [30] ]) ]
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let recur_daily () =
  let line = "KEY;VALUE=RECUR:FREQ=DAILY;COUNT=10;INTERVAL=2\n"
  and expected =
    Ok [ ("KEY", [`Valuetype `Recur], `Recur [
        `Frequency `Daily ;
        `Count 10 ;
        `Interval 2 ]) ]
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let recur_until () =
  let line = "RRULE;VALUE=RECUR:FREQ=DAILY;UNTIL=19971224T000000Z\n"
  and expected =
    Ok [ ("RRULE", [`Valuetype `Recur], `Recur [
        `Frequency `Daily ; `Until (to_ptime (1997, 12, 24) (0, 0, 0), true) ] ) ]
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let recur_every_day () =
  (* TODO according to rfc, both evaluate to the same occurences *)
  let lines = List.map (fun x -> x ^ "\n") [
    "RRULE;VALUE=RECUR:FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA" ;
    "RRULE;VALUE=RECUR:FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1"
  ]
  and expected = [
    Ok [ ("RRULE", [`Valuetype `Recur], `Recur [
        `Frequency `Yearly ; `Until (to_ptime (2000, 01, 31) (14, 00, 00), true) ; `Bymonth [('+', 1)] ; `Byday [ ('+', 0, `Sunday) ; ('+', 0, `Monday) ; ('+', 0, `Tuesday) ; ('+', 0, `Wednesday) ; ('+', 0, `Thursday) ; ('+', 0, `Friday) ; ('+', 0, `Saturday) ] ]) ] ;
    Ok [ ( "RRULE", [`Valuetype `Recur], `Recur [
        `Frequency `Daily ; `Until (to_ptime (2000, 01, 31) (14, 00, 00), true) ; `Bymonth [('+', 1)] ] ) ]
  ]
  in
  List.iter2 (fun l e -> 
    let f = Icalendar.parse l in
    Alcotest.check result __LOC__ e f
  ) lines expected

let text_value_newlines () =
  let line = {_|KEY;VALUE=TEXT:Project XYZ Final Review\nConference Room - 3B\nCome Prepared.
|_}
  and expected = Ok [ ("KEY", [`Valuetype `Text], `Text [ {_|Project XYZ Final Review
Conference Room - 3B
Come Prepared.|_} ] ) ]
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let text_values () =
  let line = {_|KEY;VALUE=TEXT:Project,abc,def
|_}
  and expected = Ok [ ("KEY", [`Valuetype `Text], `Text [ "Project" ; "abc" ; "def" ] ) ]
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let text_value_escape () =
  let line = {_|KEY;VALUE=TEXT:Project\\abc\,def
|_}
  and expected = Ok [ ("KEY", [`Valuetype `Text], `Text [ "Project\\abc,def" ] ) ]
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let time_invalid_value () =
  let line = "KEY;VALUE=TIME:230000-0800\n"
  and expected = Error "parse error"
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let time_values () =
  let fake_key s = "KEY;VALUE=TIME:" ^ s ^ "\n" in
  let lines = List.map fake_key [ "230000" ; "070000Z" ]
  and expected = [
    Ok [ ("KEY", [`Valuetype `Time], `Time (((23, 00, 00), 0), false)) ] ;
    Ok [ ("KEY", [`Valuetype `Time], `Time (((07, 00, 00), 0), true)) ]
  ]
  in
  List.iter2 (fun l e -> 
    let f = Icalendar.parse l in
    Alcotest.check result __LOC__ e f
  ) lines expected

let uri_value () =
  let line = "KEY;VALUE=URI:http://example.com/my-report.txt\n"
  and expected = Ok [ ("KEY", [`Valuetype `Uri], `Uri (Uri.of_string "http://example.com/my-report.txt") ) ]
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let utcoffset_invalid_value () =
  let fake_key s = "KEY;VALUE=UTC-OFFSET:" ^ s ^ "\n" in
  let lines = List.map fake_key [ "-0000" ; "-000000" ; "0000" ; "0100" ]
  and expected = [ Error "parse error" ; Error "parse error" ; Error "parse error" ; Error "parse error" ]
  in
  List.iter2 (fun l e ->
    let f = Icalendar.parse l in
    Alcotest.check result __LOC__ e f
  ) lines expected

let utcoffset_value () =
  let fake_key s = "KEY;VALUE=UTC-OFFSET:" ^ s ^ "\n" in
  let lines = List.map fake_key [ "-0500" ; "+0100" ]
  and expected = [
    Ok [("KEY", [`Valuetype `Utcoffset], `Utcoffset (Ptime.Span.of_int_s ((-5) * 60 * 60)))] ;
    Ok [("KEY", [`Valuetype `Utcoffset], `Utcoffset (Ptime.Span.of_int_s (1 * 60 * 60))) ]
  ]
  in
  List.iter2 (fun l e ->
    let f = Icalendar.parse l in
    Alcotest.check result __LOC__ e f
  ) lines expected

let value_tests = [
  "Test base64 binary", `Quick, binary_b64_value ;
  "Test bool", `Quick, bool_value ;
  "Test date", `Quick, date_value ;
  "Test datetime", `Quick, datetime_value ;
  "Test datetime with timezone", `Quick, datetime_with_timezone_value ;
  "Test duration", `Quick, duration_value ;
  "Test float", `Quick, float_value ;
  "Test integer", `Quick, integer_value ;
  "Test period", `Quick, period_value ;
  "Test recur", `Quick, recur_value ;
  "Test recur and datetime", `Quick, recur_and_datetime_value ;
  "Test recur daily", `Quick, recur_daily ;
  "Test recur until", `Quick, recur_until ;
  "Test recur every day", `Quick, recur_every_day ;
  "Test that in a text \\n are translated into newlines", `Quick, text_value_newlines ;
  "Test text with commas", `Quick, text_values ;
  "Test text with escaping (\\\\ and \\,)", `Quick, text_value_escape ;
  "Test time parser with invalid time", `Quick, time_invalid_value ;
  "Test time", `Quick, time_values ;
  "Test uri", `Quick, uri_value ;
  "Test utcoffset with invalid offset", `Quick, utcoffset_invalid_value ;
  "Test utcoffset", `Quick, utcoffset_value ;
]

let compare_calendar =
  let module M = struct
    type t = Icalendar.calendar
    let pp = Icalendar.pp_calendar
    let equal a b = compare a b = 0
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let result_c = Alcotest.(result compare_calendar string)

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

let object_tests = [
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
]

let tests = [
  "Line parsing tests", line_tests ;
  "Strongly typed value tests", value_tests ;
  "Object tests", object_tests ;
]

let () = 
  Printexc.record_backtrace true;
  Alcotest.run "" tests
