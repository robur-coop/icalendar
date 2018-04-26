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
  let expected = Ok [("DESCRIPTION", [], `Text "This is a long description that exists on a long line.")] in
  let f = Icalendar.parse line in
  Alcotest.check result "test short line" expected f

let test_multiline () =
  let multiline = {__|DESCRIPTION:This is a lo
 ng description
  that exists on a long line.
|__} in
  let expected = Ok [("DESCRIPTION", [], `Text "This is a long description that exists on a long line.")] in
  let f = Icalendar.parse multiline in
  Alcotest.check result "test short line" expected f

let line_with_parameters () =
  let line = "ATTENDEE;RSVP=TRUE;ROLE=REQ-PARTICIPANT:mailto:jsmith@example.com\n"
  and expected = Ok [("ATTENDEE", [`Rsvp true ; `Role `Reqparticipant], `Text "mailto:jsmith@example.com")] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let line_with_parameter () =
  let line = "RDATE;VALUE=DATE:19970304,19970504,19970704,19970904\n"
  and expected = Ok [("RDATE", [`Valuetype `Date], `Date [(1997, 03, 04) ; (1997, 05, 04) ; (1997, 07, 04) ; (1997, 09, 04) ])] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let attach () =
  let line = "ATTACH:http://example.com/public/quarterly-report.doc\n"
  and expected = Ok [("ATTACH", [], `Text "http://example.com/public/quarterly-report.doc")] in
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
  and expected = Ok [("DESCRIPTION",[`Altrep (Uri.of_string "cid:part1.0001@example.org")], `Text "The Fall'98 Wild Wizards Conference - - Las Vegas\\, NV\\, USA")] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let altprep () =
  let line = {__|DESCRIPTION;ALTREP="CID:part3.msg.970415T083000@example.com":
 Project XYZ Review Meeting will include the following agenda
  items: (a) Market Overview\, (b) Finances\, (c) Project Man
 agement
|__}
  and expected = Ok [("DESCRIPTION", [`Altrep (Uri.of_string "CID:part3.msg.970415T083000@example.com") ],
                      `Text "Project XYZ Review Meeting will include the following agenda items: (a) Market Overview\\, (b) Finances\\, (c) Project Management") ]
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let delegated_from () =
  let line = "ATTENDEE;DELEGATED-FROM=\"mailto:jsmith@example.com\":mailto:jdoe@example.com\n"
  and expected = Ok [("ATTENDEE", [`Delfrom [ Uri.of_string "mailto:jsmith@example.com" ] ], `Text "mailto:jdoe@example.com") ] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let multiple_delegated_to () =
  let line = "ATTENDEE;DELEGATED-TO=\"mailto:jdoe@example.com\",\"mailto:jqpublic@example.com\":mailto:jsmith@example.com\n"
  and expected = Ok [("ATTENDEE", [`Delto [ Uri.of_string "mailto:jdoe@example.com" ; Uri.of_string "mailto:jqpublic@example.com" ] ],
                      `Text "mailto:jsmith@example.com") ] in
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

let datetime_value () =
  let fake_key s = "KEY;VALUE=DATE-TIME:" ^ s ^ "\n"
  and to_ptime (date, time) utc = match Ptime.of_date_time (date, time) with
    | None -> Alcotest.fail "invalid date time"
    | Some p -> (p, utc)
  in
  let lines = List.map fake_key [ "19980118T230000" ; "19980119T070000Z" ; "19970630T235960Z" ]
  and expected = [
    Ok [ ("KEY", [`Valuetype `Datetime], `Datetime (to_ptime ((1998, 01, 18), ((23, 00, 00), 0)) false)) ] ;
    Ok [ ("KEY", [`Valuetype `Datetime], `Datetime (to_ptime ((1998, 01, 19), ((07, 00, 00), 0)) true)) ] ;
    Ok [ ("KEY", [`Valuetype `Datetime], `Datetime (to_ptime ((1997, 06, 30), ((23, 59, 60), 0)) true)) ] ;
  ]
  in
  List.iter2 (fun l e -> 
    let f = Icalendar.parse l in
    Alcotest.check result __LOC__ e f
  ) lines expected

let datetime_with_timezone_value () =
  let line = "KEY;VALUE=DATE-TIME;TZID=America/New_York:19980119T020000\n"
  and to_ptime (date, time) utc = match Ptime.of_date_time (date, time) with
    | None -> Alcotest.fail "invalid date time"
    | Some p -> (p, utc) in
  let expected = Ok [ ("KEY", [`Valuetype `Datetime; `Tzid (false, "America/New_York")], `Datetime (to_ptime ((1998, 01, 19), ((02, 00, 00), 0)) false)) ]
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
  let lines = List.map fake_key ["19970101T180000Z/PT5H30M" ; "19970101T180000Z/19970102T070000Z" ]
  and to_ptime (date, time) = match Ptime.of_date_time (date, time) with
    | None -> Alcotest.fail "invalid date time"
    | Some p -> p in
  let expected = [
   Ok [ ("KEY", [`Valuetype `Period], `Period (to_ptime ((1997, 01, 01), ((18, 0, 0), 0)) , to_ptime ((1997, 01, 01), ((23, 30, 0), 0)), true)) ] ;
   Ok [ ("KEY", [`Valuetype `Period], `Period (to_ptime ((1997, 01, 01), ((18, 0, 0), 0)) , to_ptime ((1997, 01, 02), ((07, 00, 0), 0)), true)) ] ;
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
]


let tests = [
  "Line parsing tests", line_tests ;
  "Strongly typed value tests", value_tests ;
]

let () = 
  Printexc.record_backtrace true;
  Alcotest.run "" tests
