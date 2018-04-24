let compare_param =
  let module M = struct
    type t = Icalendar.icalparameter
    let pp = Icalendar.pp_icalparameter
    let equal a b = compare a b = 0
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let compare_t =
  let module M = struct
    type t = (string * Icalendar.icalparameter list * string)
    let pp f (a, b, c) = Fmt.pf f "(%s, %a, %s)" a (Fmt.list ~sep:(Fmt.unit ";") Icalendar.pp_icalparameter) b c
    let equal (a, b, c) (a', b', c') = String.compare a a' = 0
      && List.for_all2 (fun x x' -> compare x x' = 0) b b'
      && String.compare c c' = 0
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let result = Alcotest.(result (list compare_t) string)

let test_line () =
  let line = "DESCRIPTION:This is a long description that exists on a long line.\n" in
  let expected = Ok [("DESCRIPTION", [], "This is a long description that exists on a long line.")] in
  let f = Icalendar.parse line in
  Alcotest.check result "test short line" expected f

let test_multiline () =
  let multiline = {__|DESCRIPTION:This is a lo
 ng description
  that exists on a long line.
|__} in
  let expected = Ok [("DESCRIPTION", [], "This is a long description that exists on a long line.")] in
  let f = Icalendar.parse multiline in
  Alcotest.check result "test short line" expected f

let line_with_parameters () =
  let line = "ATTENDEE;RSVP=TRUE;ROLE=REQ-PARTICIPANT:mailto:jsmith@example.com\n"
  and expected = Ok [("ATTENDEE", [`Rsvp true ; `Role `Reqparticipant], "mailto:jsmith@example.com")] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let line_with_parameter () =
  let line = "RDATE;VALUE=DATE:19970304,19970504,19970704,19970904\n"
  and expected = Ok [("RDATE", [`Valuetype `Date], "19970304,19970504,19970704,19970904")] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let attach () =
  let line = "ATTACH:http://example.com/public/quarterly-report.doc\n"
  and expected = Ok [("ATTACH", [], "http://example.com/public/quarterly-report.doc")] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let attach_inline_binary () =
  let line = "ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4\n"
  and expected = Ok [("ATTACH",[`Fmttype ("text", "plain"); `Encoding `Base64 ; `Valuetype `Binary],"VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4")] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let quoted_prop () =
  let line = "DESCRIPTION;ALTREP=\"cid:part1.0001@example.org\":The Fall'98 Wild Wizards Conference - - Las Vegas\\, NV\\, USA\n"
  and expected = Ok [("DESCRIPTION",[`Altrep (Uri.of_string "cid:part1.0001@example.org")],"The Fall'98 Wild Wizards Conference - - Las Vegas\\, NV\\, USA")] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let altprep () =
  let line = {__|DESCRIPTION;ALTREP="CID:part3.msg.970415T083000@example.com":
 Project XYZ Review Meeting will include the following agenda
  items: (a) Market Overview\, (b) Finances\, (c) Project Man
 agement
|__}
  and expected = Ok [("DESCRIPTION", [`Altrep (Uri.of_string "CID:part3.msg.970415T083000@example.com") ],
                      "Project XYZ Review Meeting will include the following agenda items: (a) Market Overview\\, (b) Finances\\, (c) Project Management") ]
  in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let delegated_from () =
  let line = "ATTENDEE;DELEGATED-FROM=\"mailto:jsmith@example.com\":mailto:jdoe@example.com\n"
  and expected = Ok [("ATTENDEE", [`Delfrom [ Uri.of_string "mailto:jsmith@example.com" ] ], "mailto:jdoe@example.com") ] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let multiple_delegated_to () =
  let line = "ATTENDEE;DELEGATED-TO=\"mailto:jdoe@example.com\",\"mailto:jqpublic@example.com\":mailto:jsmith@example.com\n"
  and expected = Ok [("ATTENDEE", [`Delto [ Uri.of_string "mailto:jdoe@example.com" ; Uri.of_string "mailto:jqpublic@example.com" ] ], "mailto:jsmith@example.com") ] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let line_tests = [
  "Parse line", `Quick, test_line ;
  "Parse multiline", `Quick, test_multiline ;
  "Parse parameters", `Quick, line_with_parameters ;
  "Parse parameter", `Quick, line_with_parameter ;
  "Parse attachment", `Quick, attach ;
  "Parse inline attachment", `Quick, attach_inline_binary ;
  "Parse quoted prop", `Quick, quoted_prop ;
  "Parse altrep", `Quick, altprep ;
  "Parse delegated from", `Quick, delegated_from ;
  "Parse multiple delegated to", `Quick, multiple_delegated_to ;
]

let tests = [
  "Line parsing tests", line_tests ]

let () = Alcotest.run "" tests
