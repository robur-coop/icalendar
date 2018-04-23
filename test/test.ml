let compare_t = 
  let module M = struct 
    type t = (string * (string * string) list * string) 
    let pp f (a, b, c) = Fmt.pf f "(%s, %a, %s)" a (Fmt.list ~sep:(Fmt.unit ";") (Fmt.pair ~sep:(Fmt.unit " -> ") Fmt.string Fmt.string)) b c
    let equal (a, b, c) (a', b', c') = String.compare a a' = 0 
      && List.for_all2 (fun (x, y) (x', y') -> String.compare x x' = 0 && String.compare y y' = 0) b b' 
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
  and expected = Ok [("ATTENDEE", [("RSVP", "TRUE") ; ("ROLE", "REQ-PARTICIPANT")], "mailto:jsmith@example.com")] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let line_with_parameter () =
  let line = "RDATE;VALUE=DATE:19970304,19970504,19970704,19970904\n"
  and expected = Ok [("RDATE", [("VALUE","DATE")], "19970304,19970504,19970704,19970904")] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let attach () =
  let line = "ATTACH:http://example.com/public/quarterly-report.doc\n"
  and expected = Ok [("ATTACH", [], "http://example.com/public/quarterly-report.doc")] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let attach_inline_binary () =
  let line = "ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4\n"
  and expected = Ok [("ATTACH",[("FMTTYPE","text/plain");("ENCODING","BASE64");("VALUE","BINARY")],"VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4")] in
  let f = Icalendar.parse line in
  Alcotest.check result __LOC__ expected f

let quoted_prop () =
  let line = "DESCRIPTION;ALTREP=\"cid:part1.0001@example.org\":The Fall'98 Wild Wizards Conference - - Las Vegas\\, NV\\, USA\n"
  and expected = Ok [("DESCRIPTION",[("ALTREP","cid:part1.0001@example.org")],"The Fall'98 Wild Wizards Conference - - Las Vegas\\, NV\\, USA")] in
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
]

let tests = [ 
  "Line parsing tests", line_tests ]

let () = Alcotest.run "" tests
