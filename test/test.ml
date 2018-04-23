let compare_t = 
  let module M = struct 
    type t = (string * (string * string) list * string) 
    let pp f (a, b, c) = Fmt.pf f "(%s, %a, %s)" a (Fmt.list ~sep:(Fmt.unit ";") (Fmt.pair ~sep:(Fmt.unit " -> ") Fmt.string Fmt.string)) b c
    let equal (a, b, c) (a', b', c') = String.compare a a' = 0 
      && List.for_all2 (fun (x, y) (x', y') -> String.compare x x' = 0 && String.compare y y' = 0) b b' 
      && String.compare c c' = 0  
  end in (module M: Alcotest.TESTABLE with type t = M.t)



let test_line () = 
  let line = "DESCRIPTION:This is a long description that exists on a long line.\n" in
  let expected = Ok [("DESCRIPTION", [], "This is a long description that exists on a long line.")] in
  let f = Icalendar.parse line in
  Alcotest.(check (result (list compare_t) string) "test short line" expected f)

let test_multiline () = 
  let multiline = {__|DESCRIPTION:This is a lo
      ng description
       that exists on a long line.|__} in
  let expected = Ok [("DESCRIPTION", [], "This is a long description that exists on a long line.")] in
  let f = Icalendar.parse multiline in
  Alcotest.(check (result (list compare_t) string) "test short line" expected f)


let line_tests = [
  "Parse line", `Quick, test_line ;
  "Parse multiline", `Quick, test_multiline
]

let tests = [ 
  "Line parsing tests", line_tests ]

let () = Alcotest.run "" tests
