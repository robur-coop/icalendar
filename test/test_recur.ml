let to_ptime date time =
  match Ptime.of_date_time (date, (time, 0)) with
  | None -> Alcotest.fail "invalid date time"
  | Some p -> p

let p =
  let module M = struct
    type t = Ptime.t
    let pp = Ptime.pp_human ()
    let equal = Ptime.equal
  end in (module M : Alcotest.TESTABLE with type t = M.t)

(* from RFC 5545 section 3.8.5.3, but using UTC as timezone *)
let ex_1 () =
  let date = (1997, 09, 02)
  and time = (09, 00, 00)
  and rrule = (`Daily, Some (`Count 10), None, [])
  and res_dates = [
    (1997, 09, 02) ; (1997, 09, 03) ; (1997, 09, 04) ; (1997, 09, 05) ;
    (1997, 09, 06) ; (1997, 09, 07) ; (1997, 09, 08) ; (1997, 09, 09) ;
    (1997, 09, 10) ; (1997, 09, 11)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 1"
              (List.map (fun d -> to_ptime d time) res_dates)
              (Recurrence.all (to_ptime date time) rrule))

let ex_2 () =
  (* modified end: october instead of december *)
  let date = (1997, 09, 02)
  and time = (09, 00, 00)
  and rrule = (`Daily, Some (`Until (to_ptime (1997, 10, 24) (00, 00, 00), true)), None, [])
  and res = [
    (1997, 09, 02) ; (1997, 09, 03) ; (1997, 09, 04) ; (1997, 09, 05) ;
    (1997, 09, 06) ; (1997, 09, 07) ; (1997, 09, 08) ; (1997, 09, 09) ;
    (1997, 09, 10) ; (1997, 09, 11) ; (1997, 09, 12) ; (1997, 09, 13) ;
    (1997, 09, 14) ; (1997, 09, 15) ; (1997, 09, 16) ; (1997, 09, 17) ;
    (1997, 09, 18) ; (1997, 09, 19) ; (1997, 09, 20) ; (1997, 09, 21) ;
    (1997, 09, 22) ; (1997, 09, 23) ; (1997, 09, 24) ; (1997, 09, 25) ;
    (1997, 09, 26) ; (1997, 09, 27) ; (1997, 09, 28) ; (1997, 09, 29) ;
    (1997, 09, 30) ; (1997, 10, 01) ; (1997, 10, 02) ; (1997, 10, 03) ;
    (1997, 10, 04) ; (1997, 10, 05) ; (1997, 10, 06) ; (1997, 10, 07) ;
    (1997, 10, 08) ; (1997, 10, 09) ; (1997, 10, 10) ; (1997, 10, 11) ;
    (1997, 10, 12) ; (1997, 10, 13) ; (1997, 10, 14) ; (1997, 10, 15) ;
    (1997, 10, 16) ; (1997, 10, 17) ; (1997, 10, 18) ; (1997, 10, 19) ;
    (1997, 10, 20) ; (1997, 10, 21) ; (1997, 10, 22) ; (1997, 10, 23) ;
  ]
  in
  Alcotest.(check (list p) "compute occurences example 2"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_3 () =
  (* "every other day - forever" <- won't terminate atm, introduced a count *)
  let date = (1997, 09, 02)
  and time = (09, 00, 00)
  and rrule = (`Daily, Some (`Count 10), Some 2, [])
  and res = [
    (1997, 09, 02) ; (1997, 09, 04) ; (1997, 09, 06) ; (1997, 09, 08) ;
    (1997, 09, 10) ; (1997, 09, 12) ; (1997, 09, 14) ; (1997, 09, 16) ;
    (1997, 09, 18) ; (1997, 09, 20)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 3"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule)) ;
  (* same with until *)
  let rrule' =
    (`Daily, Some (`Until (to_ptime (1997, 09, 20) (10, 00, 00), true)), Some 2, [])
  in
  Alcotest.(check (list p) "compute occurences example 3"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule'))

let ex_4 () =
  let date = (1997, 09, 02)
  and time = (09, 00, 00)
  and rrule = (`Daily, Some (`Count 5), Some 10, [])
  and res = [
    (1997, 09, 02) ; (1997, 09, 12) ; (1997, 09, 22) ; (1997, 10, 02) ;
    (1997, 10, 12)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 4"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_5 () =
  let date = (1998, 01, 01)
  and time = (09, 00, 00)
  and rrule = (`Daily, Some (`Until (to_ptime (2000, 01, 31) (14, 00, 00), true)), None, [ `Bymonth [ 1 ]])
  and res = [
    (1998, 01, 01) ; (1998, 01, 02) ; (1998, 01, 03) ; (1998, 01, 04) ;
    (1998, 01, 05) ; (1998, 01, 06) ; (1998, 01, 07) ; (1998, 01, 08) ;
    (1998, 01, 09) ; (1998, 01, 10) ; (1998, 01, 11) ; (1998, 01, 12) ;
    (1998, 01, 13) ; (1998, 01, 14) ; (1998, 01, 15) ; (1998, 01, 16) ;
    (1998, 01, 17) ; (1998, 01, 18) ; (1998, 01, 19) ; (1998, 01, 20) ;
    (1998, 01, 21) ; (1998, 01, 22) ; (1998, 01, 23) ; (1998, 01, 24) ;
    (1998, 01, 25) ; (1998, 01, 26) ; (1998, 01, 27) ; (1998, 01, 28) ;
    (1998, 01, 29) ; (1998, 01, 30) ; (1998, 01, 31) ;
    (1999, 01, 01) ; (1999, 01, 02) ; (1999, 01, 03) ; (1999, 01, 04) ;
    (1999, 01, 05) ; (1999, 01, 06) ; (1999, 01, 07) ; (1999, 01, 08) ;
    (1999, 01, 09) ; (1999, 01, 10) ; (1999, 01, 11) ; (1999, 01, 12) ;
    (1999, 01, 13) ; (1999, 01, 14) ; (1999, 01, 15) ; (1999, 01, 16) ;
    (1999, 01, 17) ; (1999, 01, 18) ; (1999, 01, 19) ; (1999, 01, 20) ;
    (1999, 01, 21) ; (1999, 01, 22) ; (1999, 01, 23) ; (1999, 01, 24) ;
    (1999, 01, 25) ; (1999, 01, 26) ; (1999, 01, 27) ; (1999, 01, 28) ;
    (1999, 01, 29) ; (1999, 01, 30) ; (1999, 01, 31) ;
    (2000, 01, 01) ; (2000, 01, 02) ; (2000, 01, 03) ; (2000, 01, 04) ;
    (2000, 01, 05) ; (2000, 01, 06) ; (2000, 01, 07) ; (2000, 01, 08) ;
    (2000, 01, 09) ; (2000, 01, 10) ; (2000, 01, 11) ; (2000, 01, 12) ;
    (2000, 01, 13) ; (2000, 01, 14) ; (2000, 01, 15) ; (2000, 01, 16) ;
    (2000, 01, 17) ; (2000, 01, 18) ; (2000, 01, 19) ; (2000, 01, 20) ;
    (2000, 01, 21) ; (2000, 01, 22) ; (2000, 01, 23) ; (2000, 01, 24) ;
    (2000, 01, 25) ; (2000, 01, 26) ; (2000, 01, 27) ; (2000, 01, 28) ;
    (2000, 01, 29) ; (2000, 01, 30) ; (2000, 01, 31) ;
  ]
  in
  Alcotest.(check (list p) "compute occurences example 5"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule)) ;
  let all_days = [
    (0, `Sunday) ; (0, `Monday) ; (0, `Tuesday) ; (0, `Wednesday) ;
    (0, `Thursday) ; (0, `Friday) ; (0, `Saturday)
  ] in
  let rrule' =
    (`Yearly,
     Some (`Until (to_ptime (2000, 01, 31) (14, 00, 00), true)),
     None, [ `Bymonth [ 1 ] ; `Byday all_days ])
  in
  Alcotest.(check (list p) "compute occurences example 5"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule'))

let ex_6 () =
  let date = (1997, 09, 02)
  and time = (09, 00, 00)
  and rrule = (`Weekly, Some (`Count 10), None, [])
  and res = [
    (1997, 09, 02) ; (1997, 09, 09) ; (1997, 09, 16) ; (1997, 09, 23) ;
    (1997, 09, 30) ; (1997, 10, 07) ; (1997, 10, 14) ; (1997, 10, 21) ;
    (1997, 10, 28) ; (1997, 11, 04)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 6"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_7 () =
  let date = (1997, 09, 02)
  and time = (09, 00, 00)
  and rrule = (`Weekly, Some (`Until (to_ptime (1997, 12, 24) (0, 0, 0), true)), None, [])
  and res = [
    (1997, 09, 02) ; (1997, 09, 09) ; (1997, 09, 16) ; (1997, 09, 23) ;
    (1997, 09, 30) ; (1997, 10, 07) ; (1997, 10, 14) ; (1997, 10, 21) ;
    (1997, 10, 28) ; (1997, 11, 04) ; (1997, 11, 11) ; (1997, 11, 18) ;
    (1997, 11, 25) ; (1997, 12, 02) ; (1997, 12, 09) ; (1997, 12, 16) ;
    (1997, 12, 23)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 7"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_8 () =
  (* every other week - forever <- limited by 10 *)
  let date = (1997, 09, 02)
  and time = (09, 00, 00)
  and rrule = (`Weekly, Some (`Count 10), Some 2, [`Weekday `Sunday])
  and res = [
    (1997, 09, 02) ; (1997, 09, 16) ; (1997, 09, 30) ; (1997, 10, 14) ;
    (1997, 10, 28) ; (1997, 11, 11) ; (1997, 11, 25) ; (1997, 12, 09) ;
    (1997, 12, 23) ; (1998, 01, 06)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 8"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_9 () =
  let date = (1997, 09, 02)
  and time = (09, 00, 00)
  and rrule = (`Weekly, Some (`Until (to_ptime (1997, 10, 07) (00, 00, 00), true)), None, [`Weekday `Sunday ; `Byday [ (0, `Tuesday) ; (0, `Thursday) ]])
  and res = [
    (1997, 09, 02) ; (1997, 09, 04) ; (1997, 09, 09) ; (1997, 09, 11) ;
    (1997, 09, 16) ; (1997, 09, 18) ; (1997, 09, 23) ; (1997, 09, 25) ;
    (1997, 09, 30) ; (1997, 10, 02)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 9"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule)) ;
  let rrule' = (`Weekly, Some (`Count 10), None, [`Weekday `Sunday ; `Byday [ (0, `Tuesday) ; (0, `Thursday) ]]) in
  Alcotest.(check (list p) "compute occurences example 9"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule'))

let ex_10 () =
  let date = (1997, 09, 01)
  and time = (09, 00, 00)
  and rrule = (`Weekly, Some (`Until (to_ptime (1997, 12, 24) (00, 00, 00), true)), Some 2, [`Weekday `Sunday ; `Byday [ (0, `Monday) ; (0, `Wednesday) ; (0, `Friday) ]])
  and res = [
    (1997, 09, 01) ; (1997, 09, 03) ; (1997, 09, 05) ; (1997, 09, 15) ;
    (1997, 09, 17) ; (1997, 09, 19) ; (1997, 09, 29) ; (1997, 10, 01) ;
    (1997, 10, 03) ; (1997, 10, 13) ; (1997, 10, 15) ; (1997, 10, 17) ;
    (1997, 10, 27) ; (1997, 10, 29) ; (1997, 10, 31) ; (1997, 11, 10) ;
    (1997, 11, 12) ; (1997, 11, 14) ; (1997, 11, 24) ; (1997, 11, 26) ;
    (1997, 11, 28) ; (1997, 12, 08) ; (1997, 12, 10) ; (1997, 12, 12) ;
    (1997, 12, 22)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 10"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_11 () =
  let date = (1997, 09, 02)
  and time = (09, 00, 00)
  and rrule = (`Weekly, Some (`Count 8), Some 2, [`Weekday `Sunday ; `Byday [ (0, `Tuesday) ; (0, `Thursday) ]])
  and res = [
    (1997, 09, 02) ; (1997, 09, 04) ; (1997, 09, 16) ; (1997, 09, 18) ;
    (1997, 09, 30) ; (1997, 10, 02) ; (1997, 10, 14) ; (1997, 10, 16)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 11"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_12 () =
  let date = (1997, 09, 05)
  and time = (09, 00, 00)
  and rrule = (`Monthly, Some (`Count 10), None, [`Byday [ (1, `Friday) ]])
  and res = [
    (1997, 09, 05) ; (1997, 10, 03) ; (1997, 11, 07) ; (1997, 12, 05) ;
    (1998, 01, 02) ; (1998, 02, 06) ; (1998, 03, 06) ; (1998, 04, 03) ;
    (1998, 05, 01) ; (1998, 06, 05)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 12"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_13 () =
  let date = (1997, 09, 05)
  and time = (09, 00, 00)
  and rrule = (`Monthly, Some (`Until (to_ptime (1997, 12, 24) (0, 0, 0), true)), None, [`Byday [ (1, `Friday) ]])
  and res = [
    (1997, 09, 05) ; (1997, 10, 03) ; (1997, 11, 07) ; (1997, 12, 05)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 13"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_14 () =
  let date = (1997, 09, 07)
  and time = (09, 00, 00)
  and rrule = (`Monthly, Some (`Count 10), Some 2, [`Byday [ (1, `Sunday) ; (-1, `Sunday) ]])
  and res = [
    (1997, 09, 07) ; (1997, 09, 28) ; (1997, 11, 02) ; (1997, 11, 30) ;
    (1998, 01, 04) ; (1998, 01, 25) ; (1998, 03, 01) ; (1998, 03, 29) ;
    (1998, 05, 03) ; (1998, 05, 31)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 14"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_15 () =
  let date = (1997, 09, 22)
  and time = (09, 00, 00)
  and rrule = (`Monthly, Some (`Count 6), None, [`Byday [ (-2, `Monday) ]])
  and res = [
    (1997, 09, 22) ; (1997, 10, 20) ; (1997, 11, 17) ; (1997, 12, 22) ;
    (1998, 01, 19) ; (1998, 02, 16)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 15"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_16 () =
  (* forever - again limiting with count 6 instead *)
  let date = (1997, 09, 28)
  and time = (09, 00, 00)
  and rrule = (`Monthly, Some (`Count 6), None, [`Bymonthday [ -3 ]])
  and res = [
    (1997, 09, 28) ; (1997, 10, 29) ; (1997, 11, 28) ; (1997, 12, 29) ;
    (1998, 01, 29) ; (1998, 02, 26)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 16"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_17 () =
  let date = (1997, 09, 02)
  and time = (09, 00, 00)
  and rrule = (`Monthly, Some (`Count 10), None, [`Bymonthday [ 2 ; 15 ]])
  and res = [
    (1997, 09, 02) ; (1997, 09, 15) ; (1997, 10, 02) ; (1997, 10, 15) ;
    (1997, 11, 02) ; (1997, 11, 15) ; (1997, 12, 02) ; (1997, 12, 15) ;
    (1998, 01, 02) ; (1998, 01, 15)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 17"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_18 () =
  (* to include last day of feb, increased count to 11 *)
  let date = (1997, 09, 30)
  and time = (09, 00, 00)
  and rrule = (`Monthly, Some (`Count 11), None, [`Bymonthday [ 1 ; -1 ]])
  and res = [
    (1997, 09, 30) ; (1997, 10, 01) ; (1997, 10, 31) ; (1997, 11, 01) ;
    (1997, 11, 30) ; (1997, 12, 01) ; (1997, 12, 31) ; (1998, 01, 01) ;
    (1998, 01, 31) ; (1998, 02, 01) ; (1998, 02, 28)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 18"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_19 () =
  let date = (1997, 09, 10)
  and time = (09, 00, 00)
  and rrule = (`Monthly, Some (`Count 10), Some 18, [`Bymonthday [ 10 ; 11 ; 12 ; 13 ; 14 ; 15 ]])
  and res = [
    (1997, 09, 10) ; (1997, 09, 11) ; (1997, 09, 12) ; (1997, 09, 13) ;
    (1997, 09, 14) ; (1997, 09, 15) ; (1999, 03, 10) ; (1999, 03, 11) ;
    (1999, 03, 12) ; (1999, 03, 13)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 19"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_20 () =
  (* limited to 10 again *)
  let date = (1997, 09, 02)
  and time = (09, 00, 00)
  and rrule = (`Monthly, Some (`Count 10), Some 2, [`Byday [ (0, `Tuesday) ]])
  and res = [
    (1997, 09, 02) ; (1997, 09, 09) ; (1997, 09, 16) ; (1997, 09, 23) ;
    (1997, 09, 30) ; (1997, 11, 04) ; (1997, 11, 11) ; (1997, 11, 18) ;
    (1997, 11, 25) ; (1998, 01, 06)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 20"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_21 () =
  let date = (1997, 06, 10)
  and time = (09, 00, 00)
  and rrule = (`Yearly, Some (`Count 10), None, [`Bymonth [ 6 ; 7 ]])
  and res = [
    (1997, 06, 10) ; (1997, 07, 10) ; (1998, 06, 10) ; (1998, 07, 10) ;
    (1999, 06, 10) ; (1999, 07, 10) ; (2000, 06, 10) ; (2000, 07, 10) ;
    (2001, 06, 10) ; (2001, 07, 10)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 21"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_22 () =
  let date = (1997, 03, 10)
  and time = (09, 00, 00)
  and rrule = (`Yearly, Some (`Count 10), Some 2, [`Bymonth [ 1 ; 2 ; 3 ]])
  and res = [
    (1997, 03, 10) ; (1999, 01, 10) ; (1999, 02, 10) ; (1999, 03, 10) ;
    (2001, 01, 10) ; (2001, 02, 10) ; (2001, 03, 10) ; (2003, 01, 10) ;
    (2003, 02, 10) ; (2003, 03, 10)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 22"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_23 () =
  let date = (1997, 01, 01)
  and time = (09, 00, 00)
  and rrule = (`Yearly, Some (`Count 10), Some 3, [`Byyearday [ 1 ; 100 ; 200 ]])
  and res = [
    (1997, 01, 01) ; (1997, 04, 10) ; (1997, 07, 19) ; (2000, 01, 01) ;
    (2000, 04, 09) ; (2000, 07, 18) ; (2003, 01, 01) ; (2003, 04, 10) ;
    (2003, 07, 19) ; (2006, 01, 01)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 23"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_24 () =
  (* forever - limiting to count 3 *)
  let date = (1997, 05, 19)
  and time = (09, 00, 00)
  and rrule = (`Yearly, Some (`Count 3), None, [`Byday [ (20, `Monday) ]])
  and res = [
    (1997, 05, 19) ; (1998, 05, 18) ; (1999, 05, 17)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 24"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_25 () =
  (* forever - limiting to count 3 *)
  let date = (1997, 05, 12)
  and time = (09, 00, 00)
  and rrule = (`Yearly, Some (`Count 3), None, [`Byweek [ 20 ] ; `Byday [ (0, `Monday) ]])
  and res = [
    (1997, 05, 12) ; (1998, 05, 11) ; (1999, 05, 17)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 25"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_26 () =
  (* forever - limiting to count 11 *)
  let date = (1997, 03, 13)
  and time = (09, 00, 00)
  and rrule = (`Yearly, Some (`Count 11), None, [`Bymonth [ 3 ] ; `Byday [ (0, `Thursday) ]])
  and res = [
    (1997, 03, 13) ; (1997, 03, 20) ; (1997, 03, 27) ; (1998, 03, 05) ;
    (1998, 03, 12) ; (1998, 03, 19) ; (1998, 03, 26) ; (1999, 03, 04) ;
    (1999, 03, 11) ; (1999, 03, 18) ; (1999, 03, 25)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 26"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_27 () =
  (* forever - limiting to count 39 *)
  let date = (1997, 06, 05)
  and time = (09, 00, 00)
  and rrule = (`Yearly, Some (`Count 39), None, [`Bymonth [ 6 ; 7 ; 8 ] ; `Byday [ (0, `Thursday) ]])
  and res = [
    (1997, 06, 05) ; (1997, 06, 12) ; (1997, 06, 19) ; (1997, 06, 26) ;
    (1997, 07, 03) ; (1997, 07, 10) ; (1997, 07, 17) ; (1997, 07, 24) ;
    (1997, 07, 31) ; (1997, 08, 07) ; (1997, 08, 14) ; (1997, 08, 21) ;
    (1997, 08, 28) ; (1998, 06, 04) ; (1998, 06, 11) ; (1998, 06, 18) ;
    (1998, 06, 25) ; (1998, 07, 02) ; (1998, 07, 09) ; (1998, 07, 16) ;
    (1998, 07, 23) ; (1998, 07, 30) ; (1998, 08, 06) ; (1998, 08, 13) ;
    (1998, 08, 20) ; (1998, 08, 27) ; (1999, 06, 03) ; (1999, 06, 10) ;
    (1999, 06, 17) ; (1999, 06, 24) ; (1999, 07, 01) ; (1999, 07, 08) ;
    (1999, 07, 15) ; (1999, 07, 22) ; (1999, 07, 29) ; (1999, 08, 05) ;
    (1999, 08, 12) ; (1999, 08, 19) ; (1999, 08, 26)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 27"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_28 () =
  (* forever - limiting to count 5 *)
  (* EXDATE for first thingy, adjusting to first real occurence *)
  let date = (1998, 02, 13)
  and time = (09, 00, 00)
  and rrule = (`Monthly, Some (`Count 5), None, [`Bymonthday [ 13 ] ; `Byday [ (0, `Friday) ]])
  and res = [
    (1998, 02, 13) ; (1998, 03, 13) ; (1998, 11, 13) ; (1999, 08, 13) ;
    (2000, 10, 13)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 28"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_29 () =
  (* forever - limiting to count 10 *)
  let date = (1997, 09, 13)
  and time = (09, 00, 00)
  and rrule = (`Monthly, Some (`Count 10), None, [`Bymonthday [ 7 ; 8 ; 9 ; 10 ; 11 ; 12 ; 13 ] ; `Byday [ (0, `Saturday) ]])
  and res = [
    (1997, 09, 13) ; (1997, 10, 11) ; (1997, 11, 08) ; (1997, 12, 13) ;
    (1998, 01, 10) ; (1998, 02, 07) ; (1998, 03, 07) ; (1998, 04, 11) ;
    (1998, 05, 09) ; (1998, 06, 13)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 29"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_30 () =
  (* forever - limiting to count 3 *)
  let date = (1996, 11, 05)
  and time = (09, 00, 00)
  and rrule = (`Yearly, Some (`Count 3), Some 4, [`Bymonth [ 11 ] ; `Byday [ (0, `Tuesday) ] ; `Bymonthday [ 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ]])
  and res = [
    (1996, 11, 05) ; (2000, 11, 07) ; (2004, 11, 02)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 30"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let ex_31 () =
  (* forever - limiting to count 3 *)
  let date = (1996, 11, 29)
  and time = (09, 00, 00)
  and rrule = (`Monthly, Some (`Count 3), None, [`Byday [ (0, `Monday) ; (0, `Tuesday) ; (0, `Wednesday) ; (0, `Thursday) ; (0, `Friday) ] ; `Bysetpos [ -1 ]])
  and res = [
    (1996, 11, 29) ; (1996, 12, 31) ; (1997, 01, 31)
  ]
  in
  Alcotest.(check (list p) "compute occurences example 31: Bysetpos; last workday in month"
              (List.map (fun d -> to_ptime d time) res)
              (Recurrence.all (to_ptime date time) rrule))

let tests = [
  "example 1", `Quick, ex_1 ;
  "example 2", `Quick, ex_2 ;
  "example 3", `Quick, ex_3 ;
  "example 4", `Quick, ex_4 ;
  "example 5", `Quick, ex_5 ;
  "example 6", `Quick, ex_6 ;
  "example 7", `Quick, ex_7 ;
  "example 8", `Quick, ex_8 ;
  "example 9", `Quick, ex_9 ;
  "example 10", `Quick, ex_10 ;
  "example 11", `Quick, ex_11 ;
  "example 12", `Quick, ex_12 ;
  "example 13", `Quick, ex_13 ;
  "example 14", `Quick, ex_14 ;
  "example 15", `Quick, ex_15 ;
  "example 16", `Quick, ex_16 ;
  "example 17", `Quick, ex_17 ;
  "example 18", `Quick, ex_18 ;
  "example 19", `Quick, ex_19 ;
  "example 20", `Quick, ex_20 ;
  "example 21", `Quick, ex_21 ;
  "example 22", `Quick, ex_22 ;
  "example 23", `Quick, ex_23 ;
  "example 24", `Quick, ex_24 ;
  "example 25", `Quick, ex_25 ;
  "example 26", `Quick, ex_26 ;
  "example 27", `Quick, ex_27 ;
  "example 28", `Quick, ex_28 ;
  "example 29", `Quick, ex_29 ;
  "example 30", `Quick, ex_30 ;
  "example 31: Bysetpos; last workday in month", `Quick, ex_31 ;
]
