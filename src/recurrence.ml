(* date arithmetic *)

let leap_year y =
  (* defined as dividible by 4, but not dividable by 100, but those by 400 *)
  match y mod 4 = 0, y mod 100 = 0, y mod 400 = 0 with
  | false, _, _ -> false
  | true, false, _ -> true
  | true, true, false -> false
  | true, true, true -> true

let days_in_month year = function
  | 1 -> 31
  | 2 when leap_year year -> 29
  | 2 -> 28
  | 3 -> 31
  | 4 -> 30
  | 5 -> 31
  | 6 -> 30
  | 7 -> 31
  | 8 -> 31
  | 9 -> 30
  | 10 -> 31
  | 11 -> 30
  | 12 -> 31
  | _ -> assert false

let add_years amount (y, m, d) = (y + amount, m, d)

let sub_years amount (y, m, d) = (y - amount, m, d)

(* TODO encapsulate in data structure? *)

let add_months amount (y, m, d) =
  let rec inc_y (ny, nm, nd) month =
    if month > 12
    then inc_y (add_years 1 (ny, nm, nd)) (month - 12)
    else (ny, month, nd)
  in
  let m' = m + amount in
  inc_y (y, m, d) m'

let sub_months amount (y, m, d) =
  let rec dec_y (ny, nm, nd) month =
    if month < 1
    then dec_y (sub_years 1 (ny, nm, nd)) (month + 12)
    else (ny, month, nd)
  in
  let m' = m - amount in
  dec_y (y, m, d) m'

let add_days amount (y, m, d) =
  let rec inc_m (ny, nm, nd) days =
    let md = days_in_month ny nm in
    if days > md
    then inc_m (add_months 1 (ny, nm, nd)) (days - md)
    else (ny, nm, days)
  in
  let d' = d + amount in
  inc_m (y, m, d) d'

let sub_days amount (y, m, d) =
  let rec dec_m (ny, nm, nd) days =
    if days < 1
    then
      let (ny', nm', nd') = sub_months 1 (ny, nm, nd) in
      let md = days_in_month ny' nm' in
      dec_m (ny', nm', nd') (days + md)
    else (ny, nm, days)
  in
  let d' = d - amount in
  dec_m (y, m, d) d'

let add_weeks amount date = add_days (7 * amount) date

(* find opt on lists *)
let find_opt f xs =
  match
    List.filter (function None -> false | Some _ -> true)
      (List.map f xs)
  with
  | [] -> None
  | [ Some x ] -> Some x
  | _ -> invalid_arg "wrong"

(* TODO remove ` from type *)
let wd_is_weekday wd wd' = match wd, wd' with
  | `Monday, `Monday | `Tuesday, `Tuesday | `Wednesday, `Wednesday
  | `Thursday, `Thursday | `Friday, `Friday | `Saturday, `Saturday
  | `Sunday, `Sunday -> true
  | _ -> false

let days_in_year y = if leap_year y then 366 else 365

let days_since_start_of_year (y, m, d) =
  let rec md = function
    | 0 -> 0
    | n -> days_in_month y n + md (pred n)
  in
  md (pred m) + d

let days_until_end_of_year (y, m, d) =
  let rec md = function
    | 12 -> 31 - d
    | n -> days_in_month y m + md (succ n)
  in
  md m

let weekday (y, m, d) =
  let d1_to_date = pred @@ days_since_start_of_year (y, m, d) in
  let epoch_to_d1 =
    let rec go = function
      | 1969 -> 0
      | x -> days_in_year x + go (pred x)
    in
    go (pred y)
  in
  (* 1970/01/01 was a thursday! *)
  match (epoch_to_d1 + d1_to_date) mod 7 with
  | 0 -> `Thursday
  | 1 -> `Friday
  | 2 -> `Saturday
  | 3 -> `Sunday
  | 4 -> `Monday
  | 5 -> `Tuesday
  | 6 -> `Wednesday
  | _ -> invalid_arg "bad input for weekday"

let wd = function
  | `Sunday -> 0
  | `Monday -> 1
  | `Tuesday -> 2
  | `Wednesday -> 3
  | `Thursday -> 4
  | `Friday -> 5
  | `Saturday -> 6

let w1d1_offset year =
  let wd = wd (weekday (year, 01, 01)) in
  (11 - wd) mod 7 - 3

(* TODO needs to be parametrised by wkst! *)
(* day 1 of week 1 in a given year *)
let w1d1 year =
  let off = w1d1_offset year
  and date = (year, 01, 01)
  in
  if off < 0
  then sub_days (abs off) date
  else add_days off date

(* returns (year * weeknumber), year can be last year, this year or next year *)
(* (date - d1w1) / 7 + 1 *)
let rec week_number (y, m, d) =
  let days = pred @@ days_since_start_of_year (y, m, d)
  and off = w1d1_offset y
  in
  let ndays = days - off in
  if ndays < 0
  then week_number (pred y, 12, 31)
  else
    let next_off = w1d1_offset (succ y) in
    if next_off < 0 && days_until_end_of_year (y, m, d) + next_off <= 0
    then (succ y, 1)
    else (y, ndays / 7 + 1)

let weeks y =
  let off = w1d1_offset (succ y) in
  let last_day = (y, 12, 31) in
  let last =
    if off >= 0
    then add_days off last_day
    else sub_days (abs (pred off)) last_day
  in
  snd (week_number last)

(* for matches: if n is negative, index from end, which is defined as -1 *)

let monthday_matches (y, m, d) n =
  if d = n
  then true
  else if n < 0
  then d = days_in_month y m + succ n
  else false

let weekno_matches date wn =
  let y, week = week_number date in
  if week = wn
  then true
  else if wn < 0
  then week = weeks y + succ wn
  else false

let yearday_matches (y, m, d) n =
  let count = days_since_start_of_year (y, m, d) in
  if count = n
  then true
  else if n < 0
  then count = days_in_year y + succ n
  else false

(* x: 0   => every $wd 
   x: pos => the xth $wd in month, 
   x: neg => the xth $wd in month, from end *)
let weekday_matches (y, m, d) (x, wd) =
  let weekday = weekday (y, m, d) in
  if wd_is_weekday weekday wd
  then 
    let n = succ (pred d / 7) in
    match x with
    | 0 -> true
    | x when x > 0 -> n = x
    | x when x < 0 -> 
        let total = n + (days_in_month y m - d) / 7 in
        n = total + succ x
  else false

let yearly_weekday_matches (y, m, d) (x, wd) =
  let weekday = weekday (y, m, d) in
  if wd_is_weekday weekday wd
  then 
    let n =
      let d = days_since_start_of_year (y, m, d) in
      succ (d / 7)
    in
    match x with
    | 0 -> true
    | x when x > 0 -> n = x
    | x when x < 0 -> 
        let total = n + (days_in_year y - n) / 7 in
        n = total + succ x
  else false

let opt f default = function
  | None -> default
  | Some x -> f x

let next_occurence start freq interval recurs =
  let s_date, s_time = Ptime.to_date_time start in
  let interval = match interval with None -> 1 | Some x -> x in
  let bymonth = find_opt (function `Bymonth x -> Some x | _ -> None) recurs
  and byweekno = find_opt (function `Byweek x -> Some x | _ -> None) recurs
  and byyearday = find_opt (function `Byyearday x -> Some x | _ -> None) recurs
  and bymonthday = find_opt (function `Bymonthday x -> Some x | _ -> None) recurs
  and byday = find_opt (function `Byday x -> Some x | _ -> None) recurs
  and bysetpos = find_opt (function `Bysetpostday x -> Some x | _ -> None) recurs
  and wkst = find_opt (function `Weekday x -> Some x | _ -> None) recurs
  in
  let wkst = match wkst with None -> `Monday | Some x -> x in
  (* as freq we implement yearly, monthly, weekly and daily;
     intervals between occurrences vary based on
     - leap year and month lengths
     - different recurrence rules combined with frequency;
     because of variable intervals, we advance day by day and apply a filter. 
     If no filter (byday, bymonthday or byyearday) is defined, we build one from the start day.
     For `Daily or `Weekly freq, we don't need to filter bymonthday. *)
  let bymonthday = match freq, byday, bymonthday, byyearday with
    | `Yearly, None, None, None
    | `Monthly, None, None, None -> let (_, _, d) = s_date in Some [ d ]
    | _ -> bymonthday
  in
  let rec next date = match freq with
    | `Daily ->
      (* generate candidate *)
      let (y, m, d) = add_days interval date in
      (* apply filter *)
      let (y, m, d) =
        let take ms =
          if List.mem m ms
          then (y, m, d)
          else next (y, m, d)
        in
        opt take (y, m, d) bymonth
      in
      let (y, m, d) =
        let take ds =
          if List.exists (monthday_matches (y, m, d)) ds
          then (y, m, d)
          else next (y, m, d)
        in
        opt take (y, m, d) bymonthday
      in
      let take ds =
        let weekday = weekday (y, m, d) in
        if List.exists (fun (_, wk') -> wd_is_weekday weekday wk') ds
        then (y, m, d)
        else next (y, m, d)
      in
      opt take (y, m, d) byday
    | `Weekly ->
      let (y, m, d) =
        match byday with
        | None -> add_weeks interval date
        | Some _ ->
          let (y, m, d) = add_days 1 date in
          if wd_is_weekday (weekday (y, m, d)) wkst && interval <> 1
          then add_weeks (pred interval) (y, m, d)
          else (y, m, d)
      in
      let (y, m, d) =
        let take ms =
          if List.mem m ms
          then (y, m, d)
          else next (y, m, d)
        in
        opt take (y, m, d) bymonth
      in
      let take ds =
        let weekday = weekday (y, m, d) in
        if List.exists (fun (_, wk') -> wd_is_weekday weekday wk') ds
        then (y, m, d)
        else next (y, m, d)
      in
      opt take (y, m, d) byday
    | `Monthly ->
      let (y, m, d) = add_days 1 date in
      let (y, m, d) =
        let (y', m', _) = date in
        let m'' = (y - y') * 12 in
        if (m'' + m - m') mod interval = 0
        then (y, m, d)
        else add_months (pred interval) (y, m, d)
      in
      let (y, m, d) =
        let take ms =
          if List.mem m ms
          then (y, m, d)
          else next (y, m, d)
        in
        opt take (y, m, d) bymonth
      in
      let (y, m, d) =
        let take md =
          if List.exists (monthday_matches (y, m, d)) md
          then (y, m, d)
          else next (y, m, d)
        in
        opt take (y, m, d) bymonthday
      in
      let take wd =
        if List.exists (weekday_matches (y, m, d)) wd
        then (y, m, d)
        else next (y, m, d)
      in
      opt take (y, m, d) byday
    | `Yearly ->
      let (y, m, d) = add_days 1 date in
      let (y, m, d) =
        let (y', _, _) = date in
        if (y - y') mod interval = 0
        then (y, m, d)
        else add_years (pred interval) (y, m, d)
      in
      let (y, m, d) =
        let take ms =
          if List.mem m ms
          then (y, m, d)
          else next (y, m, d)
        in
        opt take (y, m, d) bymonth
      in
      let (y, m, d) =
        let take wn =
          if List.exists (weekno_matches (y, m, d)) wn
          then (y, m, d)
          else next (y, m, d)
        in
        opt take (y, m, d) byweekno
      in
      let (y, m, d) =
        let take yd =
          if List.exists (yearday_matches (y, m, d)) yd
          then (y, m, d)
          else next (y, m, d)
        in
        opt take (y, m, d) byyearday
      in
      let (y, m, d) =
        let take md =
          if List.exists (monthday_matches (y, m, d)) md
          then (y, m, d)
          else next (y, m, d)
        in
        opt take (y, m, d) bymonthday
      in
      let take wd =
        if List.exists (yearly_weekday_matches (y, m, d)) wd
        then (y, m, d)
        else next (y, m, d)
      in
      opt take (y, m, d) byday
    | `Hourly | `Minutely | `Secondly -> invalid_arg "We don't support hourly, minutely or secondly for event frequencies."
  in
  let s_date' = next s_date in
  match Ptime.of_date_time (s_date', s_time) with
  | None -> invalid_arg "bad"
  | Some x -> x

(* TODO what happens if we get requests for infinite lists *)
(* TODO timezone is not applied yet *)
let all start (freq, count_or_until, interval, recurs) =
  match count_or_until with
  | Some (`Count n) ->
    let rec do_one s = function
      | 0 -> []
      | n ->
        let s' = next_occurence s freq interval recurs in
        s' :: do_one s' (pred n)
    in
    start :: do_one start (pred n) (* start must be in rule, according to rfc *)
  | Some (`Until (t, true)) ->
    let rec do_one s =
      let s' = next_occurence s freq interval recurs in
      if Ptime.is_later ~than:t s' (* desired behaviour if Ptime.equal? need to check *)
      then []
      else s' :: do_one s'
    in
    start :: do_one start
  | _ -> invalid_arg "Not Yet Imlemented"
