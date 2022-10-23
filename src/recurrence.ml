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
    | x ->
      if x > 0
      then n = x
      else
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
    | x ->
      if x > 0
      then n = x
      else
        let total = n + (days_in_year y - n) / 7 in
        n = total + succ x
  else false

let opt f default = function
  | None -> default
  | Some x -> f x

let is_occurence s_date freq (bymonth, byweekno, byyearday, bymonthday, byday) =
  match freq with
  | `Daily ->
    let (y, m, d) = s_date in
    let is_bymonth = match bymonth with
      | None -> true
      | Some ms -> List.mem m ms
    in
    let is_bymonthday () = match bymonthday with
      | None -> true
      | Some ds -> List.exists (monthday_matches (y, m, d)) ds
    in
    let is_byday () = match byday with
      | None -> true
      | Some ds ->
        let weekday = weekday (y, m, d) in
        List.exists (fun (_, wk') -> wd_is_weekday weekday wk') ds
    in
    is_bymonth && is_bymonthday () && is_byday ()
  | `Weekly ->
    let (y, m, d) = s_date in
    let is_bymonth = match bymonth with
      | None -> true
      | Some ms -> List.mem m ms
    in
    let is_byday () = match byday with
      | None -> true
      | Some ds ->
        let weekday = weekday (y, m, d) in
        List.exists (fun (_, wk') -> wd_is_weekday weekday wk') ds
    in
    is_bymonth && is_byday ()
  | `Monthly ->
    let (y, m, d) = s_date in
    let is_bymonth = match bymonth with
      | None -> true
      | Some ms -> List.mem m ms
    in
    let is_bymonthday () = match bymonthday with
      | None -> true
      | Some md -> List.exists (monthday_matches (y, m, d)) md
    in
    let is_byday () = match byday with
      | None -> true
      | Some wd -> List.exists (weekday_matches (y, m, d)) wd
    in
    is_bymonth && is_bymonthday () && is_byday ()
  | `Yearly ->
    let (y, m, d) = s_date in
    let is_bymonth = match bymonth with
      | None -> true
      | Some ms -> List.mem m ms
    in
    let is_byweekno () = match byweekno with
      | None -> true
      | Some wn -> List.exists (weekno_matches (y, m, d)) wn
    in
    let is_byyearday () = match byyearday with
      | None -> true
      | Some yd -> List.exists (yearday_matches (y, m, d)) yd
    in
    let is_bymonthday () = match bymonthday with
      | None -> true
      | Some md -> List.exists (monthday_matches (y, m, d)) md
    in
    let is_byday () = match byday, bymonth with
      | None, _ -> true
      | Some wd, None -> List.exists (yearly_weekday_matches (y, m, d)) wd
      | Some wd, Some _ -> List.exists (weekday_matches (y, m, d)) wd
    in
    is_bymonth && is_byweekno () && is_byyearday () && is_bymonthday () && is_byday ()
  | `Hourly | `Minutely | `Secondly -> invalid_arg "We don't support hourly, minutely or secondly for event frequencies."

let filter_bysetpos bysetpos set =
  match bysetpos with
  | None -> set
  | Some p ->
    let l = List.length set in
    let positions = List.map (fun i -> if i < 0 then l + i else pred i) p |>
    List.sort_uniq compare in
    List.map (List.nth set) positions

let compare_dates (y, m, d) (y', m', d') = match compare y y' with
  | 0 -> begin match compare m m' with
    | 0 -> compare d d'
    | x -> x
    end
  | x -> x

let after_start start set =
  List.filter (fun d -> compare_dates d start >= 0) set

type gen_state = {
  mutable next_interval : Ptime.t ;
  mutable set : Ptime.t list ;
  next_recurrence_set : Ptime.t -> Ptime.t * Ptime.t list
}

let init_rr next_interval freq interval filters bysetpos wkst =
  (* needs to be computed completely, because bysetpos may index from back *)
  let next_recurrence_set start =
    let s_date, s_time = Ptime.to_date_time start in
    (* start_set = beginning of recurrence set, e.g. start of month for monthly *)
    let start_set, end_set, start_next_set =
      let start_set, advance_by_freq = match freq with
        | `Daily -> s_date, add_days
        | `Weekly  -> let rec weekstart d = if wd_is_weekday wkst (weekday d) then d, add_weeks else weekstart (sub_days 1 d) in weekstart s_date
        | `Monthly -> let (y, m, _) = s_date in (y, m, 1), add_months
        | `Yearly  -> let (y, _, _) = s_date in (y, 1, 1), add_years
        | _ -> assert false
      in
      let interval' = match interval with None -> 1 | Some x -> x in
      start_set, advance_by_freq 1 start_set, advance_by_freq interval' start_set
    in
    let in_set x = compare_dates start_set x <= 0 && compare_dates x end_set < 0 in
    let rec next_elem d =
      if in_set d
      then let d' = add_days 1 d in
        if is_occurence d freq filters
        then d :: next_elem d'
        else next_elem d'
      else []
    in
    let set = next_elem start_set in
    let set' = filter_bysetpos bysetpos set in
    let set'' = after_start s_date set' in
    let to_ptime t = match Ptime.of_date_time (t, s_time) with
      | None -> assert false (*TODO*)
      | Some x -> x in
    to_ptime start_next_set, List.map to_ptime set''
  in
  { next_interval ; set = [] ; next_recurrence_set }

let rec next_rr g =
  match g.set with
  | [] ->
    let next_interval', rr_set = g.next_recurrence_set g.next_interval in
    g.next_interval <- next_interval' ;
    g.set <- rr_set ;
    next_rr g
  | hd :: tl ->
    g.set <- tl ;
    hd

type count = {
  mutable count : int ;
  f : gen_state ;
}

let init_count count f = { count ; f }

let next_count g =
  if g.count = 0
  then None
  else begin
    g.count <- pred g.count ;
    Some (next_rr g.f)
  end

type until = {
  until : Ptime.t ;
  f : gen_state
}

let init_until until f = { until ; f }

let next_until g =
  let candidate = next_rr g.f in
  (* desired behaviour if Ptime.equal? need to check *)
  if Ptime.is_earlier ~than:g.until candidate
  then Some candidate
  else None

let add_missing_filters recurs freq start =
  let s_date, _s_time = Ptime.to_date_time start in
  let bymonth = find_opt (function `Bymonth x -> Some x | _ -> None) recurs
  and byweekno = find_opt (function `Byweek x -> Some x | _ -> None) recurs
  and byyearday = find_opt (function `Byyearday x -> Some x | _ -> None) recurs
  and bymonthday = find_opt (function `Bymonthday x -> Some x | _ -> None) recurs
  and byday = find_opt (function `Byday x -> Some x | _ -> None) recurs
  in
  (* as freq we implement yearly, monthly, weekly and daily;
     intervals between occurrences vary based on
     - leap year and month lengths
     - different recurrence rules combined with frequency;
     because of variable intervals, we advance day by day and apply a filter.
     If no filter (byday, bymonthday or byyearday) is defined, we build one from the start day.
     For `Daily or `Weekly freq, we don't need to filter bymonthday. *)
  let bymonth, bymonthday = match freq, byday, byyearday, bymonth, bymonthday with
    | `Yearly, None, None, None, None ->
      let (_, m, d) = s_date in
      Some [ m ], Some [ d ]
    | `Yearly, None, None, Some _, None ->
      let (_, _, d) = s_date in
      bymonth, Some [ d ]
    | _ -> bymonth, bymonthday
  in
  let bymonthday = match freq, byday, bymonthday, byyearday with
    | `Monthly, None, None, None -> let (_, _, d) = s_date in Some [ d ]
    | _ -> bymonthday
  in
  let byday = match freq, byday with
    | `Weekly, None -> Some [ (0, weekday s_date) ]
    | _ -> byday
  in
  (bymonth, byweekno, byyearday, bymonthday, byday)

(* create correct main generator *)
(* TODO timezone is not applied yet *)
let new_gen start recurrence =
  let (freq, count_or_until, interval, recurs) = recurrence in
  let filters = add_missing_filters recurs freq start
  and bysetpos = find_opt (function `Bysetposday x -> Some x | _ -> None) recurs
  and wkst = find_opt (function `Weekday x -> Some x | _ -> None) recurs in
  let wkst = match wkst with None -> `Monday | Some x -> x in

  let gen_event = init_rr start freq interval filters bysetpos wkst in
  match count_or_until with
    | Some (`Count n) ->
      let gen_count = init_count n gen_event in
      (fun () -> next_count gen_count)
    | Some (`Until (`Utc ts)) -> (* TODO `Until (`Local ts)! *)
      let gen_until = init_until ts gen_event in
      (fun () -> next_until gen_until)
    | _ ->
      (fun () -> Some (next_rr gen_event))
