open Icalendar

let empty = Params.empty

let to_ptime date time =
  match Ptime.of_date_time (date, (time, 0)) with
  | None -> Alcotest.fail "invalid date time"
  | Some p -> p

let singleton k v = Params.add k v empty

let test_serialize_calendar () =
  let expected =
    {|BEGIN:VCALENDAR
VERSION:2.0
CALSCALE:GREGORIAN
BEGIN:VEVENT
CREATED:20191109T223039Z
LAST-MODIFIED:20191109T223039Z
DTSTAMP:20191109T223039Z
UID:ulre32v9-29gj-5k2h-me3x-eu4d4l7d316y
SUMMARY:djlkfjklsfs
TRANSP:OPAQUE
CLASS:PUBLIC
DTSTART;VALUE=DATE:20191104
DTEND;VALUE=DATE:20191105
EXDATE:20240301T000000Z,20240303T000000Z,20240305T000000Z
END:VEVENT
PRODID:-//Inf-IT//CalDavZAP 0.13.1//EN
END:VCALENDAR
|}
  in
  let input =
    let event = {
      uid = (empty, "ulre32v9-29gj-5k2h-me3x-eu4d4l7d316y");
      dtstamp = (empty, to_ptime (2019, 11, 09) (22, 30, 39));
      dtstart = (singleton Valuetype `Date , `Date (2019, 11, 04));
      dtend_or_duration =
        Some (`Dtend (singleton Valuetype `Date, `Date (2019, 11, 05)));
      rrule = None;
      props =
        [`Created (empty, to_ptime (2019, 11, 09) (22, 30, 39));
         `Lastmod (empty, to_ptime (2019, 11, 09) (22, 30, 39));
         `Summary (empty, "djlkfjklsfs");
         `Transparency (empty, `Opaque);
         `Exdate (empty, `Datetimes
                    [`Utc (to_ptime (2024, 03, 01) (00, 00, 00));
                     `Utc (to_ptime (2024, 03, 03) (00, 00, 00));
                     `Utc (to_ptime (2024, 03, 05) (00, 00, 00));
                    ]);
         `Class (empty, `Public) ];
      alarms = [] }
    in
    (to_ics ~cr:false ( [ `Version (empty, "2.0") ;
                          `Calscale (empty, "GREGORIAN");
                          `Prodid (empty, "-//Inf-IT//CalDavZAP 0.13.1//EN") ],
                        [ `Event event ]))
  in
  let sort_lines x = (String.split_on_char '\n' x) |> List.sort compare
  in
  print_endline input;
  Alcotest.(check (list string)) "test serialization"
    (sort_lines expected)
    (sort_lines input)

let test_single_exception () =
  let expected =
    {|BEGIN:VCALENDAR
VERSION:2.0
CALSCALE:GREGORIAN
BEGIN:VEVENT
CREATED:20191109T223039Z
LAST-MODIFIED:20191109T223039Z
DTSTAMP:20191109T223039Z
UID:ulre32v9-29gj-5k2h-me3x-eu4d4l7d316y
SUMMARY:djlkfjklsfs
TRANSP:OPAQUE
CLASS:PUBLIC
DTSTART;VALUE=DATE:20191104
DTEND;VALUE=DATE:20191105
EXDATE:20240301T000000Z
END:VEVENT
PRODID:-//Inf-IT//CalDavZAP 0.13.1//EN
END:VCALENDAR
|}

  in
  let input =
    let event = {
      uid = (empty, "ulre32v9-29gj-5k2h-me3x-eu4d4l7d316y");
      dtstamp = (empty, to_ptime (2019, 11, 09) (22, 30, 39));
      dtstart = (singleton Valuetype `Date , `Date (2019, 11, 04));
      dtend_or_duration =
        Some (`Dtend (singleton Valuetype `Date, `Date (2019, 11, 05)));
      rrule = None;
      props =
        [`Created (empty, to_ptime (2019, 11, 09) (22, 30, 39));
         `Lastmod (empty, to_ptime (2019, 11, 09) (22, 30, 39));
         `Summary (empty, "djlkfjklsfs");
         `Transparency (empty, `Opaque);
         `Exdate (empty, `Datetimes
                    [`Utc (to_ptime (2024, 03, 01) (00, 00, 00));
                    ]);
         `Class (empty, `Public) ];
      alarms = [] }
    in
    (to_ics ~cr:false ( [ `Version (empty, "2.0") ;
                          `Calscale (empty, "GREGORIAN");
                          `Prodid (empty, "-//Inf-IT//CalDavZAP 0.13.1//EN") ],
                        [ `Event event ]))
  in
  let sort_lines x = (String.split_on_char '\n' x) |> List.sort compare
  in
  print_endline input;
  Alcotest.(check (list string)) "test serializing with single exception"
    (sort_lines expected)
    (sort_lines input)


let tests = [
  "Write entire calendar correctly with exceptions", `Quick, test_serialize_calendar ;
  "Write entire calendar correctly with single exception", `Quick, test_single_exception ;
]
