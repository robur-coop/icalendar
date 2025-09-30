## v0.1.12 (2025-09-30)

* vjournal support (#19 by @RyanGibb, fixes #18)

## v0.1.11 (2025-04-06)

* handle recurrence ids (#13 @Khady @hannesm, tested by @RyanGibb)
* recurrence: handle until with local date
  (reported by @RyanGibb in #15, fixed #17 @hannesm)
* relax todo and event parsers to allow properties after alarms
  (reported by @RyanGibb in #14, fixed #16 @hannesm)
* relax display alarm parser to allow an alarm without description
  (reported by @RyanGibb in #14, fixed #16 @hannesm)
* fix registration of exdate test (#13 @Khady)

## v0.1.10 (2025-03-04)

* handle EXDATE in recur_event (#12 @EmileTrotignon)

## v0.1.9 (2024-11-19)

* BUGFIX: allow empty text values (e.g. in DESCRIPTION), as observed by
  "iCal import/export" (Android app) (#11 @hannesm)

## v0.1.8 (2024-03-14)

* BUGFIX: exception dates are now comma-separated (reported #9, fixed #10 by
  @monomon)

## v0.1.7 (2022-10-23)

* Fix yearly reoccuring events without any special expansions (bymonth/byday/..)
  (reported by @tobixen in robur-coop/caldav#28, fixed #8 by @hannesm)

## v0.1.6 (2022-06-20)

* Improve performance by 40x when computing occurrences of timezones and other
  recurrences (investigated by @rand00 #7)

## v0.1.5 (2021-11-02)

* Drop astring, rresult, stdlib-shims dependencies
* Require OCaml 4.08 as lower bound

## v0.1.4 (2020-08-03)

* Adapt to angstrom 0.14.0 API change #6

## v0.1.3 (2019-11-10)

* Allow PRODID (and other calprops) to be after the components in a calendar
  CalDavZAP uses such ics - and now interoperability works #5

## v0.1.2 (2019-06-30)

* Escape special characters in text values when generating ics string representation
* Iana_param/X_param constructors take a string (pair of strings) as parameter
  (Iana_param of string -> param_value list icalparameter), instead of as value
  (Iana_param of string * param_value list icalparameter).
  (Iana_param "X-FOO", "MY VALUE") and
  (Iana_param "X-BAR", "MY OTHER VALUE") are now distinct in a Param_map

## v0.1.1 (2019-05-27)

* Adapt to gmap 0.3.0 API changes.

## v0.1.0 (2018-11-12)

* initial release.
