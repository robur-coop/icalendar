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
