#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () = Pkg.describe "icalendar" @@ fun ctx ->
    Ok [
      Pkg.mllib ~api:["Icalendar"] "src/icalendar.mllib";
      Pkg.test "test/test";
    ]
