open Angstrom

let vevent = 
{__|BEGIN:VCALENDAR
PRODID:-//Example Corp.//CalDAV Client//EN
VERSION:2.0
BEGIN:VEVENT
UID:1@example.com
SUMMARY:One-off Meeting
DTSTAMP:20041210T183904Z
DTSTART:20041207T120000Z
DTEND:20041207T130000Z
END:VEVENT
BEGIN:VEVENT
UID:2@example.com
SUMMARY:Weekly Meeting
DTSTAMP:20041210T183838Z
DTSTART:20041206T120000Z
DTEND:20041206T130000Z
RRULE:FREQ=WEEKLY
END:VEVENT
BEGIN:VEVENT
UID:2@example.com
SUMMARY:Weekly Meeting
RECURRENCE-ID:20041213T120000Z
DTSTAMP:20041210T183838Z
DTSTART:20041213T130000Z
DTEND:20041213T140000Z
END:VEVENT
END:VCALENDAR|__}

let collect_param key value = (key, value)
let collect_contentline key params value = (key, params, value) 

let is_alpha_digit_minus = 
  function | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '-' -> true | _ -> false
let name = take_while1 is_alpha_digit_minus
let param_name = name
let is_control = function '\x00' .. '\x08' | '\x0a' .. '\x1f' | '\x7f' -> true | _ -> false
let is_qsafe_char = function x when is_control x -> false | '"' -> false | _ -> true
let quoted_string = char '"' *> take_while is_qsafe_char <* char '"'
let is_safe_char = function x when is_control x -> false | '"' | ';' | ':' | ',' -> false | _ -> true
let param_text = take_while is_safe_char
let param_value = param_text <|> quoted_string
let param = lift2 collect_param param_name (char '=' *> param_value)

let crlf = string "\n" <|> string "\r\n"
let value = take_while (fun x -> not (is_control x)) (*in fact it is more complicated*)
let contentline = lift3 collect_contentline name (many ( char ';' *> param )) (char ':' *> value <* crlf) 
let contentlines = many contentline

let string_of_triple (a, b, c) = "(" ^ a ^ ", " ^ String.concat "; " (List.map (fun (x, y) -> x ^ " -> " ^ y) b) ^ "," ^ c ^ ")"

let eval (str:string) =
  match parse_string contentlines str with
  | Result.Ok v      -> v
  | Result.Error msg -> failwith msg

let () = Printf.printf "Result %s\n" @@ String.concat ";" @@ List.map string_of_triple @@ eval vevent (*Sys.argv.(1)*)
