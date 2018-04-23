open Angstrom

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

let parse (str:string) = parse_string contentlines str
