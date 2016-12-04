let strip_control_chars str = 
  let rec to_list n = 
    if n = String.length str then []
    else (String.get str n)::(to_list (n+1)) in
  let is_c_char x = (int_of_char x >= 32) in
  let lst = to_list 0 |> List.filter is_c_char in
  let rec join = function
    | [] -> ""
    | h::t -> (String.make 1 h)^(join t) in
  join lst

