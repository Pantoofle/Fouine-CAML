open Types;;

let name_check expr =
  let id = fun x -> x
  and is_name = function
    |Name s       -> Name s
    |Underscore s -> failwith
       ("'" ^ s ^ "' n'est pas un nom valide, il est intedit de commencer par '_'.")
    |Reserved s -> Reserved s
  in
  try let _ = Convert.replace_expr expr id id (is_name) id in () with x -> raise x
;;
