exception  Left_missing;;
exception Right_missing;;

let rec iteri i f = function
  | [] -> ()
  | a::l -> f i a; iteri (i + 1) f l
;;

let string_of_list l =
  let n = List.length l in
  let s = String.make n ' ' in
  iteri 0 (fun i a -> Bytes.set s i a) l;
  s
;;

let new_stack_list stack list c = match stack,list,c with
  |0,'*'::l,')' -> raise Left_missing
  |n,'*'::l,')' -> (n-1,l,"")
  |n,'('::l,'*' -> (n+1,l,"")
  |n,l,'*'
  |n,l,'('
  |n,l,')'      -> (n,c::l,"")
  |n,l,_        -> (n,[],string_of_list (List.rev (c::l)))
;;

let rec uncomment stack list in_channel out_channel =
  try
    let c = input_char in_channel in
    let (new_stack, new_list, string) = new_stack_list stack list c in
    if new_stack = 0 then output_string out_channel string else ();
    uncomment new_stack new_list in_channel out_channel
  with End_of_file     -> stack
;;

(* Renvoie un entier correspondant au nombre de paranthèses ouvrantes moins
 * le nombres de parenthèses fermantes, ou -1 s'il y a plus de fermantes.
 *)
let uncomment_channel in_channel out_channel =
  try uncomment 0 [] in_channel out_channel with _ -> -1
;;
