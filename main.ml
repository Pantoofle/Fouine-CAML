(* Références globales : *)
let debug       = ref 0
let traduction  = ref 0
let input       = ref "NULL"
let exec        = Sys.argv.(0)
let interm_file = ref "NULL"
let interm      = ref false
let machine     = ref false
let interprete  = ref true
let help        = ref false

(* Affiche l'aide *)
let print_help () =
  begin
    print_string "Utilisation : ";
    print_string exec;
    print_string " [OPTIONS] <fichier contenant le code>\n";
    print_string " -d ou --debug         : Afficher BEAUCOUP d'information. Rend l'execution TRES verbeuse\n";
    print_string " -v ou --verbose       : Afficher les informations intermediaires. Rend l'execution verbeuse\n";
    print_string " -E                    : Applique la transformation Fouine + Exceptions vers Fouine avant d'appeler l'interprete et/ou la machine\n";
    print_string " -R                    : Applique la transformation Fouine + Recursion  vers Fouine avant d'appeler l'interprete et/ou la machine\n";
    print_string " -ER                   : Applique les deux transformations précédente avant d'appeler l'interprete et/ou la machine\n";
    print_string " -m ou --machine       : Passe l'execution à la machine à pile\n";
    print_string " -i ou --interm <file> : Compile le programme et le stocke dans file sous forme de liste de lexems\n";
    print_string " -a ou --all           : Appele l'interprete ET la machine à pile ET la version ZINC sur l'entrée\n";
    print_string " Par défaut, appelle l'interprete sur l'entrée\n"
  end
;;

(* Lit les argument passé à ./fouine et modifie les références globales en conséquences *)
let rec read_args i =
  let key = try Sys.argv.(i) with _ -> "NULL" in
  match key with
  | "--debug"    | "-d" -> debug := 2; read_args (i+1)
  | "--verbose"  | "-v" -> debug := 1; read_args (i+1)
  | "-E"                -> traduction := 1; read_args (i+1)
  | "-R"                -> traduction := 2; read_args (i+1)
  | "-ER"               -> traduction := 3; read_args (i+1)
  | "--machine"  | "-m" -> machine := true; interprete := false; read_args (i+1)
  | "--interm"   | "-i" -> begin interm := true; interprete := false;
				 interm_file := try Sys.argv.(i+1) with
						  _ ->  help := true;  "NULL";
			   end;
			   read_args (i+2)
  | "--all"      | "-a" -> machine := true; read_args (i+1) 
  | "NULL"             -> ();
  | _                  -> input := key; read_args (i+1)
;;

(* Fonction auxilaire d'affichage de saut à ligne *)
let rec at_least n l = match n,l with
  |0,_    -> true
  |_,[]   -> false
  |_,x::s -> if x then at_least (n-1) s else at_least n s
;;

(* Prend une in_channel commentée, et renvoie le programme parsé correspondant *)
let prog_of_commented_in_channel in_channel =
  let out_channel = open_out "/tmp/fouine_uncomment.fou.ml" in
  let n = Uncomment.uncomment_channel in_channel out_channel in
  let () = close_out out_channel in
  if n>0 then raise Uncomment.Right_missing
  else if n<0 then raise Uncomment.Left_missing else ();
  let in_channel = open_in "/tmp/fouine_uncomment.fou.ml" in
  let p = Convert.prog_of_in_channel in_channel in
  close_in in_channel;
  Sys.remove "/tmp/fouine_uncomment.fou.ml";
  p
;;

(* Prend une expression et renvoie du feedback selon les valeurs des références globales *)
let rec print_feedback expr =
  begin
    Types.print_expr expr; print_newline();
    if (!traduction != 0) then begin try let e = Convert.transformation (!traduction) expr in
					 let t = !traduction in
					 traduction := 0;
					 print_feedback e;
					 traduction := t; 
				     with | Failure x -> print_string x; print_newline ()
			       end;
    if (!debug != 0) then begin try Types.print_feedback expr
				with | Failure x -> print_string x; print_newline ()
			  end;
    if (!interprete) then begin try (Interprete.print_feedback) expr
				with | Failure x -> print_string x; print_newline ()
			  end;
    if (!machine)    then begin try (Zinc_machine.print_feedback (!debug)) expr; 
				with | Failure x -> print_string x; print_newline ()
			  end;
    if (!interm)     then begin try (Zinc_machine.compile_to_file (!debug) (!interm_file)) expr;
				with | Failure x -> print_string x; print_newline ()
			  end;
  end;
  if at_least 3 [(!debug != 0); !interprete; !machine; !interprete; (!traduction!=0)]
  then print_newline ()
;;

let _ =
  read_args 1;
  if (!input = "NULL") then print_help () else  
    let file = open_in (!input) in
    try
    if (!help) then print_help ()
    else
	  begin
	    let result = prog_of_commented_in_channel file in
	    (* On supprime le fichier destination si on va ecrire dedans *)
	    begin if (!interm) then try Sys.remove (!interm_file) with _ -> () end;
	  Types.prog_map Name_checker.name_check result;
	  Types.prog_map print_feedback result;
	  close_in file;
	  flush stdout;
	end
    with
    |Failure x ->  close_in file; failwith x
    |e -> close_in file; raise e 
;;
