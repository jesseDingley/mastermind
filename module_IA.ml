(*Algorithmes de recherche de code *)

#use "module_code.ml";;

(* #use "module_arg.ml";; *)
(* open Arg;; *)

(* open Code;; *)

module IA :
  sig
    (** Nombre d ' algorithmes developpes *)
    val nombre_methodes : int

    (** Choisit un code a proposer
      * @param methode 0 pour l ' algorithme naif,
      *                1 pour l ' algorithme de KNUTH
      *                ... et ainsi de suite
      * @param essais la liste des codes deja proposes
      * @param possibles la liste des codes possibles
      * @return le prochain code a essayer
      *)
    val choix : int -> Code.t list -> Code.t list -> Code.t

    (** Filtre les codes possibles
      * @param methode 0 pour l'algorithme naif,
      *                1 pour l'algorithme de KNUTH
      *                ... et ainsi de suite
      * @param (code, rep) le code essaye et la reponse correspondante
      * @param la liste de courante de codes possibles
      * @return la nouvelle liste de codes possibles
      *)
     val filtre : int -> (Code.t * (int * int) option) -> Code.t list -> Code.t list

  (*  val ft_nb_iteration_toute_reponse : Code.t -> Code.t list -> (int * int) option list ->((int * int) option * int) list

    val ft_garde_code_iter_max : Code.t list -> (int * int) option list -> Code.t list -> Code.t *)
   (*val ft_algo_naif_joue_partie :  Code.t list -> Code.t list -> int -> Code.t -> Code.t (* Code.t list * Code.t list *) 
   val ft_algo_knuth_joue_partie : Code.t list -> Code.t list -> int -> Code.t -> int -> Code.t*)
  end =

struct
	let nombre_methodes = 1;;





	let ft_type_of_option opt = 
		    match opt with
		    | Some x -> x
		    | None -> raise (Invalid_argument "Option.get")
		              (*Option.get = fonction de base*)
		    ;;

	(** option_of_type
	  * transforme qqchose en type option
	  * @author Périé Mathieu
	  * @param a 'a
	  * @return option a 
	*)
	
	let ft_option_of_type a  =
	    Some a 		
			;;



let filtre methode (code, rep) lcode_possible  =
		List.filter (fun x -> Code.reponse x code = rep) lcode_possible
		;;


(** ft_max
	  * retourne la valeur max d'une liste
	  * @author Périé Mathieu
	  * @param liste liste de int
	  * @return un int
	*)


let ft_max liste  =
			match liste with
			| [] -> 0
			| _ ->  (List.fold_left (fun acc x  ->  if (x > acc ) then x else acc) (List.hd liste)  liste)
			;;	

(** ft_min
	  * détermine la valeur min d'une liste et le code associé à ce minimum
	  * @author Périé Mathieu
	  * @param liste liste de couple (code, int)
	  * @param possibles liste de code possibles
	  * @return un couple code int
	*)

let ft_min liste possibles =
			match liste with
			| [] -> (List.hd liste)
			| _ -> List.fold_left (fun acc x  ->  if (((snd x) < (snd acc)) || ((snd x) = (snd acc) && List.mem (fst x) possibles && not (List.mem (fst acc) possibles))) then x else acc) (List.hd liste) liste
			;;	

(** poids
	  * Pour un code détermine pour chaque réponse son poid
	  * @author Périé Mathieu
	  * @param code de type t
	  * @param lcode_possible liste de code possible
	  * @return une liste de int
	*)

let poids code lcode_possible = 
	ft_max (List.fold_left(fun acc rep_atuel -> [List.length(filtre 1 (code, ft_option_of_type rep_atuel) lcode_possible)] @ acc) [] Code.toutes_reponses);;

(** poids
	  * Pour les codes pas encore jouer détermine son poid maximal
	  * @author Périé Mathieu
	  * @param lcode_possible liste de code possible
	  * @param essais liste de code deja jouer
	  * @return une liste de couple de code et de int
	*)

let poids_max lcode_possible essais = 
	List.fold_left(fun acc code_actuel-> [(code_actuel, poids code_actuel lcode_possible)] @ acc ) [] (List.filter(fun x -> not ((List.mem x) essais)) Code.tous) (* lcode_possible *) (*(List.filter(fun x -> not ((List.mem x) essais)) Code.tous)*);;

(** poids
	  * détermine le code dont le poid maximal est inférieur au poids maximal des autres code et le choisit comme prochain code à jouer
	  * @author Périé Mathieu
	  * @param lcode_possible liste de code possible
	  * @param essais liste de code deja jouer
	  * @return un code
	*)

let min_poids_max lcode_possible essais =
	fst (ft_min (poids_max lcode_possible essais) lcode_possible);;


let choix methode essais possibles =
		match methode with
		| 0 -> List.hd possibles
		| 1 -> min_poids_max possibles essais
		| _ -> List.hd possibles
		
			;;


end;;


(* ///////////////////////////////////////////////////////////              premiere version de l'ia             ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	(**_nb_iteration_toute_reponse
	  * pour chaque reponse associé à un code donne le nombre de fois que le code reviens
	  * @author Périé Mathieu
	  * @param code Code.t
	  * @param possibles Code.t list
	  * @param tous toute les reponse possible
	  * @return renvoie une liste de reponse avec le nombre de fois pour chaque reponse qu'elle existe
	*)
	
	let ft_nb_iteration_toute_reponse code possibles tous = (* [ft_option_of_type (0,0),2] *)
		(* Printf.printf(" ft_nb_iteration_toute_reponse \n\n"); *)
		let liste_rep_iter = (List.fold_left(fun a b -> [(b,0)] @ a ) [] tous) in
		List.fold_left(fun acc possibles_actuel -> 
					   (List.map(fun iter_actuel -> if ((Code.reponse code possibles_actuel) = (fst iter_actuel)) then (fst iter_actuel, snd iter_actuel +1 ) else(fst iter_actuel, snd iter_actuel)) liste_rep_iter) 
					   @ acc
					  ) [] possibles
		;;

	(** code_rep_iter
	  * associe pour chaque code toute ses reponse et le nombre de fois que reviens chaque reponse
	  * @author Périé Mathieu
	  * @param possibles Code.t list
	  * @param toute_rep (int * int) list
	  * @param essais la liste des codes deja proposes
	  * @return retourne une liste de code et de reponse associe et filtre les codes déja utiliser
	*)
	
	let ft_code_rep_iter possibles toute_rep essais =
			let tout_code_sans_essais = List.filter(fun x -> not ((List.mem x) essais)) Code.tous in
			List.fold_left(fun acc x -> acc @ [(x, (ft_nb_iteration_toute_reponse x possibles toute_rep))]) [] tout_code_sans_essais
			(* List.fold_left(fun acc x -> acc @ [(x, (List.filter (fun x -> snd(x)<>0 ) (ft_nb_iteration_toute_reponse x possibles toute_rep)))]) [] tout_code_sans_essais *)(* (Code.tous) *)(* liste *)
			;;


	(** min_liste
	  * description
	  * @author Périé Mathieu
	  * @param liste  type_param
	  * @return description de ce qui est retourner
	*)
	
	let ft_min_liste liste =
			match liste with
			| [] -> None
			| _ -> Some (List.fold_left (fun acc x  -> ( if snd x > snd acc then x else acc)) (List.hd liste)  liste)
			;;
		

	(** max_liste
	  * description
	  * @author Périé Mathieu
	  * @param liste  type_param
	  * @return description de ce qui est retourner
	*)
	
	let ft_max_liste liste possibles =
			match liste with
			| [] -> None
			| _ -> Some (List.fold_left (fun acc x  -> ( if  (snd (snd x) < snd (snd acc) || ((snd (snd x) = snd (snd acc)) && List.mem (fst x) possibles && not (List.mem (fst acc) possibles))) then x else acc)) (List.hd liste)  liste)
			;;	
			
	(** garde_iter_min
	  * conserve pour chaque code la reponse qui a le minimum d'iteration
	  * @author Périé Mathieu
	  * @param possibles  Code.t list
	  * @param toute_rep  (int * int) list
	  * @param essais la liste des codes deja proposes
	  * @return une liste avec des codes une reponse associe et le nombre de fois que le code obtient cette reponse.
	*)
	
	let ft_garde_iter_min possibles toute_rep essais =
			(* Printf.printf(" garde_iter_min"); *)
			List.fold_left(fun acc x -> 
				
			[(fst x, (ft_type_of_option(ft_min_liste (snd x))	))]

			@ acc)

			[] (ft_code_rep_iter possibles toute_rep essais)
			;;


	(** garde_code_iter_max
	  * apres avoir la reponse avec le minimum d'iteration on garde le code qui a le plus grand minimum
	  * @author Périé Mathieu
	  * @param possibles  Code.t list
	  * @param toute_rep  (int * int) list
	  * @param essais la liste des codes deja proposes
	  * @return un code a essayer
	*)
	
	let ft_garde_code_iter_max possibles toute_rep essais =
			(* Printf.printf(" garde_code_iter_max"); *)
			(* let liste_iter_min = (ft_garde_iter_min possibles toute_rep essais) in
			List.fold_left(fun acc x -> 
				
			(fst (ft_type_of_option(ft_max_liste (x) possibles ) ))	)

			(fst (List.hd liste_iter_min)) 
			[liste_iter_min] *)
			(fst (ft_type_of_option(ft_max_liste (ft_garde_iter_min possibles toute_rep essais) possibles ) ))
			;;


*)