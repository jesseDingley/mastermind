#use "module_code.ml";;
#use "module_IA.ml";;
#use "module_UI.ml";;

module Mastermind :
	sig

		(** matermind
		  * fonction mastermind "main" du programme
		  * @author Périé Mathieu
		  * @param nom_joueur  type string
		  * @param nb_tentative_par_partie int
		  * @param nb_partie int
		  * @param rep_auto booleen
		  * @return unit
		*)
		
 		val mastermind : string -> int -> int -> bool -> unit
	end = 
	struct
		(** try_patie
		  * verifie que le nombre d'essais par parite a un sens
		  * @author Périé Mathieu
		  * @param nb_try int
		  * @return le nombre de tentative par partie
		*)
		
		let ft_try_partie nb_try =
				match nb_try with
				| a when a <= 0 -> 10
				| a when a >= 17 -> 17
				| _ -> nb_try
				;;

		(** nb_patie
		  * verifie que le nombre de partie a un sens et est paire sinon renvoie un nombre qui vérifie ces conditions
		  * @author Périé Mathieu
		  * @param nb_partie int
		  * @return un int qui représene un nombre de partie
		*)
		
		let ft_nb_partie nb_partie =
				match nb_partie with
				| a when a <= 0 -> 2
				| a when a mod 2 <> 0 -> a+1
				| _ -> nb_partie
				;;

		(* let ft_interface = 10;;
		let ft_nom_joueur = "moi";; *)


		(** matermind
		  * fonction mastermind "main" du programme
		  * @author Périé Mathieu
		  * @param nom_joueur  type string
		  * @param nb_tentative_par_partie int
		  * @param nb_partie int
		  * @param rep_auto booleen
		  * @return unit
		*)
		
		let mastermind nom_joueur nb_tentative_par_partie nb_partie rep_auto =
				UI.ft_jeu nom_joueur (ft_try_partie nb_tentative_par_partie) (ft_nb_partie nb_partie) rep_auto
				;;

	
	end;;


(* open Mastermind;; 
Mastermind.mastermind "moi" 3 5 false; *)