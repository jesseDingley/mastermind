(** Module de definition d'un code dans le jeu Mastermind *)
module Code:
	sig
		(** Le type d'un pion *)
		type pion =  int(* A COMPLETER *)
		
		(** Le type d'un code*)
		type t = pion list
		
		(** Nombre de pions par code *)
		val nombre_pions : int
		
		(** Liste des couleurs possibles *)
		val couleurs_possibles : pion list
		
		(** Compare deux codes
		 * @param code1 premier code a comparer
		 * @param code2 second  code a comparer
		 * @return 0 si les deux codes sont identiques,
			un entier positif si [code1] est strictement plus grand que [code2]
			un entier negatif si [code1] est strictement plus petit que [code2]
		 *)
		val compare : t -> t -> int
		
		(** Conversion code vers chaine de caracteres (pour affichage)
		  * @param code code a convertir
		  * @return la representation en chaine de caracteres de [code]
		  *)
 		val string_of_code : t ->string
		
		(** Conversion chaine de caracteres vers code (pour saisie)
		  * @param string chaine de caractere saisie
		  * @return le code correspondant a la saisie si la conversion est possible
				[None] si la conversion n'est pas possible
		 *)
		val code_of_string : string -> t option
		
		(** La liste de tous les codes permis *)
		 val tous : t list 
		
		(** La liste de toutes les reponses possibles *)
		 val toutes_reponses : (int*int)list;;  
		
		(** Calcule la reponse d'un code par rapport au code cache
		  * @param      code le code propose
		  * @param vrai_code le code cache
		  * @return un couple (nombre de pions bien places, nombre de pions mal places)
			[None] si la reponse ne peut etre calculee

		  *)
		val reponse : t -> t -> (int*int) option
 
	end =
	struct
		type pion =  int(*A COMPLETER*)
		
		(**Le type d'un code*)
		type t = pion list

		type type_choix = Couleur | Forme ;;

		let choix = Couleur;;
		
		let nombre_pions = 4;;

		let couleurs_possibles = [0; 1; 2; 3; 4; 5];;

		let couleurs_affichables  = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p"];;
		let formes_affichables = ["q";"r";"s";"t";"u";"v";"w";"x";"y";"z";"=";"+";"@";"&";"*";"!"];;


        let rec compare code_1 code_2 =
                match (code_1,code_2) with
                | ([],[])         -> 0
                | (code_1,code_2) when (List.length code_1 <> List.length code_2) -> failwith "erreur"
                | (_,[]) -> failwith "erreur"
                | ([],_) -> failwith "erreur"
                | (t1::q1,t2::q2) -> if (t1 > t2) then
                                          1
                                     else 
                                          if (t1 < t2) then
                                               -1
                                          else compare q1 q2
                ;;

	    (** string_of_elt_forme
		  * Conversion elt (cas formes) vers chaine de caracteres (pour affichage)
		  * @author Dingley Jesse						
		  * @param elt  int	
		  * @return la representation en chaine de caracteres de elt (cas formes)
		  *)

		let rec ft_string_of_eltI elt liste_affichables i =
			    match elt with
		        | a when a = (List.nth couleurs_possibles i) -> List.nth liste_affichables i 
		        | _ -> ft_string_of_eltI elt liste_affichables (i+1)
	           ;;


        (** ft_string_of_elt
		  * Conversion elt vers chaine de caracteres (pour affichage)
		  * @author Dingley Jesse						
		  * @param elt  int	
		  * @return la representation en chaine de caracteres de elt
		  *)

        let ft_string_of_elt elt = 
            match choix with
            | Couleur -> ft_string_of_eltI elt couleurs_affichables 0
            | Forme   -> ft_string_of_eltI elt formes_affichables 0
           ;;


        (** string_of_code
		  * Conversion code vers chaine de caracteres (pour saisie)
		  * @author Dingley Jesse
		  * @param code t
		  * @param choix char
		  * @return la representation en chaine de caracteres de [code]
		  *)

		let string_of_code code  =
		    " "^(List.fold_left (fun acc elt -> acc ^ ((* " "^ *)(ft_string_of_elt elt)(* ^" " *)) ) "" code)^" "  
		        ;;


		let rec ft_code_of_eltI elt liste_affichable i = 
		        match elt with 
                | a when a = (List.nth liste_affichable i).[0] -> List.nth couleurs_possibles i 
                | _ -> ft_code_of_eltI elt liste_affichable (i+1)   
                ;;

        (** code_of_elt
		  * Conversion elt  vers int / char (type pion)
		  * @author Dingley Jesse
		  * @param elt char
		  * @return le int / char correspondant a l'elt
		  *)

        let ft_code_of_elt elt = 
            match choix with
            | Couleur -> ft_code_of_eltI elt couleurs_affichables 0
            | Forme -> ft_code_of_eltI elt formes_affichables 0


		(** code_of_stringI
		  * Conversion chaine de caracteres vers code
		  * @author Dingley Jesse
		  * @param saisie string  
		  * @return le code correspondant a la saisie
		  *)

		let ft_code_of_stringI saisie =
			let rec ft_code_of_stringACC saisie i acc =  
			    match saisie.[i] with
		        | _ when String.length saisie - 1  = i -> Some(List.rev acc) 	    
				| ' ' -> ft_code_of_stringACC saisie (i+1) acc
		    	| _ -> ft_code_of_stringACC saisie (i+1) (ft_code_of_elt saisie.[i]::acc)
			in ft_code_of_stringACC saisie 0 [] 
					    ;;


	    (** contient
		  * verifie si une liste est incluse dans une autre
		  * @author Dingley Jesse
		  * @param liste_1 'a list
		  * @param liste_2 'a list
		  * @return bool
		  *)

		let ft_contient liste_1 liste_2  = 
		    List.for_all (fun x -> List.mem x liste_2 ) liste_1
		    ;;
		  

		(** convertit_string_vers_liste
		  * Conversion string en string list
		  * @author Dingley Jesse
		  * @param str string  
		  * @return string list
		  *)

		let ft_convertit_string_vers_liste str =
			let rec ft_convertit_string_vers_listeACC str i acc = 
			        match str.[i] with 
			        | _ when String.length str - 1 = i -> List.rev acc
			        | ' ' -> ft_convertit_string_vers_listeACC str (i+1) acc 
			        | _ -> ft_convertit_string_vers_listeACC str (i+1) (Char.escaped str.[i]::acc)
			in ft_convertit_string_vers_listeACC str 0 []
			;; 
       
        
		(** verif_conversion_possible
		  * verifie si la conversion est possible
		  * @author Dingley Jesse
		  * @param saisie string
		  * @param liste_affichable string list
		  * @return bool
		  *)

        let ft_verif_conversion_possible saisie liste_affichable =
		    ft_contient (ft_convertit_string_vers_liste saisie) liste_affichable
		    ;; 


		(** code_of_string
		  * Conversion chaine de caracteres vers code (pour saisie)	
		  * @author Dingley Jesse
		  * @param saisie  string
		  * @return le code correspondant a la saisie si la conversion est possible
				    [None] si la conversion n'est pas possible
		  *)

		let code_of_string saisie =
		    match choix with
		    | Couleur -> if ft_verif_conversion_possible saisie couleurs_affichables then ft_code_of_stringI saisie else None
		    | Forme -> if ft_verif_conversion_possible saisie formes_affichables then ft_code_of_stringI saisie else None
		    ;;


		(**nombre_boule_noire
		  * calcule le nombre de boules avec la bonne couleur au bon endroit
		  * @author Maillot Victor
		  * @param code_proposition  t
		  * @param code_cache  t
		  * @return int | calcule le nombre de boules avec la bonne couleur au bon endroit
		  *)

		let ft_nombre_boule_noire code_proposition code_cache =
          let rec nombre_boule_noire_acc code_proposition code_cache acc =
		    match code_proposition, code_cache with
		    | [],[] -> acc
		    | [],_ -> failwith "Les deux codes sont de tailles differentes, le code cache est plus grand"
		    |  _,[] -> failwith "Les deux codes sont de tailles differentes, le code propose est plus grand"
		    | tp::qp,tc::qc  when  tp = tc  -> nombre_boule_noire_acc qp qc (acc+1)
		    | _::qp,_::qc  -> nombre_boule_noire_acc qp qc acc
		  in nombre_boule_noire_acc code_proposition code_cache 0
		;;

		        (** min
		  * renvoie le minimum entre deux nombres
		  * @author Maillot Victor
		  * @param a int
		  * @param b int
		  * @return int | le minimum
		*)

		let ft_min a =
			if fst a > snd a then snd a else fst a
		;;



		(** nombre_occurences_boule
		  * calcule le nombre de fois ou une boule du code proposition appartient au code proposition puis au code cache
		  * @author Maillot Victor
		  * @param code_proposition t
		  * @param code_cache t
		  * @param boule pion
		  * @return int*int
		*)

		let ft_nombre_occurences_boule code_proposition code_cache boule =
		   (List.fold_left (fun acc el -> if boule = el then acc+1 else acc) 0 code_proposition,
		    List.fold_left (fun acc el -> if boule = el then acc+1 else acc) 0 code_cache)     
		 ;;




		(** somme_min_nombre_occurences_boule
		  * renvoie la somme des min pour chaque couleur du code proposition
		  * @author Maillot Victor
		  * @param occu_proposition int
		  * @param occu_cache int
		  * @return int | somme pour chaque couleur du code proposition
		*)

		let ft_somme_min_nombre_occurences_boule code_proposition code_cache =
			let rec ft_somme_min_nombre_occurences_bouleAcc couleurs_possibles acc =
				match couleurs_possibles with
				| [] -> acc
				| t::q -> ft_somme_min_nombre_occurences_bouleAcc q (acc + ft_min (ft_nombre_occurences_boule code_proposition code_cache t))
			in ft_somme_min_nombre_occurences_bouleAcc couleurs_possibles 0	 
		;;


		(* au lieu de rappeler la fonction tp dans l'incrementation de l'acc il faut le faire avec seulement une couleur unique dans la liste de couleurs possibles voir doc
		idee: refaire la fonction acc avec la liste des couelurs en parametre puis passé au suivant a chaque appel *)
		   


		(**reponse 
		  * description Calcule la reponse d'un code propose par rapport au code cache
		  * @author Maillot Victor
		  * @param nom_param  t
		  * @param nom_param  t
		  * @return (int*int) option | reponse d'un code propose par rapport au code cache
		*)
				
		let reponse code_proposition code_cache =
			match code_proposition, code_cache with
			| [],[] -> None
			| p,c when List.length p = List.length c -> Some(ft_nombre_boule_noire code_proposition code_cache,
			                    ft_somme_min_nombre_occurences_boule code_proposition code_cache - ft_nombre_boule_noire code_proposition code_cache)
			| _,_ -> None
				        
		;;

		(** combinaisons
		  * calcule la liste de toutes les combinaisons possibles de la liste de couleurs pour n pions
		  * @author Dingley Jesse
		  * @param nb_pions int
		  * @param liste_clrs pion list
		  * @return liste des combinasions (type t list)
		*)

		let rec ft_combinaisons nb_pions liste_clrs = 
		        match nb_pions with
		        | 0 -> [[]]
		        | _ -> List.concat (List.map (fun elt_comb -> List.map (fun elt_liste -> elt_liste@elt_comb) liste_clrs) (ft_combinaisons (nb_pions-1) liste_clrs))
		    ;;


		(** La liste de tous les codes permis *)

		let tous = ft_combinaisons nombre_pions (List.map (fun elt -> [elt]) couleurs_possibles);;


		(** type_of_option
		  * convertit un type option en type de base	
		  * @author Dingley Jesse
		  * @param opt Option
		  * @return la represenation de opt en type de base
		*)

		let ft_type_of_option opt = 
		    match opt with
		    | Some x -> x
		    | None -> raise (Invalid_argument "Option.get")
		              (*Option.get = fonction de base*)
		    ;;


		(** supprime_un_elt
		  * supprime un element de la liste
		  * @author Dingley Jesse
		  * @param liste 'a list
		  * @param elt 'a
		  * @return la nouvelle liste sans elt
		*)

		let rec ft_supprime_un_elt  liste elt = 
		    match liste with
		     | [] -> []
		     | t::q -> if elt = t then q else t::ft_supprime_un_elt q elt
		    ;;

		(** supprime_doublons
		  * supprime un doublon d'une liste
		  * @author Dingley Jesse
		  * @param liste 'a list
		  * @return la nouvelle liste sans le doublon 
		*)

		let ft_supprime_doublon liste =
		    let rec supprime_doublonACC liste acc = 
		            match liste with
		            | []   -> List.rev acc
		            | t::q -> supprime_doublonACC (ft_supprime_un_elt q t) (t::acc)
		    in supprime_doublonACC liste []
		    ;;



		(** suppr_tous_doublons
		  * supprime tous doublon d'une liste
		  * @author Dingley Jesse
		  * @param liste 'a list
		  * @return la nouvelle liste sans les doublons 
		*)

		let rec ft_suppr_tous_doublons liste =
			    match liste with
			    | _ when liste = ft_supprime_doublon liste -> liste
			    | _ -> ft_supprime_doublon (ft_suppr_tous_doublons (ft_supprime_doublon liste))
			;;

		let genere_code_hasard nb_pions = 
			let rec genere_code_hasardACC nb_pions acc = 
			        match acc with
			        | _ when List.length acc = nb_pions -> acc
			        | _ -> let i = Random.int (List.length couleurs_possibles) in genere_code_hasardACC nb_pions ([List.nth couleurs_possibles i]@acc) 
			in genere_code_hasardACC nombre_pions [] 
		    ;;

	    let code_hasard = genere_code_hasard nombre_pions;;


		(** La liste de toutes les reponses possibles *)

		let toutes_reponses =  ft_suppr_tous_doublons (List.map (fun code -> ft_type_of_option (reponse code code_hasard)) tous) ;;





			end;;

(* open Code;;

tous;;

code_of_string " cbc ";;
string_of_code [0;1;1];; *)