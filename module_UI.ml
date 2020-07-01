#use "module_code.ml";;
#use "module_IA.ml";;
(* #use "mastermind.ml";; *)
(* #use "module_IA.ml";; *)

open Code;;
open IA;;
(* open Mastermind;; *)
(* open IA;; *)

#directory "+threads";;

#load "unix.cma";;
#load "threads.cma";;
#load "graphics.cma";;

open Graphics ;;
open Code;;
open Thread;;

module UI = struct

  exception End ;;  


  (** ouvrir_interface
    * ouvre la fenetre du jeu
    * @author Dingley Jesse
    * @param taille string
    * @param titre string
    * @return la fenetre de l'interface
      *)
          
  let ft_ouvrir_interface taille titre = 
    open_graph taille; (* " 1280x900" *)
    set_window_title titre (* "Mastermind" *)
    ;; 



   (*definition des couleurs : *)

   (*couleurs supplementaires pour les pions*)

  let rose                    = rgb 250 200 250 ;; (*rose*)
  let violet                  = rgb 143 65 240 ;; (*violet*) 
  let gris                    = rgb 200 200 200 ;; (*gris*) 
  let marron                  = rgb 150 40 40 ;; (*marron*) 
  let dore                    = rgb 210 210 0 ;; (*or*) 
  let orange                  = rgb 250 210 90 ;; (*orange*)
  let vert_sombre_moyen_moyen = rgb 160 160 10 ;; (*vert sombre moyen moyen*)  
  let bleu_nul                = rgb 100 100 230 ;; (*bleu nul*)  
  let vert_clair_moche        = rgb 140 250 140 ;; (*vert clair pas terrible*) 


  let c1 = rgb 240 240 240;;
  let c2 = rgb 240 240 241;;
  let c3 = rgb 240 241 240;;
  let c4 = rgb 241 240 240;;
  let c5 = rgb 239 239 239;;
  let c6 = rgb 239 240 240;;
  let c7 = rgb 241 241 241;;
  let c8 = rgb 240 240 239;;
  let c9 = rgb 240 239 240;;
  let c10 = rgb 241 241 240;;
  let c11 = rgb 240 241 241;;
  let c12 = rgb 241 240 241;;
  let c13 = rgb 239 240 239;;
  let c14 = rgb 239 239 240;;
  let c15 = rgb 240 239 239;;
  let c16 = rgb 242 240 240;;
  let c17 = rgb 240 242 240;;

  (* let cf1 = rgb 255 166 35 ;;
  let cf2 = rgb 255 57 35 ;;
  let cf3 = rgb 119 181 8;;
  let cf4 = rgb 119 181 220;;
  let cf5 = rgb 252 130 220;;
  let cf6 = rgb 252 21 254;;
  let cf7 = rgb 147 156 141;;
  let cf8 = rgb 7 40 242;;
  let cf9 = rgb 7 250 242;;
  let cf10 = rgb 255 0 0;;
  let cf11 = rgb 0 255 0;;
  let cf12 = rgb 85 69 24;;
  let cf13 = rgb 4 98 188;;
  let cf14 = rgb 104 27 5;;
  let cf15 = rgb 31 91 14;;
  let cf15 = rgb 42 183 136;;


  (** forme1
  * dessine la forme1 aux coordonnees x y (le centre du carre)
  * @author Maillot Victor
  * @param x int
  * @param y int
  * @return unit
*)

let ft_forme1 x y =
    set_color (rgb 255 166 35) ;
    draw_circle x y 20;
    moveto (x-20) y;
    lineto (x+20) y;
    set_color (rgb 1 1 1);
    plot x y

        ;;

(** forme2
  * dessine la forme2 aux coordonnees x y (le centre du carre)
  * @author Maillot Victor
  * @param x int
  * @param y int
  * @return unit
*)

let ft_forme2 x y =
    set_color (rgb 255 57 35) ;
    draw_circle x y 20;
    moveto x (y-20);
    lineto x (y+20);
    set_color (rgb 2 2 2);
    plot x y   
        ;;

(** forme3
  * dessine la forme3 aux coordonnees x y (le centre du carre)
  * @author Maillot Victor
  * @param x int
  * @param y int
  * @return unit
*)

let ft_forme3 x y =
    set_color (rgb 119 181 8) ;
    draw_circle x y 20;
    moveto (x-20) y;
    lineto (x+20) y;
    moveto x (y-20);
    lineto x (y+20);
    set_color (rgb 3 3 3);
    plot x y    
        ;;

(** forme4
  * dessine la forme4 aux coordonnees x y (le centre du carre)
  * @author Maillot Victor
  * @param x int
  * @param y int
  * @return unit
*)

let ft_forme4 x y =
    set_color (rgb 119 181 220) ;
    draw_rect (x-20) (y-20) 40 40;
    moveto x (y-20);
    lineto x (y+20);
    set_color (rgb 4 4 4);
    plot x y   
        ;;

let ft_forme5 x y =
    set_color (rgb 252 130 220) ;
    draw_rect (x-20) (y-20) 40 40;
    moveto (x-20) y;
    lineto (x+20) y;
    set_color (rgb 5 5 5);
    plot x y   
        ;;

let ft_forme6 x y =
    set_color (rgb 252 21 254) ;
    draw_rect (x-20) (y-20) 40 40;
    moveto (x-20) y;
    lineto (x+20) y;
    moveto x (y-20);
    lineto x (y+20);
    set_color (rgb 6 6 6);
    plot x y  
        ;;
let ft_forme7 x y =
    set_color (rgb 147 156 141) ;
    draw_rect (x-20) (y-20) 40 40;
    moveto (x-20) (y-20);
    lineto (x+20) (y+20);
    set_color (rgb 7 7 7);
    plot x y   
        ;;

let ft_forme8 x y =
    set_color (rgb 7 40 242) ;
    draw_rect (x-20) (y-20) 40 40;
    moveto (x-20) (y+20);
    lineto (x+20) (y-20);
    set_color (rgb 8 8 8);
    plot x y   
        ;;               

let ft_forme9 x y =
    set_color (rgb 7 250 242) ;
    draw_rect (x-20) (y-20) 40 40;
    moveto (x-20) (y-20);
    lineto (x+20) (y+20);
    moveto (x-20) (y+20);
    lineto (x+20) (y-20);
    set_color (rgb 9 9 9);
    plot x y
        ;;

let ft_forme10 x y =
    set_color (rgb 255 0 0) ;
    draw_rect (x-20) (y-20) 40 40;
    draw_circle x y 16;
    set_color (rgb 10 10 10);
    plot x y   
        ;;

let ft_forme11 x y =
    set_color (rgb 0 255 0) ;
    draw_circle x y 20;
    moveto (x-14) (y-14);
    lineto (x+14) (y+14);
    set_color (rgb 11 11 11);
    plot x y   
        ;;

let ft_forme12 x y =
    set_color (rgb 85 69 24) ;
    draw_circle x y 20;
    moveto (x-14) (y+14);
    lineto (x+14) (y-14);
    set_color (rgb 12 12 12);
    plot x y   
        ;;

let ft_forme13 x y =
    set_color (rgb 4 98 188) ;
    draw_circle x y 20;
    draw_circle x y 8;
    set_color (rgb 13 13 13);
    plot x y
;;       

let ft_forme14 x y =
    set_color (rgb 104 27 5) ;
    draw_circle x y 20;
    draw_rect (x-8) (y-8) 16 16;
    set_color (rgb 14 14 14);
    plot x y
;;

let ft_forme15 x y =
    set_color (rgb 31 91 14) ;
    draw_circle x y 20;
    draw_ellipse x y 20 10;
    set_color (rgb 15 15 15);
    plot x y
;;

let ft_forme16 x y =
    set_color (rgb 42 183 136) ;
    draw_circle x y 20;
    draw_ellipse x y 10 20;
    set_color (rgb 16 16 16);
    plot x y
;;  *)


  let couleur_arriere_plan = 
      rgb 220 220 220 ;;


  let couleur_derriere_mastermind =
      rgb 200 200 200 ;;


  let couleur_rand = 
      rgb (Random.int(255)) (Random.int(255)) (Random.int(255)) ;;


  (** remplir_de_clr
    * remplie le fond de la fenetre / logo mastermind avec une couleur
    * @author Dingley Jesse
    * @param couleur color
    * @return les fonds colores (unit)
    *)

  let ft_remplir_de_clr couleur =
      set_color couleur;
      match couleur with
      | clr when (clr = couleur_arriere_plan) -> fill_rect 0 0 1280 900 
      | clr when (clr = couleur_derriere_mastermind) -> fill_rect 250 800 770 200 
      ;;


  (** remplir_complement
      * remplie les cotes du logo mastermind avec une couleur
      * @author Dingley Jesse
      * @param couleur color
      * @return des bandes colores (unit)
      *)

  let ft_remplir_complement couleur = 
      set_color couleur;
        fill_rect 0 900 250 100 ;
        fill_rect 1020 900 260 100 
        ;;

       
  (** dessine_carre
    * dessine un carre pour le cas des pions de type couleurs
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return un carre (unit)
    *)
    
  let ft_dessine_carre x y  = 
      fill_rect x y 45 45 
      ;;


  let ft_draw_arrow x y = 
      moveto x y;
      set_line_width 4;
      set_color red;
      lineto (x+80) y;
      moveto (x+80) y;
      lineto ((x+80)-30) (y+20);
      moveto (x+80) y;
      lineto ((x+80)-30) (y-20)
      ;;


  let ft_efface_arrow x y = 
      set_color couleur_arriere_plan;
      fill_rect (x-5) (y-25) 90 50
      ;;


  (** affiche_mastermind
    * affiche le logo mastermind en haut
    * @author Dingley Jesse
    * @param font string
    * @return logo mastermind (unit)
    *)

  let ft_affiche_mastermind font = (*font = string*)

      moveto 400 830;
      set_color red ;
      set_font font;
      draw_char 'M';

      moveto 450 830;
      set_color blue ;
      draw_char 'A';

      moveto 500 830;
      set_color green ;
      draw_char 'S';

      moveto 550 830;
      set_color cyan ;
      draw_char 'T';

      moveto 600 830;
      set_color magenta ;
      draw_char 'E';

      moveto 650 830;
      set_color green ;
      draw_char 'R';

      moveto 700 830;
      set_color red ;
      draw_char 'M';

      moveto 750 830;
      set_color yellow ;
      draw_char 'I';

      moveto 800 830;
      set_color black ;
      draw_char 'N';

      moveto 850 830;
      set_color red ;
      draw_char 'D'
      ;;


  let ft_affiche_mastermind_debut_partie font = (*font = string*)

      moveto 400 430;
      set_color red ;
      set_font font;
      draw_char 'M';

      moveto 450 430;
      set_color blue ;
      draw_char 'A';

      moveto 500 430;
      set_color green ;
      draw_char 'S';

      moveto 550 430;
      set_color cyan ;
      draw_char 'T';

      moveto 600 430;
      set_color magenta ;
      draw_char 'E';

      moveto 650 430;
      set_color green ;
      draw_char 'R';

      moveto 700 430;
      set_color red ;
      draw_char 'M';

      moveto 750 430;
      set_color yellow ;
      draw_char 'I';

      moveto 800 430;
      set_color black ;
      draw_char 'N';

      moveto 850 430;
      set_color red ;
      draw_char 'D'
      ;;




  (** click_any_key_to_continue
    * permet d'attendre que le joueur appuie sur une touch para continuar
    * @author Dingley Jesse
    *)  

  let ft_click_any_key_to_continue () =
     (*  set_color couleur_arriere_plan; 
      fill_rect 440 390 400 200; *)
      set_line_width 5;
      moveto 500 760;
      set_color gris;
      fill_rect 440 700 420 100;
      set_font "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1";
      set_color black;
      draw_string "Appuyez sur une";
      moveto 450 710;
      draw_string "touche pour continuer";
      set_color red;
      draw_rect 440 700 420 100;
      wait_next_event[Key_pressed]
      ;;


  let ft_click_any_key_to_continue_debut_partie () =
     (*  set_color couleur_arriere_plan; 
      fill_rect 440 390 400 200; *)
      set_line_width 5;
      moveto 500 760;
      set_color gris;
      fill_rect 440 700 420 100;
      set_font "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1";
      set_color black;
      draw_string "Appuyez sur une";
      moveto 450 710;
      draw_string "touche pour demarrer";
      set_color red;
      draw_rect 440 700 420 100;
      ft_affiche_mastermind_debut_partie "-*-fixed-medium-r-semicondensed--80-*-*-*-*-*-iso8859-1";
      wait_next_event[Key_pressed]
      ;;


  let ft_click_any_key_to_continue_fin_partie () = 
      set_line_width 5;
      moveto 500 760;
      set_color gris;
      fill_rect 440 700 420 100;
      set_font "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1";
      set_color black;
      draw_string "Appuyez sur une";
      moveto 450 710;
      draw_string "touche pour quitter";
      set_color red;
      draw_rect 440 700 420 100;
      set_font "-*-fixed-medium-r-semicondensed--24-*-*-*-*-*-iso8859-1";
      set_color black;
      moveto 400 100;
      draw_string "Cree par J. Dingley, M. Perie, V. Maillot";
      wait_next_event[Key_pressed]
      ;;



  let ft_fermer_interface () =
      ft_click_any_key_to_continue_fin_partie ();
      close_graph ()
      ;;




  (** affiche_joueur
   * affiche le nom du joueur
   * @author Dingley Jesse
   * @param joueur string
   * @return nom joueur (unit)
   *)    

  let ft_affiche_joueur joueur = 
      moveto 800 650;
      set_color black ;
      set_font "-*-fixed-medium-r-semicondensed--24-*-*-*-*-*-iso8859-1";
      draw_string "Joueur : ";
      set_color red;
      draw_string joueur
      ;;


  (** affiche_nb_tentatives
    * affiche le nombre de tentatives pour la partie
    * @author Dingley Jesse
    * @param nb_tenetatives string
    * @return nombre de tentatives (unit)
    *)   

  let ft_affiche_nb_tentatives nb_tentatives = 
      moveto 800 600;
      set_color black ;
      set_font "-*-fixed-medium-r-semicondensed--24-*-*-*-*-*-iso8859-1";
      draw_string "Nombre de tentatives : ";
      set_color red;
      draw_string (string_of_int nb_tentatives)
      ;;

  
  (** affiche_nb_parties
    * affiche le nombre de parties
    * @author Dingley Jesse
    * @param nb_parties int
    * @return nombre de parties (unit)
    *)   

  let ft_affiche_nb_parties nb_parties = 
    moveto 800 550;
    set_color black ;
    set_font "-*-fixed-medium-r-semicondensed--24-*-*-*-*-*-iso8859-1";
    draw_string "Nombre de parties : ";
    set_color red;
    draw_string (string_of_int nb_parties)
    ;;

  
  (** affiche_mode
    * affiche le mode du jeu
    * @author Dingley Jesse
    * @param partie int
    * @return mode du jeu (unit)
    *)  

  let ft_affiche_mode partie = 
      moveto 800 500;
      set_color black;
      set_font "-*-fixed-medium-r-semicondensed--24-*-*-*-*-*-iso8859-1";
      draw_string "Mode : ";
      set_color red;
      match partie with 
      | _ when (partie mod 2 = 0) -> draw_string "IA"
      | _ -> draw_string "Classique"
      ;;


  (** affiche_legende
    * affiche la legende du resultat
    * @param couleur color
    * @return legende (unit)
    *)

  let ft_affiche_legende couleur = 
      set_color couleur;
      moveto 800 12;
      draw_string "Legende : [bonne place | mauvaise place]"
      ;;


  (** affiche_numero_partie
    * affiche le numero de la partie en cours
    * @author Dingley Jesse
    * @param partie int
    * @return numero partie (unit)
  *)
  
  let ft_affiche_numero_partie partie  =
      moveto 10 810;
      set_color blue;
      set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
      draw_string "Partie ";
      draw_string (string_of_int partie);
      set_font "-*-fixed-medium-r-semicondensed--24-*-*-*-*-*-iso8859-1"
      ;;


  let ft_affiche_score score = 
      moveto 10 860;
      set_color blue;
      set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
      draw_string "Score : ";
      draw_string (string_of_int score);
      set_font "-*-fixed-medium-r-semicondensed--24-*-*-*-*-*-iso8859-1"
      ;;

  
  (** trace_ligne_param
    * trace la ligne noire a cote des parametres
    * @author Dingley Jesse
    * @param couleur color
    * @return ligne noire (unit)
    *)  

  let ft_trace_ligne_param couleur =
      set_line_width 5;
      set_color couleur;
      moveto 780 670;
      lineto 780 500
      ;;

    
    (*trace le tableau de jeu : *)


  (** trace_une_colonne
    * trace une colonne du tableau de jeu 
    * @param n (numero du pion) int
    * @param nb_tentatives int
    * @return une colonne (unit)
    *) 

  let ft_trace_une_colonne n nb_tentatives =
      moveto (10+(n*50)) 10; lineto (10+(n*50)) ((nb_tentatives*50)+10)
      ;; (*ce ;; netait pas la avant*)


    (** trace_colonnes  
      * trace toutes les colonnes du tableau de jeu
      * @author Dingley Jesse
      * @param nb_tours (nombre de tours du boucle for) int
      * @param nb_tentatives int
      * @return colonnes (unit)
      *)

  let rec trace_colonnes nb_tours nb_tentatives=
          match nb_tours with
          | _ when nb_tours = nombre_pions -> ft_trace_une_colonne nombre_pions nb_tentatives
          | _ -> ft_trace_une_colonne nb_tours nb_tentatives; trace_colonnes (nb_tours+1) nb_tentatives
            ;;


  (** trace_colonnes_finale  
    * trace toutes les colonnes du tableau de jeu joli
    * @author Dingley Jesse
    * @param n (nombre de tours du boucle for) int
    * @param nb_tentatives int
    * @return colonnes jolis (unit)
    *)

  let ft_trace_colonnes_finale n nb_tentatives=
      set_line_width 5;
      set_color black ; 
      trace_colonnes n nb_tentatives
      ;;

  
  (** trace_une_ligne 
    * trace une ligne du tableau de jeu
    * @author Dingley Jesse
    * @param n (numero du tour de boucle for) int
    * @return ligne (unit)
    *)

  let ft_trace_une_ligne n =
      moveto 10 (10+(n*50)); lineto ((nombre_pions*50)+10) (10+(n*50)) 
      ;;

  

  (** trace_lignes 
    * trace toutes les lignes du tableau de jeu
    * @author Dingley Jesse
    * @param nb_tours (nombre de tours du boucle for) int
    * @param nb_tentatives int
    * @return lignes(unit)
    *)

  let rec trace_lignes nb_tours nb_tentatives = 
          match nb_tours with
          | _ when nb_tours = nb_tentatives -> ft_trace_une_ligne nb_tours 
          | _ -> ft_trace_une_ligne nb_tours ; trace_lignes (nb_tours+1) nb_tentatives
          ;;


  
  (** trace_lignes_finale  
    * trace toutes les lignes du tableau de jeu joli
    * @author Dingley Jesse
    * @param n (nombre de tours du boucle for) int
    * @param nb_tentatives int
    * @return lignes jolis (unit)
    *)

  let ft_trace_lignes_finale n nb_tentatives =
      set_line_width 5;
      set_color black ; 
      trace_lignes n nb_tentatives
      ;;


  let ft_affiche_perdu font = (*font = string*)    (*vous avez perdu 
                                                     car vous avez rentré 
                                                     une reponse incorrecte *)


                                                     (*
                                                      vous avez perdu 
                                                      car vous n'avez pas 
                                                      deviné le code
                                                     *)


                                                     (*
                                                       l'IA a perdu
                                                     *)

                                                     (*
                                                      vous avez gagne car
                                                      vous avez devine le code
                                                     *)
 
       moveto 400 530;
       set_color red ;
       set_font font;
       draw_char 'P';
 
       moveto 450 530;
       set_color blue ;
       draw_char 'E';
 
       moveto 500 530;
       set_color green ;
       draw_char 'R';
 
       moveto 550 530;
       set_color cyan ;
       draw_char 'D';
 
       moveto 600 530;
       set_color magenta ;
       draw_char 'U'

       ;;

  let ft_affiche_perdu_rep_incorrecte font =
      set_font font;
      moveto 400 630;
      set_color red;
      draw_string "Reponse incorrecte";
      moveto 400 580;
      draw_string "Vous avez perdu"
      ;;


  let ft_affiche_perdu_pas_devine font = 
      set_font font;
      moveto 400 630;
      set_color red;
      draw_string "Vous avez perdu";
      moveto 400 580;
      draw_string "car vous n'avez pas";
      moveto 400 530;
      draw_string "devine le code secret"
      ;;


   let ft_affiche_perdu_pas_devine font =
      set_font font; 
      moveto 400 530;
      set_color red;
      draw_string "l'IA a perdu"
      ;;


   let ft_affiche_gagne_car_joueur_devine_code font = 
       moveto 400 630;
       set_font font;
       set_color red;
       draw_string "Vous avez gagne";
       moveto 400 580;
       draw_string "car vous avez";
       moveto 400 530;
       draw_string "devine le code"
       ;;

      let ft_affiche_IA_gagne font = 
       moveto 400 530;
       set_font font;
       set_color red;
       draw_string "l'IA a gagne"
   ;;



  let ft_affiche_gagnee font = 

      moveto 400 530;
      set_color red ;
      set_font font;
      draw_char 'G';

      moveto 450 530;
      set_color blue ;
      draw_char 'A';

      moveto 500 530;
      set_color green ;
      draw_char 'G';

      moveto 550 530;
      set_color cyan ;
      draw_char 'N';

      moveto 600 530;
      set_color magenta ;
      draw_char 'E'
      ;;

  
  (** verif_gagne
   * affiche mesg_gagne si la partie est gagnee
   * @author Dingley Jesse
   * @param code t 
   * @param code_cache t
   * @return mesg de victoire ou pas (unit)
   *)

  let ft_verif_gagne code code_cache =
      match compare code code_cache with
      | 0 -> set_color blue; 
             set_font "-*-fixed-medium-r-semicondensed--60-*-*-*-*-*-iso8859-1";
             ft_affiche_gagnee "-*-fixed-medium-r-semicondensed--60-*-*-*-*-*-iso8859-1"  
      | _ -> failwith "not won"
      ;;


    (*fcts jeu*)


  (** couleurUI_of_couleur_affichable
    * transforme une couleur affichable en carre pour l'interface
    * @author Dingley Jesse
    * @param couleur color
    * @param x int
    * @param y int
    * @return carre du couleur correspondant (unit)
    *)

  let ft_couleurUI_of_couleur_affichable couleur x y  = 
      match couleur with
      | "a" -> set_color red; ft_dessine_carre x y 
      | "b" -> set_color green; ft_dessine_carre x y 
      | "c" -> set_color blue; ft_dessine_carre x y 
      | "d" -> set_color yellow; ft_dessine_carre x y 
      | "e" -> set_color cyan; ft_dessine_carre x y 
      | "f" -> set_color magenta; ft_dessine_carre x y 
      | "g" -> set_color black; ft_dessine_carre x y 
      | "h" -> set_color rose; ft_dessine_carre x y 
      | "i" -> set_color violet; ft_dessine_carre x y 
      | "j" -> set_color gris; ft_dessine_carre x y 
      | "k" -> set_color marron; ft_dessine_carre x y 
      | "l" -> set_color dore; ft_dessine_carre x y 
      | "m" -> set_color orange; ft_dessine_carre x y 
      | "n" -> set_color vert_sombre_moyen_moyen; ft_dessine_carre x y 
      | "o" -> set_color bleu_nul; ft_dessine_carre x y 
      | "p" -> set_color vert_clair_moche; ft_dessine_carre x y  
          ;;

 

 (*   let ft_formeUI_of_formes_affichable forme x y  = 
      match couleur with
      | "q" -> ft_forme1 x y 
      | "r" ->  ft_forme2 x y 
      | "s" ->  ft_forme3 x y 
      | "t" -> ft_forme4 x y 
      | "u" ->  ft_forme5 x y 
      | "v" -> ft_forme6 x y 
      | "w" ->  ft_forme7 x y 
      | "x" ->  ft_forme8 x y 
      | "y" ->  ft_forme9 x y 
      | "z" ->  ft_forme10 x y 
      | "=" -> ft_forme11 x y 
      | "+" ->  ft_forme12 x y 
      | "@" -> ft_forme13 x y 
      | "&" ->  ft_forme14 x y 
      | "*" ->  ft_forme15 x y 
      | "!" -> ft_forme16 x y  
          ;;
 *)



      (*FAUT MODIFIER ft_dessine_carre (AJOUTE PARAM TAILLE) *)
      (*Je ne suis pas sur*)


  let couleurs_affichables  = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p"];;


     
  (** affiche_mesg_choisir_couleurs
    * affiche mesg "choisir couleurs"
    * @author Dingley Jesse
    * @param couleur color
    * @return mesg (unit)
    *)

  let ft_affiche_mesg_choisir_couleurs couleur = 
      moveto 800 400; 
      set_color couleur;
      set_font "-*-fixed-medium-r-semicondensed--24-*-*-*-*-*-iso8859-1";
      draw_string "Choisir couleurs : "
      ;;
(* 
 let ft_affiche_mesg_choisir_formes couleur = 
      moveto 800 400;
      set_color couleur;
      set_font "-*-fixed-medium-r-semicondensed--24-*-*-*-*-*-iso8859-1";
      draw_string "Choisir formes : "
 *)

  (** trace_ligne_choisir_couleurs
    * trace lignea cote du mesg choisir couleurs
    * @author Dingley Jesse
    * @param couleur color
    * @return ligne (unit)
    *)

  let ft_trace_ligne_choisir_couleurs couleur = 
      set_color couleur;
      moveto 780 420;
      match couleurs_possibles with
      | _ when (List.length couleurs_possibles) <= 7 -> lineto 780 300
      | _ -> lineto 780 250
    ;;

      
  (** affiche_une_couleur_cliquable
    * affiche une couleur cliquable (en bas du mesg choisir couleurs)
    * @author Dingley Jesse
    * @param n (numero du couleur) int
    * @return couleur cliquable (carre) (unit)
    *)

  let ft_affiche_une_couleur_cliquable n = 
      match n with 
      | _ when n <= 7 -> moveto (800 + n*50) 300 ; ft_couleurUI_of_couleur_affichable (List.nth couleurs_affichables n)  (800 + n*50) 300
      | _ -> moveto (800 + (n-8)*50) 250 ; ft_couleurUI_of_couleur_affichable (List.nth couleurs_affichables n)  (800 + (n-8)*50) 250
      ;;


  (** affiche_couleurs_possibles
    * affiche tous les couleurs cliquables
    * @author Dingley Jesse
    * @param nb_tours (boucle for) int
    * @return couleurs cliquables (carres) (unit)
    *)

  let rec ft_affiche_couleurs_possibles nb_tours = 
          match nb_tours with
          | _ when nb_tours = (List.length couleurs_possibles) - 1 -> ft_affiche_une_couleur_cliquable (List.nth couleurs_possibles nb_tours)
          | _ -> ft_affiche_une_couleur_cliquable nb_tours ; ft_affiche_couleurs_possibles (nb_tours+1) 
          ;;


   (** affiche_une_couleur_cliquable
    * affiche une couleur cliquable (en bas du mesg choisir couleurs)
    * @author Dingley Jesse
    * @param n (numero du couleur) int
    * @return couleur cliquable (carre) (unit)
    *)

(*   let ft_affiche_une_forme_cliquable n = 
      match n with 
      | _ when n <= 7 -> moveto (822 + n*50) 322 ; ft_formeUI_of_formes_affichable (List.nth formes_affichables n)  (822 + n*50) 322
      | _ -> moveto (822 + (n-8)*50) 272 ; ft_formeUI_of_formes_affichable (List.nth formes_affichables n)  (822 + (n-8)*50) 272
      ;; *)


  (** affiche_couleurs_possibles
    * affiche tous les couleurs cliquables
    * @author Dingley Jesse
    * @param nb_tours (boucle for) int
    * @return couleurs cliquables (carres) (unit)
    *)

(*   let rec ft_affiche_formes_possibles nb_tours = 
          match nb_tours with
          | _ when nb_tours = (List.length couleurs_possibles) - 1 -> ft_affiche_une_forme_cliquable (List.nth couleurs_possibles nb_tours)
          | _ -> ft_affiche_une_forme_cliquable nb_tours ; ft_affiche_couleurs_possibles (nb_tours+1) 
          ;; *)

  (** affiche_elt_apres_insertion
    * affiche un elt du code choisi dans le tableau
    * @author Dingley Jesse
    * @param nieme (pion) int
    * @param tentative int
    * @param n (boucle) int
    * @return couleur dans le tableau (carres) (unit)
    *)

  let ft_affiche_elt_apres_insertion nieme tentative n = 
      try 
          moveto (12+(n*50)) (12+(tentative*50));
          ft_couleurUI_of_couleur_affichable (List.nth couleurs_affichables nieme) (12+(n*50)) (12+(tentative*50));
      with 
      | End -> raise End
      ;;






  (** affiche_elt_apres_insertion
    * affiche un elt du code choisi dans le tableau
    * @author Dingley Jesse
    * @param nieme (pion) int
    * @param tentative int
    * @param n (boucle) int
    * @return couleur dans le tableau (carres) (unit)
    *)

(*   let ft_affiche_elt_apres_insertion_formes nieme tentative n = 
      try 
          moveto (34+(n*50)) (34+(tentative*50));
          ft_formeUI_of_formes_affichable (List.nth formes_affichables nieme) (34+(n*50)) (34+(tentative*50));
      with 
      | End -> raise End
      ;;



 *)



  (* les 16 fonction suivantes sont les conditions du souris pour cliquer sur les couleurs : *)


  (** cond_encadrement_cliquer_1
    * condtion pour pion 1
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_1 x y = 
      (800 <= x && x <= 845) && (300 <= y && y <= 345)
        ;;


  (** cond_encadrement_cliquer_2
    * condtion pour pion 2
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_2 x y = 
      (850 <= x && x <= 895) && (300 <= y && y <= 345)
        ;;


  (** cond_encadrement_cliquer_3
    * condtion pour pion 3
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_3 x y = 
      (900 <= x && x <= 945) && (300 <= y && y <= 345)
        ;;


  (** cond_encadrement_cliquer_4
    * condtion pour pion 4
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_4 x y = 
      (950 <= x && x <= 995) && (300 <= y && y <= 345)
        ;;


  (** cond_encadrement_cliquer_5
    * condtion pour pion 5
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_5 x y = 
      (1000 <= x && x <= 1045) && (300 <= y && y <= 345)
        ;;


  (** cond_encadrement_cliquer_6
    * condtion pour pion 6
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_6 x y = 
      (1050 <= x && x <= 1095) && (300 <= y && y <= 345)
        ;;


  (** cond_encadrement_cliquer_7
    * condtion pour pion 7
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_7 x y = 
      (1100 <= x && x <= 1145) && (300 <= y && y <= 345)
        ;;


  (** cond_encadrement_cliquer_8
    * condtion pour pion 8
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_8 x y = 
      (1150 <= x && x <= 1195) && (300 <= y && y <= 345)
        ;;


  (** cond_encadrement_cliquer_9
    * condtion pour pion 9
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_9 x y = 
      (800 <= x && x <= 845) && (250 <= y && y <= 295)
        ;;


  (** cond_encadrement_cliquer_10
    * condtion pour pion 10
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_10 x y = 
      (850 <= x && x <= 895) && (250 <= y && y <= 295)
        ;;


  (** cond_encadrement_cliquer_11
    * condtion pour pion 11
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_11 x y = 
      (900 <= x && x <= 945) && (250 <= y && y <= 295)
      ;;


  (** cond_encadrement_cliquer_12
    * condtion pour pion 12
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_12 x y = 
      (950 <= x && x <= 995) && (250 <= y && y <= 295)
      ;;


  (** cond_encadrement_cliquer_13
    * condtion pour pion 13
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_13 x y = 
      (1000 <= x && x <= 1045) && (250 <= y && y <= 295)
      ;;


  (** cond_encadrement_cliquer_14
    * condtion pour pion 14
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_14 x y = 
      (1050 <= x && x <= 1095) && (250 <= y && y <= 295)
      ;;


  (** cond_encadrement_cliquer_15
    * condtion pour pion 15
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_15 x y = 
     (1100 <= x && x <= 1145) && (250 <= y && y <= 295)
     ;;


  (** cond_encadrement_cliquer_16
    * condtion pour pion 16
    * @author Dingley Jesse
    * @param x int
    * @param y int
    * @return (bool)
    *)

  let ft_cond_encadrement_cliquer_16 x y = 
      (1150 <= x && x <= 1195) && (250 <= y && y <= 295)
      ;;


  (** genere_code_hasard 
    * calcule un code aleatoire 
    * @author Dingley Jesse 
    * @param nb_pions int
    * @return un code aleatoire (t)
    *)

  let genere_code_hasard nb_pions =  (*   FCT A REMPLACER PAR RAND DANS TOUS*)
      let rec genere_code_hasardACC nb_pions acc = 
              match acc with
              | _ when List.length acc = nb_pions -> acc
              | _ -> genere_code_hasardACC nb_pions ([List.nth couleurs_possibles (Random.int (List.length couleurs_possibles))]@acc) 
      in genere_code_hasardACC nombre_pions [] 
        ;;




  (** affiche_elt_cc
    * fait la manipulation pour mettre la couleur dans le code cache
    * @author Dingley Jesse
    * @param nieme_clr int
    * @param n (boucle) int
    * @return elt   (unit)
    *)   

  let ft_affiche_elt_cc nieme_clr n = 
      try 
          moveto (800+(n*50)) 40;
          ft_couleurUI_of_couleur_affichable (List.nth couleurs_affichables nieme_clr) (800+(n*50)) 40
      with
      | End -> raise End
      ;;





  (** clique_sur_une_couleur_cc 
    * permet de cliquer sur les couleurs possibles et afficher en bas pour un elt du code cache (cc)
    * @author Dingley Jesse
    * @param opt status 
    * @param n (boucle) int
    * @return elt   (unit)
    *)   

  let rec ft_clique_sur_une_couleur_cc opt n  = 
          match (opt.mouse_x, opt.mouse_y) with 
          | _ when (ft_cond_encadrement_cliquer_1 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 1) -> ft_affiche_elt_cc 0 n 
          | _ when (ft_cond_encadrement_cliquer_2 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 2) -> ft_affiche_elt_cc 1  n
          | _ when (ft_cond_encadrement_cliquer_3 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 3) -> ft_affiche_elt_cc 2  n 
          | _ when (ft_cond_encadrement_cliquer_4 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 4) -> ft_affiche_elt_cc 3  n 
          | _ when (ft_cond_encadrement_cliquer_5 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 5) -> ft_affiche_elt_cc 4  n 
          | _ when (ft_cond_encadrement_cliquer_6 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 6) -> ft_affiche_elt_cc 5  n 
          | _ when (ft_cond_encadrement_cliquer_7 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 7) -> ft_affiche_elt_cc 6  n 
          | _ when (ft_cond_encadrement_cliquer_8 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 8) -> ft_affiche_elt_cc 7 n 
          | _ when (ft_cond_encadrement_cliquer_9 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 9) -> ft_affiche_elt_cc 8  n 
          | _ when (ft_cond_encadrement_cliquer_10 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 10) -> ft_affiche_elt_cc 9  n
          | _ when (ft_cond_encadrement_cliquer_11 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 11) -> ft_affiche_elt_cc 10  n
          | _ when (ft_cond_encadrement_cliquer_12 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 12) -> ft_affiche_elt_cc 11  n
          | _ when (ft_cond_encadrement_cliquer_13 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 13) -> ft_affiche_elt_cc 12  n
          | _ when (ft_cond_encadrement_cliquer_14 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 14) -> ft_affiche_elt_cc 13  n
          | _ when (ft_cond_encadrement_cliquer_15 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 15) -> ft_affiche_elt_cc 14  n 
          | _ when (ft_cond_encadrement_cliquer_16 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles = 16) -> ft_affiche_elt_cc 15  n
          | _ -> ft_clique_sur_une_couleur_cc (wait_next_event [Button_down]) n 
          ;; 



  (** creer_elt_code_cache_a_la_main 
    * cree un elt du code cache par l'utilisateur (et affiche)
    * @author Dingley Jesse
    * @param n (boucle) int
    * @return elt  (unit)
    *)   

  let ft_creer_elt_code_cache_a_la_main n = 
      ft_draw_arrow 700 150;
      moveto 800 150;
      set_color black;
      draw_string "Creation du code cache : ";
      moveto 800 100;
      draw_string "Choisir la couleur "  ; 
      set_color couleur_arriere_plan;
      fill_rect 1020 100 35 35;
      set_color black;
      draw_string (string_of_int (n+1));
      ft_clique_sur_une_couleur_cc (wait_next_event [Button_down])  n;
      ft_efface_arrow 700 150
      ;;


  (** creer_code_cache_a_la_main 
    * cree le code cache par l'utilisateur (et affiche)
    * @author Dingley Jesse
    * @param nb_tours (boucle) int
    * @return code  (unit)
    *)   

  let rec ft_creer_code_cache_a_la_main nb_tours =
          match nb_tours with
          | _ when nb_tours = (nombre_pions-1) -> ft_creer_elt_code_cache_a_la_main nb_tours
          | _ -> ft_creer_elt_code_cache_a_la_main nb_tours; ft_creer_code_cache_a_la_main (nb_tours+1)
          ;; 

        

  (** obtenir_elt_code_cache_a_la_main
    * obtient un element du code cache apres affichage
    * @author Dingley Jesse
    * @param n (boucle) int
    * @return elt int
    *)    

  let ft_obtenir_elt_code_cache_a_la_main n = 
      match point_color (810+n*50) 50 with 
      | 16711680 -> 0
      | 65280 -> 1
      | 255 -> 2
      | 16776960 -> 3
      | 65535 -> 4
      | 16711935 -> 5
      | 0 -> 6
      | 16435450 -> 7
      | 9388528 -> 8
      | 13158600-> 9
      | 9840680 -> 10
      | 13816320 -> 11
      | 16437850 -> 12
      | 10526730 -> 13
      | 6579430 -> 14
      | 9239180 -> 15
      ;;








  (** obtenir_code_cache_a_la_main
    * obtient le code cache apres affichage
    * @author Dingley Jesse
    * @param nb_tours (boucle) int
    * @return code cache (t)
    *)    

  let ft_obtenir_code_cache_a_la_main nb_tours = 
      let rec ft_obtenir_code_cache_a_la_mainACC nb_tours acc =
              match nb_tours with 
              | _ when nb_tours=(nombre_pions) -> acc
              | _ -> [ft_obtenir_elt_code_cache_a_la_main nb_tours]@(ft_obtenir_code_cache_a_la_mainACC (nb_tours+1) acc)
        in ft_obtenir_code_cache_a_la_mainACC nb_tours []
      ;;





  (** creation_code_cache
    * cree le code cache
    * @author Dingley Jesse
    * @param numero_partie int                                  (*coulerus = 1 formes = 0*)
    * @return code cache (t)
    *)
  
  let ft_creation_code_cache numero_partie =
      match numero_partie with
      | _  when (numero_partie mod 2) <> 0  -> genere_code_hasard nombre_pions (*pair : IA*) 
      | _ -> ft_obtenir_code_cache_a_la_main 0
      ;; 



  (** clique_sur_une_couleur
    * permet de cliquer et mettre dans le tableau pour un pion
    * @author Dingley Jesse
    * @param opt status
    * @param tentative int
    * @param n int
    * @return pion dans tableau
    *)

  let rec ft_clique_sur_une_couleur opt tentative n  = 
          match (opt.mouse_x, opt.mouse_y) with 
          | _ when (ft_cond_encadrement_cliquer_1 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 1) -> ft_affiche_elt_apres_insertion 0 tentative n 
          | _ when (ft_cond_encadrement_cliquer_2 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 2) -> ft_affiche_elt_apres_insertion 1 tentative n 
          | _ when (ft_cond_encadrement_cliquer_3 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 3) -> ft_affiche_elt_apres_insertion 2 tentative n 
          | _ when (ft_cond_encadrement_cliquer_4 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 4) -> ft_affiche_elt_apres_insertion 3 tentative n 
          | _ when (ft_cond_encadrement_cliquer_5 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 5) -> ft_affiche_elt_apres_insertion 4 tentative n 
          | _ when (ft_cond_encadrement_cliquer_6 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 6) -> ft_affiche_elt_apres_insertion 5 tentative n 
          | _ when (ft_cond_encadrement_cliquer_7 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 7) -> ft_affiche_elt_apres_insertion 6 tentative n 
          | _ when (ft_cond_encadrement_cliquer_8 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 8) -> ft_affiche_elt_apres_insertion 7 tentative n 
          | _ when (ft_cond_encadrement_cliquer_9 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 9) -> ft_affiche_elt_apres_insertion 8 tentative n 
          | _ when (ft_cond_encadrement_cliquer_10 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 10) -> ft_affiche_elt_apres_insertion 9 tentative n 
          | _ when (ft_cond_encadrement_cliquer_11 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 11) -> ft_affiche_elt_apres_insertion 10 tentative n 
          | _ when (ft_cond_encadrement_cliquer_12 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 12) -> ft_affiche_elt_apres_insertion 11 tentative n 
          | _ when (ft_cond_encadrement_cliquer_13 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 13) -> ft_affiche_elt_apres_insertion 12 tentative n 
          | _ when (ft_cond_encadrement_cliquer_14 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 14) -> ft_affiche_elt_apres_insertion 13 tentative n 
          | _ when (ft_cond_encadrement_cliquer_15 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles >= 15) -> ft_affiche_elt_apres_insertion 14 tentative n 
          | _ when (ft_cond_encadrement_cliquer_16 opt.mouse_x opt.mouse_y) && (List.length couleurs_possibles = 16) -> ft_affiche_elt_apres_insertion 15 tentative n 
          | _ -> ft_clique_sur_une_couleur (wait_next_event [Button_down]) tentative n 
          ;; 




  (** insers_elt_code
    * permet de cliquer et mettre dans le tableau pour un pion avec mesg
    * @author Dingley Jesse
    * @param tentative int
    * @param n int
    * @return pion dans tableau + mesg
    *)

  let ft_insere_elt_code tentative n = 
      ft_draw_arrow 700 200;
      moveto 800 200;
      set_color black;
      draw_string "Choisir la couleur ";
      set_color couleur_arriere_plan;
      fill_rect 1020 200 35 35;
      set_color black;
      draw_string (string_of_int (n+1));
      ft_clique_sur_une_couleur (wait_next_event [Button_down]) tentative n ;
      ft_efface_arrow 700 200;
      ;;


    
  (** insers_elt_code
    * permet de choisir code et mettre dans tableau
    * @author Dingley Jesse
    * @param tentative int
    * @param nb_tours (boucle) int
    * @return pion dans tableau
    *)

  let rec ft_choisir_un_code tentative nb_tours =
          match nb_tours with 
          | _ when nb_tours = (nombre_pions-1) -> ft_insere_elt_code tentative nb_tours
          | _ -> ft_insere_elt_code tentative nb_tours; ft_choisir_un_code tentative (nb_tours+1)
          ;;


  (*deux fonctions qui permettent d'afficher un code donne*)


  (** affiche_elt_code
    * affiche un elt du code donne
    * @author Dingley Jesse
    * @param code t
    * @param tentative int
    * @param nieme_elt int
    * @return nieme elt dans tableau (unit)
    *)

  let ft_affiche_elt_code code tentative nieme_elt =
      moveto (12+(nieme_elt*50)) (12+(tentative*50)); ft_couleurUI_of_couleur_affichable (List.nth code nieme_elt)  (12+(nieme_elt*50)) (12+(tentative*50))
      ;;




  (** affiche_elt_code_cas_perdu
    * affiche un elt du code secret magique donne
    * @author Dingley Jesse
    * @param code t
    * @param n int
    * @return nieme elt au milieu (unit)
    *)

  let ft_affiche_elt_code_cas_perdu code n = 
      moveto (400+(50*n)) 390; ft_couleurUI_of_couleur_affichable (List.nth code n) (400+(50*n)) 390
  ;;



  (** affiche_code_cas_perdu
    * affiche code secret magique
    * @author Dingley Jesse
    * @param code t
    * @param nb_tours (boucle) int
    * @return code (unit)
    *)

  let rec ft_affiche_code_cas_perdu code nb_tours = 
          match nb_tours with
          | _ when nb_tours = ((List.length code)-1) -> ft_affiche_elt_code_cas_perdu code nb_tours
          | _ -> ft_affiche_elt_code_cas_perdu code nb_tours; ft_affiche_code_cas_perdu code (nb_tours+1) 



  (** affiche_code
    * affiche code donne 
    * @author Dingley Jesse
    * @param code t
    * @param tentative int
    * @param nb_tours (boucle) int
    * @return code dans tableau (unit)
    *)

  let rec ft_affiche_code code tentative nb_tours =
          match nb_tours with
          | _ when nb_tours = ((List.length code)-1)  -> ft_affiche_elt_code code tentative nb_tours
          | _ -> ft_affiche_elt_code code tentative nb_tours; ft_affiche_code code tentative (nb_tours+1)
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


  (** string_of_couple
    * transforme un couple en string
    * @author Maillot Victor
    * @param couple  (int,int)
    * @return string
    *)
     
  let ft_string_of_couple couple =
      set_color black;
      match couple with
      | a,b -> "["^string_of_int a^" | "^string_of_int b^"]"     
         ;;
         
         
  (** affiche_reponse
    * retourne la reponse associe au code en parametre
    * @author Maillot Victor
    * @param code t list
    * @param tentative int
    * @return la reponse a cote du code
    *)
  
  let ft_affiche_reponse code tentative numero_partie code_secret =
      moveto (30+((List.length code))*50) (tentative*50 + 25);
      let reponse_associe_au_code = ft_type_of_option (reponse code (code_secret)) in         (*changer [0;1;2;3] avec la variable code_secret !!!!!!*)
       let reponse_string = ft_string_of_couple reponse_associe_au_code in
        draw_string reponse_string     
      ;;


  
  (** obtenir_elt_code_et_reponse
    * obtient un element d'un code  apres affichage dans tableau
    * @author Dingley Jesse
    * @param tentative int
    * @param n (boucle) int
    * @return elt int
    *)   

  let ft_obtenir_elt_code tentative n = 
      match point_color (22+(n*50)) (22+(tentative*50)) with 
      | 16711680 -> 0
      | 65280 -> 1
      | 255 -> 2
      | 16776960 -> 3
      | 65535 -> 4
      | 16711935 -> 5
      | 0 -> 6
      | 16435450 -> 7
      | 9388528 -> 8
      | 13158600-> 9
      | 9840680 -> 10
      | 13816320 -> 11
      | 16437850 -> 12
      | 10526730 -> 13
      | 6579430 -> 14
      | 9239180 -> 15
      ;;


     

  (** obtenir_code_et_reponse
    * obtient le code apres affichage dans tableau
    * @author Dingley Jesse
    * @param tentative int
    * @param nb_tours (boucle) int
    * @return code  (t)
    *)  

  let ft_obtenir_code tentative nb_tours = 
      let rec ft_obtenir_codeACC nb_tours acc = 
              match nb_tours with
              | _ when nb_tours = (nombre_pions) -> acc
              | _ -> [ft_obtenir_elt_code tentative nb_tours]@(ft_obtenir_codeACC (nb_tours+1) acc)
      in ft_obtenir_codeACC nb_tours []
      ;; 







  (** affiche_reponse_finale
    * affiche reponse apres avoir affiche code dans tableau
    * @author Dingley Jesse
    * @param tentative int
    * @return reponse du code (unit)
    *) 

  let ft_affiche_reponse_finale tentative numero_partie code_secret = 
      ft_affiche_reponse (ft_obtenir_code tentative 0) tentative numero_partie code_secret
      ;;


  (** affiche_code_secret_apres_avoir_perdu
    * affiche code secret joli 
    * @author Dingley Jesse
    *) 

  let ft_affiche_code_secret_apres_avoir_perdu code_secret = 
      moveto 400 480;
      set_color black;
      set_font "-*-fixed-medium-r-semicondensed--24-*-*-*-*-*-iso8859-1";
      draw_string "Code secret : ";
      ft_affiche_code_cas_perdu (ft_convertit_string_vers_liste(string_of_code code_secret)) 0
    ;;




    let ft_generer_code_depart_IA couleurs_possibles = 
  let rec ft_generer_code_depart_IAACC compt acc couleurs_possibles = 
          match compt with 
          | _ when compt = nombre_pions -> acc
          | _ -> ft_generer_code_depart_IAACC (compt+1) (acc @ [List.nth couleurs_possibles 1]) couleurs_possibles

      in ft_generer_code_depart_IAACC 2 ([List.hd couleurs_possibles]@[List.hd couleurs_possibles]) couleurs_possibles
;;



  (** boucle_jeu_classique
    * fait jouer le joueur en mode classique (1 seule partie) (sans les jolis affichages)
    * @author Dingley Jesse
    * @param numero_partie int
    * @param nb_tentatives int
    * @return jeu mode classique partie n (sans affichage joli) (unit) 
    *) 

  let ft_boucle_jeu_classique numero_partie nb_tentatives = 
      let rec ft_boucle_jeu_classiqueREC tentative numero_partie nb_tentatives code_secret  = 
             (*  Printf.printf "%s" (string_of_code a); *)
              match tentative with
              | _ when tentative = nb_tentatives -> ft_affiche_perdu "-*-fixed-medium-r-semicondensed--60-*-*-*-*-*-iso8859-1"; ft_affiche_code_secret_apres_avoir_perdu code_secret
              | 0 -> ft_choisir_un_code 0 0 ;ft_affiche_reponse_finale 0 numero_partie code_secret; ft_boucle_jeu_classiqueREC (tentative+1) numero_partie nb_tentatives code_secret
              | _ when (ft_obtenir_code (tentative-1) 0) <> code_secret -> ft_choisir_un_code tentative 0; ft_affiche_reponse_finale (tentative) numero_partie code_secret; ft_boucle_jeu_classiqueREC (tentative+1) numero_partie nb_tentatives code_secret
              | _  when (ft_obtenir_code (tentative-1) 0) = code_secret ->  ft_affiche_gagnee "-*-fixed-medium-r-semicondensed--60-*-*-*-*-*-iso8859-1" 
        
      in ft_boucle_jeu_classiqueREC 0 numero_partie nb_tentatives (ft_creation_code_cache numero_partie) 
      ;;







let ft_boucle_jeu_IA numero_partie nb_tentatives knuth_naif = 
      let rec ft_boucle_jeu_IA_REC  numero_partie nb_tentatives essais possibles knuth_naif code_secret_IA tentative = 
             (*  Printf.printf "%s" (string_of_code a); *)

             let chx = if((tentative = 0) && (knuth_naif = 1)) then (ft_generer_code_depart_IA couleurs_possibles) else (IA.choix knuth_naif essais possibles) in
             let rep = Code.reponse chx code_secret_IA in 
			 let filtrer = filtre 1 (chx, rep) possibles in                        
             

			 if(chx <> code_secret_IA) then (
						if(tentative = (nb_tentatives-1)) then (ft_affiche_perdu_pas_devine "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1"; ft_affiche_code_secret_apres_avoir_perdu code_secret_IA;
																ft_affiche_code  (ft_convertit_string_vers_liste(string_of_code( chx(* IA.choix knuth_naif essais possibles *)))) tentative 0;
                            									ft_affiche_reponse_finale (tentative) numero_partie code_secret_IA) 
						else
              (delay 1.0;
							ft_affiche_code  (ft_convertit_string_vers_liste(string_of_code( chx(* IA.choix knuth_naif essais possibles *)))) tentative 0;
                            ft_affiche_reponse_finale (tentative) numero_partie code_secret_IA; 
                            ft_boucle_jeu_IA_REC numero_partie nb_tentatives (essais @ [chx]) filtrer knuth_naif code_secret_IA (tentative+1)))

            else 
                  (delay 1.0; ft_affiche_code (ft_convertit_string_vers_liste((string_of_code(chx))))  tentative 0 ; 
            	  ft_affiche_reponse_finale (tentative) numero_partie code_secret_IA; 
            	  ft_affiche_IA_gagne "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1");


      in ft_creer_code_cache_a_la_main 0; ft_boucle_jeu_IA_REC numero_partie nb_tentatives [] Code.tous knuth_naif ((* [5;4;5;4] *) ft_creation_code_cache numero_partie) 0; (* ft_affiche_code  (ft_convertit_string_vers_liste( string_of_code([5;4;5;4]))) 12 0 *)
      ;;







  (** jeu_classique
    * fait jouer le joueur en mode classique (1 seule partie)
    * @author Dingley Jesse
    * @param numero_partie int
    * @param nb_tentatives int
    * @return jeu mode classique partie n (unit)
    *) 

  let ft_jeu_classique numero_partie nb_tentatives =
      ft_affiche_mode numero_partie;
      ft_affiche_numero_partie numero_partie; 
      ft_boucle_jeu_classique numero_partie nb_tentatives
      ;;






  let ft_affiche_naif_ou_knuth () = 
      moveto 800 200;
      set_color black;
      draw_string "Choisir l'IA : ";
      moveto 960 200;
      set_color red;
      draw_string " IA Naif ";
      set_color black;
      draw_string "|";
      set_color red;
      draw_string " Knuth ";
      set_color black
   (*    draw_rect 965 150 90 30;
      draw_rect 1082 150 90 30 *)
      ;;



  let ft_affiche_choisir_reponse_a_la_main tentative =
      set_color couleur_arriere_plan; 
      fill_rect 800 150 500 40;
      moveto 800 150;
      set_color black;
      draw_string "Choisir rep pour tentative ";
      draw_string (string_of_int tentative);
      draw_string " (Clavier)";
      moveto 1110 100;
      draw_string " [ | ]"
    ;;
 



   let ft_affiche_rep_bn valeur couleur = 
      moveto 1132 100;
      set_color red;
      draw_string (string_of_int valeur);
      moveto 1 1;
      set_color couleur;
      plot 1 1
      ;;


  let ft_affiche_rep_mv valeur couleur = 
      moveto 1160 100;
      set_color red;
      draw_string (string_of_int valeur);
      set_color couleur;
      moveto 2 1;
      plot 2 1
      ;;




  let ft_affiche_rep_bn_mv bn_mv valeur couleur= 
      match bn_mv with 
      | true -> ft_affiche_rep_bn valeur couleur  
      | false -> ft_affiche_rep_mv valeur couleur
      ;;


  let rec ft_affiche_int_rep opt bn_mv =  
          match opt.key with
          | _ when (opt.key = '0') && (nombre_pions >= 1) -> ft_affiche_rep_bn_mv bn_mv 0 c1
          | _ when (opt.key = '1') && (nombre_pions >= 1) -> ft_affiche_rep_bn_mv bn_mv 1 c2
          | _ when (opt.key = '2') && (nombre_pions >= 2)-> ft_affiche_rep_bn_mv bn_mv 2 c3
          | _ when (opt.key = '3') && (nombre_pions >= 3)-> ft_affiche_rep_bn_mv bn_mv 3 c4
          | _ when (opt.key = '4') && (nombre_pions >= 4) -> ft_affiche_rep_bn_mv bn_mv 4 c5
          | _ when (opt.key = '5') && (nombre_pions >= 5) -> ft_affiche_rep_bn_mv bn_mv 5 c6
          | _ when (opt.key = '6') && (nombre_pions >= 6) -> ft_affiche_rep_bn_mv bn_mv 6 c7(*ou alors enleve couple et affichage apres*)
          | _ when (opt.key = '7')&& (nombre_pions >= 7) -> ft_affiche_rep_bn_mv bn_mv 7 c8
          | _ when (opt.key = '8') && (nombre_pions >= 8) -> ft_affiche_rep_bn_mv bn_mv 8 c9
          | _ when (opt.key = '9') && (nombre_pions >= 9)-> ft_affiche_rep_bn_mv bn_mv 9 c10
          | _ when (opt.key = 'a') && (nombre_pions >= 10) -> ft_affiche_rep_bn_mv bn_mv 10 c11                (*    NOMBRE PIONS <= 17 *)
          | _ when (opt.key = 'b') && (nombre_pions >= 11) -> ft_affiche_rep_bn_mv bn_mv 11 c12
          | _ when (opt.key = 'c') && (nombre_pions >= 12) -> ft_affiche_rep_bn_mv bn_mv 12 c13
          | _ when (opt.key = 'd') && (nombre_pions >= 13) -> ft_affiche_rep_bn_mv bn_mv 13 c14
          | _ when (opt.key = 'e') && (nombre_pions >= 14) -> ft_affiche_rep_bn_mv bn_mv 14 c15
          | _ when (opt.key = 'f') && (nombre_pions >= 15) -> ft_affiche_rep_bn_mv bn_mv 15 c16
          | _ when (opt.key = 'g')&& (nombre_pions >= 16) -> ft_affiche_rep_bn_mv bn_mv 16 c17
          | _ -> ft_affiche_int_rep (wait_next_event[Key_pressed]) bn_mv
          ;;




  let ft_tapper_a_la_main_reponse () =   
      set_color couleur_arriere_plan;
      fill_rect 800 90 300 40;
      set_color black;
      ft_draw_arrow 680 100;                    
      moveto 800 100;
      set_color black;
      draw_string "Rentrer bonne reponse : ";
      ft_affiche_int_rep (wait_next_event[Key_pressed]) true;
      moveto 800 100;
      set_color couleur_arriere_plan;
      fill_rect 800 90 300 40;
      set_color black;
      draw_string "Rentrer mauvaise reponse : ";
      ft_affiche_int_rep (wait_next_event[Key_pressed]) false;
      set_color couleur_arriere_plan;
      fill_rect 800 90 300 40;
      set_color black;
      ft_efface_arrow 680 100
      ;;


  let ft_obtenir_un_element_couple_reponse_a_la_main x y =
      match point_color x y with  
      | 15790320 -> 0
      | 15790321 -> 1
      | 15790576 -> 2
      | 15855856 -> 3
      | 15724527 -> 4
      | 15724784 -> 5 
      | 15856113 -> 6
      | 15790319 -> 7
      | 15790064 -> 8
      | 15856112 -> 9
      | 15790577 -> 10
      | 15855857 -> 11
      | 15724783 -> 12
      | 15724528 -> 13
      | 15790063 -> 14
      | 15921392 -> 15 
      | 15790832 -> 16
      ;;


      


  let ft_obtenir_couple_reponse_a_la_main () = 
      (ft_obtenir_un_element_couple_reponse_a_la_main 1 1,ft_obtenir_un_element_couple_reponse_a_la_main 2 1)
      ;;


  let ft_verif_resultat_rentree_par_lutilisateur code code_secret = 
      (ft_type_of_option (reponse code code_secret)) = (ft_obtenir_couple_reponse_a_la_main ())
      ;;


  let ft_verif_resultat_rentree_par_lutilisateur_par_rapport_a_laffichage tentative  code_secret= 
      ft_verif_resultat_rentree_par_lutilisateur (ft_obtenir_code tentative 0) code_secret
      ;;





  let ft_boucle_jeu_IA_rep_auto numero_partie nb_tentatives knuth_naif = 
      let rec ft_boucle_jeu_IA_reo_auto_REC  numero_partie nb_tentatives essais possibles knuth_naif code_secret_IA tentative = 
             (*  Printf.printf "%s" (string_of_code a); *)

             let chx = if((tentative = 0) && (knuth_naif = 1)) then (ft_generer_code_depart_IA couleurs_possibles) else (IA.choix knuth_naif essais possibles) in
             let rep = Code.reponse chx code_secret_IA in 
			 let filtrer = filtre 1 (chx, rep) possibles in                        
             

			 if(chx <> code_secret_IA) 
			    then 
			        (
						if(tentative = (nb_tentatives-1)) 
						   then 
						       (
						       	   (*  ft_affiche_perdu "-*-fixed-medium-r-semicondensed--60-*-*-*-*-*-iso8859-1"; ft_affiche_code_secret_apres_avoir_perdu code_secret_IA *)

						       	   ft_affiche_code  (ft_convertit_string_vers_liste(string_of_code( chx(* IA.choix knuth_naif essais possibles *)))) tentative 0;
							        ft_affiche_choisir_reponse_a_la_main tentative;
							        ft_tapper_a_la_main_reponse ();
                                    set_color couleur_arriere_plan;
                                    fill_rect 800 90 600 40;
                                    set_color black;
                                    if ft_verif_resultat_rentree_par_lutilisateur chx code_secret_IA 
                                       then 
                                           (
                                           	ft_affiche_perdu_pas_devine "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1";

                            	            ft_affiche_reponse_finale (tentative) numero_partie code_secret_IA; 
                                           )
                                    else 
                                            
						       	    ft_affiche_perdu_rep_incorrecte "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1"; ft_affiche_code_secret_apres_avoir_perdu code_secret_IA
                            									
						       )
						   else
                              (

							        ft_affiche_code  (ft_convertit_string_vers_liste(string_of_code( chx(* IA.choix knuth_naif essais possibles *)))) tentative 0;
							        ft_affiche_choisir_reponse_a_la_main tentative;
							        ft_tapper_a_la_main_reponse ();
                                    set_color couleur_arriere_plan;
                                    fill_rect 800 90 600 40;
                                    set_color black;
                                   
                                    if ft_verif_resultat_rentree_par_lutilisateur chx code_secret_IA 
                                       then 
                                           (
                            	            ft_affiche_reponse_finale (tentative) numero_partie code_secret_IA; 
                                            ft_boucle_jeu_IA_reo_auto_REC numero_partie nb_tentatives (essais @ [chx]) filtrer knuth_naif code_secret_IA (tentative+1)
                                           )
                                       else 
                                            (ft_affiche_perdu_rep_incorrecte "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1")

                               )
                    )
              else 
                  (

                                 ft_affiche_code (ft_convertit_string_vers_liste((string_of_code(chx))))  tentative 0 ; 
                                 ft_affiche_choisir_reponse_a_la_main tentative;
                                 ft_tapper_a_la_main_reponse ();
                                 set_color couleur_arriere_plan;
                                 fill_rect 800 90 600 40;
                                 set_color black;
                             
                            if (ft_verif_resultat_rentree_par_lutilisateur chx code_secret_IA ) then 
                            	(
                  
					            	  ft_affiche_reponse_finale (tentative) numero_partie code_secret_IA; 
					            	  ft_affiche_IA_gagne "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1" )else 
					            	 ( ft_affiche_perdu_rep_incorrecte "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1")
					            

                  )


      in ft_creer_code_cache_a_la_main 0; ft_boucle_jeu_IA_reo_auto_REC numero_partie nb_tentatives [] Code.tous knuth_naif ((* [5;4;5;4] *) ft_creation_code_cache numero_partie) 0 (* ft_affiche_code  (ft_convertit_string_vers_liste( string_of_code([5;4;5;4]))) 12 0 *)
      ;;




  let ft_naif rep_auto numero_partie nb_tentatives = 
      match rep_auto with 
      | true -> ft_boucle_jeu_IA numero_partie nb_tentatives 0 (* ft_naif_cas_true    *)         (*machine qui fait les reponses*) 
      | _ ->  ft_boucle_jeu_IA_rep_auto numero_partie nb_tentatives 0
      ;;


  let ft_knuth rep_auto numero_partie nb_tentatives = 
      match rep_auto with 
      | true -> ft_boucle_jeu_IA numero_partie  nb_tentatives  1    (* ft_knuth_cas_true 0 *) (* ft_knuth_cas_true *)
      | _ ->  ft_boucle_jeu_IA_rep_auto numero_partie nb_tentatives 1 (* ft_knuth_cas_false *)
      ;;


  let ft_cond_encadrement_cliquer_IA_1 x y = 
      (960 <= x && x <= (965+90)) && (200 <= y && y <= 230)
      ;;


  let ft_cond_encadrement_cliquer_IA_2 x y = 
      (1082 <= x && x <= (1082+90)) && (200 <= y && y <= 230)
      ;;




  (** ft_cliquer_sur_naif_ou_knuth
    * permet de chosir entre knuth et naif (1 seule partie)
    * @author Dingley Jesse
    * @param numero_partie int
    * @param rep_auto bool
    * @return jeu partie n IA (sans affichage joli debut) (unit)
    *)

  let rec ft_cliquer_sur_naif_ou_knuth opt rep_auto numero_partie nb_tentatives =
          match (opt.mouse_x, opt.mouse_y) with 
          | _ when (ft_cond_encadrement_cliquer_IA_1 opt.mouse_x opt.mouse_y) -> ft_efface_arrow 700 200;ft_naif rep_auto  numero_partie nb_tentatives
          | _ when (ft_cond_encadrement_cliquer_IA_2 opt.mouse_x opt.mouse_y) -> ft_efface_arrow 700 200; ft_knuth rep_auto numero_partie nb_tentatives
          | _ -> ft_cliquer_sur_naif_ou_knuth (wait_next_event[Button_down]) rep_auto numero_partie nb_tentatives
          ;;  
      



  (* let ft_choisir_naif_ou_knuth *)
 

  (** jeu_IA
    * fait jouer le joueur mode IA (1 seule partie)
    * @author Dingley Jesse
    * @param numero_partie int
    * @param rep_auto bool
    * @return jeu partie n IA (unit)
    *)

  let ft_jeu_IA numero_partie rep_auto nb_tentatives =
      ft_affiche_mode numero_partie;
      ft_affiche_numero_partie numero_partie;
      ft_affiche_naif_ou_knuth ();
      ft_draw_arrow 700 200;
      ft_cliquer_sur_naif_ou_knuth (wait_next_event[Button_down]) rep_auto numero_partie nb_tentatives
      ;;
 

  (** jeu_dependant_de_nb_parties
    * fait jouer le joueur  (1 seule partie)
    * @author Dingley Jesse
    * @param numero_partie int
    * @param nb_tentatives int
    * @return jeu partie n (unit)
    *)   

  let ft_jeu_dependant_de_nb_parties numero_partie nb_tentatives rep_auto = 
      match numero_partie with
      | _ when (numero_partie mod 2 ) = 0 -> ft_jeu_IA numero_partie rep_auto nb_tentatives 
      | _ -> ft_jeu_classique numero_partie nb_tentatives
      ;;




  (** jeu_affichages
    * affiche les  bases de l'interface 
    * @author Dingley Jesse                                              (Random.int (List.length tous)
    * @param nom_joueur  string
    * @param nb_tentative_par_partie  int
    * @param nb_partie int
    * @return interface de base (unit)
    *)

  let ft_jeu_affichages nom_joueur nb_tentative_par_partie nb_partie =
      ft_remplir_de_clr couleur_arriere_plan;
      ft_remplir_de_clr couleur_derriere_mastermind;   
      ft_remplir_complement couleur_derriere_mastermind;
      ft_affiche_mastermind "-*-fixed-medium-r-semicondensed--60-*-*-*-*-*-iso8859-1";
      ft_affiche_joueur nom_joueur;
      ft_affiche_nb_tentatives nb_tentative_par_partie ;
      ft_affiche_nb_parties nb_partie;
      ft_affiche_legende black;
     (*  ft_affiche_mode ON VERRA; *)
      ft_trace_ligne_param black;
      ft_trace_colonnes_finale 0 nb_tentative_par_partie;
      ft_trace_lignes_finale 0 nb_tentative_par_partie;
      ft_affiche_mesg_choisir_couleurs black;
      ft_trace_ligne_choisir_couleurs black;
      ft_affiche_couleurs_possibles 0
      ;;        




(*ft_finale*)

let ft_jeu nom_joueur nb_tentative_par_partie nb_partie rep_auto = 
    let rec ft_jeuREC nom_joueur nb_tentative_par_partie nb_partie rep_auto nb_tours = 
            match nb_tours with
            | _ when nb_tours = nb_partie -> (* (delay 1.0) ;  *)ft_click_any_key_to_continue ();
                                             ft_remplir_de_clr couleur_arriere_plan; 
                                             ft_jeu_affichages nom_joueur nb_tentative_par_partie nb_partie ;
                                             ft_jeu_dependant_de_nb_parties nb_tours nb_tentative_par_partie rep_auto 
            | 1 -> ft_click_any_key_to_continue_debut_partie ();
                   ft_remplir_de_clr couleur_arriere_plan;
                   ft_jeu_affichages nom_joueur nb_tentative_par_partie nb_partie ;
                   ft_jeu_dependant_de_nb_parties nb_tours nb_tentative_par_partie rep_auto ;
                   ft_jeuREC nom_joueur nb_tentative_par_partie nb_partie rep_auto (nb_tours+1) 
            | _ -> (* (delay 1.0);  *)ft_click_any_key_to_continue () ; ft_remplir_de_clr couleur_arriere_plan; 
                   ft_jeu_affichages nom_joueur nb_tentative_par_partie nb_partie; 
                   ft_jeu_dependant_de_nb_parties nb_tours nb_tentative_par_partie rep_auto ; 
                   ft_jeuREC nom_joueur nb_tentative_par_partie nb_partie rep_auto (nb_tours+1) 
    in ft_ouvrir_interface " 1280x900" "Mastermind";
       ft_jeuREC nom_joueur nb_tentative_par_partie nb_partie rep_auto 1 ; 
       ft_click_any_key_to_continue (); 
       ft_remplir_de_clr couleur_arriere_plan; 
       ft_fermer_interface ()
    ;;

end;;

open UI;;