open TP10_A
open TP10_B
open TP10_D

let rec saisir_coup jeu_ref = 
  print_newline ();
  print_string "Saisissez l'abscisse de votre coup :\n";
  let x = int_of_string (read_line()) in 
  print_string "Saisissez l'ordonnée de votre coup :\n";
  let y = int_of_string (read_line()) in 
  let coup = (x,y) in
  if x= -1 && y = -1 then
    begin
      print_string "Le joueur a passé son tour\n";
      jeu_ref := { plateau = !jeu_ref.plateau;
                      joueur = (if (!jeu_ref.joueur)=Blanc then Noir else Blanc);
                      nombre_passe = !jeu_ref.nombre_passe+1};
      false
    end
  else
    if est_legal (!jeu_ref.plateau) (!jeu_ref.joueur) coup then
      begin 
        let nouveau_plateau,modif = retourner (!jeu_ref.plateau) (!jeu_ref.joueur) coup in
        jeu_ref := { plateau = nouveau_plateau;
                      joueur = (if (!jeu_ref.joueur)=Blanc then Noir else Blanc);
                      nombre_passe = 0};
                      afficher_jeu !jeu_ref;
        true
      end
    else 
      begin
        print_string "Le joueur n'a pas joué un coup légal, il doit recommencer son choix. Si aucun coup légal n'est possible, il doit jouer le coup -1, -1 pour passer son tour\n";
        saisir_coup jeu_ref
      end

let rec calculer_coup jeu_ref = 
  print_newline ();
  let t1 = Sys.time () in
  let (x,y) = trouver_coup (!jeu_ref) heuristique 5 in
  let t2 = Sys.time () in
  print_string "Après ";
  print_float (t2-.t1);
  print_string "s,";
  let coup = (x,y) in 
  if x= -1 && y = -1 then
    begin
      print_string "l'ordi a passé son tour\n";
      jeu_ref := { plateau = !jeu_ref.plateau;
                      joueur = (if (!jeu_ref.joueur)=Blanc then Noir else Blanc);
                      nombre_passe = !jeu_ref.nombre_passe+1};
      false
    end
  else
    if est_legal (!jeu_ref.plateau) (!jeu_ref.joueur) coup then
      begin 
        print_string "l'ordi a joué le coup x = ";
        print_int x;
        print_string ", y =";
        print_int y;
        print_newline ();
        let nouveau_plateau,modif = retourner (!jeu_ref.plateau) (!jeu_ref.joueur) coup in
        jeu_ref := { plateau = nouveau_plateau;
                      joueur = (if (!jeu_ref.joueur)=Blanc then Noir else Blanc);
                      nombre_passe = 0};
                      afficher_jeu !jeu_ref;
        true
      end
    else 
      begin
        print_string "l'ordi n'a pas joué un coup légal, il doit recommencer son choix. Si aucun coup légal n'est possible, il doit jouer le coup -1, -1 pour passer son tour\n";
        calculer_coup jeu_ref
      end

let jouer () = 
    let jeu = configuration_initiale () in
    afficher_jeu jeu;
    let jeu_ref = ref jeu in
    let noir_joue = ref true in
    let blanc_joue = ref true in
    let tours = ref 1 in
    while (!noir_joue || !blanc_joue) do
        incr tours;
        print_string "Tour n°";
        print_int (!tours);
        print_newline ();
        blanc_joue := saisir_coup jeu_ref;
        incr tours;
        print_string "Tour n°";
        print_int (!tours);
        print_newline ();
        noir_joue := saisir_coup jeu_ref;
    done;
    print_string "Fin de la partie !\n";
    afficher_jeu !jeu_ref;
    match gagnant !jeu_ref with
    | Blanc -> print_string "Le joueur blanc l'emporte !\n"
    | Noir -> print_string "Le joueur noir l'emporte !\n"
    | Vide -> print_string "Match nul\n"

let jouer_ordi () = 
    let jeu = configuration_initiale () in
    afficher_jeu jeu;
    let jeu_ref = ref jeu in
    let noir_joue = ref true in
    let blanc_joue = ref true in
    let tours = ref 0 in
    while (!noir_joue || !blanc_joue) do
        incr tours;
        print_string "Tour n°";
        print_int (!tours);
        print_newline ();
        blanc_joue := saisir_coup jeu_ref;
        incr tours;
        print_string "Tour n°";
        print_int (!tours);
        print_newline ();
        noir_joue := calculer_coup jeu_ref;
    done;
    print_string "Fin de la partie !\n";
    afficher_jeu !jeu_ref;
    match gagnant !jeu_ref with
    | Blanc -> print_string "Le joueur blanc l'emporte !\n"
    | Noir -> print_string "Le joueur noir l'emporte !\n"
    | Vide -> print_string "Match nul\n"
;;

jouer_ordi ()