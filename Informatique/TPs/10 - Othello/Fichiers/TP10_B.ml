open TP10_A

let afficher_case case = match case with
    | Vide -> "□ "
    | Noir -> "○ "
    | Blanc -> "● "


let afficher_jeu jeu =
    let plateau = jeu.plateau in
    let joueur = jeu.joueur in
    print_string "  ";
    for i = 0 to 7 do
        print_int i; print_string " ";
    done;
    print_newline();
    for i = 0 to 7 do
        print_int (7-i); print_string " ";
        for j = 0 to 7 do
            print_string (afficher_case (plateau.(j).(7-i)));
        done;
        print_newline();
    done;
    print_string ("C'est maintenant au joueur ");
    print_string (afficher_case joueur);
    print_newline ()

let afficher_coups (jeu,(x,y)) =
    print_string ("En jouant le coup (");
    print_int x; print_string ","; print_int y; print_string "), la nouvelle configuration est :\n";
    afficher_jeu jeu