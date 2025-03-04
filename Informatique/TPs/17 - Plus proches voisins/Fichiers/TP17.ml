(* Fonctions pour lire les fichiers et récupérer les images *)

type image_etiquette = Etiquetage of (int array array) * int

let ligne_en_int ligne = 
    List.map (fun x-> int_of_string x) (String.split_on_char ' ' ligne)

let rec afficher liste = match liste with
  | [] -> print_newline ()
  | h::q -> print_int h; print_string " "; afficher q

let rec transfert t1 l i = match l with
  | [a] -> ()
  | h::q -> t1.(i) <- h; transfert t1 q (i+1)
  
let recuperer_image ic = 
  let image = Array.make_matrix 28 28 0 in
  for i = 0 to 27 do
    let ligne = input_line ic in
    let liste = ligne_en_int ligne in
    transfert (image.(i)) liste 0;
  done;
  image

let rec recuperer_fichier ic i acc = match i with
  | 0 -> acc
  | n -> let a = (recuperer_image ic) in 
                      let _ = input_line ic in
                      recuperer_fichier ic (i-1) (a::acc)

let rec recuperer_images_fichier nom taille =
  let ic = open_in nom in
  let liste_images = recuperer_fichier ic taille [] in
  close_in ic;
  liste_images

let rec recuperer_etiquettes ic i acc =  match i with
  | 0 -> acc
  | n -> let [a] = ligne_en_int (input_line ic) in
          recuperer_etiquettes ic (i-1) (a::acc)

let etiqueter_image t n = Etiquetage(t,n)

let rec tout_etiqueter l1 l2 acc = match l1 with
  | [] -> acc
  | h::q -> let h2::q2 = l2 in
            tout_etiqueter q q2 ((etiqueter_image h h2)::acc)

let etiqueter_liste l nom taille = 
  let ic = open_in nom in
  let liste_etiquettes = recuperer_etiquettes ic taille [] in
  close_in ic;
  tout_etiqueter l liste_etiquettes []

let afficher_image t = 
  for i = 0 to 27 do
    for j = 0 to 27 do
      if t.(i).(j)>122 then print_string "■ " else print_string "□ ";
    done;
    print_newline ();
  done

let rec afficher_nombre l i = match i with
    | 0 -> ()
    | i -> let Etiquetage(t,n)::q = l in
            print_int n; print_newline (); afficher_image t; afficher_nombre q (i-1)


(* Fonctions à compléter *)

type kdArbre = Vide | Noeud of image_etiquette * kdArbre * kdArbre

let compter t k = 0

let partitionner t k =  [||], [||]
    
let construire t = Vide
  
let distance_Manhattan image1 image2 = 0

type file_priorite = (image_etiquette * int) list * int * int

let distance_max (f : file_priorite) = 0

let inserer (f : file_priorite) image d = [],0,0

let distance_hyperplan reference image k = 0
            
let recherche_k_plus_proches arbre k image = []

let majoritaire l = 0
                          
let prediction arbre k image = 0

(* Fonction de tests des performance de l'algorithme *)

let test_conforme_affichage arbre (Etiquetage(img,n)) k i =
  let resultat = prediction arbre k img in
  print_string "J'aurais du prédire ";
  print_int n;
  print_string " et j'ai prédit ";
  print_int resultat;
  print_string " sur l'image numéro ";
  print_int i;
  print_newline();
  afficher_image img;
  resultat = n

let test_conforme_affichage_erreur arbre (Etiquetage(img,n)) k i = 
  let resultat = prediction arbre k img in
  if resultat != n then begin
    print_string "J'aurais du prédire ";
    print_int n;
    print_string " et j'ai prédit ";
    print_int resultat;
    print_string " sur l'image numéro ";
    print_int i;
    print_newline();
    afficher_image img;
    false  
  end else
    true

let test_conforme arbre (Etiquetage(img,n)) k i = 
  let resultat = prediction arbre k img in
  resultat = n

let prediction_reussie k = 
  let nombre_tests = 1000 in
  let l_entrainement = recuperer_images_fichier "train-images-idx3.txt" 60000 in
  let l_train_etique = etiqueter_liste l_entrainement "train-labels-idx1.txt" 60000 in
  let l_test = recuperer_images_fichier "t10k-images-idx3.txt" nombre_tests in
  let l_test_etique = etiqueter_liste l_test "t10k-labels-idx1.txt" nombre_tests in
  print_string "Fin de récupération des données\n";
  let arbre = construire (Array.of_list l_train_etique) in
  print_string "Fin de construction de l'arbre\n";
  let rec valide l i j= 
  match l with
    | [] -> []
    | h::q -> let b = (test_conforme arbre h j i) in b::(valide q (i+1)j )
  in
  valide l_test_etique 0 k 
;;

prediction_reussie 5