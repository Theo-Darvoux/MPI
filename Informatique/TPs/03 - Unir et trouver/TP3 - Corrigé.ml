(* Définition du type buf qui représente une structure pour stocker la forêt. *)
type buf = { 
  parent : int array; 
  repr : bool array; 
  rang : int array
}

(* Chaque élément est initialement son propre parent, est un représentant, et a un rang de 0. *)
let init n = 
  { 
    parent = Array.init n (fun i -> i);
    repr = Array.make n true;
    rang = Array.make n 0 
  }

(* Si s est son propre représentant, on le renvoie, sinon on continue la recherche via son parent. *)
let rec find foret s = 
  if foret.repr.(s) then s
  else find foret (foret.parent.(s)) 

(* Si s est un représentant, on le renvoie.
    Sinon, on calcule le représentant de son père, on met à jour le père de s et on renvoie le représentant obtenu. *)
let rec find_compresse foret s = 
  if foret.repr.(s) then s
  else 
    let p = foret.parent.(s) in
    let r = find_compresse foret p in
    foret.parent.(s) <- r;
    r

(* Pour chaque élément, on l'ajoute au bloc correspondant à son représentant. *)
let blocs_array foret =
  let n = Array.length foret.parent in
  let tab = Array.make n [] in
  for i = 0 to n - 1 do
    let x = find_compresse foret i in
    tab.(x) <- i :: tab.(x);
  done;
  tab

(* On utilise blocs_array pour obtenir la structure des blocs et on ne conserve ensuite que les blocs non vides. 
    La fonction aux réalise simplement une boucle sur les entiers de 0 à la taille de tab moins 1 *)
let blocs foret =
  let tab = blocs_array foret in
  let rec aux l i =
    if i = Array.length tab then l
    else if tab.(i) <> [] then aux (tab.(i) :: l) (i + 1)
    else aux l (i + 1)
  in aux [] 0

(* On  utilise une fonction aux qui remplit la liste fournie en paramètre.
    Si x est la racine, on l'ajout et on renvoie la liste.
    Si x est un représentant, on l'ajoute à la liste et on réalise un appel récursif sur son père.
    Sinon, on réalise un appel récursif sur le père. *)
let chaine_racine foret x =
  let rec aux x l = 
    if foret.parent.(x) = x then (x :: l)
    else if foret.repr.(x) then aux (foret.parent.(x)) (x :: l)
    else aux (foret.parent.(x)) l
  in aux x []

(* S'il n'y aucun sommet dans la liste, on ne fait rien
    S'il n'y a qu'un élément, ce sera la racine de l'arbre retourné.
    Sinon, on récupère les deux premiers et l'on retourne la relation parent. 
      On réalise enfin un appel récursif sur la suite de la liste en prenant soin de remettre le parent obtenu. *)
let rec retourner_chaine foret l  = match l with
  | [] -> ()
  | [a] -> foret.parent.(a)<-a
  | a :: b :: suite -> 
    begin
      foret.parent.(a) <- b;
      retourner_chaine foret (b :: suite)
    end
    
(* On distingue les différents cas selon les rangs des deux sommets. *)
let union foret x y =
  let rx = foret.rang.(x) in
  let ry = foret.rang.(y) in
  if rx < ry then
    begin
      foret.parent.(x) <- y;
      foret.repr.(x) <- false;
      y
    end
  else if rx > ry then
    begin
      foret.parent.(y) <- x;
      foret.repr.(y) <- false;
      x
    end
  else
    begin
      foret.parent.(x) <- y;
      foret.repr.(x) <- false;
      foret.rang.(y) <- foret.rang.(y) + 1;
      y
    end

(* On réalise l'union successive des éléments de la liste en rajoutant le sommet obtenu à chaque fois.
    Lorsqu'il ne reste plus qu'un sommet, les blocs ont étés fusionés.
    Il faut alors les remettre au bon endroit dans la forêt. Pour cela, on distingue deux cas :
    - Les blocs comprenaient la racine auquel cas le représentant final en sera une aussi
    - Les blocs ne comprenaient pas la racine. Le père du représentant final sera donc celui qu'on avait noté initialement. *)
let fusion_chaine foret l =
  let rec aux l parent est_racine = match l with
    | [] -> ()
    | [a] -> if est_racine then foret.parent.(a) <- a 
                    else foret.parent.(a) <- parent
    | a :: b :: suite -> 
        let x = union foret a b in
        aux (x :: suite) parent est_racine
  in 
  match l with
    | [] -> ()
    | a :: t -> aux (a :: t) foret.parent.(a) (a = foret.parent.(a))

(* Étant donnés deux listes, on retire le début en commun.
    On fait attntion à conserver le plus proche ancêtre commun qui correspond à la racine de l'union. *)
let rec retirer_prefixe l1 l2 dernier = match l1, l2 with
  | [],_ -> dernier::l2
  | _,[] -> dernier::l1
  | h1::q1, h2::q2 -> if h1 = h2 then retirer_prefixe q1 q2 h1
                                    else dernier::(l1@l2)

(* On distingue les différents cas en observant les représentants et les chaines jusqu'à la racine.
    - Si les représentants sont égaux, les deux sommets sont déjà dans le même bloc, il ne se passe rien.
    - Si les chaînes ont le même premier sommet, ils sont donc dans le même arbre.
      On réalise alors la fusion des blocs jusqu'au plus proche ancêtre commun
    - Sinon, on retourne la chaine de représentants de l'un des deux que l'on fait pointer sur l'autre. *)
let ajout foret u v =
  let ru = find_compresse foret u in
  let rv = find_compresse foret v in
  if (ru!=rv) then
  begin
    let chaine_u = chaine_racine foret ru in
    let chaine_v = chaine_racine foret rv in
    match chaine_u,chaine_v with
      |a::s1,b::s2 when a<>b -> 
              retourner_chaine foret chaine_v;
              foret.parent.(rv)<-ru;
      |_->  let l = retirer_prefixe chaine_u chaine_v  0 in 
              fusion_chaine foret (l)
  end

(* Fonction qui construit la structure but associée à un graphe représenté sous forme de liste d'adjacence.
    Pour chaque sommet u, on ajoute une arête entre u et chacun de ses voisins v.
    On fait attention ) ne pas ajouter deux fois une même arête (cela serait vu comme la fusion de deux blocs).
    Pour cela, on n'ajoute que les arêtes de la forme (u,v) avec u<v. *)
let construire graphe = 
  let n = Array.length graphe in
  let foret = init n in
  for u = 0 to n - 1 do
    List.iter (fun v -> if u < v then ajout foret u v) graphe.(u)
  done;
  foret

(* Fonction qui calcule et retourne les blocs d'un graphe sous forme de composants connexes. *)
let calcul_blocs graphe = blocs (construire graphe)
;;

(* Initialisation de la structure de la forêt *)
let foret_test = {
  parent = [|3; 3; 3; 3; 3; 4; 4; 6; 4; 8; 11; 11; 11; 11; 13; 13|];
  repr = [|false; true; false; true; true; false; false; true; false; false; false; true; false; true; false; false|];
  rang = [|0; 0; 0; 1; 2; 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0|]
} in

(* Test de la fonction find *)
Printf.printf "\nTest de find :\n";
let r1 = find foret_test 2 in
let r2 = find foret_test 7 in
let r3 = find foret_test 10 in
Printf.printf "find 2: %d\n" r1;
Printf.printf "find 7: %d\n" r2;
Printf.printf "find 10: %d\n" r3;

(* Test de la fonction find_compresse *)
Printf.printf "\nTest de find_compresse :\n";
let r1_compresse = find_compresse foret_test 2 in
let r2_compresse = find_compresse foret_test 7 in
let r3_compresse = find_compresse foret_test 10 in
Printf.printf "find_compresse 2: %d\n" r1_compresse;
Printf.printf "find_compresse 7: %d\n" r2_compresse;
Printf.printf "find_compresse 10: %d\n" r3_compresse;
Array.iteri (fun i p -> Printf.printf "parent(%d) = %d\n" i p) foret_test.parent;

(* Test de la fonction blocs_array *)
Printf.printf "\nTest de blocs_array :\n";
let tab_blocs = blocs_array foret_test in
Array.iteri (fun i l -> Printf.printf "Bloc %d: %s\n" i (String.concat ", " (List.map string_of_int l))) tab_blocs;

(* Test de la fonction blocs *)
Printf.printf "\nTest de blocs :\n";
let blocs_non_vides = blocs foret_test in
List.iter (fun l -> Printf.printf "Bloc: %s\n" (String.concat ", " (List.map string_of_int l))) blocs_non_vides;

(* Test de la fonction chaine_racine *)
Printf.printf "\nTest de chaine_racine :\n";
let chaine_racine_7 = chaine_racine foret_test 7 in
Printf.printf "Chaine racine de 7: %s\n" (String.concat ", " (List.map string_of_int chaine_racine_7));

(* Test de la fonction retourner_chaine *)
Printf.printf "\nTest de retourner_chaine :\n";
let chaine_a_retourner = chaine_racine foret_test 7 in
Printf.printf "Avant retourner_chaine, parent(7) = %d\n" foret_test.parent.(7);
retourner_chaine foret_test chaine_a_retourner;
Printf.printf "Après retourner_chaine, parent(7) = %d\n" foret_test.parent.(7);

(* Test de la fonction union *)
let foret_test = {
  parent = [|3; 3; 3; 3; 3; 4; 4; 6; 4; 8; 11; 11; 11; 11; 13; 13|];
  repr = [|false; true; false; true; true; false; false; true; false; false; false; true; false; true; false; false|];
  rang = [|0; 0; 0; 1; 2; 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0|]
} in
Printf.printf "\nTest de union :\n";
let r1_union = find_compresse foret_test 3 in
let r2_union = find_compresse foret_test 4 in
let new_rep = union foret_test r1_union r2_union in
Printf.printf "Union de 3 et 4, représentant: %d\n" new_rep;
Array.iteri (fun i p -> Printf.printf "parent(%d) = %d\n" i p) foret_test.parent;

(* Test de la fonction fusion_chaine *)
let foret_test = {
  parent = [|3; 3; 3; 3; 3; 4; 4; 6; 4; 8; 11; 11; 11; 11; 13; 13|];
  repr = [|false; true; false; true; true; false; false; true; false; false; false; true; false; true; false; false|];
  rang = [|0; 0; 0; 1; 2; 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0|]
} in
Printf.printf "\nTest de fusion_chaine :\n";
fusion_chaine foret_test [3;4;7];
Printf.printf "Fusion des blocs de 3, 4 et 7\n";
Array.iteri (fun i p -> Printf.printf "parent(%d) = %d\n" i p) foret_test.parent;

(* Test de la fonction ajout *)
let foret_test = {
  parent = [|3; 3; 3; 3; 3; 4; 4; 6; 4; 8; 11; 11; 11; 11; 13; 13|];
  repr = [|false; true; false; true; true; false; false; true; false; false; false; true; false; true; false; false|];
  rang = [|0; 0; 0; 1; 2; 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0|]
} in
Printf.printf "\nTest de ajout :\n";
ajout foret_test 1 5;
Array.iteri (fun i p -> Printf.printf "parent(%d) = %d\n" i p) foret_test.parent;;

(* Test de la fonction calcul_blocs *)
let graphe = 
  [|
  [2;3];
  [3];
  [0;3];
  [0;1;2;4];
  [3;5;6];
  [4;6];
  [4;5;7;8;9];
  [6];
  [6;9];
  [6;8];
  [11;12];
  [10;12;15];
  [10;11];
  [14;15];
  [13;15];
  [11;13;14]
  |];;
Printf.printf "\nTest de blocs :\n";
let blocs_non_vides = calcul_blocs graphe in
List.iter (fun l -> Printf.printf "Bloc: %s\n" (String.concat ", " (List.map string_of_int l))) blocs_non_vides;