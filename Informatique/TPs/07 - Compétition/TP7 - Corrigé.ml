let creer_v n u0 = 
  let t = Array.make n u0 in
  for i = 1 to n-1 do
    t.(i) <- (101833*t.(i-1)) mod 1000000007;
  done;
  t

let creer_u n u0 = 
  let tv = creer_v n u0 in
  let tu = Array.make n u0 in
  for i = 1 to n-1 do
    tu.(i) <- tv.(i mod 1000003)
  done;
  tu

  let rec terme_v n u0 = 
    if n = 0 then u0 else terme_v (n-1) ((101833*u0) mod 1000000007)
    
let terme_u n u0 = terme_v (n mod 1000003) u0

let automate u0 nL nQ = let tu = creer_u (nQ*(nL+1)) u0 in 
  let delta = (fun q -> fun c -> tu.(q*nL+c) mod nQ) in
  let f = (fun q -> tu.(nQ*nL+q) mod 2 = 1) in 
  (nL, nQ, delta, f)

let nombre_acceptants u0 nL nQ = 
  let (nL,nQ,delta,est_final) = automate u0 nL nQ in
  let nombre = ref 0 in
  for i = 0 to (nQ-1) do
    if est_final i then nombre := !nombre +1;
  done;
  !nombre

let nombre_transition u0 nL nQ = 
  let (nL,nQ,delta,est_final) = automate u0 nL nQ in
  let nombre = ref 0 in
  for q = 0 to (nQ-1) do
    for c = 0 to (nL-1) do
      if est_final (delta q c) then nombre := !nombre+1;
    done;
  done;
  !nombre

let mot u0 l t = 
  let tableau = creer_u t u0 in
  let rec aux i = match i with
    | -1 -> []
    | _ -> (tableau.(i) mod l)::(aux (i-1))
  in List.rev (aux (t-1))

let rec afficher l = match l with
  | [] -> ()
  | h::q -> print_int h; print_string " "; afficher q

let etat u0 nL nQ nT =
  let (nL,nQ,delta,est_final) = automate u0 nL nQ in
  let rec delta_star q l = match l with
    | [] -> q
    | c::tl -> delta_star (delta q c) tl
  in delta_star 0 (mot u0 nL nT)

let compter t n =
  let rec aux i = 
    if i = n then 0 else
      aux (i+1) + (if t.(i) then 1 else 0)
  in aux 0

let accessibles u0 nL nQ =
  let vus = Array.make nQ false in  
  let (nL,nQ,delta,est_finaux) = automate u0 nL nQ in
  let rec parcours q c = 
    if c<nL then 
      begin
      let etat = delta q c in
      if not vus.(etat) then
        begin
        vus.(etat)<-true;
        parcours etat 0;
        end;
      parcours q (c+1);
      end
  in 
  parcours 0 0;
  compter vus nQ

let plus_court u0 nL nQ =
  let vus = Array.make nQ false in  
  let (nL,nQ,delta,est_finaux) = automate u0 nL nQ in
  let rec ajouter_voisins q c liste = 
    if c = nL || vus.(q) then begin
      vus.(q)<-true; 
      liste
    end
    else
      ajouter_voisins q (c+1) ((delta q c)::liste)
  in
  let rec parcours tete queue = 
    match tete with
    | [] -> if queue = [] then -1 else 1+parcours queue []
    | q::tl -> if est_finaux q then 0 else parcours tl (ajouter_voisins q 0 queue)
  in 
  parcours [0] []
;;

print_string "Automates\n";

let u0 = 50 in

print_string "Question 1 :\n";
print_int ((terme_u 1 u0) mod 1000);
print_string "\n";
print_int ((terme_u 42 u0) mod 1000);
print_string "\n";
print_int ((terme_u 1000000000 u0) mod 1000);
print_string "\n\n";

print_string "Question 2 :\n";
print_int (nombre_acceptants u0 2 100);
print_string "\n";
print_int (nombre_acceptants u0 10 10000);
print_string "\n";
print_int (nombre_acceptants u0 3 30000);
print_string "\n\n";

print_string "Question 3 :\n";
print_int (nombre_transition u0 2 100);
print_string "\n";
print_int (nombre_transition u0 10 10000);
print_string "\n";
print_int (nombre_transition u0 3 30000);
print_string "\n\n";

print_string "Question 4 :\n";
afficher (mot u0 4 3);
print_string "\n";
afficher (mot u0 8 4);
print_string "\n";
afficher (mot u0 10 5);
print_string "\n\n";

print_string "Question 5 :\n";
print_int (etat u0 2 100 100);
print_string "\n";
print_int (etat u0 10 10000 50000);
print_string "\n";
print_int (etat u0 3 30000 100000);
print_string "\n\n";

print_string "Question 6 :\n";
print_int (accessibles u0 2 100);
print_string "\n";
print_int (accessibles u0 10 10000);
print_string "\n";
print_int (accessibles u0 3 30000);
print_string "\n\n";

print_string "Question 7 :\n";
print_int (plus_court u0 2 100);
print_string "\n";
print_int (plus_court u0 10 10000);
print_string "\n";
print_int (plus_court u0 3 30000);
print_string "\n\n"

;;


















type grand_nombre =
  | Zero
  | Un
  | Noeud of grand_nombre*grand_nombre*grand_nombre

let m = 2147483647 

let creer_u u0 n = 
  let t = Array.make n u0 in
  for i = 1 to n-1 do
    t.(i) <- (16807*t.(i-1)+17) mod m;
  done;
  t

let rec terme_u u0 n = 
  if n = 0 then u0 else terme_u ((16807*u0+17) mod m) (n-1)

let rec pow n = match n with
    | 0 -> 1
    | 1 -> 2
    | n when n mod 2 = 0 -> let x = pow (n/2) in x*x
    | _ -> 2*(pow(n-1))

let terme_v u0 k n = ((terme_u u0 n) mod (pow k)) + (pow k)

let question2 u0 k = (terme_v u0 k (97*k mod 997)) mod 101

let rec decompose n = match n with
    | 0 -> Zero
    | 1 -> Un
    | n -> let p = ref 0 in
          let x = ref (pow (pow !p)) in
          while n / !x >= !x do
            p:= !p+1;
            x := (pow (pow !p));
          done;
          Noeud((decompose (n mod !x)), decompose (!p) , decompose (n/ !x))
  
let rec max x y z =
  if x > y then (if x>z then x else z)
  else (if y>z then y else z)

let rec hauteur arbre = match arbre with
  | Zero -> 1
  | Un -> 1
  | Noeud(g,p,d) -> 1+ max (hauteur g) (hauteur p) (hauteur d)

let question3 u0 k n = hauteur (decompose (terme_v u0 k n))

let rec est_pair x = match x with
  | Un -> false
  | Zero -> true
  | Noeud(g,p,d) -> est_pair g

let rec signature u0 x = match x with
  | Zero -> 0
  | Un -> (terme_u u0 10) mod 97
  | Noeud(g,p,d) when est_pair p -> ((signature u0 g)+ (terme_u u0 30)*(signature u0 d)) mod 97
  | Noeud(g,p,d) -> ((signature u0 g)+ (terme_u u0 20)*(signature u0 d)) mod 97
        
let question4 u0 k n = signature u0 (decompose (terme_v u0 k n))

let rec h n = match n with
  | 0 -> Un
  | n -> let x = h (n-1) in Noeud(x,x,x)

let rec signaturebis u0 x = match x with
  | Zero -> 0
  | Un -> (terme_u u0 10) mod 97
  | Noeud(g,p,d) -> let x = (signaturebis u0 g) in 
        if est_pair p then (x+ (terme_u u0 30)*x) mod 97
        else (x+ (terme_u u0 20)*x) mod 97

let question5 u0 k =
  let x = terme_v u0 k (7*k) in
  signaturebis u0 (h x)

let rec generer u0 k n = match k with
  | 0 -> if (terme_u u0 n) mod 7 = 0 then
          Zero
        else
          Un
  | _ -> let k' = (if k-1-((terme_u u0 n) mod 2) < 0 then 0 else k-1-((terme_u u0 n) mod 2)) in
      let g = generer u0 k' ((n+1) mod 997) in
      let p = terme_v u0 k' n in
      let d = generer u0 k' ((n+2) mod 997) in
      begin
        match d with
          | Zero -> g
          | _ -> Noeud(g,decompose p,d)
      end

let question6 u0 k n = hauteur (generer u0 k n)

let question7 u0 k n = signature u0 (generer u0 k n)
  ;;

print_string "Grands nombres\n";

let u0 = 50 in

print_string "Question 1 :\n";
print_int ((terme_u u0 5) mod 101);
print_string "\n";
print_int ((terme_u u0 100) mod 101);
print_string "\n";
print_int ((terme_u u0 997) mod 101);
print_string "\n\n";

print_string "Question 2 :\n";
print_int (question2 u0 5);
print_string "\n";
print_int (question2 u0 30);
print_string "\n";
print_int (question2 u0 61);
print_string "\n\n";

print_string "Question 3 :\n";
print_int (question3 u0 1 10);
print_string "\n";
print_int (question3 u0 2 20);
print_string "\n";
print_int (question3 u0 32 30);
print_string "\n";
print_int (question3 u0 61 40);
print_string "\n\n";

print_string "Question 4 :\n";
print_int (question4 u0 1 10);
print_string "\n";
print_int (question4 u0 2 20);
print_string "\n";
print_int (question4 u0 32 30);
print_string "\n";
print_int (question4 u0 61 40);
print_string "\n\n";

print_string "Question 5 :\n";
print_int (question5 u0 0);
print_string "\n";
print_int (question5 u0 2);
print_string "\n";
print_int (question5 u0 4);
print_string "\n";
print_int (question5 u0 8);
print_string "\n\n";

print_string "Question 6 :\n";
print_int (question6 u0 6 35);
print_string "\n";
print_int (question6 u0 8 45);
print_string "\n";
print_int (question6 u0 10 55);
print_string "\n";
print_int (question6 u0 12 65);
print_string "\n";
print_int (question6 u0 14 75);
print_string "\n\n";

print_string "Question 7 :\n";
print_int (question7 u0 6 35);
print_string "\n";
print_int (question7 u0 8 45);
print_string "\n";
print_int (question7 u0 10 55);
print_string "\n";
print_int (question7 u0 12 65);
print_string "\n";
print_int (question7 u0 14 75);
print_string "\n\n";