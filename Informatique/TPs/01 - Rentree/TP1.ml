let getNext x = (900007 * x) mod 1000000007

let u n =
  let result = ref 54 in
  for i = 1 to n do
    result := getNext !result
  done;
  !result;;

let elimineDoublon nmax l =
  let estDoublon = Array.make nmax false in
  let rec aux = function
    | [] -> []
    | hd::tl -> if not estDoublon.(hd) then  
        (estDoublon.(hd) <- true; hd::aux tl)
      else aux tl
  in aux l;;

let arrayUi n =
  let arr = Array.make n (u 0) in
  for i = 1 to n - 1 do
    arr.(i) <- getNext arr.(i - 1)
  done;
  arr;;

let listeL m = Array.to_list (Array.map (fun x -> x mod m) (arrayUi m));;

let aretes n m =
  let lst = ref [] in
  let ui = arrayUi (2*m) in
  for i = 0 to m - 1 do
    let li = ui.(2*i) mod n in
    let gi = (li + 1 + (ui.(2*i+1) mod (n-1))) mod n in
      lst := (li, gi) :: !lst
  done;
  !lst;;

let voisins_de_x x n m = 
  let rec aux = function
    | [] -> []
    | (a,b)::tl when a = x -> b::aux tl
    | (a,b)::tl when b = x -> a::aux tl
    | _::tl -> aux tl
  in elimineDoublon m (aux (aretes n m))

let compteVoisins x n m = List.length (voisins_de_x x n m);;

let compteAretes n m = List.length (aretes n m);;

print_int (u 1 mod 1000); print_char '\n';
print_int (u 42 mod 1000); print_char '\n';
print_int (u 1000 mod 1000); print_char '\n';
print_int (u 1000000 mod 1000); print_char '\n';

print_char '\n';

print_int (List.length (elimineDoublon 10 (listeL 10))); print_char '\n';
print_int (List.length (elimineDoublon 5432 (listeL 5432))); print_char '\n';
print_int (List.length (elimineDoublon 98765 (listeL 98765))); print_char '\n';

print_char '\n';

print_int (compteVoisins 0 5 10); print_char '\n';
print_int (compteVoisins 0 100 300); print_char '\n';
print_int (compteVoisins 0 1234 123456); print_char '\n';
print_int (compteVoisins 0 98765 234567); print_char '\n';

print_char '\n';

print_int (compteAretes 5 10); print_char '\n';
print_int (compteAretes 100 300); print_char '\n';
print_int (compteAretes 1234 123456); print_char '\n';
print_int (compteAretes 98765 234567); print_char '\n';