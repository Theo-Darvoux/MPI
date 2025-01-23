type case = Blanc | Noir | Vide

type configuration = 
{
    plateau : case array array;
    joueur : case;
    nombre_passe : int;
}

type coup = int * int

let dedans (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

let configuration_initiale () =
    let plateau = Array.make_matrix 8 8 Vide in
    plateau.(3).(3) <- Blanc; plateau.(4).(4) <- Blanc;
    plateau.(3).(4) <- Noir; plateau.(4).(3) <- Noir;
    { plateau; joueur = Noir; nombre_passe = 0 }

let joueur_oppose joueur = match joueur with
    | Blanc -> Noir
    | Noir -> Blanc
    | Vide -> Vide

let trouver_direction plateau joueur (x, y) (dx, dy) =
    let i = ref 1 in
    while dedans (x+ !i*dx, y+ !i*dy) && plateau.(x+ !i*dx).(y+ !i*dy) = joueur_oppose joueur do
        i := !i+1;
    done;
    if not (dedans (x+ !i*dx, y+ !i*dy)) || plateau.(x+ !i*dx).(y+ !i*dy) = Vide then
        0
    else
        !i-1

let retourner_direction plateau joueur (x, y) (dx, dy) =
    let n = trouver_direction plateau joueur (x, y) (dx, dy) in
    if n <= 0 then false
    else 
        begin
        for i = 1 to n do
            let nx, ny = x + i * dx, y + i * dy in
            plateau.(nx).(ny) <- joueur
        done;
        true
    end

let retourner plateau joueur (x, y)=
    if not (dedans (x, y)) || plateau.(x).(y) <> Vide then plateau, false
    else
        let nouveau_plateau = Array.map Array.copy plateau in
        nouveau_plateau.(x).(y) <- joueur;
        let directions = [ (0, 1); (1, 1); (-1, 1); (1, 0); (-1, 0); (-1, -1); (0, -1); (1, -1) ] in
        let rec aux l = match l with
            | [] -> false
            | (dx,dy)::q -> let b = retourner_direction nouveau_plateau joueur (x,y) (dx,dy)
                            in aux q || b
        in nouveau_plateau,aux directions

let est_legal plateau joueur coup =
    let x,y = coup in
    let nouveau_plateau,b = retourner plateau joueur coup in b && plateau.(x).(y) = Vide

let gagnant config =
    let blancs = ref 0 in
    let noirs = ref 0 in
    Array.iter (fun ligne ->
        Array.iter (function
            | Blanc -> incr blancs
            | Noir -> incr noirs
            | Vide -> ()
        ) ligne
    ) config.plateau;
    if !blancs > !noirs then Blanc
    else if !noirs > !blancs then Noir
    else Vide

let coups_possibles config = 
    let plateau = config.plateau in
    let joueur = config.joueur in
    let rec aux i j =
        if j = 8 && i = 8 then []
        else if j = 8 then aux (i+1) 0
        else (i,j)::(aux i (j+1))
    in
    let rec garde_si_legal l = match l with
        | [] -> []
        | (i,j)::q -> let nouvelle_config, est_legal = retourner plateau joueur (i,j) in
                    if est_legal then nouvelle_config::(garde_si_legal q)
                    else garde_si_legal q
    in let creer_config plateau = 
        {
            plateau = plateau;
            joueur = joueur_oppose config.joueur;
            nombre_passe = 0
        }
    in let l = aux 0 0
    in let res = garde_si_legal l in
    if res = [] then 
        begin
            let nouvelle_config = { plateau = Array.map (Array.copy) config.plateau;
                                    joueur = joueur_oppose config.joueur;
                                    nombre_passe = config.nombre_passe + 1}
            in
            if nouvelle_config.nombre_passe = 2 then []
            else [nouvelle_config] 
        end
    else List.map creer_config res