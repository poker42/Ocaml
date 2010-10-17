	(***********************************)
let insererEnTete
(***********************************)
( x : 'a) (** un objet de type quelconque *)
(l : 'a list) (* une liste d'éléments du meme type que x *)
(*******************)
(* retourne une liste formée en ajoutant x en tete de l *)
: 'a list
(*******************)
= x::l;;


(***********************************)
let rec insererEnQueue 
(***********************************)
(x :'a) (* element de type quelconque *)
(l : 'a list) (* une liste quelconque *)
: 'a list (* retourne une liste de quelconque *)
 =
match l with
[] -> x::[]  (** si vide on insere x dans la liste vide *)
| h::s -> h::insererEnQueue x s;; (** si element on l'ajoute en tete *)

(***********************************)
let rec somme = function
(***********************************)
[] -> 0   (* liste vide on renvoie 0 *)
| x::r -> x+somme r;;  (* on ajoute la valeur de la tete *)
(* retourne un type int *)

(***********************************)
let maxi
(***********************************)
 (x : int)   (* premiere valeur *)
 (y : int)   (* seconde valeur *)
 : int
 =
 if x<y then y else x;;  (** renvoie la plus grande des deux *)

(***********************************)
let maximum
(***********************************)
 (l : int list) (* la liste *)
 : int
 =
 List.fold_right maxi l 0;; (** applique la fonction max à la liste l**)


(***********************************)
let rec insererEntierDansListeOrdonnee
(***********************************)
(x : int) (* element à insérer *)
(l : int list)
: int list (* liste ordonnée *)
 = match l with  (* on regarde l *)
[] -> [x]  (* si liste vide on renvoie juste la liste avec x *)
| h::r -> if x<h then x::l (* si x<h on a trouvé la position à insérer *)
                 else h::insererEntierDansListeOrdonnee x r;; (* sinon on continu de chercher *)


(*** Liste d'associations ***)
let l = [2,'b';3,'c';4,'z';8,'p'];;

(***********************************)
(** MiniArec : renvoie le couple   *)
(** dont la valeur est minimum    **)

(** Entrée : 'a list              **)
(** Sortie : 'a                   **)
let minimumArec = function
(***********************************)
	|[] -> failwith "Erreur"
	|t::q ->List.fold_left min t q
;;

(*************************************)
(** minimumA : Renvoie l'objet associé
à la valeur minimum du couple        *)
(* param : ('a * 'b) list        *****)
(* Sortie : 'b                    ****)
let minimumA l = let (a,b) = minimumArec l in b;; 

(*************************************)
let rec insererA
(*************************************)
(n,a)   (**** le couple **************)
l       (**** la liste  **************)
(*************************************)
= match l with
[] -> []
  | (v,e)::r -> if a==e && v<=n (* si l'élément est présent et <=*)
	        then (v,e)::insererA (n,a) r
                else if a==e && n<v
                      then (n,a)::l;;
					  

let rec contient (n,t) = function
  | [] -> false
  | (a,v)::q when v = t -> true
  | _::q -> contient (n,t) q;;

let elimine_doublon l = 
  let rec aux acc = function
    | [] -> List.rev acc
    | t::q when contient t acc -> aux acc q
    | t::q -> aux (t::acc) q in
  aux [] l;;
  
(** Insérer un couple dans une liste ordonnée*)  
let rec insererCouple
n
l
 = match l with
	[] -> [n]
	| e::r -> if  n<e
					then n::l
					else e::insererCouple n r;;
										       
let insererA n l = elimine_doublon (insererCouple n l);;


