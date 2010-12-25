(* Aetoile.ml  mise à jour 25/12 *)

(**************************************************************************)	
let etat_aux
(**************************************************************************)
(ft : ('a -> ('b * ('a * int)) list))   	  (** Fonction de transition **)
(e : 'a)									  (** Etat ********************)
(op : 'b)									  (** Opération à effectuer ***************)
 = let res = ft e in List.filter (fun (x,(l,v)) -> x = op) res;;
 
 
(**************************************************************************)
let etatSuivant
(**************************************************************************)
(ft : ('a -> ('b * ('a * int)) list))         (** Fonction de transition **)
(e : 'a)   							          (** Etat ********************)
(op : 'b) 							          (* Opération à effectuer ****)
: 'a
(**************************************************************************)
= let n = etat_aux ft e op in match n with
[] -> failwith "Erreur etat suivant"
|(x,(s,v))::r -> s;; 

(** Exmple :   etatSuivant g1.opPoss "B" "b";;  **)

(**************************************************************************)	
let coutOp
(**************************************************************************)	
(ft : ('a -> ('b * ('a * int)) list))  		  (** Fonction de transition **)
(e : 'a)   							   		  (** Etat ********************)
(op : 'b) 							   		  (* Opération à effectuer ****)
: int
(**************************************************************************)
= let n = etat_aux ft e op in match n with
|[] -> failwith "Erreur etat coutOp"
|(x,(s,v))::r -> v;; 

(** Exemple :    coutOp g1.opPoss "B" "b";; **)


(** Chemins **)

(**************************************************************************)
let rec etatFinal
(**************************************************************************)
(ft : ('a -> ('b * ('a * int)) list))  		  (** Fonction de transition **)
(e : 'a)   							   		  (** Etat ********************)
(op : 'b list) 						   		  (* Opération à effectuer ****)
: 'a
(**************************************************************************)
= match op with
[] -> e
| a::r -> let s = etatSuivant ft e a in etatFinal ft s r;;

(** Exemple:      etatFinal g1.opPoss "A" ["b";"h";"h"];; **)


(**************************************************************************)
let rec coutParcouru
(**************************************************************************)
(ft : ('a -> ('b * ('a * int)) list))  		  (** Fonction de transition **)
(e : 'a)   							   		  (** Etat ********************)
(op : 'b list) 						   		  (* Opération à effectuer ****)
: int
(**************************************************************************)
= match op with
[] -> 0
| a::q -> let s = etatSuivant ft e a
	in let r = coutOp ft e a
		in r+coutParcouru ft s q;;

(** Exemple:		coutParcouru g1.opPoss "A" ["b";"h";"h"];;  *)

(**************************************************************************)
type ('etatT, 'opT) chemin =
{ 
	depart : 'etatT;
	ops    : 'opT list;
	final  : 'etatT;
	cout   :  int;
	estim  :  int; };;

	
(**************************************************************************)
let creerChemin
(**************************************************************************)
(ft : ('a -> ('b * ('a * int)) list))  		  (** Fonction de transition **)
(h  : ('a -> int))
(e  : 'a)
(op : 'b list)
:('a,'b) chemin
(**************************************************************************)
=
{
	depart = e;
	ops = op;
	final = etatFinal ft e op;
	cout = coutParcouru ft e op;
	estim = h (etatFinal ft e op);
	};;
		
(** Exemple: **)
	let ch1 = creerChemin g1.opPoss g1.hEtat "A" ["b";"h";"h"];;
	let ch2 = creerChemin g1.opPoss g1.hEtat "A" ["h";"b"];;
	let ch3 = creerChemin g1.opPoss g1.hEtat "A" ["b"];;


(**************************************************************************)	
(* Fonction auxiliaire pour afficherChemin								  *)
(**************************************************************************)
let rec aff_aux ft d op cu ct = match op with
[] -> print_string d; print_string " "; print_string "cout: ";print_int cu; print_string " estim: "; print_int ct; print_newline();
| a::b -> let s = etatSuivant ft d a in begin 
print_string d; print_string " "; print_string a; print_string " "; end; aff_aux ft s b cu ct;;
	
	
(**************************************************************************)
let afficherChemin
(**************************************************************************)
(fpe : 'a -> unit)
(fpo : 'b -> unit)
(ft  : ('a -> ('b * ('a * int)) list))
(ch  : ('a,'b) chemin)
(**************************************************************************)
= aff_aux ft ch.depart ch.ops ch.cout ch.estim;; 

(*	afficherChemin print_string print_string g1.opPoss ch1;; *)



(**************************************************************************)
let supprimerEtat
(**************************************************************************)
(e : 'a)
(lch : ('a,'b) chemin list)
: ('a,'b) chemin list
(**************************************************************************)
= List.filter (fun x -> x.final <> e) lch;;



(**************************************************************************)
let rec insererChemin_aux
(**************************************************************************)
(ch1 : ('a,'b) chemin )
(lch : ('a,'b) chemin list)
: ('a,'b) chemin list
(**************************************************************************)
= 
match lch with 
[] -> [ch1]
| a::r ->  if (ch1.cout + ch1.estim)<(a.cout + a.estim) 
			then ch1::lch
			else a::insererChemin_aux ch1 r;;

			
let ch4 = creerChemin g1.opPoss g1.hEtat "A" ["h";"h"];;
let ch5 = creerChemin g1.opPoss g1.hEtat "A" ["b";"h"];;

(** Exemple : 		insererChemin ch5 lch1;;  *)

(** (Filtre les chemins avec le même élément final 
	ET un coût+estim inférieur ou égal à l'élément à inserer)
    OU avec un état final différent **)
let suppr_doublon e lch = List.filter (fun x -> x.final <> e.final || (x.final = e.final && (x.cout+x.estim)<=(e.cout+e.estim))) lch;;

(**************************************************************************)
let insererChemin e lch = suppr_doublon e (insererChemin_aux e lch);;
(**************************************************************************)

(**************************************************************************)
(* obtenir l'état suivant
	obtenir le chemin qui va jusqu'à cet état
*)
(**************************************************************************)

let rec fils li = match li with
[] -> []
| a::b -> a::fils b;;

let rec filtrer m = match m with
[] -> []
| (a,(b,c))::r -> a::filtrer r;;

let getChemins
(ft : ('a -> ('b * ('a * int)) list))  
(fh : ('a -> int))
(ch : ('a,'b) chemin)
= let r = fils (filtrer (ft ch.final)) in r;;

(**************************************************************************)
let rec creerFils ft fh ch li = let m = ch.ops in match li with
[] -> []
| a::b -> (creerChemin ft fh ch.depart (m@a::[]))::creerFils ft fh ch b;;


(**************************************************************************)
type 'a etatCout =
{ etat: 'a;
  coutreel : int};;
(**************************************************************************)


let simp1 = [{etat="C";coutreel=3};{etat="D";coutreel=11};{etat="E";coutreel=8}];;
(* List.filter (fun x -> testSimplif ch5 x) simp2;;  *)
(**************************************************************************)

(***************************************************************)
let rec interetSimplif
(***************************************************************)
(ch : ('a, 'b) chemin)
(lvus : 'a etatCout list)
(***************************************************************)
(* Retourne si le chemin est interessant par rapport aux       *)
(* chemins vus et retourne la liste de chemins vus mise a jour *)
(***************************************************************)
: bool * 'a etatCout list
(***************************************************************)
= match lvus with
  [] -> (true, [])
| (ec::r) -> let (b, l) = (interetSimplif ch r) in
	if (ch.final = ec.etat) then
		if (ch.cout < ec.coutreel) then
			(true && b, l)
		else
			(false, ec::l)
	else
		(true && b, ec::l);;
(***************************************************************)

(*

 interetSimplif ch5 [{etat="C";coutreel=3};{etat="D";coutreel=11};{etat="E";coutreel=8}];;
	interetSimplif ch5 [{etat="C";coutreel=3};{etat="D";coutreel=5};{etat="E";coutreel=8}];;
 *)
 

(** retourne le couple formé de la liste d'attente latt
dans laquelle on a inséré les chemins intéressants de lch
et la liste lvus qui a pu subir des suppressions lors de test
d'intérêts

Soit la liste d'attente et la liste vue
*)
(***************************************************************)
let rec insererListeChemins
(***************************************************************)
(lch: ('a,'b) chemin list)
(latt: ('a,'b) chemin list)
(lvus: 'a etatCout list)
: ('a,'b) chemin list * 'a etatCout list
(***************************************************************)
= (lch, lvus);;


let list_simp l = match l with
(a, []) -> []
| (a, b) -> b;;

let rec insererListeChemins lch latt lvus = match lch with
[] -> ([],[])
| a::r -> let (b,l) = (insererListeChemins r latt lvus) in
((insererChemin a latt), list_simp(interetSimplif a lvus)@l);;
		


	(***************************************************************)let rec aEtoile teb ft fh fA lvus = match fA with
[] -> failwith "pas de solution"
| a::r -> if(teb a) then a else


let rec maux 

		
		
		
		
		
		
		
		
		
	
 