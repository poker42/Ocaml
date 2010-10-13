(** Aetoile.ml **)

(**************************************************************************)	
let etat_aux
(**************************************************************************)
(ft : ('a -> ('b * ('a * int)) list))   	  (** Fonction de transition **)
(e : 'a)									  (** Etat ********************)
(op : 'b)									  (** Opération ***************)
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
	estim  :  int };;

(**************************************************************************)
let creerChemin
(ft : ('a -> ('b * ('a * int)) list))  		  (** Fonction de transition **)
(h  : ('a -> int))
(e  : 'a)
(op : 'b list)
:('a,'b) chemin
=
{
	depart = e;
	ops = op;
	final = etatFinal ft e op;
	cout = coutParcouru ft e op;
	estim = h e;
	};;
	
# let f = creerChemin g1.opPoss g1.hEtat "A" ["b";"h";"h"];
