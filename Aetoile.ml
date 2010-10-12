(** Aetoile.ml **)

(**************************************************************************)	
let etat_aux
(**************************************************************************)
(ft : ('a -> ('b * ('a * int)) list))   	  (** Fonction de transition **)
(e : 'a)									  (** Etat ********************)
(op : 'b)									  (** Op�ration ***************)
 = let res = ft e in List.filter (fun (x,(l,v)) -> x = op) res;;
 
 
(**************************************************************************)
let etatSuivant
(**************************************************************************)
(ft : ('a -> ('b * ('a * int)) list))         (** Fonction de transition **)
(e : 'a)   							          (** Etat ********************)
(op : 'b) 							          (* Op�ration � effectuer ****)
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
(op : 'b) 							   		  (* Op�ration � effectuer ****)
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
(op : 'b list) 						   		  (* Op�ration � effectuer ****)
: 'a
= match op with
[] -> failwith "Erreur etat final"
| a::r -> let s = etatSuivant ft e a in etatFinal ft e s;;






