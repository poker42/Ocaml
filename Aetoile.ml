(** Aetoile.ml **)


(**************************************************************************)
let etat_aux
(**************************************************************************)
(ft : string -> (string * (string * int)) list) (* fonction de transition *)
(e : string) (* Un état donné *)
(op : string) (* Opération à effectuer *)
(**************************************************************************)
 = let res = ft e in List.filter (fun (x,(l,v)) -> x = op) res;;
	

(**************************************************************************)
let etatSuivant
(ft : string -> (string * (string * int)) list) (* fonction de transition *)
(e : string) (* Un état donné *)
(op : string) (* Opération à effectuer *)
(**************************************************************************)
= let n = etat_aux ft e op in match n with
[] -> failwith "Erreur etat suivant"
|(x,(s,v))::r -> s;; 

etatSuivant g1.opPoss "B" "b";;

(**************************************************************************	
let coutOp
(ft : string -> (string * (string * int)) list) (* fonction de transition *)
(e : string) (* Un état donné *)
(op : string) (* Opération à effectuer *)
(**************************************************************************
= let n = etat_aux ft e op in match n with
|[] -> failwith "Erreur etat suivant"
|(x,(s,v))::r -> v;; 

coutOp g1.opPoss "B" "b";;