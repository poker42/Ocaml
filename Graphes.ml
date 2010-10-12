(* Fichier Graphes.ml - version incompl�te - ao�t 2010
   Ma�trise d'Informatique - module TC3 - 2010-2011

   Ce fichier contient des graphes sur lesquels tester un
   algorithme de recherche dans un graphe d'�tat.

   On d�finit d'abord un type g�n�rique pour repr�senter ces graphes.
*)


type ('etatT,'transT) graphe =
{ init      : 'etatT ;
  estBut    : 'etatT -> bool ;
  opPoss    : 'etatT -> ('transT * ('etatT * int)) list ;
  hEtat : 'etatT -> int ; }

(* Une description incompl�te du graphe g1 de l'�nonc�,
   � compl�ter *)

let g1 =
{ init = "A" ;
  estBut = (function "G" -> true | "H" -> true | _ -> false) ;
  opPoss = (function
    "A" -> ["h",("B",4); "b",("C",3)]
  | "B" -> ["h",("E",4); "b",("D",7)]
  | "C" -> ["h",("D",2); "b",("F",9)]
  | "D" -> ["h",("G",6)]
  | "E" -> ["b",("G",4)]
  | "F" -> ["h",("G",7); "b",("H",9)]
  | _ -> [] );
  hEtat = (function
    "A" -> 7
  | "B" -> 3	
  | "C" -> 5
  | "D" -> 2
  | "E" -> 2
  | "F" -> 7
  | _ -> 0 );}

(* Description du graphe g2 *)

let g2 =
{ init = "A" ;
  estBut = (function "G" -> true | "H" -> true | _ -> false) ;
  opPoss = (function
    "A" -> ["h",("B",4); "b",("C",3)]
  | "B" -> ["h",("E",7); "b",("D",5)]
  | "C" -> ["h",("D",2); "b",("F",9)]
  | "D" -> ["h",("G",6)]
  | "E" -> ["b",("G",2)]
  | "F" -> ["h",("G",7); "b",("H",9)]
  | _ -> [] );
  hEtat = (function
    "A" -> 7
  | "B" -> 3	
  | "C" -> 6
  | "D" -> 4
  | "E" -> 1
  | "F" -> 7
  | _ -> 0 );}

let g3 =
{ init = "A" ;
  estBut = (function "G" -> true | "H" -> true | _ -> false) ;
  opPoss = (function
    "A" -> ["h",("B",4); "b",("C",2)]
  | "B" -> ["h",("E",7); "b",("D",2)]
  | "C" -> ["h",("D",3); "b",("F",9)]
  | "D" -> ["h",("G",4); "b",("F",1)]
  | "E" -> ["b",("G",2)]
  | "F" -> ["h",("G",7); "b",("H",9)]
  | _ -> [] );
  hEtat = (function
    "A" -> 7
  | "B" -> 3	
  | "C" -> 6
  | "D" -> 4
  | "E" -> 1
  | "F" -> 7
  | _ -> 0 );}

(* g4 permet de tester la gestion de la pile d'attente:
  l'�tat c est mis en attente comme fils de a, associ�
  � un co�t de 10, mais en traitant le premier fils de a,
  qui est b, on rencontre � nouveau c associ� cette
  fois-ci � un co�t de 6: il faut donc modifier la pile
  d'attente.
*)

let g4 =
{ init = "A";
  estBut =(function "F" -> true | "G" -> true | _ -> false);
  opPoss =(function
	 "A"-> ["h",("B",4); "b",("C",10)]
	| "B"-> ["b",("C",2); "h",("D",4)]
	| "C"-> ["b",("F",6)]
	| "D"-> ["b",("C",2)]
	|  _ -> []) ;
  hEtat =(function
      "A"->8
	| "B"->3
	| "C"->2
	| "D"->5
	|  _ ->0) ;
}


(* g5 permet de tester la gestion de la liste des
    �tats vus: a a deux successeurs b et c. b ne
    permet pas d'arriver � l'�tat but � un co�t optimal,
    il faut d'abord passer par c qui revient ensuite sur
    b avec un co�t inf�rieur, alors que b est d�j� dans vus.
*)

let g5 =
{ init = "A" ;
  estBut = (function "D" -> true | _ -> false) ;
  opPoss = (function
    "A" -> ["h",("B",10) ; "b",("C",4)]
  | "B" -> ["h",("D",5)]
  | "C" -> ["h",("B",4)]
  | "D" -> []
  | _->[] ) ;
  hEtat = (function
    "A" -> 10
  | "B" -> 2
  | "C" -> 5
  | "D" -> 0
  |_->0) ;
}

(* g6 permet de tester la gestion des cycles: c'est une
  boucle � deux noeuds sans �tat but. *)

let g6 =
{ init = "A" ;
  estBut = (function _ -> false) ;
  opPoss = (function
    "A" -> ["h",("B",2)]
  | "B" -> ["h",("A",2)]
  |_->[]) ;
  hEtat = (function
    "A"-> 4
  | "B" -> 3
  |_->0) ;
}
