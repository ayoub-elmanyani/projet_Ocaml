(* Définition du type de l'expression booléene *)

type eb = V of int | TRUE | FALSE | AND of eb * eb | OR of eb * eb | XOR of eb * eb | NOT of eb ;;   



(* Définition du type equal permettant de présenter une équation booléene *)

type equal = EQUAL of eb * eb ;;








(* la fonction "union" prend en paramètre deux listes de variables entières ,et renvoie l'union de ces deux listes *) 


let rec union l1 l2 =

 	match (l1,l2) with

	|([],[]) -> []
     	|(l1,[])-> l1
   	|([],l2)-> l2

    	|(V(i)::ll1 , V(j)::ll2) -> 
        	if i=j then V(i)::(union ll1 ll2)
        	else V(i)::V(j)::(union ll1 ll2)  
	
	|e::l,f::k -> [] ;;	




(* la fonction "variable" prend en paramètre une expression booléene , et récupère les variables de cette expression *)


let rec variable equ =

	match equ with 

	|TRUE->[]
	|FALSE->[]
	|V(a) -> [V(a)]
	|AND(p1,p2)-> union (variable p1) (variable p2)
	|OR(p1,p2)-> union (variable p1) (variable p2)
	|XOR(p1,p2)-> union (variable p1) (variable p2)
	|NOT(p)-> variable p  ;;
	  


(* la fonction "variableEqu" prend en paramètre une équation booléene , et récupère les variables du côté droit et celui du gauche de cette équation *)


let variableEqu equ = 

	match equ with 

	|EQUAL(a,b) -> union (variable a) (variable b) ;;




(* la fonction "variableSys" prend en paramètre un système d'équations booléenes , et récupère tous les variables de ce système *)





let rec variableSys systemEqu = 

	match systemEqu with

	|[]->[]
	|e::l -> union (variableEqu e) (variableSys l) ;;










(* la fonction "environment" prend en paramètre une liste de variables entières et une liste vide qui nous sera utile pour accumuler les couples (V(i),TRUE) et (V(i),FALSE) , elle renvoie tous les environements possibles *)


 
let rec environment list_var list_empty =                          

	match list_var with

	|[]->[]
	|v::[]-> ( ( (v,TRUE)::list_empty)::[]) @ ( ( (v,FALSE)::list_empty)::[] ) 
	|v::l ->  (environment l ((v,TRUE)::list_empty)) @ (environment l ((v,FALSE)::list_empty) )  ;;











(* la fonction "truth" prend en paramètre une expression booléen ne contenant qu'un connecteur logique et des TRUE ou FALSE , elle renvoie la valeur de cette expression en se basant sur les cas initiales *) 




let truth exp = 

	match exp with 
	
	|TRUE -> TRUE
	|FALSE -> FALSE
	|AND(TRUE,TRUE) -> TRUE |AND(TRUE,FALSE) -> FALSE  |AND(FALSE,TRUE) -> FALSE  |AND(FALSE,FALSE) -> FALSE
	|OR(TRUE,TRUE) -> TRUE |OR(TRUE,FALSE) -> TRUE  |OR(FALSE,TRUE) -> TRUE  |OR(FALSE,FALSE) -> FALSE
	|XOR(TRUE,TRUE) -> FALSE |XOR(TRUE,FALSE) -> TRUE  |XOR(FALSE,TRUE) -> TRUE  |XOR(FALSE,FALSE) -> FALSE
	|NOT(TRUE) -> FALSE |NOT(FALSE) -> TRUE ;;




(* la fonction "solve" prend en paramètre une expression booléenne ne contenant pas de variables , elle renvoie la valeur de cette expression en utilisant la fonction "truth" *) 

let rec solve exp = 

	match exp with 

	|V(i) -> FALSE
	|TRUE -> TRUE
	|FALSE -> FALSE
	|AND(a,b) -> truth (AND(solve a, solve b))
	|OR(a,b) -> truth (OR(solve a, solve b))
	|XOR(a,b) -> truth (XOR(solve a, solve b))
	|NOT(p) -> truth (NOT(solve p)) ;;




(* la fonction "solveEqu" prend en paramètre une équation booléenne ne contenant pas de variables , elle renvoie la valeur de vérité de l'équation en utilisant la fonction "solve" *) 


let solveEqu equ =
	
	match equ with

	|EQUAL(a,b) -> solve a = solve b ;;


(* la fonction "solveSys" prend en paramètre un système d'équations booléennes ne contenant pas de variables , elle renvoie la valeur de vérité de ce système en utilisant la fonction "solveEqu" *) 

let rec solveSys sys =
	
	match sys with

	|[]->true
	|EQUAL(a,b)::l -> (solveEqu (EQUAL(a,b))) && (solveSys l);;




(* la fonction "replace" prend en paramètre une expression booléenne et un environement , elle remplace cet environement dans l'expression *)


let rec replace exp listValue = 

	match (exp,listValue) with
	
	|(a,[]) -> a
	|(TRUE,listValue) -> TRUE
	|(FALSE,listValue) -> FALSE

	|(V(i),(x,y)::l) -> if (V(i) = x)
				then y
				else replace (V(i)) l 

	|(AND(a,b),listValue) -> AND(replace a listValue, replace b listValue) 
	|(OR(a,b),listValue) -> OR(replace a listValue, replace b listValue)
	|(XOR(a,b),listValue) -> XOR(replace a listValue, replace b listValue)
	|(NOT(p),listValue) -> NOT(replace p listValue)  ;;



(* la fonction "replaceEqu" prend en paramètre une équation booléenne et un environement , elle remplace cet environement dans l'équation *)


let replaceEqu equ listValue =

	match equ with
	
	|(EQUAL(a,b)) -> EQUAL((replace a listValue) , (replace b listValue)) ;;




(* la fonction "replaceSys" prend en paramètre un système d'équations booléennes et un environement , elle remplace cet environement dans ce système *)


let rec replaceSys systemEqu listValue = 

	match systemEqu with
	
	|[]-> []
	|(EQUAL(a,b)::l) -> (replaceEqu (EQUAL(a,b)) listValue):: (replaceSys l listValue) ;;
					




(* la fonction "evaluation" prend en paramètre un système d'équations booléennes et une liste contenant tous les environements possibles , elle renvoie une liste contenante tous les environements satisfaisants ce système *)




let rec evaluation systemEqu envir =

	match envir with

	|[] -> []
	|(e::l) -> if(solveSys (replaceSys systemEqu e)) then [e]@(evaluation systemEqu l)
				else (evaluation systemEqu l) ;;




(* la fonction "projet" prend en paramètre un système d'équations booléennes , elle renvoie tous les solutions ce système*)


let projet systemEqu = evaluation systemEqu (environment (variableSys systemEqu) []) ;;






projet [EQUAL(AND(V(1),V(2)),FALSE) ; EQUAL(OR(V(1),V(3)),V(2)) ; EQUAL(OR(V(1),AND(V(2),V(3))),TRUE )] ;;























