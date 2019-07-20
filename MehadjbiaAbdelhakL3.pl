/*lecture */

lecture([[X1,X2]|L]).

/*afficher*/

afficher([]):-!.
afficher([[X1,0]|L]):-afficher(L),write(X1).
afficher([[X1,X2]|L]):-X1>0,afficher(L),write(X1),write('*x^'),write(X2),write('+').
afficher([[X1,X2]|L]):-X1<0,afficher(L),write(X1),write('*x^'),write(X2).
puissance(X,0,1).
/*simplifier*/

simplifier([],[]).
simplifier([[X,X1],[X2,X3]|L],[[X4,X5]|K]):-X1==X3,X4 is X+X2,X5 is X1,simplifier(L,K).
simplifier([[X,X1],[X2,X3]|L],[[X4,X5]|K]):-X4 is X,X5 is X1,simplifier([[X2,X3]|L],K).

/*evaluation*/

puissance(X,0,1).
puissance(X,N,P):- N>0,
                   N1 is N-1,
                   puissance(X,N1,P1),
                   P is X*P1.

evaluer([[X1,X2]|M],V,C):-
evaluer(M,V,C2),
puissance(V,X2,C3),
C4 is X1*C3,
C is C2+C4.
evaluer([],V,0).

/*derivation*/

derivation([],[]).
derivation([[_,0]|M],V):-derivation(M,V).
derivation([[X1,X2]|M],[[X3,X4]|V]):-derivation(M,V),X2>0,X3 is X2*X1 ,X4 is X2-1.	

/*somme*/

somme([],[],[]).
somme([[X,X1]],[],[[X,X1]|K]):-somme(L,[],K).
somme([[X,X1]|L],[[X2,X3]|K],[[X4,X5]|J]):-X1==X3,X4 is X+X2,X5 is X1,somme(L,K,J).
somme([[X,X1]|L],[[X2,X3]|K],[[X4,X5]|J]):-X1<X3,X4 is X,X5 is X1,somme(L,[[X2,X3]|K],J).
somme([[X,X1]|L],[[X2,X3]|K],[[X4,X5]|J]):-X1>X3,X4 is X2,X5 is X3,somme([[X,X1]|L],K,J).

/*soustraction*/

soustraction([],[],[]).
soustraction([[X,X1]],[],[[-X,X1]|V]):-soustraction(M,[],V).
soustraction([[X,X1]|M],[[X2,X3]|V],[[X4,X5]|S]):-X1=:=X3,X7 is X-X2,X4 is X-X2,X5 is X1,soustraction(M,V,S).
soustraction([[X,X1]|M],[[X2,X3]|V],[[X4,X5]|S]):-X1<X3,X4 is -X,X5 is X1,soustraction(M,[[X2,X3]|V],S).
soustraction([[X,X1]|M],[[X2,X3]|V],[[X4,X5]|S]):-X1>X3,X4 is -X2,X5 is X3,soustraction([[X,X1]|M],V,S).			   

/*produit*/

mult(_,[],[]).
mult([X,X1],[[X2,X3]|L],[[X4,X5]|T]):-X4 is X*X2,X5 is X3+X1,mult([X,X1],L,T).
concact([[X,X1]|[]],L,[[X,X1]|L]).
concact([[X,X1]|L],K,[[X,X1]|R]):-concact(L,K,R).
production([],_,[]).
production([[X,X1]|L],P,[I|K]):-mult([X,X1],P,[Y|T]),concact(Y,T,I),production(L,P,K).


/*2eme partie */



p-->const,est,suivant.
const-->[p].
suivant-->function|operat.
function-->[deri] |[simpl] .
operat-> [+]|[-]|[*].
:-op(700,xfy,est).
:-op(500, yfx,-).
:-op(500, yfx,*).
:-op(500, yfx,+).


/* interrogate  prolog */

// addtion 

--1 somme([[1,2]],[[1,2]],N).
N=[[2,2]]


// calculate the polynomial with a number

--2 evaluer([[1,2],[1,1]],2,N).
N=6

// soustraction

--3 soustraction([[1,2],[1,1]],[[1,2]],N).
N = [[0, 2], [- 1, 1]] 

// derivation

--4  derivation([[1,2],[1,1]],N).
N = [[2, 1], [1, 0]].

// calculate power  example x2 and x=5  => result = 25

--5  puissance(5,2,P).
P=25

// print the list us reel polynome
--6 afficher([[1,2],[2,2],[3,3]]).

3*x^3+2*x^2+1*x^2











