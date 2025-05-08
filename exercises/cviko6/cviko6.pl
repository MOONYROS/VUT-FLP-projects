:- dynamic velikost/2, pozice/2.

% ======= UKOL 1 =======
% tohle skipnul, protoze to nechtene leaknul
prvek(H, [H|_]) :- !.
prvek(H, [_|T]) :- prvek(H, T).

rozdil([], _, []).
rozdil([H|T], S, R) :- prvek(H, S), !, rozdil(T, S, R).
rozdil([H|T], S, [H|P]) :- rozdil(T, S, P).

% ======= UKOL 2 =======
% ------- 2a -------
sequence(0, []) :- !.
sequence(N, [N|T]) :- Nn is N - 1, sequence(Nn, T).

% ------- 2b -------
queens(Solution) :- queens(8, Solution).
queens(N, Solution) :-
    sequence(N, List),
    permutation(List, Solution),
    test(Solution).
	
% test(Solution) :- true.

% ------- 2c -------
test([]) :- !.
test([H|T]) :-
    test(H ,1 ,T),
    test(T).
test(_, _, []) :- !.
test(Pos, Dist, [H|T]) :-
    Pos \= H,
    X is abs(Pos - H),
    X \= Dist,
    Dn is Dist + 1,
    test(Pos, Dn, T).

% ======= UKOL 3 =======
% cesty(3,3,1,1,3,3) -> 2
% cesty(4,4,1,1,4,4) -> 138

% idk, tohle jsem missnul
% cesty(XR, YR, XS, YS, XE, YE, N) :-

% ------- 3a -------
testPoz(X,Y) :-
    velikost(XR, YR),
    X > 0, Y > 0,
    X < XR, Y < YR.

skok(X,Y,XN,YN) :- XN is X + 2, YN is Y + 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X + 2, YN is Y - 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 2, YN is Y + 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 2, YN is Y - 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X + 1, YN is Y + 2, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 1, YN is Y + 2, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X + 1, YN is Y - 2, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 1, YN is Y - 2, testPoz(XN, YN).

% ------- 3b -------
% ?- cesta(1,1,3,3, Cesta).
% Cesta = [1:1, 3:2, 1:3, 2:1, 3:3] ;
% Cesta = [1:1, 2:2, 3:1, 1:2, 3:3] ;

:- dynamic poz/2.

cesta(X,Y,X,Y,[X:Y]) :- !. % koncim, kdyz pozice a startu a konce splynou
cesta(X,Y,XE,YE,[X:Y|T]) :-
	assert(poz(X, Y)),
	skok(X, Y, XN, YN),
	\+ poz(XN, YN),
	cesta(XN, YN, XE, YE, T).
cesta(X, Y, _, _, _) :- % musim odstranovat pozice pri backtrackingu
    retract(poz(X, Y)),
    !,
    fail.

cesty(XR, YR, XS, YS, XE, YE, N) :-
	XR > 0, YR > 1,
	assert(velikost(XR, YR)),
	testPoz(XS, YS), testPoz(XE, YE),
	findall(C, cesta(XS, YS, XE, YE, C), B),
	length(B, N),
	retractall(poz(_ ,_)), % odstraneni vsech tech pozic
	retract(velikost(_ ,_)). % i tady

% ======= UKOL 4 =======
% predvyplnim si nejake hodnoty
muj(aa, cau).
muj(bb, cau).
muj(cc, ahojda).

% kontroly
slovnik(D, _, _) :- var(D), !, fail.
slovnik(_, K, V) :- var(K), var(V), !, fail.
% vyhledani hodnoty
slovnik(D, K, V) :- var(V), !, call(D, K, V).
% vyhledani klicu
slovnik(D, K, V) :- var(K), !,
	G =.. [D, X, V], % pomoci =.. se postavi predikat
    bagof(X, G, K). % G se vyuzije v bagof a potom se vypise K
% modifikace
slovnik(D, K, V) :-
    dynamic(D/2),
    O =.. [D, K, _], call(O), !, retract(O),
    N =.. [D, K, V], assert(N).
% vlozeni
slovnik(D, K, V) :-
    dynamic(D/2),
    N =.. [D, K, V],
    assert(N).
