% ======= UKOL 1 =======
subbags([], [[]]).
subbags([X|XS], Res) :-
    subbags(XS, Sub),
    addOneToAll(X, Sub, WithX), % pridam prvek a dostanu WithX
    append(Sub, WithX, Res). % dostanu vysledek

addOneToAll(_, [], []).
addOneToAll(E, [L|LS], [[E|L]|T]) :- addOneToAll(E, LS, T).

% ======= UKOL 2 =======
% ------- 2a -------
:- dynamic robot/2, dira/1.

obsazeno(P) :- dira(P); robot(_,P). % zjistuje, jestli je obsazeno
vytvor(I, P) :- obsazeno(P) -> false; assertz(robot(I, P)). % vytvari robota
vytvor(P) :- obsazeno(P) -> false; assertz(dira(P)). % vytvari diru

odstran(P) :- obsazeno(P) -> retract(dira(P)), retract(robot(_,P)); false. % odstranuje cokoliv z pozice

% ------- 2b -------
obsazene_pozice(X) :- bagof(P, obsazeno(P), X).
obsazene_roboty(X) :- bagof(P, robot(_,P), X).
% nebo alternativne:
% obsazene_roboty(X) :- bagof(P, I^robot(I,P), X).

% ------- 2c -------
inkrementuj(X,Y) :- Y is X+1.
dekrementuj(X,Y) :- Y is X-1.

doleva(I) :- pohni(I, dekrementuj).
doprava(I) :- pohni(I, inkrementuj).

pohni(I, Operace) :-
    robot(I, P),                                    % zjisteni, jestli robot existuje
    retract(robot(I, P)), call(Operace, P, Pn),     % odstraneni robota z aktualni pozice
    (obsazeno(Pn) ->                                % pokud je pozice obsazena...
        (robot(Pn, _) -> odstran(Pn; true))         % pak oba roboti vybuchnou...
    ;
        assertz(robot(I, Pn))                       % pokud nebyla, pak se tam robot presune
    ).

% ------- 2d -------
armageddon :- forall(robot(_, P), vybuch(P)). % vsichni roboti vybuchnou
vybuch(P) :- odstran(P), vytvor(P). %odstran robota a vytvor diru

% ======= UKOL 3 =======
% ------- 3a -------
g_size(3).

g_test(X:Y) :-
    g_size(S),
    X > 0, X =< S,
    Y > 0, Y =< S.

% ------- 3b -------
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 + 0, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 + 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 0, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 0, Y2 is Y1 + 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 + 0, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 + 1, g_test(X2:Y2).

% testovani gesta - je L spravne dlouhe?
g_one(X:Y, Len, L, R) :- reverse([X:Y|L], R), length(R, Len).
% dalsi gesta z pozice - posun na dalsi pozici Xn:Yn.
g_one(X:Y, Len, L, R) :-
    g_move(X:Y, Xn:Yn),             % posuneme se na dalsi pozici
    \+ memberchk(Xn:Yn, L),         % podivam se, jestli uz neni v seznamu projitych pozic
    g_one(Xn:Yn, Len, [X:Y|L], R). % rekurzivne volam g_one()

% ------- 3c -------
% staci jenom ten prvni predikat, druhy je zbytecny
g_all(R, Len) :-
    g_size(S),
    between(1, S, X),
    between(1, S, Y),
    g_one(X:Y, Len, [], R).
% g_all(R, Len) :-

% ------- 3d -------
g_allLength(R) :- g_allLength(R, 1).

g_allLength(R, Len) :- g_all(R, Len).
g_allLength(R, Len) :-
    HelpLen is Len + 1,
    g_allLength(R, HelpLen).
