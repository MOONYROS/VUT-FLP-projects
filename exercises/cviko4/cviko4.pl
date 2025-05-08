:- dynamic muz/1.

% FLP CVICENI 4 - PROLOG 1 - UVOD
 
% ukazka predikatu pro vypocet funkce faktorial
factorial( 0, 1 ).
factorial( N, Value ) :-
     N > 0,
     Prev is N - 1,
     factorial( Prev, Prevfact ),
     Value is Prevfact * N.

% databaze rodinnych vztahu
muz(jan).
muz(pavel).
muz(robert).
muz(tomas).
muz(petr).

zena(marie).
zena(jana).
zena(linda).
zena(eva).

otec(tomas,jan).
otec(jan,robert).
otec(jan,jana).
otec(pavel,linda).
otec(pavel,eva).

matka(marie,robert).
matka(linda,jana).
matka(eva,petr).

% Implementujte nasledujici predikaty:
rodic(X,Y) :- otec(X,Y); matka(X,Y).
sourozenec(X,Y) :- rodic(Z,X), rodic(Z,Y), X\=Y.
sestra(X,Y) :- sourozenec(X,Y), zena(X).
deda(X,Y) :- otec(X,T), rodic(T,Y).
je_matka(X) :- matka(X,_).
teta(X,Y) :- sestra(X,T), rodic(T,Y).

% Seznamy:
neprazdny([_|_]).

hlavicka([H|_], H).
posledni([H], H) :- !.
posledni([_|T], Res) :- posledni(T, Res).


% Dalsi ukoly:
spoj([], L, L).
spoj([H|T], L, [H|TT]) :- spoj(T, L, TT).

obrat([],[]).
obrat([H|T], Res) :- 
    obrat(T, RT),
    spoj(RT, [H], Res).

sluc(L, [], L).
sluc([], L, L).
sluc([X|Xs], [Y|Ys], [X|T]) :- X @< Y, sluc(Xs, [Y|Ys], T).
sluc([X|Xs], [Y|Ys], [Y|T]) :- Y @< X, sluc([X|Xs], Ys, T).

serad([], []).
serad([H|T], SL) :- serad(T, RT), sluc([H], RT, SL).


plus(X,Y,Z) :- Z is X + Y.

% UKOL 6 - asi bonus ig
% ?- split([a,h,o,j,' ',s,v,e,t,e], S)
% S = [[a,h,o,j], [s,v,e,t,e]].
split([], [[]]) :- !.
split([' '|T], [[]|S1]) :- split(T|S1).
split([H|T], [[H|G] | S1]) :- split(T, [G|S1]).

% NA CV5 
% findall(X, muz(X), B).
