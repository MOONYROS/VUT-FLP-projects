% FLP 2024/2025 - Projekt 2 - Turinguv Stroj
% Ondrej Lukasek (xlukas15@stud.fit.vutbr.cz)
% 15.4.2025

% ========= POMOCNE FUNKCE =========

/** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).

/** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

% nacita vsechny radky ze vstupu
read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).

/** rozdeli radek na podseznamy */
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1

/** vstupem je seznam radku (kazdy radek je seznam znaku) */
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).

% ========= START PROGRAMU =========

start :-
    prompt(_, ''),
    read_lines(LL),
    split_lines(LL, S),
    process_file(S),
    halt.

% ========= ZPRACOVANI VSTUPU =========

% pravidlo ma 4 parametry: aktualni stav, symbol pod hlavou, novy stav, akce
:- dynamic rule/4.

% spustime program
:- initialization(start).

% vezmeme si vstup a rozdelime jej na
% pravidla (vsechny radky krome posledniho) a pasku (posledni radek)
process_file(AllLines) :-
    append(RuleLines, [Tape], AllLines),
    maplist(process_rule, RuleLines),
    process_tape(Tape).

% zpracuje radek pravidla na ctverici rule (viz vyse)
process_rule(Rule) :-
    Rule = [PrevState, ReadSymbol, NextState, Action],

    % prevede si seznamy na atomy
    atom_chars(PrevAtom, PrevState),
    atom_chars(ReadAtom, ReadSymbol),
    atom_chars(NextAtom, NextState),
    atom_chars(ActionAtom, Action),

    % tyto atomy se pak rozextrahuji
    sub_atom(PrevAtom, 0, 1, _, PrevStateChar),
    sub_atom(ReadAtom, 0, 1, _, ReadSymbolChar),
    sub_atom(NextAtom, 0, 1, _, NextStateChar),

    % ale action muze byt L, R nebo prepsani => musime udelat sloziteji
    (
        ActionAtom = 'L' -> ActionChar = 'L';
        ActionAtom = 'R' -> ActionChar = 'R';
        sub_atom(ActionAtom, 0, 1, _, ActionChar)
    ),

    assertz(rule(PrevStateChar, ReadSymbolChar, NextStateChar, ActionChar)).

% zpracuje pasky a spusti simulaci TS
% nalevo je prazdno, napravo paska, pocatecni stav je S
process_tape([Tape|_]) :-
    flatten(Tape, FlatTape),
    simulate([], FlatTape, 'S', []).

% ========= SIMULACE TS =========

% samotna simulace TS
% dostava pasku vlevo a vpravo od hlavy, aktualni stav a historii konfiguraci
simulate(Left, Right, State, History) :-
    print_configuration(Left, Right, State),
    (
        State = 'F' -> true % pokud je TS v koncovem stavu, konci se
        ;
            \+ member((Left, Right, State), History), % kontrola, jestli tento stav uz nebyl
            make_move(Left, Right, State, [(Left, Right, State)|History]) % aplikace prechodu + zapis do historie
    ).

% vypise konfiguraci automatu na vystup
print_configuration(Left, Right, State) :-
    reverse(Left, RevLeft), % nejdriv si otocim pasku
    (
        % TODO - checknout, idk, jestli to dava smysl
        Right \= [] -> % pokud neni prava cast prazdna...
            append(RevLeft, [State|Right], Configuration) % ... spojime ji se stavem do configu...
        ;
            append(RevLeft, [State], Configuration) % ... jinak bude na konci stav
    ),
    atomic_list_concat(Configuration, '', Output),
    writeln(Output).

% aplikace pravidla, pokud jsme na konci pasky
make_move(Left, [], State, History) :-
    make_move(Left, [' '], State, History).

% aplikace pravidla, pro neprazdnou pravou pasku
make_move(Left, [Head|Tail], State, History) :-
    % najdeme vsechna pravidla vyhovujici aktualnimu stavu
    findall(rule(State, Head, NewState, Action), rule(State, Head, NewState, Action), Rules),
    (
        Rules = [] -> % pokud jsme nenasli zadne pravidlo - konec
            writeln('No rule can be applied!'),
            halt(1)
        ;
            (
                % zkusime najit ukoncujici pravidlo - pokud jsme nasli, pouzijeme ho,
                % jinak pouzijeme prvni pravidlo
                get_finishing_rule(Rules, FinishingRule) ->
                    apply_rule(Left, Tail, State, Head, FinishingRule, History)
                ;
                    Rules = [FirstRule|_],
                    apply_rule(Left, Tail, State, Head, FirstRule, History)
            )
    ).

% ziska ukoncujici pravidlo
get_finishing_rule(Rules, rule(State, Symbol, 'F', Action)) :-
    member(rule(State, Symbol, 'F', Action), Rules).

% aplikace konkretniho pravidla
apply_rule(Left, Tail, State, Head, rule(_, _, NewState, Action), History) :-
    (
        % posun doleva
        Action = 'L' ->
            (
                Left = [NewHead|NewLeft] ->
                    simulate(NewLeft, [Head|Tail], NewState, History)
                ;
                    simulate([], [' ', Head|Tail], NewState, History)
            )
        ;
            (
                % posun doprava
                Action = 'R' ->
                    simulate([Head|Left], Tail, NewState, History)
                ;
                    % zapis/prepis symbolu
                    simulate(Left, [Action|Tail], NewState, History)
            )
    ).