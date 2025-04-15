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

start :-
		prompt(_, ''),
		read_lines(LL),
		split_lines(LL,S),
		write(S),
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

    % TODO: ZBAVIT SE MEZER?

    % prevede si seznamy na atomy
    atom_chars(PrevAtom, PrevState),
    atom_chars(ReadAtom, ReadSymbol),
    atom_chars(NextAtom, NextState),
    atom_chars(ActionAtom, Action),

    % tyto atomy se pak rozextrahuji
    sub_atom(PrevAtom, 0, 1, After, PrevState),
    sub_atom(ReadAtom, 0, 1, After, ReadSymbol),
    sub_atom(NextAtom, 0, 1, After, NextState),

    % ale action muze byt L, R nebo prepsani => musime udelat sloziteji
    (
        ActionAtom = 'L' -> Action = 'L';
        ActionAtom = 'R' -> Action = 'R';
        sub_atom(ActionAtom, 0, 1, After, Action)
    ),

    assertz(rule(PrevState, ReadSymbol, NextState, Action)).

% zpracuje pasky a spusti simulaci TS
% nalevo je prazdno, napravo paska, pocatecni stav je S
process_tape([Tape|_]) :-
    flatten(Tape, FlatTape),
    simulate([], FlatTape, 'S', []).

% ========= SIMULACE TS =========

simulate([], Tape, 'S', []).
% TODO
