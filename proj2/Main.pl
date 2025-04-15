% FLP 2025 – Simulator NTS
% pouziva pomocne predikaty read_line, read_lines, split_line, split_lines

% pravidla jsou dynamicka - lze je mazat a pridavat za behu
:- dynamic rule/4.

% po spusteni programu se automaticky vola start
:- initialization(start).

%%%%%%%%%%%%%%%%%%%%
% cteni vstupu
%%%%%%%%%%%%%%%%%%%%

% hlavni predikat - cte vstup, zpracuje ho a spusti simulaci
start :-
    prompt(_, ''), % vypne vyzvu pro zadani (lepsi nacitani ze souboru)
    read_lines(Lines), % nacte vsechny radky ze vstupu
    split_lines(Lines, SplitLines), % radky se rozdeli na seznamy "slov" (znaky oddelene mezerami)
    parse_input(SplitLines). % zpracovani vstupu

% rozpoznani vstupu - vsechny radky krome posledniho jsou pravidla, posledni pravidlo je pocatecni paska
parse_input(SplitLines) :-
    append(RuleLines, [TapeLine], SplitLines),
    maplist(parse_rule, RuleLines), % parsuje a uklada pravidla do databaze
    TapeLine = [TapeChars|_], % ziska prvni slovo posledniho radku jako seznam znaku pasky (proste paska)
    flatten(TapeChars, FlatTapeChars),
    simulate([], FlatTapeChars, 'S', []). % spusti simulaci z vychoziho stavu 'S'

parse_rule([From, Read, To, Action]) :-
    % odstranujeme prazdne seznamu, ktere muze vytvorit split_line
    delete(From, [], FromClean),
    delete(Read, [], ReadClean),
    delete(To, [], ToClean),
    delete(Action, [], ActionClean),
    
    % prevedeme seznamy znaku na atomy
    atom_chars(FromAtom, FromClean),
    atom_chars(ReadAtom, ReadClean),
    atom_chars(ToAtom, ToClean),
    atom_chars(ActionAtom, ActionClean),
    
    % extrahujeme prvni znaky - stavy a symboly
    sub_atom(FromAtom, 0, 1, _, FromState),
    sub_atom(ReadAtom, 0, 1, _, ReadSymbol),
    sub_atom(ToAtom, 0, 1, _, ToState),
    
    % urceni, jestli je akce posun nebo zapis
    (
        ActionAtom = 'L' -> FinalAction = 'L' ; % posun vlevo
        ActionAtom = 'R' -> FinalAction = 'R' ; % posun vpravo
        sub_atom(ActionAtom, 0, 1, _, FinalAction)  % jinak se jedna o zapis symbolu
    ),
    
    assertz(rule(FromState, ReadSymbol, ToState, FinalAction)). % ulozeni pravidla do databaze

%%%%%%%%%%%%%%%%%%%%
% simulace stroje
%%%%%%%%%%%%%%%%%%%%

% simulace stroje:
%   - paska vlevo
%   - paska vpravo (vcetne hlavicky)
%   - aktualni stav
%   - historie konfiguraci
simulate(Left, Right, State, History) :-
    print_config(Left, Right, State), % vypise aktualni konfiguraci
    (
        State = 'F' -> true % pokud jsme v koncovem stavu, tak koncime
    ;
        \+ member((Left, Right, State), History), % kontrola cykleni
        move(Left, Right, State, [(Left, Right, State)|History]) % zkusime aplikovat prechody
    ).

% vypis konfigurace TS - sleduje se stav aktualni pozice hlavicky
print_config(Left, Right, State) :-
    reverse(Left, LeftRev), % otocime pasku vlevo (aby sla zprava-doleva)
    (
        Right = [Head|Tail] ->
        append(LeftRev, [State, Head | Tail], Config),
        atomic_list_concat(Config, '', Output),
        writeln(Output)
    ;
        append(LeftRev, [State], Config), % pokud je prava cast prazdna
        atomic_list_concat(Config, '', Output),
        writeln(Output)
    ).

% aplikace prechodu
move(Left, [Head|Tail], State, History) :-
    !,
    findall(rule(State, Head, NewState, Action), rule(State, Head, NewState, Action), Rules),
    (
        Rules = [] -> writeln('No applicable rule.'), halt(1) ;
        (select_terminal_rule(Rules, TerminalRule) -> % nejdrive zkusime pravidlo vedouci do F
            apply_rule_direct(Left, Tail, Head, TerminalRule, History)
        ;
            try_rules(Left, Tail, State, Head, Rules, History)
        )
    ).
% pokud je prava cast pasky prazdna, pak ji povazujeme za mezeru
move(Left, [], State, History) :-
    move(Left, [' '], State, History).

% vyber pravidla, ktere prechazi do koncoveho stavu
select_terminal_rule(Rules, rule(State, Symbol, 'F', Action)) :-
    member(rule(State, Symbol, 'F', Action), Rules).

% prima aplikace pravidla vedouciho do stavu F
apply_rule_direct(Left, Tail, _, rule(_, _, NewState, Action), History) :-
    apply_rule(Left, Tail, _, NewState, Action, History).

% aplikace posunu vlevo
apply_rule(Left, Tail, _, NewState, 'L', History) :-
    (
        Left = [NewHead|NewLeft] ->
        simulate(NewLeft, [NewHead|Tail], NewState, History)
    ;
        simulate([], [' '|Tail], NewState, History) % pokud jsme na levem okraji, pridame mezeru
    ).

% aplikace presunu vpravo
apply_rule(Left, Tail, Head, NewState, 'R', History) :-
    simulate([Head|Left], Tail, NewState, History).

% aplikace zapisu symbolu - nahradime aktualni hlavicku novym symbolem
apply_rule(Left, Tail, _, NewState, WriteSymbol, History) :-
    \+ (WriteSymbol = 'L' ; WriteSymbol = 'R'),
    simulate(Left, [WriteSymbol|Tail], NewState, History).

% postupne zkouseni pravidel (jednoho po druhem)
try_rules(Left, Tail, State, Head, [rule(State, Head, NewState, Action)|_], History) :-
    apply_rule(Left, Tail, Head, NewState, Action, History).

try_rules(Left, Tail, State, Head, [_|OtherRules], History) :-
    try_rules(Left, Tail, State, Head, OtherRules, History).

%%%%%%%%%%%%%%%%%%%%
% pomocne funkce
%%%%%%%%%%%%%%%%%%%%

% cte jeden radek jako seznam znaku
read_line(L,C) :-
    get_char(C),
    (isEOFEOL(C), L = [], !;
        read_line(LL,_),
        [C|LL] = L).

% zjisti konec radku nebo konec souboru
isEOFEOL(C) :-
    C == end_of_file;
    (char_code(C,Code), Code==10).

% nacita vsechny radky ze vstupu
read_lines(Ls) :-
    read_line(L,C),
    ( C == end_of_file, Ls = [] ;
        read_lines(LLs), Ls = [L|LLs]
    ).

% rozdeli radek na slova - seznamy znaku oddelene mezerou
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]).

% aplikuje split_line na kazdy radek
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).