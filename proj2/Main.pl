% FLP 2020 – Simulátor nedeterministického Turingova stroje
% Používá pomocné predikáty read_line, read_lines, split_line, split_lines

% Deklarace dynamických pravidel
:- dynamic rule/4.

% Po spuštění volá start
:- initialization(start).

%%%%%%%%%%%%%%%%%%%%
% Čtení vstupu
%%%%%%%%%%%%%%%%%%%%
start :-
    prompt(_, ''),
    read_lines(Lines),
    split_lines(Lines, SplitLines),
    parse_input(SplitLines).

parse_input(SplitLines) :-
    append(RuleLines, [TapeLine], SplitLines),
    maplist(parse_rule, RuleLines),
    % Správné zpracování pásky - bereme jen první element z TapeLine a z něj vytvoříme seznam znaků
    TapeLine = [TapeChars|_],
    flatten(TapeChars, FlatTapeChars),
    simulate([], FlatTapeChars, 'S', []).

parse_rule([From, Read, To, Action]) :-
    % Odstraňujeme prázdné seznamy, které může vytvořit split_line
    delete(From, [], FromClean),
    delete(Read, [], ReadClean),
    delete(To, [], ToClean),
    delete(Action, [], ActionClean),
    
    % Převedeme seznamy znaků na atomy
    atom_chars(FromAtom, FromClean),
    atom_chars(ReadAtom, ReadClean),
    atom_chars(ToAtom, ToClean),
    atom_chars(ActionAtom, ActionClean),
    
    % Extrahujeme první znaky (stavy a symboly)
    sub_atom(FromAtom, 0, 1, _, FromState),
    sub_atom(ReadAtom, 0, 1, _, ReadSymbol),
    sub_atom(ToAtom, 0, 1, _, ToState),
    
    % Určíme, zda je akce posun nebo zápis
    (
        ActionAtom = 'L' -> FinalAction = 'L' ;
        ActionAtom = 'R' -> FinalAction = 'R' ;
        sub_atom(ActionAtom, 0, 1, _, FinalAction)  % Jinak je to symbol na zápis
    ),
    
    % Přidáme pravidlo do databáze
    assertz(rule(FromState, ReadSymbol, ToState, FinalAction)).

%%%%%%%%%%%%%%%%%%%%
% Simulace stroje
%%%%%%%%%%%%%%%%%%%%
simulate(Left, Right, State, History) :-
    print_config(Left, Right, State),
    (
        State = 'F' -> true  % Konec simulace při dosažení koncového stavu
    ;
        % Kontrola cyklení
        \+ member((Left, Right, State), History),
        move(Left, Right, State, [(Left, Right, State)|History])
    ).

% Výpis konfigurace
print_config(Left, Right, State) :-
    reverse(Left, LeftRev),
    (
        Right = [Head|Tail] ->
        append(LeftRev, [State, Head | Tail], Config),
        atomic_list_concat(Config, '', Output),
        writeln(Output)
    ;
        % Prázdná páska vpravo
        append(LeftRev, [State], Config),
        atomic_list_concat(Config, '', Output),
        writeln(Output)
    ).

% Přechod podle pravidel
move(Left, [Head|Tail], State, History) :-
    !,
    findall(rule(State, Head, NewState, Action), rule(State, Head, NewState, Action), Rules),
    (
        Rules = [] -> writeln('No applicable rule.'), halt(1) ;
        % Nejdříve zkusíme najít pravidlo vedoucí do stavu F
        (select_terminal_rule(Rules, TerminalRule) ->
            apply_rule_direct(Left, Tail, Head, TerminalRule, History)
        ;
            try_rules(Left, Tail, State, Head, Rules, History)
        )
    ).
% Prázdné políčko vpravo reprezentujeme jako mezeru
move(Left, [], State, History) :-
    move(Left, [' '], State, History).

% Najde pravidlo vedoucí do koncového stavu F, pokud existuje
select_terminal_rule(Rules, rule(State, Symbol, 'F', Action)) :-
    member(rule(State, Symbol, 'F', Action), Rules).

% Aplikace konkrétního pravidla přímo
apply_rule_direct(Left, Tail, _, rule(_, _, NewState, Action), History) :-
    apply_rule(Left, Tail, _, NewState, Action, History).

% Aplikace pravidla přesunu vlevo
apply_rule(Left, Tail, _, NewState, 'L', History) :-
    (
        Left = [NewHead|NewLeft] ->
        simulate(NewLeft, [NewHead|Tail], NewState, History)
    ;
        % Pokud jsme na levém okraji, přidáme mezeru (nekonečná páska)
        simulate([], [' '|Tail], NewState, History)
    ).

% Aplikace pravidla přesunu vpravo
apply_rule(Left, Tail, Head, NewState, 'R', History) :-
    simulate([Head|Left], Tail, NewState, History).

% Aplikace pravidla zápisu symbolu
apply_rule(Left, Tail, _, NewState, WriteSymbol, History) :-
    \+ (WriteSymbol = 'L' ; WriteSymbol = 'R'),
    simulate(Left, [WriteSymbol|Tail], NewState, History).

% Zkoušení pravidel (nedeterminismus)
try_rules(Left, Tail, State, Head, [rule(State, Head, NewState, Action)|_], History) :-
    apply_rule(Left, Tail, Head, NewState, Action, History).

try_rules(Left, Tail, State, Head, [_|OtherRules], History) :-
    try_rules(Left, Tail, State, Head, OtherRules, History).

%%%%%%%%%%%%%%%%%%%%
% Helping functions
%%%%%%%%%%%%%%%%%%%%
read_line(L,C) :-
    get_char(C),
    (isEOFEOL(C), L = [], !;
        read_line(LL,_),
        [C|LL] = L).

isEOFEOL(C) :-
    C == end_of_file;
    (char_code(C,Code), Code==10).

read_lines(Ls) :-
    read_line(L,C),
    ( C == end_of_file, Ls = [] ;
        read_lines(LLs), Ls = [L|LLs]
    ).

split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]).

split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).