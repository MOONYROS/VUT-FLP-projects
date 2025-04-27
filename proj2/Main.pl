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

% ========= START PROGRAMU =========

start :-
    prompt(_, ''),
    read_lines(LL),
    process_file(LL),
    halt.

% ========= ZPRACOVANI VSTUPU =========

% pravidlo ma 4 parametry: aktualni stav, symbol pod hlavou, novy stav, akce
:- dynamic rule/4.

% vezmeme si vstup a rozdelime jej na
% pravidla (vsechny radky krome posledniho) a pasku (posledni radek)
process_file(AllLines) :-
    append(RuleLines, [Tape], AllLines),
    maplist(process_rule, RuleLines),
    
    % odstraneni duplicitnich pravidel
    findall(rule(A, B, C, D), rule(A, B, C, D), AllRules),
    retractall(rule(_, _, _, _)),
    sort(AllRules, UniqueRules),
    maplist(assert, UniqueRules),
    
    process_tape(Tape).

% zpracuje radek pravidla na ctverici rule (viz vyse)
process_rule(Rule) :-
    % vezmeme znaky na pozicich 0, 2, 4, 6 - ocekavame spravny format pravidla
    nth0(0, Rule, CurrentState),
    nth0(2, Rule, CurrentSymbol),
    nth0(4, Rule, NewState),
    nth0(6, Rule, Action),
    
    % pridame pravidlo do databaze
    assertz(rule(CurrentState, CurrentSymbol, NewState, Action)).

% zpracuje pasku a spusti simulaci NTS
process_tape(Tape) :-
    ( 
        % nalevo je prazdno, napravo paska, pocatecni stav je S
        simulate([], Tape, 'S', [], [], Path) ->
            print_path(Path) % pokud existuje cesta, vypiseme
        ;
            halt(1) % jinak koncime s chybou
    ).

% ========= SIMULACE NTS =========

% vytvoreni konfiguarace z pasky vlevo, vpravo a stavu
create_configuration(Left, Right, State, Configuration) :-
    reverse(Left, RevLeft), % nejdriv si otocim pasku
    (
        Right \= [] -> % pokud neni prava cast prazdna...
            append(RevLeft, [State|Right], ConfigList) % ... spojime ji se stavem do configu...
        ;
            append(RevLeft, [State], ConfigList) % ... jinak bude na konci stav
    ),
    atom_chars(Configuration, ConfigList).


% simulace nedeterministickeho Turingova stroje
% pokud jsme ve stavu F, vratime cestu az do konce
simulate(Left, Right, 'F', _, PrevConfigs, Path) :-
    create_configuration(Left, Right, 'F', FinalConfig),
    reverse([FinalConfig|PrevConfigs], Path),
    !.
% pokud nejsme v koncovem stavu, pokracujeme
simulate(Left, Right, State, History, PrevConfigs, Path) :-
    % kontrola cykleni
    \+ member((Left, Right, State), History),
    
    % vytvoreni konfigurace a aktializace historie
    create_configuration(Left, Right, State, Config),
    NewHistory = [(Left, Right, State)|History],
    NewPrevConfigs = [Config|PrevConfigs],
    
    % ziskani aktuálního symbolu a pravidel
    get_current_symbol(Right, CurrentSymbol),
    findall(rule(State, CurrentSymbol, NewState, Action), 
            rule(State, CurrentSymbol, NewState, Action), 
            Rules),
    
    % kontrola existence pravidel
    Rules \= [],
    
    % vyber pravidla a aplikace akce
    (
        get_finishing_rule(Rules, Rule) ->
            % pokud existuje pravidlo vedouci do F, pouzijeme ho
            apply_rule(Left, Right, Rule, NewHistory, NewPrevConfigs, Path)
        ;
            % jinak pouzijeme libovolne pravidlo
            member(Rule, Rules),
            apply_rule(Left, Right, Rule, NewHistory, NewPrevConfigs, Path)
    ).

% ziskani pravidla vedoucího do F
get_finishing_rule(Rules, Rule) :-
    member(Rule, Rules),
    Rule = rule(_, _, 'F', _).

% ziskani aktuálního symbolu pod hlavou
get_current_symbol([], ' ').
get_current_symbol([Head|_], Head).

% aplikace vybraného pravidla
apply_rule(Left, Right, rule(_, _, NewState, Action), History, PrevConfigs, Path) :-
    (
        Action = 'L' -> % posun doleva
        (
            Left = [_|_] -> 
                apply_left_move(Left, Right, NewState, History, PrevConfigs, Path)
            ;
                fail % nelze jit pred zacatek pasky
        )
        ;
        Action = 'R' -> % posun doprava nebo zapis symbolu
            apply_right_move(Left, Right, NewState, History, PrevConfigs, Path)
        ;
            apply_symbol_write(Left, Right, Action, NewState, History, PrevConfigs, Path)
    ).

% aplikace pohybu doleva
apply_left_move(Left, Right, NewState, History, PrevConfigs, Path) :-
    Left = [NewHead|NewLeft],
    Right = [Head|Tail],
    simulate(NewLeft, [NewHead, Head|Tail], NewState, History, PrevConfigs, Path).

% aplikace pohybu doprava
apply_right_move(Left, Right, NewState, History, PrevConfigs, Path) :-
    Right = [Head|Tail],
    (
        Tail \= [] ->
            simulate([Head|Left], Tail, NewState, History, PrevConfigs, Path)
        ;
            simulate([Head|Left], [' '], NewState, History, PrevConfigs, Path)
    ).

% aplikace zapisu symbolu
apply_symbol_write(Left, Right, Symbol, NewState, History, PrevConfigs, Path) :-
    Right = [_|Tail],
    simulate(Left, [Symbol|Tail], NewState, History, PrevConfigs, Path).

% ========= VYPIS CESTY =========

print_path([]) :- !.
print_path([Config|Rest]) :-
    writeln(Config),
    print_path(Rest).
