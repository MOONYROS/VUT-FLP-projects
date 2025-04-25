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

% simulace nedeterministickeho Turingova stroje
% pokud jsme ve stavu F, vratime cestu az do konce
simulate(Left, Right, 'F', _, PrevConfigs, Path) :-
    create_configuration(Left, Right, 'F', FinalConfig),
    reverse([FinalConfig|PrevConfigs], Path),
    !.
% pokud nejsme v koncovem stavu, pokracujeme
simulate(Left, Right, State, History, PrevConfigs, Path) :-
    create_configuration(Left, Right, State, Config),
    
    \+ member((Left, Right, State), History), % kontrola cykleni
    
    NewHistory = [(Left, Right, State)|History], % pridame stav do historie
    NewPrevConfigs = [Config|PrevConfigs], % pridame konfiguraci
    
    % kontrola symbolu pod hlavou
    (
        Right = [] ->
            CurrentSymbol = ' '
        ; 
            Right = [CurrentSymbol|_]
    ),

    % najdeme vsechna pravidla pro aktualni stav
    findall(rule(State, CurrentSymbol, NewState, Action), rule(State, CurrentSymbol, NewState, Action), Rules),
    
    (
        Rules = [] -> 
            fail % pokud neexistuje zadne pravidlo, selhani
        ;
            apply_rule(Left, Right, State, NewHistory, NewPrevConfigs, Path) % jinak je pouzijeme
    ).

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

% funkce pro ziskani pravidla, ktere vede do koncoveho stavu 'F'
get_finishing_rule(Rules, rule(State, Symbol, 'F', Action)) :-
    member(rule(State, Symbol, 'F', Action), Rules).

% aplikace pravidel NTS
% na konci pasky pridame mezeru
apply_rule(Left, [], State, History, PrevConfigs, Path) :-
    apply_rule(Left, [' '], State, History, PrevConfigs, Path).
% najdeme vsechna aplikovatelna pravidla a aplikujeme je
apply_rule(Left, [Head|Tail], State, History, PrevConfigs, Path) :-
    findall(rule(State, Head, NewState, Action), rule(State, Head, NewState, Action), Rules),
    
    Rules \= [], % pokud jsme nejaka pravidla nasli
    
    % zkusime najit pravidlo vedouci do 'F'
    (
        get_finishing_rule(Rules, FinishingRule) ->
            Rule = FinishingRule  % pokud existuje, pouzijeme ho
        ;
            member(Rule, Rules)  % jinak vezmeme libovolne pravidlo
    ),
    
    Rule = rule(_, _, NewState, Action), % a aplikujeme ho
    
    % podle akce zvolime operaci
    (
        % posun doleva
        Action = 'L' ->
            (
                % pokud existuje symbol nalevo
                Left = [NewHead|NewLeft] ->
                    % posuneme se doleva na pasce
                    simulate(NewLeft, [NewHead, Head|Tail], NewState, History, PrevConfigs, Path)
                ;
                    % pokud vlevo nic neni, koncime s chybou - nelze jit pred zacatek pasky
                    fail
            )
        ;
            (
                % posun doprava
                Action = 'R' ->
                    (
                        % jestlize vpravo mame symbol, posuneme se na nej
                        Tail \= [] ->
                            simulate([Head|Left], Tail, NewState, History, PrevConfigs, Path)
                        ;
                            % pokud vpravo nic není, pridame mezeru
                            simulate([Head|Left], [' '], NewState, History, PrevConfigs, Path)
                    )
                ;
                    % zapis/prepis symbolu
                    simulate(Left, [Action|Tail], NewState, History, PrevConfigs, Path)
            )
    ).

% ========= VYPIS CESTY =========

print_path([]) :- !.
print_path([Config|Rest]) :-
    writeln(Config),
    print_path(Rest).
