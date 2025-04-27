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

%% rule(+CurrentState, +CurrentSymbol, +NewState, +Action)
%
% Reprezentuje pravidlo turingova stroje.
%
% @param CurrentState aktualni stav stroje
% @param CurrentSymbol symbol pod hlavou
% @param NewState novy stav po aplikaci pravidla
% @param Action akce (symbol k zapisu, 'L' pro posun doleva nebo 'R' pro posun doprava)
%
:- dynamic rule/4.

%% process_file(+AllLines)
%
% Zpracuje vstupni radky souboru, rozdeli je na pravidla a pasku.
% Nasledne odstrani duplicitni pravidla a spusti simulaci.
%
% @param AllLines seznam radku vstupniho souboru
%
process_file(AllLines) :-
    append(RuleLines, [Tape], AllLines),
    maplist(process_rule, RuleLines),
    
    % odstraneni duplicitnich pravidel
    findall(rule(A, B, C, D), rule(A, B, C, D), AllRules),
    retractall(rule(_, _, _, _)),
    sort(AllRules, UniqueRules),
    maplist(assert, UniqueRules),
    
    process_tape(Tape).

%% process_rule(+Rule)
%
% Prevede radek vstupu na pravidlo TS a vlozi jej do databaze.
% Ocekava presny format pravidla, jinak nefunguje spravne.
%
% @param Rule seznam znaku tvoricich radek pravidla
%
process_rule(Rule) :-
    % vezmeme znaky na pozicich 0, 2, 4, 6 - ocekavame spravny format pravidla
    nth0(0, Rule, CurrentState),
    nth0(2, Rule, CurrentSymbol),
    nth0(4, Rule, NewState),
    nth0(6, Rule, Action),
    
    % pridame pravidlo do databaze
    assertz(rule(CurrentState, CurrentSymbol, NewState, Action)).

%% process_tape(+Tape)
%
% Spusti simulaci turingova stroje s danou paskou a pocatecnim stavem 'S'.
%
% @param Tape pocatecni obsah pasky
% @throws halt(1) pokud nelze dosahnout stavu 'F'
%
process_tape(Tape) :-
    ( 
        % nalevo je prazdno, napravo paska, pocatecni stav je S
        simulate([], Tape, 'S', [], [], Path) ->
            print_path(Path) % pokud existuje cesta, vypiseme
        ;
            halt(1) % jinak koncime s chybou
    ).

% ========= SIMULACE NTS =========

%% create_configuration(+Left, +Right, +State, -Configuration)
%
% Vytvori aktualni konfiguraci stroje jako retezec.
%
% @param Left seznam symbolu nalevo od hlavy (v obracenem poradi)
% @param Right seznam symbolu napravo od hlavy (prvni symbol je symbol po hlavou)
% @param State aktualni stav stroje
% @param Configuration konfigurace stroje v textovem formatu
%
create_configuration(Left, Right, State, Configuration) :-
    reverse(Left, RevLeft), % nejdriv si otocim pasku
    (
        Right \= [] -> % pokud neni prava cast prazdna...
            append(RevLeft, [State|Right], ConfigList) % ... spojime ji se stavem do configu...
        ;
            append(RevLeft, [State], ConfigList) % ... jinak bude na konci stav
    ),
    atom_chars(Configuration, ConfigList).


%% simulate(+Left, +Right, +State, +History, +PrevConfigs, -Path)
%
% Simuluje nedeterministicky turinguv stroj.
% Tato verze zpracovava koncovy stav.
%
% @param Left seznam symbolu nalevo od hlavy (v obracenem poradi)
% @param Right seznam symbolu napravo od hlavy (prvni symbol je symbol po hlavou)
% @param State aktualni stav stroje ('F')
% @param History seznam navstivenych konfiguraci (nevyuzito)
% @param PrevConfigs seznam predchozich konfiguraci
% @param Path vysledna cesta od pocatecni k cilove konfiguraci
%
simulate(Left, Right, 'F', _, PrevConfigs, Path) :-
    create_configuration(Left, Right, 'F', FinalConfig),
    reverse([FinalConfig|PrevConfigs], Path),
    !.

%% simulate(+Left, +Right, +State, +History, +PrevConfigs, -Path)
%
% Simuluje nedeterministicky turinguv stroj.
% Tato verze zpracovava nekoncove stavy.
%
% @param Left seznam symbolu nalevo od hlavy (v obracenem poradi)
% @param Right seznam symbolu napravo od hlavy (prvni symbol je symbol po hlavou)
% @param State aktualni stav stroje (ne 'F')
% @param History seznam navstivenych konfiguraci (detekce cyklu)
% @param PrevConfigs seznam predchozich konfiguraci
% @param Path vysledna cesta od pocatecni k cilove konfiguraci
% @throws fail pokud nelze najit cestu k cilove konfiguraci
%
simulate(Left, Right, State, History, PrevConfigs, Path) :-
    % kontrola cykleni
    \+ member((Left, Right, State), History),
    
    % vytvoreni konfigurace a aktializace historie
    create_configuration(Left, Right, State, Config),
    NewHistory = [(Left, Right, State)|History],
    NewPrevConfigs = [Config|PrevConfigs],
    
    % ziskani aktualniho symbolu a pravidel
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

%% get_finishing_rule(+Rules, -Rule)
%
% Najde pravidlo, vedouci do koncoveho stavu 'F'.
%
% @param Rules seznam pravidel k prohledani
% @param Rule nalezene pravidlo vedouci do 'F'
%
get_finishing_rule(Rules, Rule) :-
    member(Rule, Rules),
    Rule = rule(_, _, 'F', _).

%% get_current_symbol(+Right, -Symbol)
%
% Ziska aktualni symbol pod hlavou nebo mezeru, pokud jsme na (pravem) konci pasky.
%
% @param Right seznam symbolu napravo od hlavy (prvni symbol je symbol po hlavou)
% @param Symbol aktualni symbol pod hlavou nebo mezernik (v pripade prazdne pasky)
%
get_current_symbol([], ' ').
get_current_symbol([Head|_], Head).

%% apply_rule(+Left, +Right, +Rule, +History, +PrevConfigs, -Path)
%
% Aplikuje dane pravidlo stroje podle typu akce.
%
% @param Left seznam symbolu nalevo od hlavy (v obracenem poradi)
% @param Right seznam symbolu napravo od hlavy (prvni symbol je symbol po hlavou)
% @param Rule pravidlo k aplikaci ve formatu rule(_, _, NewState, Action)
% @param History seznam navstivenych konfiguraci
% @param PrevConfigs seznam predchozich konfiguraci
% @param Path vysledna cesta od pocatecni k cilove konfiguraci
% @throws fail pokud nelze aplikovat pravidlo (posun pred levy okraj pasky)
%
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
            apply_write(Left, Right, Action, NewState, History, PrevConfigs, Path)
    ).

%% apply_left_move(+Left, +Right, +NewState, +History, +PrevConfigs, -Path)
%
% Provede posun hlavy doleva a pokracuje v simulaci.
%
% @param Left seznam symbolu nalevo od hlavy (v obracenem poradi)
% @param Right seznam symbolu napravo od hlavy (prvni symbol je symbol po hlavou)
% @param NewState novy stav stroje po aplikaci pravidla
% @param History seznam navstivenych konfiguraci
% @param PrevConfigs seznam predchozich konfiguraci
% @param Path vysledna cesta od pocatecni k cilove konfiguraci
%
apply_left_move(Left, Right, NewState, History, PrevConfigs, Path) :-
    Left = [NewHead|NewLeft],
    Right = [Head|Tail],
    simulate(NewLeft, [NewHead, Head|Tail], NewState, History, PrevConfigs, Path).

%% apply_right_move(+Left, +Right, +NewState, +History, +PrevConfigs, -Path)
%
% Provede posun hlavy doprava a pokracuje v simulaci.
%
% @param Left seznam symbolu nalevo od hlavy (v obracenem poradi)
% @param Right seznam symbolu napravo od hlavy (prvni symbol je symbol po hlavou)
% @param NewState novy stav stroje po aplikaci pravidla
% @param History seznam navstivenych konfiguraci
% @param PrevConfigs seznam predchozich konfiguraci
% @param Path vysledna cesta od pocatecni k cilove konfiguraci
%
apply_right_move(Left, Right, NewState, History, PrevConfigs, Path) :-
    Right = [Head|Tail],
    (
        Tail \= [] ->
            simulate([Head|Left], Tail, NewState, History, PrevConfigs, Path)
        ;
            simulate([Head|Left], [' '], NewState, History, PrevConfigs, Path)
    ).

%% apply_write(+Left, +Right, +Symbol, +NewState, +History, +PrevConfigs, -Path)
%
% prepise aktualni symbol pod hlavou a pokracuje v simulaci.
%
% @param Left seznam symbolu nalevo od hlavy (v obracenem poradi)
% @param Right seznam symbolu napravo od hlavy (prvni symbol je symbol po hlavou)
% @param Symbol novy symbol k zapisu pod hlavou
% @param NewState novy stav stroje po aplikaci pravidla
% @param History seznam navstivenych konfiguraci
% @param PrevConfigs seznam predchozich konfiguraci
% @param Path vysledna cesta od pocatecni k cilove konfiguraci
%
apply_write(Left, Right, Symbol, NewState, History, PrevConfigs, Path) :-
    Right = [_|Tail],
    simulate(Left, [Symbol|Tail], NewState, History, PrevConfigs, Path).

% ========= VYPIS CESTY =========

%% print_path(+Path)
%
% Vypise sekvenci konfiguraci na standardni vystup.
% Kazdou na samostatny radek.
%
% @param Path seznam konfiguraci k vypsani
%
print_path([]) :- !.
print_path([Config|Rest]) :-
    writeln(Config),
    print_path(Rest).
