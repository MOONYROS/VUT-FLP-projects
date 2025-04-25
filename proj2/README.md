# FLP projekt 2 - Turingův stroj

- **Autor:** Ondřej Lukášek (xlukas15)
- **Akademický rok:** 2024/2025

## Popis použité metody řešení

### Interní reprezentace

Simulátor využívá následující reprezentace:

1. Pravidla stroje jsou reprezentována jako `rule(CurrentState, CurrentSymbol, NewState, Action)`, kde

   - `CurrentState` je aktuální stav,
   - `CurrentSymbol` je aktuální symbol pod hlavou stroje,
   - `NewState` je nový stav stroje po aplikaci pravidla,
   - `Action` je akce, která se při aplikaci pravidla provede (pohyb nebo přepis).

2. Konfigrace pásky, kde

   - `Left` je seznam symbolů nalevo od hlavy, ale v obráceném pořadí (pro lepší manipulaci),
   - `Right` je seznam symbolů napravo od hlavy, včetně symbolu po hlavou (ten je prvním symbolem pravé části).

### Prohledávání stavového prostoru

Pro svoje chování využívá simulátor následující mechanismy:

- **Prohledávání do hloubky** - Program prohledává stavový prostor pomocí DFS mechanismu a pokračuje, dokud nedojde do koncového stavu nedojde k zastavení.
K tomu může dojít na základě nemožnosti aplikovat žádné z pravidel nebo z důvodu detekce cyklení.
Program si totiž uchovává historii navštívených konfigurací, pomocí které je schopen cyklení detekovat.
Simulátor využívá **backtracking** vestavěný v Prologu, což umožňuje prozkoumávat všechny možné cesty a v případě selhání se vracet a zkoušet další možnosti.
- **Výběr pravidel** - Jak bylo řečeno výše, program využívá prohledávání do hloubky.
Nicméně pokud to lze, program se snaží primárně aplikovat pravidla, která vedou do koncového stavu (takový koncový stav musí být označen pomocí `F`).
Díky tomu je možné dojít k řešení o něco rychleji.

Tento přístup jsem si zvolil, protože mi přišel, že dobře funguje pro simulaci nedeterministického Turingova stroje.
Je poměrně jednoduchý, čitelný, ale zdá se mi i v kombinaci s prioritním výběrem pravidel, jako docela efektivní.
Detekce cyklů je poměrně jednoduchá, nicméně si myslím, že na tento projekt stačí.
Nicméně v případě spouštění velmi složitých programů může dojít k detekci cyklu i v situaci, kdy se o něj zcela nejedná.

## Návod k použití

Program lze přeložit za použití přiloženého Makefilu.

```sh
# překlad zdrojového souboru Main.pl
make

# spuštění programu s vlastním vstupem ze souboru
./flp24-log < <vstupní soubor>
```

Překlad samotný pak probíhá pomocí následujícího příkazu:

```sh
swipl -t halt -q -g start -o flp24-log -c Main.pl
```

### Formát vstupu

Každý řádek vstupního souboru (kromě posledního) očekává **formát pravidel** `<stav> <symbol> <nový stav> <akce>`.
V případě, že formát pravidel nebude dodržen, nebude program fungovat dle očekávání, protože bude docházet k chybnému zpracování pravidel.
Poslední řádek vstupního souboru souboru obsahuje **obsah vstupní pásky na začátku simulace**.
Po vstupní pásce může soubor obsahovat i symbol nového řádku `\LF`.

Tedy vstup ze souboru může vypadat třeba následovně (tento příklad je převzat ze zadání je obsažen v prvním testu v `test/inputs/in1.txt`):

```txt
S a B a
B a B b
B b B R
B c B a
B c F c
B c B a
aaacaa
```

### Formát výstupu

Výstupem je sekvence konfigurací Turingova stroje na standardním výstupu, přičemž každá je na jednom řádku až do dosažení koncového stavu F.
V případě abnormálního zastavení není na výstup není vypsáno nic.

## Testovací skripty

Jsou celkem dvě možnosti, jak spustit testovací skripty:

1. Spuštěním z makefile příkazem `make test` v kořenovém adresáři projektu.
2. Přímým spuštěním testovacího skriptu. Pro to je potřeba se přesunout do adresáře `test/`, kde lze spustit testovací skript `test.sh`.

Skript porovnává očekávané výstupy (v adresáři `expected/`) s aktuálními výstupy programu ná základě testovacího vstupu (adresář `inputs/`).
Skript zobrazuje přehledný průběh testů a jejich výsledky.
Na konci svého běhu zobrazí celkovou úspěšnost.

## Omezení

1. Program **očekává na vstupu přesný formát**, jako je popsáno v zadání projektu, případně v sekci **formát vstupu**.
2. Simulátor **nepodporuje pohyb za levý okraj vstupní pásky**. Pokud se stroj za ni pokusí přesunout, dojde k abnormálnímu zastavení.
3. Pro velmi hluboké výpočty může dojít k vyčerpání paměti.
4. Program **nemusí najít optimální cestu** k řešení v případě existence více možných řešení.
