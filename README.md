# Interní verze

## Analýza dat z pilotního šetření dotazníku pro začínající učitele a absolventy učitelských programů

Kód v tomto repositáři byl použit k analýze dat sebraných ve spolupráci Ministerstva školství, mládeže a tělovýchovy a fakult připravujících učitele v akademickém roce 2021/2022.

### Jak replikovat analýzu

Kód je rozdělen do seřazených souborů. K replikaci celé analýzy stačí:

1. Ve složce s projektem vytvořit složku s daty. Složka má název **data** a obsahuje soubory:
    i. AU_2022_09_12.csv
    ii. ZU_2022_07_04.csv
    iii. SIMS.xlsx
    
2. Spustit kód v souboru **code/0_4run.R**

Výpočet může zabrat delší čas. Části kódu produkují varovné zprávy, které je možné ignorovat.

Výsledkem výpočtu by měly být výsledky v nově vzniklé složce outputs. Grafy použité ve zprávě lze nalézt ve sločce **outputs/code/0_3_save/report**.
