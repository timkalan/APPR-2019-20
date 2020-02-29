# Migracija po svetu

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2019/20

* [![Shiny](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/timkalan/APPR-2019-20/master?urlpath=shiny/APPR-2019-20/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/timkalan/APPR-2019-20/master?urlpath=rstudio) RStudio

## Analiza svetovne migracije

#### Osnovna ideja
Analiziral bom migracijo po svetu (oziroma čim večih državah). Zajeta bosta tako imigracija kot emigracija. Primerjal bom podatke po starosti, izobrazbi, spolu, religiji. Države od kjer ljudje prihajajo in kamor grejo pa bom analiziral glede na kazalnike razvoja, kot 
je npr. BDP. Poskušal bom odkriti tudi časovne trende, 
saj so na voljo podatki iz zadnjih nekaj deset let. Države bom primerjal med sabo in iskal povezave med njimi ter jih s tem razvrstil v skupine.  


#### Viri
Za vire bom uporabil spletne strani [World Bank](https://www.worldbank.org), [Migration Data Portal](https://migrationdataportal.org), [Združenih narodov](https://www.un.org), [Wikipedio](https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita), [World Population Review](http://worldpopulationreview.com/countries/religion-by-country/).

Viri imajo podatke v obliki CSV in v obliki HTML, zemljevid sveta, ki je trenutno uporabljen pa je vgrajen v R.

#### Tabele
* migranti (origin, destination, leto, število),
* stock(država, leto, starost, spol, število), 
* države (država, leto, BDP, populacija, izobraženost, HDI), 
* religije (država, stolpci, ki prikazujejo število vernikov).

Cilj je najti korelacije med migracijo in raznimi indikatorji, ter ugotoviti, katere države so za emigracijo "najboljše".

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega projekta je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `rgdal` - za uvoz zemljevidov
* `rgeos` - za podporo zemljevidom
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `tidyr` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `mosaic` - za pretvorbo zemljevidov v obliko za risanje z `ggplot2`
* `maptools` - za delo z zemljevidi
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
* `tmap` - za risanje zemljevidov
* `StandardizeText` - za poenotenje imen držav
* `readxl` - za branje excel datotek
* `plotly` - za dodatno funkcionalnost grafov

## Binder

Zgornje [povezave](#analiza-podatkov-s-programom-r-201819)
omogočajo poganjanje projekta na spletu z orodjem [Binder](https://mybinder.org/).
V ta namen je bila pripravljena slika za [Docker](https://www.docker.com/),
ki vsebuje večino paketov, ki jih boste potrebovali za svoj projekt.

Če se izkaže, da katerega od paketov, ki ji potrebujete, ni v sliki,
lahko za sprotno namestitev poskrbite tako,
da jih v datoteki [`install.R`](install.R) namestite z ukazom `install.packages`.
Te datoteke (ali ukaza `install.packages`) **ne vključujte** v svoj program -
gre samo za navodilo za Binder, katere pakete naj namesti pred poganjanjem vašega projekta.

Tako nameščanje paketov se bo izvedlo pred vsakim poganjanjem v Binderju.
Če se izkaže, da je to preveč zamudno,
lahko pripravite [lastno sliko](https://github.com/jaanos/APPR-docker) z želenimi paketi.

Če želite v Binderju delati z git,
v datoteki `gitconfig` nastavite svoje ime in priimek ter e-poštni naslov
(odkomentirajte vzorec in zamenjajte s svojimi podatki) -
ob naslednjem zagonu bo mogoče delati commite.
Te podatke lahko nastavite tudi z `git config --global` v konzoli
(vendar bodo veljale le v trenutni seji).
