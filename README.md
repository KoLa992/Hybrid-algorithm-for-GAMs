# Hybrid algorithm for GAMs
Implementation of a Hybrid genetic - harmony search algorithm for feature selection in GAMs. Some test cases are also included.

The rest of the documentation is in Hungarian for now.

## Hibrid algoritmus függvényei

A gyökérkönyvtárban két fájl található: *HybridFunctions.R* és *HybridFunctions_Parallelized.R*.
Mindkét fájlban ugyan azon függvények találhatók meg, csak a *HybridFunctions_Parallelized.R* fájlban az algoritmus szimultán számítja ki az aktuális memóriában lévő egyedekhez tartozó modelleket. Ezeket a fájlokat kell a *source* paranccsal meghívni, ha alkalmazni akarjuk a Hibrid algoritmust.

Az almappák tartalma:
1. Bank-Credit-Card-Default: A Hibrid és a Benchmarkként használt algoritmusok futási eredményeit tartalmazza a *banki ügyfelek* adatbázison.
2. Concrete-Data: A Hibrid és a Benchmarkként használt algoritmusok futási eredményeit tartalmazza a *betongerendák* adatbázison.

### A *Hibrid* függvény
A Hibrid algoritmust futtató *fő (main)* függvény.

Bemeneti paraméterek:
1. **genszam**: Egy *int*, ami a lehetséges magyarázóváltozók számát adja meg az adatbázisban.
2. **pop_meret**: Egy *int*, ami a populáció/memória méretét adja meg.
3. **maxlepes**: Egy *int*, ami a maximális generációszámot adja meg, ameddíg még elmehet az algoritmus.
4. **mutacio**: Egy *double*, ami az induló mutációs (*bw*) valószínűséget adja meg.
5. **HMCR**: Egy *double*, ami az induló *HMCR* valószínűséget adja meg.
6. **vegmutacio**: Egy *double*, ami az utolsó generációban (aminek értékét a *maxlepes* paraméter adja meg) elérendő mutációs (*bw*) valószínűséget adja meg.
7. **vegHMCR**: Egy *double*, ami az utolsó generációban (aminek értékét a *maxlepes* paraméter adja meg) elérendő *HMCR* valószínűséget adja meg.
8. **konvergKrit**: Egy *int*, ami a korai kilépési feltétel adja meg. Ha az itt megadott értéknek megfelelő számú generáción keresztül nem változik a legjobb megoldás, akkor az algoritmus futása leáll.
9. **X**: Egy *dataframe* vagy *named matrix*, ami lehetséges magyarázóváltozók értékét tartalmazza a tanító adatbázison. **Fontos**, hogy a faktorváltozók már dummy változókkal legyenek reprezentálva ebben a táblában!
10. **Y**: Egy *vector*, ami az eredményváltozó értékeit tartalmazza a tanító adatbázison (az *X* paraméterben megadott táblával egyező sorrendben).
11. **csalad**: Egy *string*, ami az eredményváltozó eloszlását adja meg. A lehetséges értékek listáját lásd az *mgcv* csomag *bam* függvényének *family* paraméteréhez tartozó <a href="https://www.rdocumentation.org/packages/mgcv/versions/1.8-31/topics/family.mgcv" target="_blank">dokumentációban</a>.
12. **faktorok**: Egy *string* lista, ami a faktorkat reprezentáló dummy változók neveit tartalmazza az *X* paraméterben.
13. **konkurv_strict**: Egy *int*, ami azt szabályozza, hogy a *concurvity* korlát az *mgcv* csomag által számolt megfigyelt vagy pesszimista concurvity mértékre vonatkozzon-e. Pesszimista mérték = 1; Megfigyelt mérték = 2.
14. **magok**: Egy *int*, ami megadja futtatáshoz hány CPU magot vehet igénybe az algoritmus. Érdemes az elérhető CPU magok száma - 1 értéket megadni, hogy ne terhelje a futtatás teljesen a CPU-t. A *HybridFunctions.R* fájlban lévő verzió a paramétert az egy egyedhez tartozó GAM párhuzamosított számításához használja. A *HybridFunctions_Parallelized.R* fájlban lévő verzió az aktuális memóriában lévő egyedekhez tartozó modellek szimultán kiszámításához használja fel a paraméter értékét.

Kimeneti paraméterek:
* Egy **kételemű *double listát*** ad vissza, melynek elemei:
  1. **legjobb**: Egy négyelemű lista, ami az utolsó generáció *legjobb egyedének* paramétereit adja vissza.
     1. Az egyed bináris reprezentációját tartalmazó *string*.
     2. Az egyedhez tartozó GAM pszeudo R-négyzet értéke, ami egy *double* típusú érték.
     3. Egy *logical* érték, ami leírja, hogy az egyedhez tartozó GAM teljesíti-e a bázisfüggvények szignifikanciájára szóló *S<sub>i</sub>* korlátot.
     4. Egy *logical* érték, ami leírja, hogy az egyedhez tartozó GAM teljesíti-e a concurvity-re szóló *C<sub>i</sub>* korlátot.
  2. **konvergszamlalo**: Egy *int* érték, ami azt mutatja, hogy az utolsó generáció legjobb egyede hány generáció óta szerepelt a memóriákban.

### A *ModellEpit* függvény
Egy egyed bináris reprezentációja alapján felépíti a hozzá tartozó GAM modellt és visszaadja annak változószelekció szempontjából fontos paramétereit.

Bemeneti paraméterek:
1. **egyed**: Egy *logical lista*, ami az egyed bináris reprezentációját tartalmazza.
2. **X**: A *Hibrid* függvénytől örökli.
3. **Y**: A *Hibrid* függvénytől örökli.
4. **csalad**: A *Hibrid* függvénytől örökli.
5. **faktorok**: A *Hibrid* függvénytől örökli.
6. **konkurv_strict**: A *Hibrid* függvénytől örökli.
7. **magok**: A *Hibrid* függvénytől örökli. Csak a *HybridFunctions.R* fájlban található verzió használja. Ebben a verzióban egy GAM modell kiszámítása került párhuzamosításra. 
Az aktuális memóriában lévő egyedekhez tartozó modellek soros módon kerülnek feldolgozásra ebben a verzióban.

Kimeneti paraméterek:
* Egy **háromelemű *double listát*** ad vissza, melynek elemei:
  1. Az egyedhez tartozó GAM pszeudo R-négyzet értéke, ami egy *double* típusú érték.
  2. Egy *logical* érték, ami leírja, hogy az egyedhez tartozó GAM teljesíti-e a bázisfüggvények szignifikanciájára szóló *S<sub>i</sub>* korlátot.
  3. Egy *logical* érték, ami leírja, hogy az egyedhez tartozó GAM teljesíti-e a concurvity-re szóló *C<sub>i</sub>* korlátot.

### A *ModellEpit_B* függvény
Nem közvetlenül a Hibrid algoritmus része. Az algoritmus lefutása után használható. A legjobb egyed bináris reprezentációja alapján felépíti a hozzá tartozó GAM modellt és visszaadja a **teljes modellt leíró R objektumot**. A célja, hogy a Hibrid algoritmus legjobb modelljén további diagnosztikai vizsgálatokat tudjunk végezni. Pl. kiértékelni a teljesítményét egy teszt adatbázison.

Bemeneti paraméterek:
1. **egyed**: Egy *logical lista*, ami az egyed bináris reprezentációját tartalmazza.
2. **X**: A *Hibrid* függvénytől örökli.
3. **Y**: A *Hibrid* függvénytől örökli.
4. **csalad**: A *Hibrid* függvénytől örökli.
5. **magok**: A *Hibrid* függvénytől örökli. Ez a függvény az aktuális egyedhez tartozó GAM-ot mindig párhuzamosítottan számolja ki, hiszen itt biztosan nincs szükség több egyedhez tartozó modell párhuzamosított kiszámítására.

Kimeneti paraméterek:
1. **gam.mod**: Az egyedhez tartozó GAM-ot leíró R objektum.
