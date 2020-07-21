## A mappa tartalma

 * **train.Rda és test.Rda**: A betongerendák adatbázisból készített tanító és teszt halmazok R dataframe objektumai.
 * **trainConcr.csv**: A *train.Rda* fájlból készített csv állomány olyan formátumban, ami a HSIC-Lasso algoritmus Python implementációjával kompatibilis.
 * **Benchmark.R**: A vizsgált benchmark algoritmusokat (HSIC-Lasso és mRMR) futtató és az eredményeket kiértékelő R szkript.
 * **BestSubsets.R**: R szkript, ami a betongerendák adatbázis összes lehetséges magyarázóváltozójának minden részhalmazához kiszámítja a GAM-ot. Ezek után megkeresi a változószelekciós feladat  globális optimumát a concurvity korlátokkal és anélkül.
 * **HybridApplication_ConcreteData.R**: A Hibrid algoritmust a betongerendák adatbázison futtató R szkript. A futtatások outputját "resHibrid_*X*.csv" elnevezési konvenciójú fájlokba exportálja. A *csv* állományok *xlsx* formátumba mentve további feldolgozásra is alkalmasak.
 * **resHibrid_initialParams.xlsx**: A Hibrid algoritmus első futtatása a betongerendák adatbázison, kezdeti paraméter értékekkel. A *HybridApplication_ConcreteData.R* szkript generálta a *csv*-t, majd Excellel kimutatások készültek további elemzés céljából.
 * **ConcreteTestsSummary.xlsx**: A Hibrid algoritmusparamétereinek érzékenységvizsgálat érdeményeit összesítő Excel tábla. A konkrét futtatások outputjai a *Hybrid-Results* almappában találhatók.
 * **resHibrid_optParams.xlsx**: A Hibrid algoritmus futtatása a betongerendák adatbázison, az optimális paraméter értékekkel. A *HybridApplication_ConcreteData.R* szkript generálta a *csv*-t, majd Excellel kimutatások készültek további elemzés céljából.
