## A mappa tartalma
 * **train.Rda és test.Rda**: A banki ügyfelek adatbázisból készített tanító és teszt halmazok R dataframe objektumai.
 * **trainDefault.csv**: A *train.Rda* fájlból készített csv állomány olyan formátumban, ami a HSIC-Lasso algoritmus Python implementációjával kompatibilis.
 * **BenchmarkModels.R**: A vizsgált benchmark algoritmusokat (HSIC-Lasso, mRMR, CART, Random Forest RFE szelekcióval) futtató és az eredményeket kiértékelő R szkript. Az algoritmusok futásidői 20 replikáció során a **RunTime_mRMR_HSICLasso_DT_RF.xlsx** fájlban kerültek rögzítésre.
 * **BestSubsets_RunTimes.R**: A két eltérő párhuzamosítási logika és az 5000 elemű véletlen alminták alkalmazásának hatását tesztelő R szkrpit.
    * A szkript első része a futásidőket 100 véletlenszerűen generált változóhalmazhoz (egyedhez) tartozó GAM esetében vizsgálja. A szkript ezen része vegül a **FullData_RunTimes.xlsx** fájlt generálja.
    * A szkript második része vizsgálja az 5000 elemű véletlen alminták alkalmazása esetén 100 replikációval a pszeudo R-négyzet mintavételezési eloszlását. Az szkript végül a **SubSample_Rsq.csv** fájlt generálja.
* **HybridApplication.R**: A Hibrid algoritmust a banki ügyfelek adatbázison futtató R szkript. A futtatások outputját egy "resHibrid_Default.csv" elnevezésű fájlba exportálja. A *csv* állomány tartalma a további feldolgozás és elemzések során a **resHibrid_BankClientsDefault.xlsx** fájl *resHibrid_Default.csv* munkalapjára került.
