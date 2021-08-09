## Contents of this Folder

 * **train.Rda and test.Rda**: R dataframe objects of the training and test sets sampled from the Concrete Comprehensive Strength Dataset.
 * **trainConcr.csv**: A csv file created from the *train.Rda* file in a format that is comaptible with the Python implementation of the HSIC-Lasso algorithm.
 * **Benchmark.R**: An R script for running the primary benchamrk algorithms (HSIC-Lasso, mRMR) and for evaluating the results of their proposed models.
 * **BestSubsets.R**: An R script for generating every possible feature subsets of the Concrete Comprehensive Strength Dataset and estimating their corresponding GAMs. Next, the script gives the global optimum of the feature selection task with and without applying constraints for concurvity.
 * **HybridApplication_ConcreteData.R**: An R script for running the Hybrid Algorithm on the Concrete Comprehensive Strength Dataset. Output of the algorithm is exported to files with the naming convention "resHibrid_*X*.csv". These *csv* files are suitable for further processing when converted to *xlsx* format.
 * **resHibrid_initialParams.xlsx**: Results from  the first run of the Hybrid Algorithm on the Concrete Comprehensive Strength Dataset with initial parameter values. The script titled *HybridApplication_ConcreteData.R* generated the output *csv*, then it was processed for further analysis with Excel pivots.
 * **ConcreteTestsSummary.xlsx**: An Excel table summarising the sensitivity analysis results of the Hybrid Algorithm's parameters. Specific output files for each examined parameter setup can be found in the *Hybrid-Results* subfolder.
 * **resHibrid_optParams.xlsx**: Results from running of the Hybrid Algorithm on the Concrete Comprehensive Strength Dataset with the optimal parameter values. The script titled *HybridApplication_ConcreteData.R* generated the output *csv*, then it was processed for further analysis with Excel pivots.
