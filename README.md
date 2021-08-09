# Hybrid Algorithm for GAMs
An R implementation of a *hybrid genetic - harmony search algorithm* for feature selection in GAMs. Some test cases are also included.

## Functions Implementing the Hybrid Algorithm

There two files in the root directory: *HybridFunctions.R* és *HybridFunctions_Parallelized.R*.
<br>Both files contain the same functions. The only difference is that the *HybridFunctions_Parallelized.R* file the algorithm computes the GAMs corresponding to each individual in the current harmony memory simultaneously.<br> These files need to be called with the *source* command if the Hybrid Algorithm needs to be applied on a dataset.

Contents of the Subfolders:
1. **Bank-Credit-Card-Default**: Results from applying the Hybrid and the examined Benchmark algorithms on the *Credit Card Deafult Dataset*.
2. **Concrete-Data**: Results from applying the Hybrid and the examined Benchmark algorithms on the *Concrete Comprehensive Strength Dataset*.

Important terminology:
1. **individual** = a possible solution for the optimal feature subset.
2. **Populaton / Harmony Memory** = Individuals that are evaluated based on their corresponding GAMs during an interation (or generation) of the Hybrid Algorithm.

### The *Hibrid* function
The *main* function running the Hybrid Algorithm.

Input Parameters:
1. **genszam**: An *int*, that gives the number of possible feature variables in the current dataset.
2. **pop_meret**: An *int*, that determines the size of the populaton / harmony memory.
3. **maxlepes**: An *int*, that determines the maximum number of iterations (or generations) the algorithm can run.
4. **mutacio**: A *double*, determines the initial mutation (*bw*) probability.
5. **HMCR**: A *double*, that determines the inital *HMCR* probability.
6. **vegmutacio**: A *double*, that determines the mutation (*bw*) probability in the last generation (the last generation is determined in the *maxlepes* parameter).
7. **vegHMCR**: A *double*, that determines the *HMCR* probability in the last generation (the last generation is determined in the *maxlepes* parameter).
8. **konvergKrit**: An *int*, that determines the early stopping criterion. If the best solution does not change for the number of generations given here, the algorithm stops.
9. **X**: A *dataframe* or a *named matrix* object, that contains the realized values of all the feature variables on the training set. **Important:** feature variables of *factor* type have to be represented by dummy variables in this object!
10. **Y**: A *vector*, that contains the realized values of the target variable on the training set (in an order matching with that of the table given in the *X* parameter).
11. **csalad**: A *string*, that gives the distribution of the target variable. A list of acceptable values can be found in the <a href="https://www.rdocumentation.org/packages/mgcv/versions/1.8-31/topics/family.mgcv" target="_blank">documentation</a> for the *family* parameter of the *bam* function in the *mgcv* package.
12. **faktorok**: A *vector of strings*, that contains the names of the dummy variables representing *factor*s in the table given in the *X* parameter.
13. **konkurv_strict**: An *int*, that controls whether the *concurvity* constraint should consider the pessimistic or the observed concurvity measure from the *mgcv* package. Pessimistic measure = 1; Observed measure = 2.
14. **magok**: An *int*, that determines the number of CPU cores the algorithm can use for parallel computation of GAMs. It is advisable to use the number of available cores inus 1 here, not to overload your CPU. The version of the algorithm in the *HybridFunctions.R* file applies this parameter for the parallel computation of the parameter estimates of a single GAM. The version in *HybridFunctions_Parallelized.R* applies this parameter for the simultaneous computation of GAMs corresponding to each individual in the current harmony memory.

Output parameters:
* Returns a ***list* with two elements**:
  1. **best**: a list with four elements that contains the parameters of the *best individual* in the last generation.
     1. A *string* describing the binary representation of the individual/solution.
     2. A *double*, that gives the pseudo R-squared value of the GAM corresponding to the best individual in the last generation.
     3. A *logical*, that describes whether the GAM corresponding to the best individual in the last generation satisfies the significance constraint, *S<sub>i</sub>*.
     4. A *logical*, that describes whether the GAM corresponding to the best individual in the last generation satisfies the concurvity constraint, *C<sub>i</sub>*.
  2. **konvergszamlalo**: An *int*, that indicates how many generations the best individual in the last generation spent in the memory/population before the algorithm stopped.

### The *ModellEpit* function
It estimates a GAM based on the binary representation of an individual's feature subset and returns the model parameters necessary for feature selection in the Hybrid Algorithm.

Input parameters:
1. **egyed**: A *list of logicals*, that contains the binary representation of an individual's feature subset.
2. **X**: Inherited from the *Hibrid* function.
3. **Y**: Inherited from the *Hibrid* function.
4. **csalad**: Inherited from the *Hibrid* function.
5. **faktorok**: Inherited from the *Hibrid* function.
6. **konkurv_strict**: Inherited from the *Hibrid* function.
7. **magok**: Inherited from the *Hibrid* function. Only used in the version found in the *HybridFunctions.R* file. In this version, computation of the parameter estimates of a single GAM is parallelized. Individuals in the current harmony memory are processed in a serial manner for this version.

Output parameters:
* returns a **list of *doubles* with three elements**:
  1. A *double*, that gives the pseudo R-squared value of GAM corresponding to the individual.
  2. A *logical*, that describes whether the GAM corresponding to the individual satisfies the significance constraint, *S<sub>i</sub>*.
  3. A *logical*, that describes whether the GAM corresponding to the individual satisfies the concurvity constraint, *C<sub>i</sub>*.

### The *ModellEpit_B* function
Not directly a part of the Hybrid Algorithm. It can be called after the *Hibrid* function was run. Estimates a GAM based on the binary representation of the best individual in the last generation of the Hybrid Algorithm and returns the **full R object describing the GAM**. Aim of this function is to allow the user to execute further diagnostic tests on the GAM proposed by the Hybrid Algorithm. E.g. evaluating its performance on a separate test set.

Input parameters:
1. **egyed**: A *list of logicals*, that contains the binary representation of an individual's feature subset.
2. **X**: Inherited from the *Hibrid* function.
3. **Y**: Inherited from the *Hibrid* function.
4. **csalad**: Inherited from the *Hibrid* function.
5. **magok**: Inherited from the *Hibrid* function. This function always estimates the parameters of the GAM corresponding to the input individual parallelized, as in the use case for this function, there is no need to estimate GAMs corresponding to several individuals.

Output parameters:
1. **gam.mod**: The R object describing the GAM corresponding to the individual.
