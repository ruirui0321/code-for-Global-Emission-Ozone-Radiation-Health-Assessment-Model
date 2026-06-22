# code-for-Global-Emission-Ozone-Radiation-Health-Assessment-Model

## Code Structure and Workflow

The R scripts follow the sequential order below:

1. **`calculate the ozone depletion.R`**  
   Estimates ozone depletion based on modeled scenarios or input data.

2. **`calculate the population.R`**  
   Processes population data, including age-group stratification and temporal harmonization.

3. **`compute the radiation.R`**  
   Computes surface ultraviolet (UV) radiation levels under different ozone conditions.

4. **`Read and integrate the radiation from a single file.R`**  
   Reads and aggregates radiation data from individual output files for downstream use.

5. **`drawing the distribution of radiation and sum the TUV results.R`**  
   Visualizes the spatial distribution of UV radiation and summarizes results from the TUV radiative transfer model.

6. **`calculate the b through Monte Carlo.R`**  
   Estimates the exposure-response coefficient (*b*) using Monte Carlo simulations.

7. **`calculate Y in any special year.R`**  
   Calculates health-related outcome variables (*Y*) for specific target years.

8. **`calculate the cost.R`**  
   Estimates the associated economic costs based on avoided health impacts.

## Contact

For questions or collaboration inquiries, please contact:  
Mingrui Ji  
Zhejiang University, China  
mingruiji@zju.edu.cn
