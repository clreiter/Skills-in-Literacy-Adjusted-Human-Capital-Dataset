# Skills-in-Literacy Adjusted Human Capital Dataset (SLAMYS)

This repository contains the **code and input data** for the estimation of the *Skills-in-Literacy Adjusted Mean Years of Schooling (SLAMYS)* indicator.  

SLAMYS integrates educational attainment (mean years of schooling) with literacy skills, producing a globally comparable measure of human capital for **185 countries, in five-year intervals from 1970 to 2025**, disaggregated by gender and age group (20–39 and 40–64 years).  

Building on the work of [Lutz et al. (2021, *PNAS*)](https://www.pnas.org/doi/10.1073/pnas.2015826118)—which introduced the SLAMYS indicator and provided a consistent global dataset for 1970–2015—this study extends the dataset to 2025 and adds disaggregation by gender and broad age groups, enabling more nuanced analyses of gender-specific trends and generational shifts in skill formation.  
This extension goes beyond a simple update by using broader and more recent data sources that capture both the quantitative and qualitative dimensions of human capital. It incorporates new literacy data from the second cycle of the OECD’s Programme for the International Assessment of Adult Competencies (PIAAC) and expands empirical data coverage for the Global South through additional waves of Demographic and Health Surveys (DHS) and, for the first time, Multiple Indicator Cluster Surveys (MICS).  

The full dataset is available via Zenodo:  
[https://doi.org/10.5281/zenodo.16902375](https://doi.org/10.5281/zenodo.16902375)  

---

## 📦 Repository Contents

- **R scripts** (6 files, see `/R/` folder):  
  - `1_saf_from_piaac_step.R`  
  - `2_prediction_male_literacy.R`  
  - `3_global_illiteracy_estimates.R`  
  - `4_saf_from_dhs_mics.R`  
  - `5_saf_predicted.R`  
  - `6_slamys.R`  

- **Input data** (18 files, in `/Input/`) used for the construction of SAFs (Skill Adjustment Factors) and SLAMYS estimates.
- 
- **Output folder** (`/Output/`): created by the scripts when run. It is empty by default.  

---

## 📊 Dataset Access

The official dataset can be cited as:  

> Potančoková, M., Reiter, C., & Spiegeler Castaneda, I. (2025). *Skills-in-Literacy Adjusted Human Capital Dataset (SLAMYS)* (Version v1) [Data set]. Zenodo. [https://doi.org/10.5281/zenodo.16902375](https://doi.org/10.5281/zenodo.16902375)  

---

## ⚙️ How to Use

1. Clone this repository:  
   ```bash
   git clone https://github.com/clreiter/Skills-in-Literacy-Adjusted-Human-Capital-Dataset
   
2. Open the R scripts in your R environment.  

3. Run the scripts in numerical order (`1_` … `6_`) to reproduce the SLAMYS estimates from the input files.  

---

## 📖 Citation

If you use this code or dataset, please cite both:  

- **The dataset itself**:  
  > Potančoková, M., Reiter, C., & Spiegeler Castaneda, I. (2025). *Skills-in-Literacy Adjusted Human Capital Dataset (SLAMYS)* (Version v1) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.16902375  

- **The conceptual foundation**:  
  > Lutz, W., Goujon, A., Samir, K. C., & Sanderson, W. (2021). *Quantifying human capital: Why, how, and what for*. *Proceedings of the National Academy of Sciences*, 118(21), e2015826118. https://doi.org/10.1073/pnas.2015826118  

---

## 🤝 Acknowledgements

The research leading to these results has received funding from the **European Union’s Horizon Europe** project call *HORIZON-CL2-2023-TRANSFORMATIONS-01*, grant agreement **101132476**.  
