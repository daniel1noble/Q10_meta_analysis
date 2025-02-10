# Limited physiological plasticity and reduced relative variance in physiological rates of ectotherm populations under climate change

[![DOI](https://zenodo.org/badge/463093254.svg)](https://doi.org/10.5281/zenodo.11123599)

This repository contains the data, code, model objects and figures used in the following paper:

Noble et al. 2025. Limited plasticity but increased variance in physiological rates across ectotherm populations under climate change. 

**IMPORTANT NOTE**: Please note that data and models are extremely large files! We've used Git's 'large file storage' (LFS) to handle these but all large files are replaced with pointers to these files which are stored on the LFS server, so these will not contain the correct data / models even though the structure of the repository is exactly the same. As such, you will need to download the data and models separately by cloning the repository using the following command. 

```
git clone --recurse-submodules git@github.com:daniel1noble/Q10_meta_analysis.git
```

Alternatively, if you have downloaded the repo from the most recent [Zenodo](https://doi.org/10.5281/zenodo.11123599) version you should have all the data and models on hand (note the download may take a while due to the size of the files).

Should you have *any* issues at all please get in touch with Dan Noble at daniel.noble@anu.edu.au. All data and code should be available and we are happy to help you get it running on your machine.

## Reproducing results and manuscript

**Main Analyses & Manuscript**: The manuscript is written in Quarto. As such, the code, data and models used, along with how those objects were used, are integrated within the :page_facing_up: `ms.qmd` file. Relevant code chunks contain calls to the key data files and model objects used along with pre- and post-processing code. 

**Supplementary Results**: At the end of the :page_facing_up: `ms.qmd` file the supplementary materials, methods and results are found.

**Models**: Models are already run. As such, one doesn't need to re-run them, which is important as they take quite some time to run. Models are run using both Bayesian and frequentist approaches. This was done to: 1) check that results are consistent and 2) allow us to create orchard and bubble plots easier (i.e., Frequentist models run using `metafor`). 

**Pre-processing Code**: Some preprocessing code to obtain climate data and process/clean main data files is located within the `R/` folder along with functions used throughout the analysis. 

## Data

The cleaned data files for reproducing analyses are in the :open_file_folder: `output/data/` folder. There are two files that are relevant. The main one, :page_facing_up: `data_final_wide.csv` and a re-organized version for analysis :page_facing_up: `data_final_long.csv`.

Column descriptors for :page_facing_up: `data_final_wide.csv` and `data_final_long.csv` are as follows:

- record_num: Unique paper identifier. Papers marked "_o" are the original data from Seebacher et al. 2015, whereas any identifier without "_o" is data added from updated searches.
- phylum: Phylum of the species
- class: Class of the species	
- order: Order of the species
- family: Family of the species	
- habitat: Habitat of the species. There categories: 'm' = marine, 'f' = freshwater, 't' = terrestrial.
- source: Source of the animals. Two categories: 'w' = wild, 'c' = captive.
- life.hist: Life-stage of the species. Two categories: 'a' = adult, 'j' = juvenile.
- lat: Latitude of the study site. Decimal degrees.
- long: Longitude of the study site. Decimal degrees.	
- acclim_type: Whether the study contained both acute and acclimation ('2') or just acclimation ('1'). 
- acclim_period: Acclimation period in days. 	
- trait_category: Broad category of the trait measured. 12 categories: 'cardiac' = heart function; 'rest_MR' = resting metabolic rate; 'max_MR' = maximum metabolic rate; 'ATPase': ATPase enzyme; 'sprint' = sprinting or swimming speed; 'muscle' = muscle function; 'endurance' = endurance traits; 'metabolic_enzyme'= metabolic enzymes; 'other' = other.	
- trait: Detailed trait name from paper
- temp_1: Acclimation temperature 1 in degrees Celsius.
- temp_2: Acclimation temperature 2 in degrees Celsius.	
- r1.1: Physiological rate for animals measured in acclimation temperature 1 at acute temperature 1.	
- r1.1_N: Sample size for r1.1.	
- r1.2: Physiological rate for animals measured in acclimation temperature 1 at acute temperature 2.	
- r1.2_N: Sample size for r1.2.	
- r2.1: Physiological rate for animals measured in acclimation temperature 2 at acute temperature 1.	
- r2.1_N: Sample size for r2.1.
- r2.2: Physiological rate for animals measured in acclimation temperature 2 at acute temperature 2.	
- r2.2_N: Sample size for r2.2.	
- units: Units of the physiological rate.	
- species_full: Full species name.	
- r1.1_sd: Standard deviation of r1.1.	
- r1.2_sd: Standard deviation of r1.2.	
- r2.1_sd: Standard deviation of r2.1.	
- r2.2_sd: 	Standard deviation of r2.2.	
- lnRR_Q10_acute_t1: Acute Log response ratio for Q10 at temperature 1.	
- V_lnRR_Q10_acute_t1: Variance of lnRR_Q10_acute_t1.	
- lnRR_Q10_acute_t2: Acute log response ratio for Q10 at temperature 2.	
- V_lnRR_Q10_acute_t2: Variance of lnRR_Q10_acute_t2.	
- lnRR_Q10_acclim: Acclimation Log response ratio for Q10 at acclimation.
- V_lnRR_Q10_acclim: Variance of lnRR_Q10_acclim. 	
- lnVR_Q10_acute_t1: Acute Log variance ratio for Q10 at temperature 1.
- V_lnVR_Q10_acute_t1: Variance of lnVR_Q10_acute_t1.	
- lnVR_Q10_acute_t2: Acute Log variance ratio for Q10 at temperature 2.	
- V_lnVR_Q10_acute_t2: Variance of lnVR_Q10_acute_t2.	
- lnVR_Q10_acclim: Acclimation Log variance ratio for Q10 at acclimation.	
- V_lnVR_Q10_acclim: Variance of lnVR_Q10_acclim.	
- lnCVR_Q10_acute_t1: Acute Log coefficient of variation ratio for Q10 at temperature 1.	
- V_lnCVR_Q10_acute_t1: Variance of lnCVR_Q10_acute_t1.	
- lnCVR_Q10_acute_t2: Acute Log coefficient of variation ratio for Q10 at temperature 2.	
- V_lnCVR_Q10_acute_t2: Variance of lnCVR_Q10_acute_t2.	
- lnCVR_Q10_acclim: Acclimation Log coefficient of variation ratio for Q10 at acclimation.	
- V_lnCVR_Q10_acclim: Variance of lnCVR_Q10_acclim.	
- obs: Observation number. 
