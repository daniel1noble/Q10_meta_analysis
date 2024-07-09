# Limited physiological plasticity and reduced relative variance in physiological rates of ectotherm populations under climate change

This repository contains the data, code, model objects and figures used in the following paper:

Noble, D. W., Kar, F., Bush, A., Seebacher, F., & Nakagawa, S. (2024). Reduced plasticity and variance in physiological rates of ectotherm populations under climate change. *EcoEvoRxiv*, https://doi.org/10.32942/X2RS4W. 

The manuscript is written in Quarto. As such, the code, data and models used, along with how those objects were used, are integrated within the `ms.qmd` file. Relevant code chunks contain calls to the key data files and model objects used along with pre- and post-processing code. 

Models are already run. As such, one doesn't need to re-run them, which is important as they take quite some time to run. Models are run using both Bayesian and frequentist approaches. This was done to: 1) check that results are consistent and 2) allow us to create orchard and bubble plots easier (i.e., Frequentist models run using `metafor`). 

Some preprocessing code to obtain climate data and process/clean main data files is located within the `R/` folder along with functions used throughout the analysis. 
