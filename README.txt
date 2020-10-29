1. System requirements & installation guide:
This set of codes was tested on R software version 4.0.3 on Windows.
To install R on Windows, please download the installer through this link: https://cran.r-project.org/bin/windows/base/

Please install the latest version of packages listed in the first few lines of 'FIG_1.R'.

To install 'squire' package in R, please follow instructions in this link: https://mrc-ide.github.io/squire/
To install 'odin' package in R, please follow instructiosn in this link: https://github.com/mrc-ide/odin

The rest of the required packages are available in CRAN repository.

2. How to run the codes:
To ensure that all scripts can be run smoothly, please run the scripts as follows:
1. FIG_1.R
2. FIG_2.R
2. metapopulation_model_run.R - results of re-running metapopulation model will not be identical, but similar. simulation results are available to be loaded in FIG_4.R and are available in 'Output/Metapopulation/' folder.
3. FIG_3.R
4. FIG_4.R
5. suspected_deaths_simulations.R
6. province_level_fitting_squire.R - to obtain 'best_40k_14d.rds' required for FIG_5.R - results might not be identical
7. FIG_5.R

Typically, it takes less than 10 minutes to run FIG_1.R, FIG_2.R, FIG_3.R, and FIG_4.R using pre-run processed_inputs provided in the folder. However, it might take longer to rerun each metapopulation model simulations, where each model takes around 30 minutes to complete.
Both province_level_fitting_squire.R and FIG_5.R need a high computing memory to run.