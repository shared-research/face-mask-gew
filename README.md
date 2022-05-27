
<!-- README.md is generated from README.Rmd. Please edit that file -->

    #> Warning in setup_ns_exports(path, export_all, export_imports): Objects listed as
    #> exports, but not present in namespace: tidy_brm_from_fit

# Mapping the Perception-space of Facial Expressions in the Era of Face Masks

<!-- badges: start -->

[<img alt="alt_text" src="https://img.shields.io/badge/OSF-10.17605%2FOSF.IO%2FE2KCW-blue" />](https://osf.io/e2kcw/)
<!-- badges: end -->

This repository contains all data and scripts to reproduce statistical
analysis, figures and tables from the paper “**Mapping the
Perception-space of Facial Expressions in the Era of Face Masks**” by
*Verroca*, *de Rienzo*, *Gambarota* and *Sessa* (2022). The project is
also on Open Science Framework (<https://osf.io/e2kcw/>). Supplementary
materials are available [here](docs/supplementary/supplementary.pdf)

## Repository

The repository is organized with the following structure:

-   `data/`: contains the cleaned data. The only pre-processing step is
    to combine different files from the Gorilla platform within a single
    dataset for each participant.
    -   `dat_clean.rds`: dataset with all participants, catch and valid
        trials
    -   `dat_fit`: dataset without the neutral facial expression and
        extra pre-processing steps used for model fitting
    -   `cleaned/catch/`: contains data from catch trials used as
        attention check during the task
        -   `dat_catch_ang`: dataset with catch trials and all
            pre-processing steps
        -   `dat_catch_ang_acc`: dataset with catch trials and all
            pre-processing steps with computed accuracy
    -   `cleaned/valid`: contains data from valid trials used for
        plotting and modelling
        -   `dat_valid_ang`: contains all participants, valid trials and
            all pre-processing steps
        -   `dat_valid_ang_final`: contains only good participants
            (excluded from catch trials) with all pre-processing steps
-   `figures/`: contains all figures included in the paper or
    supplementary materials
-   `tables/`: contains all tables included in the paper or
    supplementary materials
-   `objects/`: contains all R objects used through the project. In
    particular contains all post-processed fitted models.
-   `scripts/`: contains all scripts to produce datasets, models,
    figures and tables. The numbering suggest the order to correctly
    reproduce the analysis.
-   `files/`: contains extra files used in the project
-   `docs/`: contains the `Rmd` script and all files to reproduce the
    supplementary materials document
-   `R/`: contains all custom functions used through the project.

## Model fitting

All models are computed within a Bayesian framework using the `brms` R
package. Models were fitted using cluster computing in order to speed-up
the process. The final size of each model is on average more than
\~200mb and for this reason we included only post-processing information
(within the `objects/` folder, created with `05a/b_post_processing_*.R`
scripts). Running again the `04a/b_*_models.R` scripts will reproduce
the same results.

## Packages

-   `rmarkdown`
-   `bookdown`
-   `knitr`
-   `devtools`
-   `dplyr`
-   `flextable`
-   `ggplot2`
-   `here`
-   `kableExtra`
-   `magrittr`
-   `tidyr`
-   `cowplot`
-   `forcats`
-   `ggh4x`
-   `ggpubr`
-   `magick`
-   `stringr`
-   `cli`
-   `renv`
-   `rlang`
-   `tools`
-   `tidyverse`
-   `brms`
-   `tidybayes`
-   `CircStats`
-   `ftExtra`
-   `officer`
-   `purrr`
-   `latex2exp`
-   `ragg`
