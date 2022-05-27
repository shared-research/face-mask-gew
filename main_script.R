# Running the entire analysis

devtools::load_all()

create_dir_structure()

# Pre-processing

run_script("scripts/02_calc-angles.R")
run_script("scripts/03_catch-trials.R")
run_script("scripts/04_prepare-data-fit.R")

# Models

run_script("scripts/04a_circular-models.R")
run_script("scripts/04b_intensity-models.R")

# Prior Sensitivity

run_script("scripts/05a_circular-prior-sensitivity.R")
run_script("scripts/05b_intensity-prior-sensitivity.R")

# Post-processing Models

run_script("scripts/06a_post-processing_circular.R")
run_script("scripts/06b_post-processing-intensity.R")

# Tables

run_script("scripts/07_tables.R")

# Figures

run_script("scripts/08_plots.R")
run_script("scripts/09_journal_plots.R")


# Compiling Supplementary Materials

rmarkdown::render("docs/supplementary/supplementary.Rmd", quiet = TRUE) # osf supplementary
rmarkdown::render("docs/supplementary/supplementary_frontiers.Rmd", quiet = TRUE) # frontiers supplementary