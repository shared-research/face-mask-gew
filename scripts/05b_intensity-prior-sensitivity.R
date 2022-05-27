# Packages ----------------------------------------------------------------

library(tidyverse)
library(brms)

# Functions ---------------------------------------------------------------

devtools::load_all()

# Setup -------------------------------------------------------------------

seed <- 2022
chains <- 15
iter <- 4000
cores <- chains
samp_prior <- "yes"

# Loading Models ----------------------------------------------------------

fit_ri_int <- readRDS(file.path("models", "intensity", "fit_ri_int.rds"))
fit_ri_no3int <- readRDS(file.path("models", "intensity", "fit_ri_no3int.rds"))
fit_ri_tas_mask <- readRDS(file.path("models", "intensity", "fit_ri_tas_mask.rds"))
fit_ri_aq_mask <- readRDS(file.path("models", "intensity", "fit_ri_aq_mask.rds"))
fit_ri_tas_maskint <- readRDS(file.path("models", "intensity", "fit_ri_tas_maskint.rds"))
fit_ri_aq_maskint <- readRDS(file.path("models", "intensity", "fit_ri_aq_maskint.rds"))

# Legend ------------------------------------------------------------------

# flat = completely flat priors (brms default)

# Data --------------------------------------------------------------------

dat_fit <- readRDS(file = file.path("data", "cleaned", "dat_fit.rds"))


# Model 1 - mask * intensity * emotion ------------------------------------

fit_ri_int_flat <- brm(fit_ri_int$formula,
                       data = dat_fit,
                       family = gaussian(),
                       chains = chains,
                       cores = cores,
                       iter = iter,
                       file = "models/intensity/fit_ri_int_flat",
                       save_pars = save_pars(all = TRUE),
                       backend = "cmdstanr",
                       sample_prior = samp_prior,
                       seed = seed)

success_step(fit_ri_int_flat)


# Model 2a - mask * tas ---------------------------------------------------

fit_ri_tas_mask_flat <- brm(fit_ri_tas_mask$formula,
                   data = dat_fit,
                   family = gaussian(),
                   chains = chains,
                   cores = cores,
                   iter = iter,
                   file = "models/intensity/fit_ri_tas_mask_flat",
                   save_pars = save_pars(all = TRUE),
                   sample_prior = samp_prior,
                   seed = seed)

success_step(fit_ri_tas_mask_flat)


# Model 2b - mask * intensity * tas ---------------------------------------

fit_ri_tas_maskint_flat <- brm(fit_ri_tas_maskint$formula,
                       data = dat_fit,
                       family = gaussian(),
                       chains = chains,
                       cores = cores,
                       iter = iter,
                       file = "models/intensity/fit_ri_tas_maskint_flat",
                       save_pars = save_pars(all = TRUE),
                       sample_prior = samp_prior,
                       seed = seed)

success_step(fit_ri_tas_maskint_flat)

# Model 3a - mask * aq ----------------------------------------------------

fit_ri_aq_mask_flat <- brm(fit_ri_aq_mask$formula,
                      data = dat_fit,
                      family = gaussian(),
                      chains = chains,
                      cores = cores,
                      iter = iter,
                      file = "models/intensity/fit_ri_aq_mask_flat",
                      save_pars = save_pars(all = TRUE),
                      sample_prior = samp_prior,
                      seed = seed)

success_step(fit_ri_aq_mask_flat)

# Model 3b - mask * intensity * aq ----------------------------------------

fit_ri_aq_maskint_flat <- brm(fit_ri_aq_maskint$formula,
                      data = dat_fit,
                      family = gaussian(),
                      chains = chains,
                      cores = cores,
                      iter = iter,
                      file = "models/intensity/fit_ri_aq_maskint_flat",
                      save_pars = save_pars(all = TRUE),
                      sample_prior = samp_prior,
                      seed = seed)

success_step(fit_ri_aq_maskint_flat)