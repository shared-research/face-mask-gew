# Packages ----------------------------------------------------------------

library(tidyverse)
library(brms)

# Functions ---------------------------------------------------------------

devtools::load_all()

# Setup -------------------------------------------------------------------

seed <- 2022
chains <- 4
iter <- 10000
cores <- chains
samp_prior <- "yes"

# Loading Models ----------------------------------------------------------

fit_ri_int <- readRDS(file.path("models", "theta", "fit_ri_int.rds"))
fit_ri_no3int <- readRDS(file.path("models", "theta", "fit_ri_no3int.rds"))
fit_ri_tas_mask <- readRDS(file.path("models", "theta", "fit_ri_tas_mask.rds"))
fit_ri_aq_mask <- readRDS(file.path("models", "theta", "fit_ri_aq_mask.rds"))
fit_ri_tas_mask_subtle <- readRDS(file.path("models", "theta", "fit_ri_tas_mask_subtle.rds"))
fit_ri_aq_mask_subtle <- readRDS(file.path("models", "theta", "fit_ri_aq_mask_subtle.rds"))

# Legend ------------------------------------------------------------------

# un = uninformative

# Data --------------------------------------------------------------------

dat_fit <- readRDS(file = file.path("data", "cleaned", "dat_fit.rds"))

# Model 1 - mask * intensity * emotion -------------------------------------

prior_von_mises_un <- c(
  prior(normal(0, 5), class = "b", dpar = ""), # betas prior
  prior(normal(0, 5), class = "b", dpar = "kappa") # kappa prior
)

fit_ri_int_un <- brm(fit_ri_int$formula,
                     data = dat_fit,
                     family = von_mises(link = "tan_half", link_kappa = "log"),
                     chains = chains,
                     prior = prior_von_mises_un,
                     cores = cores,
                     iter = iter,
                     sample_prior = samp_prior,
                     backend = "cmdstanr",
                     threads = threading(6),
                     file = "models/theta/fit_ri_int_un",
                     save_pars = save_pars(all = TRUE),
                     seed = seed)

success_step(fit_ri_int_un)

# Model 2a - tas * mask -----------------------------------------------------

prior_von_mises_tas_mask_un <- c(
  # theta
  prior(normal(0, 5), class = "b", dpar = "", coef = "Intercept"),
  prior(normal(0, 5), class = "b", dpar = "", coef = "mask_e1"),
  prior(normal(0, 1), class = "b", dpar = "", coef = "tas"),
  prior(normal(0, 1), class = "b", dpar = "", coef = "mask_e1:tas"),

  # kappa
  prior(normal(0, 5), class = "b", dpar = "kappa", coef = "Intercept"),
  prior(normal(0, 5), class = "b", dpar = "kappa", coef = "mask_e1"),
  prior(normal(0, 1), class = "b", dpar = "kappa", coef = "tas"),
  prior(normal(0, 1), class = "b", dpar = "kappa", coef = "mask_e1:tas")
)

fit_ri_tas_mask_un <- brm(fit_ri_tas_mask$formula,
                     data = dat_fit,
                     family = von_mises(link = "tan_half", link_kappa = "log"),
                     chains = chains,
                     prior = prior_von_mises_tas_mask_un,
                     cores = cores,
                     iter = iter,
                     backend = "cmdstanr",
                     threads = threading(6),
                     sample_prior = samp_prior,
                     file = "models/theta/fit_ri_tas_mask_un",
                     save_pars = save_pars(all = TRUE),
                     seed = seed)

success_step(fit_ri_tas_mask_un)

# Model 2b - tas * mask (subtle) -------------------------------------------------

prior_von_mises_tas_mask_subtle_un <- c(
  # theta
  prior(normal(0, 7), class = "b", dpar = "", coef = "Intercept"),
  prior(normal(0, 7), class = "b", dpar = "", coef = "mask_e1"),
  prior(normal(0, 2), class = "b", dpar = "", coef = "tas"),
  prior(normal(0, 2), class = "b", dpar = "", coef = "mask_e1:tas"),
  
  # kappa
  prior(normal(0, 7), class = "b", dpar = "kappa", coef = "Intercept"),
  prior(normal(0, 7), class = "b", dpar = "kappa", coef = "mask_e1"),
  prior(normal(0, 2), class = "b", dpar = "kappa", coef = "tas"),
  prior(normal(0, 2), class = "b", dpar = "kappa", coef = "mask_e1:tas")
)

fit_ri_tas_mask_subtle_un <- brm(fit_ri_tas_mask_subtle$formula,
                                 data = fit_ri_tas_mask_subtle$data,
                                 family = von_mises(link = "tan_half", link_kappa = "log"),
                                 chains = chains,
                                 prior = prior_von_mises_tas_mask_subtle_un,
                                 cores = cores,
                                 iter = iter,
                                 backend = "cmdstanr",
                                 threads = threading(6),
                                 sample_prior = samp_prior,
                                 control = list(adapt_delta = 0.9),
                                 file = "models/theta/fit_ri_tas_mask_subtle_un",
                                 save_pars = save_pars(all = TRUE),
                                 seed = seed)

success_step(fit_ri_tas_mask_subtle_un)

# Model 3a - aq * mask ------------------------------------------------------

prior_von_mises_aq_mask_un <- c(
  # theta
  prior(normal(0, 5), class = "b", dpar = "", coef = "Intercept"),
  prior(normal(0, 5), class = "b", dpar = "", coef = "mask_e1"),
  prior(normal(0, 1), class = "b", dpar = "", coef = "aq"),
  prior(normal(0, 1), class = "b", dpar = "", coef = "mask_e1:aq"),

  # kappa
  prior(normal(0, 5), class = "b", dpar = "kappa", coef = "Intercept"),
  prior(normal(0, 5), class = "b", dpar = "kappa", coef = "mask_e1"),
  prior(normal(0, 1), class = "b", dpar = "kappa", coef = "aq"),
  prior(normal(0, 1), class = "b", dpar = "kappa", coef = "mask_e1:aq")
)

fit_ri_aq_mask_un  <- brm(fit_ri_aq_mask$formula,
                     data = dat_fit,
                     family = von_mises(link = "tan_half", link_kappa = "log"),
                     chains = chains,
                     prior = prior_von_mises_aq_mask_un,
                     cores = cores,
                     iter = iter,
                     backend = "cmdstanr",
                     threads = threading(6),
                     sample_prior = samp_prior,
                     file = "models/theta/fit_ri_aq_mask_un",
                     save_pars = save_pars(all = TRUE),
                     seed = seed)

success_step(fit_ri_aq_mask_un)

# Model 3b - aq * mask (subtle) ---------------------------------------------------

fit_ri_aq_mask_subtle_un  <- brm(fit_ri_aq_mask_subtle$formula,
                             data = fit_ri_aq_mask_subtle$data,
                             family = von_mises(link = "tan_half", link_kappa = "log"),
                             chains = chains,
                             prior = prior_von_mises_aq_mask_un,
                             cores = cores,
                             iter = iter,
                             backend = "cmdstanr",
                             threads = threading(6),
                             sample_prior = samp_prior,
                             file = "models/theta/fit_ri_aq_mask_subtle_un",
                             save_pars = save_pars(all = TRUE),
                             seed = seed)

success_step(fit_ri_aq_mask_subtle_un)
