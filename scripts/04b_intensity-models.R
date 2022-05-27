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

# Data --------------------------------------------------------------------

dat_fit <- readRDS(file = file.path("data", "cleaned", "dat_fit.rds"))
dat <- readRDS(file = file.path("data", "cleaned", "valid", "dat_valid_ang_final.rds"))
dat_fit_subtle <- dat_fit %>% 
  filter(intensity == "subtle")


# Legend ------------------------------------------------------------------

# ri = by-subject random intercept
# int = 3 way interaction (mask * emotion * intensity)
# no3int = no 3 way interaction (mask * emotion + intensity)
# tas_mask = tas * mask
# aq_mask = aq * mask
# tas_maskint = tas * mask * int
# aq_maskint = aq * mask * int
# neu = mask (only neutral)

# Model 1 - Emotion * mask * intensity ------------------------------------

prior_gaussian <- c(
  prior(normal(150, 100), class = "b", coef = "Intercept"),
  prior(normal(0, 50), class = "b")
)

form_ri_int <- bf(
  int ~  0 + Intercept + emotion * mask * intensity + (1|id)
)

fit_ri_int <- brm(form_ri_int,
                  data = dat_fit,
                  prior = prior_gaussian,
                  family = gaussian(),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  file = "models/intensity/fit_ri_int",
                  backend = "cmdstanr",
                  save_pars = save_pars(all = TRUE),
                  sample_prior = samp_prior,
                  seed = seed)

success_step(fit_ri_int)

# Model 2 - Emotion * Mask + intensity ----------------------------------------

form_ri_no3int <- bf(int ~ 0 + Intercept + emotion + mask + intensity + emotion:mask + emotion:intensity + mask:intensity + (1|id))

fit_ri_no3int <- brm(form_ri_no3int,
                  data = dat_fit,
                  prior = prior_gaussian,
                  family = gaussian(),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  file = "models/intensity/fit_ri_no3int",
                  backend = "cmdstanr",
                  save_pars = save_pars(all = TRUE),
                  sample_prior = samp_prior,
                  seed = seed)

success_step(fit_ri_no3int)

# Model 3a - tas * mask -----------------------------------------------------

prior_gaussian_tas_mask <- c(
  # int
  prior(normal(150, 100), class = "b", coef = "Intercept"),
  prior(normal(0, 50), class = "b", dpar = "", coef = "mask_e1"),
  prior(normal(0, 5), class = "b", dpar = "", coef = "tas"),
  prior(normal(0, 5), class = "b", dpar = "", coef = "mask_e1:tas")
)

form_ri_tas_mask <- bf(
  int ~  0 + Intercept + mask_e * tas + (1|id)
)

fit_ri_tas_mask <- brm(form_ri_tas_mask,
                  data = dat_fit,
                  prior = prior_gaussian_tas_mask,
                  family = gaussian(),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  file = "models/intensity/fit_ri_tas_mask",
                  backend = "cmdstanr",
                  save_pars = save_pars(all = TRUE),
                  sample_prior = samp_prior,
                  seed = seed)

success_step(fit_ri_tas_mask)

# Model 3b - tas * mask (subtle) -----------------------------------------

fit_ri_tas_mask_subtle <- brm(form_ri_tas_mask,
                          data = dat_fit_subtle,
                          prior = prior_gaussian_tas_mask,
                          family = gaussian(),
                          chains = chains,
                          cores = cores,
                          iter = iter,
                          file = "models/intensity/fit_ri_tas_mask_subtle",
                          backend = "cmdstanr",
                          save_pars = save_pars(all = TRUE),
                          sample_prior = samp_prior,
                          seed = seed)

success_step(fit_ri_tas_mask_subtle)

# Model 4a - aq * mask ------------------------------------------------------

form_ri_aq_mask <- bf(
  int ~  0 + Intercept + mask_e * aq + (1|id)
)

prior_gaussian_aq_mask <- c(
  # int
  prior(normal(150, 100), class = "b", coef = "Intercept"),
  prior(normal(0, 50), class = "b", dpar = "", coef = "mask_e1"),
  prior(normal(0, 5), class = "b", dpar = "", coef = "aq"),
  prior(normal(0, 5), class = "b", dpar = "", coef = "mask_e1:aq")
)

fit_ri_aq_mask <- brm(form_ri_aq_mask,
                  data = dat_fit,
                  prior = prior_gaussian_aq_mask,
                  family = gaussian(),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  file = "models/intensity/fit_ri_aq_mask",
                  backend = "cmdstanr",
                  save_pars = save_pars(all = TRUE),
                  sample_prior = samp_prior,
                  seed = seed)

success_step(fit_ri_aq_mask)

# Model 4b - aq * mask (subtle) ------------------------------------------------------

fit_ri_aq_mask_subtle <- brm(form_ri_aq_mask,
                         data = dat_fit_subtle,
                         prior = prior_gaussian_aq_mask,
                         family = gaussian(),
                         chains = chains,
                         cores = cores,
                         iter = iter,
                         file = "models/intensity/fit_ri_aq_mask_subtle",
                         backend = "cmdstanr",
                         save_pars = save_pars(all = TRUE),
                         sample_prior = samp_prior,
                         seed = seed)

success_step(fit_ri_aq_mask_subtle)


# Model 5 - mask (neutral faces) ------------------------------------------

form_ri_neu <- bf(
  int ~  0 + Intercept + mask + (1|id)
)

dat_neutral <- dat %>% 
  filter(emotion == "neutrality") %>% 
  mutate(mask_e = factor(mask))

contrasts(dat_neutral$mask_e) <- contr.sum(2)/2

fit_ri_neu <- brm(form_ri_neu,
                  data = dat_neutral,
                  prior = prior_gaussian,
                  family = gaussian(),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  file = "models/intensity/fit_ri_neu",
                  backend = "cmdstanr",
                  save_pars = save_pars(all = TRUE),
                  sample_prior = samp_prior,
                  seed = seed)

success_step(fit_ri_neu)