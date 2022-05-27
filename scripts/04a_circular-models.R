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

# Circular Models ----------------------------------------------------------

prior_von_mises <- c(
  prior(normal(0, 2), class = "b", dpar = ""), # betas prior
  prior(normal(0, 2), class = "b", dpar = "kappa") # kappa prior
)

# Model 1 - Emotion * mask * intensity ------------------------------------

form_ri_int <- bf(diff_theta ~ emotion * mask * intensity + (1|id), 
                  kappa ~  emotion * mask * intensity + (1|id))


fit_ri_int <- brm(form_ri_int,
                  data = dat_fit,
                  prior = prior_von_mises,
                  family = von_mises(link = "tan_half", link_kappa = "log"),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  backend = "cmdstanr",
                  sample_prior = samp_prior,
                  file = "models/theta/fit_ri_int",
                  save_pars = save_pars(all = TRUE),
                  seed = seed)

success_step(fit_ri_int)

# Model 2 - Emotion * mask + intensity ------------------------------------

form_ri_no3int <- bf(diff_theta ~ emotion + mask + intensity + emotion:mask + emotion:intensity + mask:intensity + (1|id), 
                     kappa ~ emotion + mask + intensity + emotion:mask + emotion:intensity + mask:intensity + (1|id))
                  
fit_ri_no3int <- brm(form_ri_no3int,
                  data = dat_fit,
                  prior = prior_von_mises,
                  family = von_mises(link = "tan_half", link_kappa = "log"),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  backend = "cmdstanr",
                  sample_prior = samp_prior,
                  file = "models/theta/fit_ri_no3int",
                  save_pars = save_pars(all = TRUE),
                  seed = seed)

success_step(fit_ri_no3int)

# Model 3a - tas * mask -----------------------------------------------------

prior_von_mises_tas_mask <- c(
  # theta
  prior(normal(0, 2), class = "b", dpar = "", coef = "Intercept"),
  prior(normal(0, 2), class = "b", dpar = "", coef = "mask_e1"),
  prior(normal(0, 0.5), class = "b", dpar = "", coef = "tas"),
  prior(normal(0, 0.5), class = "b", dpar = "", coef = "mask_e1:tas"),
  
  # kappa
  prior(normal(0, 2), class = "b", dpar = "kappa", coef = "Intercept"),
  prior(normal(0, 2), class = "b", dpar = "kappa", coef = "mask_e1"),
  prior(normal(0, 0.5), class = "b", dpar = "kappa", coef = "tas"),
  prior(normal(0, 0.5), class = "b", dpar = "kappa", coef = "mask_e1:tas")
)

form_ri_tas_mask <- bf(diff_theta ~ 0 + Intercept + mask_e * tas + (1|id), 
                       kappa ~ 0 + Intercept + mask_e * tas + (1|id))

fit_ri_tas_mask <- brm(form_ri_tas_mask,
                  data = dat_fit,
                  prior = prior_von_mises_tas_mask,
                  family = von_mises(link = "tan_half", link_kappa = "log"),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  #backend = "cmdstanr",
                  sample_prior = samp_prior,
                  file = "models/theta/fit_ri_tas_mask",
                  save_pars = save_pars(all = TRUE),
                  seed = seed)

success_step(fit_ri_tas_mask)

# Model 3b - tas * mask (subtle) -----------------------------------------

# here we use a different approach because the fitting process is more
# difficult

prior_von_mises_tas_mask_subtle <- c(
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

chains_subtle <- 4
iter_subtle <- 10000
cores_subtle <- chains_subtle

fit_ri_tas_mask_subtle <- brm(fit_ri_tas_mask$formula,
                          data = dat_fit,
                          family = von_mises(link = "tan_half", link_kappa = "log"),
                          chains = chains_subtle,
                          prior = prior_von_mises_tas_mask_un,
                          cores = cores_subtle,
                          iter = iter_subtle,
                          backend = "cmdstanr",
                          threads = threading(6),
                          sample_prior = samp_prior,
                          file = "models/theta/fit_ri_tas_mask_subtle",
                          save_pars = save_pars(all = TRUE),
                          seed = seed)

success_step(fit_ri_tas_mask_subtle)

# Model 4a - aq * mask ------------------------------------------------------

prior_von_mises_aq_mask <- c(
  # theta
  prior(normal(0, 2), class = "b", dpar = "", coef = "Intercept"),
  prior(normal(0, 2), class = "b", dpar = "", coef = "mask_e1"),
  prior(normal(0, 0.5), class = "b", dpar = "", coef = "aq"),
  prior(normal(0, 0.5), class = "b", dpar = "", coef = "mask_e1:aq"),
  
  # kappa
  prior(normal(0, 2), class = "b", dpar = "kappa", coef = "Intercept"),
  prior(normal(0, 2), class = "b", dpar = "kappa", coef = "mask_e1"),
  prior(normal(0, 0.5), class = "b", dpar = "kappa", coef = "aq"),
  prior(normal(0, 0.5), class = "b", dpar = "kappa", coef = "mask_e1:aq")
)

form_ri_aq_mask <- bf(diff_theta ~ 0 + Intercept + mask_e * aq + (1|id), 
                 kappa ~ 0 + Intercept + mask_e * aq + (1|id))

fit_ri_aq_mask  <- brm(form_ri_aq_mask,
                  data = dat_fit,
                  prior = prior_von_mises_aq_mask,
                  family = von_mises(link = "tan_half", link_kappa = "log"),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  backend = "cmdstanr",
                  sample_prior = samp_prior,
                  file = "models/theta/fit_ri_aq_mask",
                  save_pars = save_pars(all = TRUE),
                  seed = seed)

success_step(fit_ri_aq_mask)

# Model 4b - aq * mask (subtle) ------------------------------------------------------

fit_ri_aq_mask_subtle  <- brm(form_ri_aq_mask,
                       data = dat_fit_subtle,
                       prior = prior_von_mises_aq_mask,
                       family = von_mises(link = "tan_half", link_kappa = "log"),
                       chains = chains,
                       cores = cores,
                       iter = iter,
                       backend = "cmdstanr",
                       sample_prior = samp_prior,
                       file = "models/theta/fit_ri_aq_mask_subtle",
                       save_pars = save_pars(all = TRUE),
                       seed = seed)

success_step(fit_ri_aq_mask_subtle)


# Model 5 - mask (neutral faces) ------------------------------------------

prior_von_mises_neu <- c(
  prior(uniform(-3.141593, 3.141593), class = "b", dpar = "", lb = -3.141593, ub = 3.141593), # betas prior
  prior(normal(0, 2), class = "b", dpar = "kappa") # kappa prior
)

dat_neutral <- dat %>% 
  filter(emotion == "neutrality") %>% 
  mutate(mask_e = factor(mask),
         theta_cen = theta - pi) # centering pi

contrasts(dat_neutral$mask_e) <- contr.sum(2)/2

form_ri_neu <- bf(theta_cen ~ 0 + Intercept + mask_e + (1|id), 
                  kappa ~  0 + Intercept + mask_e + (1|id))

fit_ri_neu <- brm(form_ri_neu,
                  data = dat_neutral,
                  prior = prior_von_mises_neu,
                  family = von_mises(link = "tan_half", link_kappa = "log"),
                  chains = chains,
                  cores = cores,
                  iter = iter,
                  backend = "cmdstanr",
                  sample_prior = samp_prior,
                  file = "models/theta/fit_ri_neu",
                  save_pars = save_pars(all = TRUE),
                  seed = seed)

success_step(fit_ri_neu)