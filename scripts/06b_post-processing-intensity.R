# Packages ----------------------------------------------------------------

library(brms)
library(tidybayes)
library(dplyr)
library(tidyr)

# Functions ---------------------------------------------------------------

devtools::load_all()

# Setup -------------------------------------------------------------------

seed <- 2022
set.seed(seed)

# Loading Model -----------------------------------------------------------

fit_list <- load_models("models/intensity")

# Model Summary -----------------------------------------------------------

tidy_list_fit <- lapply(fit_list, tidy_brm_from_fit)
tidy_list_priors <- lapply(fit_list, tidy_priors)
fit_info <- lapply(fit_list, get_model_info)

# Posterior Draws ---------------------------------------------------------

# fit_ri_int

data_grid_fit_ri_int <- expand_grid(
  mask = unique(fit_list$fit_ri_int$data$mask),
  emotion = unique(fit_list$fit_ri_int$data$emotion),
  intensity = unique(fit_list$fit_ri_int$data$intensity)
)

# getting posterior predictions

post_fit_ri_int <- epred_draws(fit_list$fit_ri_int, newdata = data_grid_fit_ri_int,
                               re_formula = NA)

post_fit_ri_int <- post_fit_ri_int %>% 
  rename("int" = .epred)

# computing relevant posterior transformations, mask_ratio and mask_diff
# difference = mask_yes - mask_no

post_fit_ri_diff_mask <- post_fit_ri_int %>% 
  ungroup() %>% 
  select(mask, emotion, intensity, int, .draw, -.row) %>% 
  pivot_wider(names_from = mask, values_from = int) %>% 
  mutate(int_diff = yes - no)

# Adding Information Criteria ---------------------------------------------

fit_list$fit_ri_int <- add_criterion(fit_list$fit_ri_int, "loo", ndraws = 3000)
fit_list$fit_ri_no3int <- add_criterion(fit_list$fit_ri_no3int, "loo", ndraws = 3000)

int_effect_weights <- model_weights(fit_list$fit_ri_int, fit_list$fit_ri_no3int,
                                    ndraws = 3000)

int_effect_loo_diff <- loo_compare(fit_list$fit_ri_int, fit_list$fit_ri_no3int)

loo_list <- list(
  fit_ri_int = fit_list$fit_ri_int$criteria$loo,
  fit_ri_no3int = fit_list$fit_ri_no3int$criteria$loo,
  weights = int_effect_weights,
  diff = int_effect_loo_diff
)

# Bayes Factor ------------------------------------------------------------

bf_list <- list(
  tas = hypothesis(fit_list$fit_ri_tas_mask, "tas = 0"),
  tas_mask = hypothesis(fit_list$fit_ri_tas_mask, "mask_e1:tas = 0"),
  tas_subtle = hypothesis(fit_list$fit_ri_tas_mask, "tas = 0"),
  tas_mask_subtle = hypothesis(fit_list$fit_ri_tas_mask, "mask_e1:tas = 0"),
  aq = hypothesis(fit_list$fit_ri_aq_mask, "aq = 0"),
  aq_mask = hypothesis(fit_list$fit_ri_aq_mask, "mask_e1:aq = 0"),
  aq_subtle = hypothesis(fit_list$fit_ri_aq_mask_subtle, "aq = 0"),
  aq_mask_subtle = hypothesis(fit_list$fit_ri_aq_subtle, "mask_e1:aq = 0"),
)

# Saving ------------------------------------------------------------------

intensity <- list(
  fit_info = fit_info,
  tidy_fit = tidy_list_fit,
  priors = tidy_list_priors,
  tidy_post = list(post_fit_ri_int = post_fit_ri_int,
                   post_fit_ri_diff_mask = post_fit_ri_diff_mask),
  loo = loo_list,
  bf = bf_list
)

saveRDS(intensity, file = file.path("objects", "intensity_objects.rds"))