# Packages ----------------------------------------------------------------

library(tidyr)
library(dplyr)
library(brms)
library(tidybayes)

# Functions ---------------------------------------------------------------

devtools::load_all()

# Setup -------------------------------------------------------------------

seed <- 2022
set.seed(seed)

# Loading Model -----------------------------------------------------------

fit_list <- load_models("models/theta")

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

# getting posterior predictions. Here the inverse of the link functions are applied
# tan-half for mu and exp for kappa

post_fit_ri_int <- epred_draws(fit_list$fit_ri_int, newdata = data_grid_fit_ri_int,
                               re_formula = NA,
                               dpar = "kappa")

# getting posterior in degrees. Given that is a linear transformation, results are
# conceptually the same

post_fit_ri_int$angle <- rad_to_deg(post_fit_ri_int$.epred)


post_fit_ri_int <- post_fit_ri_int %>% 
  rename("theta" = .epred) %>% 
  mutate(kappa_inv = kappa_to_var(kappa)) # inverse of concentration

# computing relevant posterior transformations, mask_ratio and mask_diff
# the ratio is mask_yes / mask_no
# the difference is mask_yes - mask_no

post_fit_ri_diff_mask <- post_fit_ri_int %>% 
  ungroup() %>% 
  select(mask, emotion, intensity, angle, kappa, kappa_inv, .draw) %>% 
  pivot_wider(names_from = mask, values_from = c(kappa, angle, kappa_inv)) %>% 
  mutate(kappa_inv_ratio = kappa_inv_yes/kappa_inv_no,
         kappa_log_ratio_inv = log(kappa_inv_ratio),
         kappa_ratio = kappa_yes/kappa_no,
         kappa_log_ratio = log(kappa_ratio),
         angle_diff = angle_yes - angle_no)

post_fit_ri_diff_int <- post_fit_ri_int %>% 
  ungroup() %>% 
  select(mask, emotion, intensity, angle, kappa, kappa_inv, .draw) %>% 
  pivot_wider(names_from = intensity, values_from = c(kappa, angle, kappa_inv)) %>% 
  mutate(kappa_inv_ratio = kappa_inv_full/kappa_inv_subtle,
         kappa_log_ratio_inv = log(kappa_inv_ratio),
         kappa_ratio = kappa_full/kappa_subtle,
         kappa_log_ratio = log(kappa_ratio),
         angle_diff = angle_full - angle_subtle)

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
  bias = list(
    tas = hypothesis(fit_list$fit_ri_tas_mask, "tas = 0"),
    tas_mask = hypothesis(fit_list$fit_ri_tas_mask, "mask_e1:tas = 0"),
    tas_subtle = hypothesis(fit_list$fit_ri_tas_mask_subtle, "tas = 0"),
    tas_mask_subtle = hypothesis(fit_list$fit_ri_tas_mask_subtle, "mask_e1:tas = 0"),
    aq = hypothesis(fit_list$fit_ri_aq_mask, "aq = 0"),
    aq_mask = hypothesis(fit_list$fit_ri_aq_mask, "mask_e1:aq = 0"),
    aq_subtle = hypothesis(fit_list$fit_ri_aq_mask_subtle, "aq = 0"),
    aq_mask_subtle = hypothesis(fit_list$fit_ri_aq_mask_subtle, "mask_e1:aq = 0"),
  ),
  uncertainty = list(
    tas = hypothesis(fit_list$fit_ri_tas_mask, "kappa_tas = 0"),
    tas_mask = hypothesis(fit_list$fit_ri_tas_mask, "kappa_mask_e1:tas = 0"),
    tas_subtle = hypothesis(fit_list$fit_ri_tas_mask_subtle, "kappa_tas = 0"),
    tas_mask_subtle = hypothesis(fit_list$fit_ri_tas_mask_subtle, "kappa_mask_e1:tas = 0"),
    aq = hypothesis(fit_list$fit_ri_aq_mask, "kappa_aq = 0"),
    aq_mask = hypothesis(fit_list$fit_ri_aq_mask, "kappa_mask_e1:aq = 0"),
    aq_subtle = hypothesis(fit_list$fit_ri_aq_mask_subtle, "kappa_aq = 0"),
    aq_mask_subtle = hypothesis(fit_list$fit_ri_aq_mask_subtle, "kappa_mask_e1:aq = 0")
  )
)

# Saving ------------------------------------------------------------------

circular <- list(
  fit_info = fit_info,
  tidy_fit = tidy_list_fit,
  priors = tidy_list_priors,
  tidy_post = list(post_fit_ri_int = post_fit_ri_int,
                   post_fit_ri_diff_mask = post_fit_ri_diff_mask,
                   post_fit_ri_diff_int = post_fit_ri_diff_int),
  loo = loo_list,
  bf = bf_list
)

saveRDS(circular, file = file.path("objects", "circular_objects.rds"))