# Packages ----------------------------------------------------------------

library(tidyverse)

# Functions ---------------------------------------------------------------

devtools::load_all()

# Data --------------------------------------------------------------------

dat <- readRDS(file = file.path("data", "cleaned", "valid", "dat_valid_ang_final.rds"))

dat_fit <- dat %>% 
  filter(trial_type == "valid", emotion != "neutrality") %>% 
  mutate(diff_theta = unname(deg_to_rad(diff)),
         id = as.numeric(id),
         emotion = factor(emotion),
         mask = factor(mask),
         mask_e = mask,
         intensity = factor(intensity),
         tas = center_var(as.integer(TAS_score)),
         aq = center_var(as.integer(AQ_total_score)))

# Setting sum to 0 contrast 

contrasts(dat_fit$mask) <- contr.sum(length(unique(dat_fit$mask)))
contrasts(dat_fit$mask_e) <- contr.sum(length(unique(dat_fit$mask)))/2
contrasts(dat_fit$emotion) <- contr.sum(length(unique(dat_fit$emotion)))
contrasts(dat_fit$intensity) <- contr.sum(length(unique(dat_fit$intensity)))


# Saving ------------------------------------------------------------------

saveRDS(dat_fit, file = file.path("data", "cleaned", "dat_fit.rds"))