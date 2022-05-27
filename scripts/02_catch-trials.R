# Packages ----------------------------------------------------------------

library(tidyverse)

# Functions ---------------------------------------------------------------

# all the function in the R folder so they are automatically loaded or using
# devtools::load_all()

devtools::load_all()

# Importing Data & Data Correction----------------------------------------------------------

dat_catch <- read_rds(file.path("data", "cleaned", "catch", "dat_catch_ang.rds"))
dat_valid <- read_rds(file.path("data", "cleaned", "valid","dat_valid_ang.rds"))

dat_catch <- dat_catch %>%
  select(c("trial","trial_type", "id", "image", "intensity",
           "emotion", "y_cen", "x_cen", "angle", "int")) %>% 
  mutate(intensity = case_when(image == "Immagine2.png" ~ "2_medium_low",
                               image == "Immagine15.png" ~ "1_low",
                               TRUE ~ intensity))

# Coordinates -------------------------------------------------------------

dat_catch <- dat_catch %>% 
  group_by(id) %>% 
  mutate(trial = 1:n()) %>% 
  ungroup() %>% 
  mutate(# TODO check the integer conversion. I keep only int because is the same
         #dist_cen = as.integer(dist_cen),
         etiquette_int = level_int_position(int),
         etiquette_emo = seg_position(round(angle, 1)))

# TODO check what emoetiq is

emo_etiq <- tibble(
  emotion = c("satisfaction", "happiness", "elation", "pride", "anger", "contempt",
              "disgust", "envy", "guilt", "shame", "fear", "sadness", "surprise", 
              "interest", "hope", "relief"),
  angle_emo_end = seq(22.5,360, by=(360/16))
)

# Compute Catch Trials Accuracy -------------------------------------------

dat_catch <- dat_catch %>% 
  mutate(corr_int = ifelse(intensity == etiquette_int, 1, 0),
         corr_emo = ifelse(emotion == etiquette_emo, 1, 0),
         # correcting scripting error
         corr_int = case_when(image == "Immagine6.png" & etiquette_int == "1_low" ~ 1,
                              image == "Immagine14.png" & etiquette_int == "2_medium_low" ~ 1,
                              TRUE ~ corr_int))

# Remove Participants < 75% accuracy -------------------------------------

subj_to_remove <- dat_catch %>% 
  drop_na() %>% 
  mutate(corr_tot = ifelse(corr_int == corr_emo, 1, 0)) %>% 
  group_by(id) %>% 
  summarise(corr_int = mean(corr_int),
            corr_emo = mean(corr_emo),
            corr_tot = mean(corr_tot)) %>% 
  filter(corr_tot < 0.75) %>% 
  distinct() %>% 
  pull(id)

# removing bad subjects
dat_valid <- dat_valid %>% filter(!(id %in% subj_to_remove))

# Saving ------------------------------------------------------------------

saveRDS(dat_catch, file = file.path("data", "cleaned", "catch" ,"dat_catch_ang_acc.rds"))
saveRDS(dat_valid, file = file.path("data", "cleaned", "valid", "dat_valid_ang_final.rds"))