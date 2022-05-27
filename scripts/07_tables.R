# Packages ----------------------------------------------------------------

library(dplyr)
library(flextable)
library(forcats)
library(officer)
library(ftExtra)
library(tidybayes)
library(tidyr)
library(here)
library(purrr)

# Functions ---------------------------------------------------------------

devtools::load_all()

save_table <- function(table, path){
  save_as_docx(table, path = path)
}

flextable_with_param <- function(data){
  data %>% 
    flextable() %>% 
    autofit() %>% 
    theme_vanilla() %>% 
    colformat_md(part = "all") %>% 
    fontsize(part = "all", size = 9)
}

get_post_summary <- function(data, group, sign = FALSE, null = 0){
  group <- rlang::enexpr(group)
  out <- data %>% 
    group_by(!!group) %>% 
    median_hdi(value) %>% 
    select(emotion, value, .lower, .upper)
  if(sign){
    out %>% 
      mutate(across(where(is.numeric), round, 3),
             value_chr = sprintf("**%s** [%s, %s]", value, .lower, .upper),
             value_chr = ifelse(.lower <= null & null <= .upper,
                                value_chr,
                                paste(value_chr, "*")))
  }else{
    out
  }
}

set_emotion_order <- function(data, col, levels, dpar = TRUE){
  col <- rlang::enexpr(col)
  data %>% 
    mutate(!!col := factor(!!col, levels = levels)) %>% {
      if(dpar){
        arrange(., main_param, !!col)
      }else{
        arrange(., !!col)
      }
    }
}

# Loading Data ------------------------------------------------------------

dat <- readRDS(file.path("data", "cleaned", "valid", "dat_valid_ang_final.rds"))
emo_coords <- readRDS(file.path("objects", "emo_coords.rds"))
intensity_objects <- readRDS(file.path("objects", "intensity_objects.rds"))
circular_objects <- readRDS(file.path("objects", "circular_objects.rds"))
emo_order = c("Surprise", "Sadness", "Happiness", "Fear", "Disgust", "Anger")

# Theta/Kappa Mask vs No Mask ---------------------------------------------

tab_kappa_angle_mask_effect <- circular_objects$tidy_post$post_fit_ri_diff_mask %>% 
  group_by(emotion, .draw) %>% 
  summarise(angle_yes = mean(angle_yes),
            angle_no = mean(angle_no),
            angle_diff = mean(angle_diff),
            kappa_inv_yes = mean(kappa_inv_yes),
            kappa_inv_no = mean(kappa_inv_no),
            kappa_inv_ratio = mean(kappa_inv_ratio)) %>% 
  ungroup() %>% 
  select(emotion, angle_yes, angle_no, angle_diff, kappa_inv_yes, kappa_inv_no, kappa_inv_ratio, .draw) %>% 
  pivot_longer(2:(ncol(.)-1), names_to = "param", values_to = "value") %>% 
  group_by(param) %>% 
  nest() %>% 
  mutate(null = ifelse(startsWith(param, "kappa"), 1, 0)) %>% 
  mutate(summary = map2(data, null, function(x, y) get_post_summary(x, emotion, sign = TRUE, null = y))) %>% 
  unnest(summary) %>% 
  select(param, emotion, value_chr) %>% 
  mutate(main_param = ifelse(startsWith(param, "kappa"), "Uncertainty", "Bias"),
         contr_param = case_when(grepl("yes", param) ~ "Mask~yes~",
                                 grepl("no", param) ~ "Mask~no~",
                                 TRUE ~ "Contrast"),
         emotion = as.character(emotion)) %>% 
  ungroup() %>% 
  select(-param) %>% 
  pivot_wider(names_from = contr_param, values_from = value_chr) %>%
  clean_emotion_names(emotion) %>% 
  set_emotion_order(emotion, levels = emo_order) %>% 
  flextable_with_param() %>% 
  align(part = "header", align = "center") %>% 
  align(j = 2, part = "body", align = "center") %>% 
  merge_v(2) %>% 
  set_header_labels(values = list(
    emotion = "Emotion",
    main_param = "Parameter"))

# Theta/Kappa Delta Mask - Full vs Subtle ---------------------------------

tab_kappa_angle_mask_intensity_effect <- circular_objects$tidy_post$post_fit_ri_diff_mask %>% 
  select(emotion, intensity, angle_diff, kappa_inv_ratio, .draw) %>% 
  pivot_wider(names_from = intensity, values_from = c(angle_diff, kappa_inv_ratio)) %>% 
  mutate(angle_diff_int = angle_diff_full - angle_diff_subtle,
         kappa_inv_ratio_int = kappa_inv_ratio_full / kappa_inv_ratio_subtle) %>% 
  select(emotion, starts_with("angle"), starts_with("kappa"), .draw) %>% 
  pivot_longer(2:(ncol(.) - 1), names_to = "param", values_to = "value") %>% 
  group_by(param) %>% 
  nest() %>% 
  mutate(null = ifelse(startsWith(param, "kappa"), 1, 0)) %>% 
  mutate(summary = map2(data, null, function(x, y) get_post_summary(x, emotion, sign = TRUE, null = y))) %>% 
  unnest(summary) %>% 
  select(param, emotion, value_chr) %>% 
  mutate(main_param = ifelse(startsWith(param, "kappa"), "Uncertainty", "Bias"),
         contr_param = case_when(grepl("full", param) ~ "$\\Delta\\;Mask_{full}$",
                                 grepl("subtle", param) ~ "$\\Delta\\;Mask_{subtle}$",
                                 TRUE ~ "Contrast"),
         emotion = as.character(emotion)) %>% 
  ungroup() %>% 
  select(-param) %>% 
  pivot_wider(names_from = contr_param, values_from = value_chr) %>%
  clean_emotion_names(emotion) %>% 
  set_emotion_order(emotion, levels = emo_order) %>% 
  flextable_with_param() %>% 
  align(part = "header", align = "center") %>% 
  align(j = 2, part = "body", align = "center") %>% 
  merge_v(2) %>% 
  set_header_labels(values = list(
    emotion = "Emotion",
    main_param = "Parameter"))

# Int Mask vs No Mask -----------------------------------------------

tab_int_mask_effect <- intensity_objects$tidy_post$post_fit_ri_diff_mask %>% 
  group_by(emotion, .draw) %>% 
  summarise(int_diff = mean(int_diff),
            yes = mean(yes),
            no = mean(no)) %>% 
  select(emotion, yes, no, int_diff, .draw) %>% 
  pivot_longer(2:(ncol(.) - 1), names_to = "param", values_to = "value") %>% 
  group_by(param) %>% 
  nest() %>% 
  mutate(null = 0,
         summary = map(data, get_post_summary, emotion, TRUE)) %>% 
  unnest(summary) %>% 
  select(param, emotion, value_chr) %>% 
  mutate(param = case_when(grepl("yes", param) ~ "Mask~yes~",
                           grepl("no", param) ~ "Mask~no~",
                           TRUE ~ "Contrast"),
         emotion = as.character(emotion)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = param, values_from = value_chr) %>%
  clean_emotion_names(emotion) %>% 
  set_emotion_order(emotion, levels = emo_order, dpar = FALSE) %>% 
  flextable_with_param() %>% 
  align(part = "header", align = "center") %>% 
  align(j = 2, part = "body", align = "center") %>% 
  merge_v(2) %>% 
  set_header_labels(values = list(
    emotion = "Emotion"
  ))

# Int Delta Mask - Full vs Subtle -----------------------------------

tab_int_mask_intensity_effect <- intensity_objects$tidy_post$post_fit_ri_diff_mask %>% 
  select(emotion, intensity, int_diff, .draw) %>% 
  pivot_wider(names_from = intensity, values_from = int_diff) %>% 
  mutate(int_diff_intensity = full - subtle) %>% 
  select(emotion, full, subtle, int_diff_intensity, .draw) %>% 
  pivot_longer(2:(ncol(.) - 1), names_to = "param", values_to = "value") %>% 
  group_by(param) %>% 
  nest() %>% 
  mutate(null = 0,
         summary = map(data, get_post_summary, emotion, TRUE)) %>% 
  unnest(summary) %>% 
  select(param, emotion, value_chr) %>% 
  mutate(param = case_when(grepl("full", param) ~ "$\\Delta\\;Mask_{full}$",
                           grepl("subtle", param) ~ "$\\Delta\\;Mask_{subtle}$",
                           TRUE ~ "Contrast"),
         emotion = as.character(emotion)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = param, values_from = value_chr) %>%
  clean_emotion_names(emotion) %>% 
  set_emotion_order(emotion, levels = emo_order, dpar = FALSE) %>% 
  flextable_with_param() %>% 
  align(part = "header", align = "center") %>% 
  align(j = 2, part = "body", align = "center") %>% 
  merge_v(2) %>% 
  set_header_labels(values = list(
    emotion = "Emotion"
  ))

# Angle/Theta Full vs Subtle ----------------------------------------------

tab_kappa_angle_intensity_effect <- circular_objects$tidy_post$post_fit_ri_diff_int %>% 
  group_by(emotion, .draw) %>% 
  summarise(angle_full = mean(angle_full),
            angle_subtle = mean(angle_subtle),
            angle_diff = mean(angle_diff),
            kappa_inv_full = mean(kappa_inv_full),
            kappa_inv_subtle = mean(kappa_inv_subtle),
            kappa_inv_ratio = mean(kappa_inv_ratio)) %>% 
  ungroup() %>% 
  select(emotion, angle_full, angle_subtle, angle_diff, kappa_inv_full, kappa_inv_subtle, kappa_inv_ratio, .draw) %>% 
  pivot_longer(2:(ncol(.)-1), names_to = "param", values_to = "value") %>% 
  group_by(param) %>% 
  nest() %>% 
  mutate(null = ifelse(startsWith(param, "kappa"), 1, 0)) %>% 
  mutate(summary = map2(data, null, function(x, y) get_post_summary(x, emotion, sign = TRUE, null = y))) %>% 
  unnest(summary) %>% 
  select(param, emotion, value_chr) %>% 
  mutate(main_param = ifelse(startsWith(param, "kappa"), "Uncertainty", "Bias"),
         contr_param = case_when(grepl("full", param) ~ "Intensity~full~",
                                 grepl("subtle", param) ~ "Intensity~subtle~",
                                 TRUE ~ "Contrast"),
         emotion = as.character(emotion)) %>% 
  ungroup() %>% 
  select(-param) %>% 
  pivot_wider(names_from = contr_param, values_from = value_chr) %>%
  clean_emotion_names(emotion) %>% 
  set_emotion_order(emotion, levels = emo_order) %>% 
  flextable_with_param() %>% 
  align(part = "header", align = "center") %>% 
  align(j = 2, part = "body", align = "center") %>% 
  merge_v(2) %>% 
  set_header_labels(values = list(
    emotion = "Emotion",
    main_param = "Parameter"))

# Int full vs subtle ------------------------------------------------------

tab_int_intensity_effect <- intensity_objects$tidy_post$post_fit_ri_int %>% 
  group_by(emotion, intensity, .draw) %>% 
  summarise(int = mean(int)) %>% 
  pivot_wider(names_from = intensity, values_from = int) %>% 
  mutate(int_diff = full - subtle) %>% 
  select(emotion, full, subtle, int_diff, .draw) %>% 
  pivot_longer(2:(ncol(.) - 1), names_to = "param", values_to = "value") %>% 
  group_by(param) %>% 
  nest() %>% 
  mutate(null = 0,
         summary = map(data, get_post_summary, emotion, TRUE)) %>% 
  unnest(summary) %>% 
  select(param, emotion, value_chr) %>% 
  mutate(param = case_when(grepl("full", param) ~ "Intensity~full~",
                           grepl("subtle", param) ~ "Intensity~subtle~",
                           TRUE ~ "Contrast"),
         emotion = as.character(emotion)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = param, values_from = value_chr) %>%
  clean_emotion_names(emotion) %>%
  set_emotion_order(emotion, emo_order, dpar = FALSE) %>% 
  flextable_with_param() %>% 
  align(part = "header", align = "center") %>% 
  align(j = 2, part = "body", align = "center") %>% 
  merge_v(2) %>% 
  set_header_labels(values = list(
    emotion = "Emotion"
  ))

# Saving ------------------------------------------------------------------

tab_list <- make_named_list(tab_kappa_angle_mask_effect, tab_kappa_angle_mask_intensity_effect,
                tab_int_mask_effect, tab_int_mask_intensity_effect,
                tab_kappa_angle_intensity_effect, tab_int_intensity_effect)

tab_files <- paste0(names(tab_list), ".docx")

saveRDS(tab_list, file = here("objects", "paper_tables.rds"))
purrr::walk2(tab_list, file.path("tables", tab_files), save_table)
