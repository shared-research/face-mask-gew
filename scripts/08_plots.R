# Packages ----------------------------------------------------------------

library(brms)
library(tidybayes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(purrr)
library(magick)

# Functions ---------------------------------------------------------------

devtools::load_all()

ggsave_plot <- function(plot, name, device = c("png", "tiff", "eps", "pdf", "svg"),
                        width = width, height = height){
  device <- match.arg(device)
  name <- paste0(name, ".", device)
  ggsave(plot, filename = name, device = device, width = width, height = height)
}

theme_paper <- function(font_size = 12){
  cowplot::theme_minimal_grid(font_size = font_size)
}

put_note <- function(x, y, text, size = 3, ...){
    annotate("label", x, y, label = text, 
             fontface = 2, 
             fill = "white", 
             label.size = NA,
             size = size,
             ...)
}

w_stat_halfeye <- function(alpha = 0.8){
  stat_halfeye(alpha = alpha, 
               size = 2.5, 
               .width = 0.95)
}
  

# Importing Data ----------------------------------------------------------

intensity_objects <- readRDS(file.path("objects", "intensity_objects.rds"))
circular_objects <- readRDS(file.path("objects", "circular_objects.rds"))
emo_coords <- readRDS(file.path("objects", "emo_coords.rds"))
dat <- readRDS(file.path("data", "cleaned", "valid", "dat_valid_ang_final.rds"))

# EDA Plots ---------------------------------------------------------------

bg <- magick::image_read("files/gew_low_res.png")
bg <- magick::image_modulate(bg, brightness = 80)

gew_legend <- emo_coords %>%   
  mutate(mask = "Legend",
         flip = ifelse(x_emo < 0, angle_emo + 180, angle_emo),
         emotion = stringr::str_to_title(emotion)) %>% 
  ggplot() +
  ggpubr::background_image(bg) +
  geom_text(aes(x = x_emo*0.75, y = y_emo*0.75, 
                label = emotion, 
                angle = flip),
            size = 6, fontface = "bold",
            check_overlap = TRUE) +
  facet_grid(. ~ mask) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)) +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300))

dat_plot <- dat %>% 
  select(emotion, mask, intensity, x_cen, y_cen) %>% 
  mutate(mask = ifelse(mask == "yes", "Mask", "No Mask"),
         intensity = stringr::str_to_title(intensity),
         emotion = stringr::str_to_title(emotion),
         emotion = ifelse(emotion == "Neutrality", "Neutral", emotion),
         emotion = factor(emotion),
         emotion = forcats::fct_relevel(emotion, "Neutral"))

neutral_plot <- dat_plot %>% 
  filter(emotion == "Neutral") %>% 
  ggplot(aes(x = x_cen, y = y_cen)) +
  ggpubr::background_image(bg) +
  geom_point(alpha = 0.5, aes(color = mask), show.legend = FALSE, size = 3) +
  ggh4x::facet_nested(mask ~ emotion, switch="y") +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(size = 20, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)) +
  scale_color_manual(values = c("black", "black", "NA"))

plot_gew_legend_neutral <- plot_grid(neutral_plot, gew_legend, labels = "AUTO")

plot_gew_emotions <- dat_plot %>% 
  filter(emotion != "Neutral") %>% 
  ggplot(aes(x = x_cen, y = y_cen)) +
  ggpubr::background_image(bg) +
  geom_point(alpha = 0.5, size = 2) +
  ggh4x::facet_nested(mask + intensity ~ emotion) +
  coord_fixed(xlim = c(-300, 300), ylim = c(-300, 300)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)) +
  scale_fill_manual(values = c("NA", "white"))

# Plot Angle Mask vs No Mask ----------------------------------------------

plot_angle_mask_a <- circular_objects$tidy_post$post_fit_ri_int %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>% 
  group_by(emotion, mask, .draw) %>% 
  summarise(angle = mean(angle)) %>% 
  ggplot(aes(x = angle, y = emotion, fill = mask)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
  stat_halfeye(alpha = 0.8, size = 3) +
  theme_paper() +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.9, 0.15)) +
  xlab("Bias") +
  labs(fill = "Mask") 

plot_angle_mask_b <- circular_objects$tidy_post$post_fit_ri_diff_mask %>% 
  group_by(emotion, .draw) %>% 
  summarise(angle_diff = mean(angle_diff)) %>% 
  ggplot(aes(x = angle_diff, y = emotion)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
  stat_halfeye(size = 3) +
  theme_paper() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab(latex2exp::TeX("$\\Delta_{mask}$ Bias$"))

plot_angle_mask <- plot_grid(plot_angle_mask_a, plot_angle_mask_b, 
                             labels = "AUTO", rel_widths = c(3, 2), align = "hv")

# Plot Kappa Mask vs No Mask ----------------------------------------------

plot_kappa_mask_a <- circular_objects$tidy_post$post_fit_ri_int %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>% 
  group_by(emotion, mask, .draw) %>% 
  summarise(kappa_inv = mean(kappa_inv)) %>% 
  ggplot(aes(x = log(kappa_inv), y = emotion)) +
  stat_halfeye(aes(fill = mask),
               alpha = 0.8, size = 3) +
  theme_minimal(base_size = 15) +
  theme_paper() +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.90, 0.2)) +
  xlab("Uncertainty")

plot_kappa_mask_b <- circular_objects$tidy_post$post_fit_ri_diff_mask %>% 
  group_by(emotion, .draw) %>% 
  summarise(kappa_inv_ratio = mean(kappa_inv_ratio)) %>% 
  ggplot(aes(x = log(kappa_inv_ratio), y = emotion)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
  stat_halfeye(size = 3,
               position = position_dodge(width = 0.9)) +
  theme_paper() +
  theme(axis.title.y = element_blank(),axis.text.y = element_blank()) +
  xlab(latex2exp::TeX("log $Ratio_{mask}$")) +
  scale_x_continuous(n.breaks = 4)

plot_kappa_mask <- plot_grid(plot_kappa_mask_a, plot_kappa_mask_b,
                             labels = "AUTO", rel_widths = c(3, 2), align = "hv")


# Plot Mask Delta Angle Full vs Subtle ------------------------------------

plot_angle_mask_int_a <- circular_objects$tidy_post$post_fit_ri_diff_mask %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>% 
  select(emotion, intensity, .draw , angle_diff) %>% 
  ggplot(aes(x = angle_diff, y = emotion, fill = intensity)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
  w_stat_halfeye() +
  theme_paper() +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.85, 0.1)) +
  xlab(latex2exp::TeX("$\\Delta_{mask}$ Bias")) +
  labs(fill = "Intensity") +
  put_note(-15, 6.8, text = "paste(bold(\" Mask \"), \"<\", bold(\" No Mask \"))", 
           parse = TRUE) +
  put_note(15, 6.8, text = "paste(bold(\" Mask \"), \">\", bold(\" No Mask \"))", 
           parse = TRUE)

plot_angle_mask_int_b <- circular_objects$tidy_post$post_fit_ri_diff_mask %>% 
  select(emotion, intensity, .draw , angle_diff) %>% 
  pivot_wider(names_from = intensity, values_from = angle_diff) %>% 
  mutate(angle_diff = full - subtle) %>% 
  ggplot(aes(x = angle_diff, y = emotion)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
  w_stat_halfeye() +
  theme_paper() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab(latex2exp::TeX("$\\Delta_{intensity}$ Bias"))

plot_angle_mask_int <- plot_grid(plot_angle_mask_int_a, plot_angle_mask_int_b, 
                                 labels = "AUTO", rel_widths = c(3, 2), align = "hv")

# Plot Mask Delta Kappa Full vs Subtle ------------------------------------

plot_kappa_mask_int_a <- circular_objects$tidy_post$post_fit_ri_diff_mask %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>% 
  select(emotion, intensity, .draw, kappa_inv_ratio) %>% 
  ggplot(aes(x = log(kappa_inv_ratio), y = emotion, fill = intensity)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
  w_stat_halfeye() +
  theme_paper() +
  xlim(-1, 2) +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.85, 0.2)) +
  xlab(latex2exp::TeX("log $Ratio_{mask}$")) +
  labs(fill = "Intensity") +
  put_note(-0.5, 6.9, text = "paste(bold(\" Mask \"), \"<\", bold(\" No Mask \"))", 
           parse = TRUE) +
  put_note(1, 6.9, text = "paste(bold(\" Mask \"), \">\", bold(\" No Mask \"))", 
           parse = TRUE)

plot_kappa_mask_int_b <- circular_objects$tidy_post$post_fit_ri_diff_mask %>% 
  select(emotion, intensity, .draw, kappa_inv_ratio) %>% 
  pivot_wider(names_from = intensity, values_from = kappa_inv_ratio) %>% 
  mutate(kappa_inv_ratio = full/subtle) %>% 
  ggplot(aes(x = log(kappa_inv_ratio), y = emotion)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
  w_stat_halfeye() +
  theme_paper() +
  theme(axis.title.y = element_blank(),axis.text.y = element_blank()) +
  xlab(latex2exp::TeX("log $Ratio_{intensity}$"))

plot_kappa_mask_int <- plot_grid(plot_kappa_mask_int_a, plot_kappa_mask_int_b, 
                            labels = "AUTO", rel_widths = c(3, 2), align = "hv")

# Plot Intensity Mask vs No Mask ------------------------------------------

plot_int_mask_a <- intensity_objects$tidy_post$post_fit_ri_int %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>% 
  group_by(emotion, mask, .draw) %>% 
  summarise(int = mean(int)) %>% 
  ggplot(aes(x = int, y = emotion, fill = mask)) +
  w_stat_halfeye() +
  theme_paper() +
  xlab("Perceived Intensity") +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.85, 0.10))

plot_int_mask_b <- intensity_objects$tidy_post$post_fit_ri_diff_mask %>% 
  group_by(emotion, .draw) %>% 
  summarise(int_diff = mean(int_diff)) %>% 
  ggplot(aes(x = int_diff, y = emotion)) +
  w_stat_halfeye() +
  theme_paper() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab(latex2exp::TeX("$\\Delta_{mask}\\; Perceived \\;Intensity$"))

plot_int_mask <- plot_grid(plot_int_mask_a, plot_int_mask_b, 
                           labels = "AUTO", rel_widths = c(3, 2), align = "hv")


# Plot Delta Mask Intensity Full vs Subtle --------------------------------

plot_int_mask_intensity_a <- intensity_objects$tidy_post$post_fit_ri_diff_mask %>% 
  mutate(emotion = as.character(emotion)) %>% 
  clean_emotion_names(emotion) %>% 
  ggplot(aes(x = int_diff, y = emotion, fill = intensity)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
  w_stat_halfeye() +
  theme_paper() +
  xlim(c(-60, 25)) +
  xlab(latex2exp::TeX("$\\Delta_{mask}\\; Perceived \\;Intensity$")) +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.8, 0.10)) +
  put_note(-20, 6.8, text = "paste(bold(\" Mask \"), \"<\", bold(\" No Mask \"))", 
           parse = TRUE) +
  put_note(15, 6.8, text = "paste(bold(\" Mask \"), \">\", bold(\" No Mask \"))", 
           parse = TRUE)

plot_int_mask_intensity_b <- intensity_objects$tidy_post$post_fit_ri_diff_mask %>% 
  select(emotion, intensity, .draw, int_diff) %>% 
  pivot_wider(names_from = intensity, values_from = int_diff) %>% 
  mutate(int_diff = full - subtle) %>% 
  ggplot(aes(x = int_diff, y = emotion)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
  w_stat_halfeye() +
  theme_paper() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab(latex2exp::TeX("$\\Delta_{Intensity}\\;Perceived \\;Intensity$"))

plot_int_mask_intensity <- plot_grid(plot_int_mask_intensity_a, plot_int_mask_intensity_b, 
                                labels = "AUTO", rel_widths = c(3, 2), align = "hv")


# Categorical Responses ---------------------------------------------------

# order as the wheel
dat$resp_emotion_label <- factor(dat$resp_emotion_label, levels = emo_coords$emo_order)

dat_summ <- dat %>% 
  filter(emotion != "neutrality") %>% 
  group_by(emotion, mask, intensity, resp_emotion_label) %>% 
  summarise(n = n())

plot_gew_discrete <- dat_summ %>% 
  mutate(intensity = stringr::str_to_title(intensity), 
         mask = stringr::str_to_title(mask)) %>% 
  clean_emotion_names(emotion) %>% 
  ggplot(aes(x = resp_emotion_label, y = n, fill = mask)) +
  geom_col(position = position_dodge()) +
  facet_grid(emotion~intensity) +
  cowplot::theme_minimal_hgrid() +
  theme_paper(font_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,
                                   face = ifelse(levels(dat_summ$resp_emotion_label) %in% unique(dat_summ$emotion),
                                                 "bold", "plain"),
                                   size = ifelse(levels(dat_summ$resp_emotion_label) %in% unique(dat_summ$emotion),
                                                 10, 8)),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 10),
        panel.grid.major.x = element_blank()) +
  labs(fill = "Mask")

# Saving ------------------------------------------------------------------

plot_list <- make_named_list(plot_gew_legend_neutral, plot_gew_emotions,
                             plot_gew_discrete,
                             plot_angle_mask, plot_angle_mask_int,
                             plot_kappa_mask, plot_kappa_mask_int,
                             plot_int_mask, plot_int_mask_intensity)

saveRDS(plot_list, file = "objects/paper_plots.rds")

# Good size for posterior plots

mkdir_if("figures/png")
mkdir_if("figures/pdf")
mkdir_if("figures/tiff")

width <- 16
height = 10

for(i in 1:length(plot_list)){
  plot_name <- names(plot_list)[i]
  if(grepl("mask|int", names(plot_list[i]))){
    ggsave_plot(plot_list[[i]], 
                name = file.path("figures", "pdf", plot_name), 
                device = "pdf",
                width = width, 
                height = height)
    ggsave_plot(plot_list[[i]],
                device = "png",
                name = file.path("figures", "png", plot_name), 
                width = width, 
                height = height)
  }
}

# GEW Plots

ggsave_plot(plot_list$plot_gew_legend_neutral,
            name = file.path("figures", "png", "plot_gew_legend_neutral"),
            device = "png", width = 16, height = 9)

ggsave_plot(plot_list$plot_gew_legend_neutral,
            name = file.path("figures", "pdf", "plot_gew_legend_neutral"),
            device = "pdf", width = 16, height = 9)

ggsave_plot(plot_list$plot_gew_discrete,
            name = file.path("figures", "png", "plot_gew_discrete"),
            device = "png", width = 15, height = 10)

ggsave_plot(plot_list$plot_gew_discrete,
            name = file.path("figures", "pdf", "plot_gew_discrete"),
            device = "pdf", width = 15, height = 10)