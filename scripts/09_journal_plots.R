# Packages ----------------------------------------------------------------

library(ragg)
library(ggplot2)
library(cowplot)

# Functions ---------------------------------------------------------------

devtools::load_all()

ggsave_plot <- function(plot, name, device = c("png", "tiff", "eps", "pdf", "svg"),
                        width = width, height = height){
  device <- match.arg(device)
  name <- paste0(name, ".", device)
  ggsave(plot, filename = name, device = device, width = width, height = height)
}


# Plots -------------------------------------------------------------------

plot_list <- readRDS(here::here("objects", "paper_plots.rds"))

# Saving ------------------------------------------------------------------

width_tiff <- 20
height_tiff <- 13

for(i in 1:length(plot_list)){
  plot_name <- names(plot_list)[i]
  if(grepl("mask|int", names(plot_list[i]))){
  
    # Journal size
    ragg::agg_tiff(file.path("figures", "tiff", paste0(plot_name, ".tiff")),
                   width = width_tiff, height = height_tiff, 
                   units = "cm", res = 300)
    plot(plot_list[[i]])
    dev.off()
  }
}

# Journal size
ragg::agg_tiff(file.path("figures", "tiff", paste0("plot_gew_discrete", ".tiff")),
               width = 20, height = 15, 
               units = "cm", res = 300)
plot_list$plot_gew_discrete
dev.off()
