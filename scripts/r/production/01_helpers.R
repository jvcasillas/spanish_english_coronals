# Source libraries ----------- ------------------------------------------------

source(here::here("scripts", "r", "production", "00_libraries.R"))

# -----------------------------------------------------------------------------



# Plotting functions ----------------------------------------------------------

# Named vector of facet labels
facet_labels <- c(
  `vot` = "VOT",
  `ri` = "Relative intensity",
  `cog` = "COG",
  `kt` = "Kurtosis",
  `sd` = "Standard deviation",
  `sk` = "Skewness"
  )

# Custom colors
my_colors <- c("#7A475D", "#2980B9", "#2C9286")

# Plot adjustments
my_theme_adj <- function() {
  list(
    theme(
      legend.position = "bottom",
      axis.title.y = element_text(size = rel(.9), hjust = 0.95),
      axis.title.x = element_text(size = rel(.9)),
      legend.key = element_blank()
      ),
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3)),
           shape = guide_legend(override.aes = list(size = 3)))
  )
}

# Generic plotting function
plot_metrics <- function(dataframe, posterior, x, color, shape,
                         color_labs = c("/d/", "/t/"),
                         shape_labs = c("Stressed", "Unstressed")) {
  x <- enquo(x)
  color <- enquo(color)
  shape <- enquo(shape)

  ggplot(dataframe) +
    aes(x = !!x, y = val, fill = !!color, color = !!color, shape = !!shape) +
    facet_wrap(~ metric, scales = "free_y",
               labeller = as_labeller(facet_labels)) +
    geom_beeswarm(dodge.width = 0.5, alpha = 0.3) +
    stat_pointinterval(data = posterior, show.legend = F,
                       color = "black", .width = c(.80, .95),
                       position = position_dodge(0.5)) +
    stat_summary(data = posterior, fun.y = mean, geom = "point",
                 position = position_dodge(0.5), size = 2, show.legend = F) +
    scale_color_manual(values = my_colors, name = NULL,
                       labels = color_labs) +
    scale_fill_manual(values = my_colors, name = NULL,
                      labels = color_labs) +
    scale_shape_discrete(name = NULL, labels = shape_labs) +
    scale_x_discrete(labels = c("English", "Spanish")) +
    labs(y = "Metric (std. units)", x = NULL) +
    theme_grey(base_family = "Times", base_size = 16) +
    my_theme_adj()
}

# -----------------------------------------------------------------------------
