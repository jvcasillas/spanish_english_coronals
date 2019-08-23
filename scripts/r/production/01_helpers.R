# Source libraries ----------- ------------------------------------------------

source(here::here("scripts", "r", "production", "00_libraries.R"))

# -----------------------------------------------------------------------------



# Helper functions ------------------------------------------------------------

facet_labels <- c(`vot` = "VOT",
                  `ri` = "Relative intensity",
                  `cog` = "COG",
                  `kt` = "Kurtosis",
                  `sd` = "Standard deviation",
                  `sk` = "Skewness")

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




# -----------------------------------------------------------------------------
