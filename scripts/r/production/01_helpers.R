# Source libraries ----------- ------------------------------------------------

source(here::here("scripts", "r", "production", "00_libraries.R"))

# -----------------------------------------------------------------------------



# Helper functions ------------------------------------------------------------

# Custom colors
my_colors <- c("-1" = "#7A475D", "1" = "#2980B9", "0" = "#2C9286")

# Plot adjustments
my_theme_adj <- theme(
  legend.position = "bottom",
  axis.title.y = element_text(size = rel(.9), hjust = 0.95),
  axis.title.x = element_text(size = rel(.9))
)

# -----------------------------------------------------------------------------
