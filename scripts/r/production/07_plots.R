# Plots -----------------------------------------------------------------------
#
# - Generate a single plot with all metrics
# - Include raw data and model estimates from posterior distributions
# - Save plots as .png and .pdf
#
# -----------------------------------------------------------------------------



# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "04a_mono_analysis.R"))
source(here::here("scripts", "r", "production", "05a_bi_analysis.R"))
source(here::here("scripts", "r", "production", "06a_bi_poa_analysis.R"))

posterior_mono_all <-
  readRDS(here("data", "models", "posterior_mono_all.rds")) %>%
    mutate(metric = fct_relevel(metric, "vot", "ri"))

posterior_bi_all <-
  readRDS(here("data", "models", "posterior_bi_all.rds")) %>%
    mutate(metric = fct_relevel(metric, "vot", "ri"))

posterior_poa_all <-
  readRDS(here("data", "models", "posterior_poa_comp_all.rds")) %>%
    mutate(place = if_else(place == "t", "coronal", "bilabial"),
           metric = fct_relevel(metric, "vot", "ri"))

# -----------------------------------------------------------------------------





# Average over item reps ------------------------------------------------------

mono_subj_item_means <-
  plot_prep(coronals_mono, grouping_var = group_sum, color_var = phon_sum)
bi_subj_item_means <-
  plot_prep(coronals_bi, grouping_var = language_sum, color_var = phon_sum)
poa_subj_item_means <-
  plot_prep(poa_bi, grouping_var = language_sum, color_var = poa_sum, poa = T)

# -----------------------------------------------------------------------------


# Plot raw data with posterior summaries --------------------------------------

mono_all_metrics <- plot_metrics(
  dataframe = mono_subj_item_means, posterior = posterior_mono_all,
  x = language, color = phon, shape = stress)

bi_all_metrics <- plot_metrics(
  dataframe = bi_subj_item_means, posterior = posterior_bi_all,
  x = language, color = phon, shape = stress)

poa_all_metrics <- plot_metrics(
  dataframe = poa_subj_item_means, posterior = posterior_poa_all,
  x = language, color = place, shape = stress,
  color_labs =  c("Bilabial", "Coronal"))

# -----------------------------------------------------------------------------



# Save plots ------------------------------------------------------------------

path_mono <- file.path(here("figs"), "mono_all_metrics.")
path_bi   <- file.path(here("figs"), "bi_all_metrics.")
path_poa  <- file.path(here("figs"), "poa_all_metrics.")
devices <- c('pdf', 'png')

walk(devices, ~ ggsave(filename = glue(path_mono, .x), plot = mono_all_metrics,
                       device = .x), height = 6.43, width = 11.4, units = "in")
walk(devices, ~ ggsave(filename = glue(path_bi, .x), plot = bi_all_metrics,
                       device = .x), height = 6.43, width = 11.4, units = "in")
walk(devices, ~ ggsave(filename = glue(path_poa, .x), plot = poa_all_metrics,
                       device = .x), height = 6.43, width = 11.4, units = "in")

# -----------------------------------------------------------------------------
