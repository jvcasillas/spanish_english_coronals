# Plots -----------------------------------------------------------------------
#
# - Generate a single plot with all metrics
# - Include raw data and model estimates from posterior distributions
# - Save plots as .png and .pdf
#
# -----------------------------------------------------------------------------



# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "04a_mono_analysis.R"))
source(here::here("scripts", "r", "production", "06a_bi_analysis.R"))
source(here::here("scripts", "r", "production", "07a_bi_poa_analysis.R"))

posterior_mono_adj <-
  readRDS(here("data", "models", "posterior_mono_adj.rds")) %>%
    mutate(metric = fct_relevel(metric, "vot", "ri"))

posterior_bi_adj <-
  readRDS(here("data", "models", "posterior_bi_adj.rds")) %>%
    mutate(metric = fct_relevel(metric, "vot", "ri"))

posterior_poa_adj <-
  readRDS(here("data", "models", "posterior_poa_comp_adj.rds")) %>%
    mutate(place = if_else(place == "t", "coronal", "bilabial"),
           metric = fct_relevel(metric, "vot", "ri"))

posterior_mono <-
  readRDS(here("data", "models", "posterior_mono.rds"))

posterior_bi <-
  readRDS(here("data", "models", "posterior_bi.rds"))

posterior_poa <-
  readRDS(here("data", "models", "posterior_poa_comp.rds"))

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
  dataframe = mono_subj_item_means, posterior = posterior_mono_adj,
  x = language, color = phon)

bi_all_metrics <- plot_metrics(
  dataframe = bi_subj_item_means, posterior = posterior_bi_adj,
  x = language, color = phon, shape = stress)

poa_all_metrics <- plot_metrics(
  dataframe = poa_subj_item_means, posterior = posterior_poa_adj,
  x = language, color = place, shape = stress,
  color_labs =  c("Bilabial", "Coronal"))

# -----------------------------------------------------------------------------





# Posterior summary plots -----------------------------------------------------

mono_summary <- model_summary_plot(posterior_mono, model_plot_mono_y_labs)
bi_summary   <- model_summary_plot(posterior_bi, model_plot_bi_y_labs)
poa_summary  <- model_summary_plot(posterior_poa, model_plot_poa_y_labs)

# -----------------------------------------------------------------------------



# Save plots ------------------------------------------------------------------

devices       <- c('pdf', 'png')
path_mono     <- file.path(here("figs"), "mono_all_metrics.")
path_bi       <- file.path(here("figs"), "bi_all_metrics.")
path_poa      <- file.path(here("figs"), "poa_all_metrics.")
path_mono_sum <- file.path(here("figs"), "mono_summary.")
path_bi_sum   <- file.path(here("figs"), "bi_summary.")
path_poa_sum  <- file.path(here("figs"), "poa_summary.")

walk(devices, ~ ggsave(filename = glue(path_mono, .x), plot = mono_all_metrics,
                       device = .x), height = 6.43, width = 11.4, units = "in")
walk(devices, ~ ggsave(filename = glue(path_bi, .x), plot = bi_all_metrics,
                       device = .x), height = 6.43, width = 11.4, units = "in")
walk(devices, ~ ggsave(filename = glue(path_poa, .x), plot = poa_all_metrics,
                       device = .x), height = 6.43, width = 11.4, units = "in")

walk(devices, ~ ggsave(filename = glue(path_mono_sum, .x), plot = mono_summary,
                       device = .x), height = 6.43, width = 11.4, units = "in")
walk(devices, ~ ggsave(filename = glue(path_bi_sum, .x), plot = bi_summary,
                       device = .x), height = 6.43, width = 11.4, units = "in")
walk(devices, ~ ggsave(filename = glue(path_poa_sum, .x), plot = poa_summary,
                       device = .x), height = 6.43, width = 11.4, units = "in")

# -----------------------------------------------------------------------------
