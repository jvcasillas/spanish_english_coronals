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









posterior_mono %>%
  ggplot(., aes(y = parameters, x = estimate, color = metric)) +
    geom_vline(xintercept = 0, lty = 3) +
    stat_halfeyeh(position = position_dodgev(0.6)) +
    scale_y_discrete(labels = model_plot_mono_y_labs) +
    scale_color_brewer(name = NULL, palette = "Dark2") +
    coord_cartesian(xlim = c(-1, 1)) +
    labs(y = "Parameters", x = "Estimates") +
    theme_minimal(base_family = "Times", base_size = 16) +
    model_theme_adj

posterior_bi %>%
  ggplot(., aes(y = parameters, x = estimate, color = metric)) +
    geom_vline(xintercept = 0, lty = 3) +
    stat_halfeyeh(position = position_dodgev(0.6)) +
    scale_y_discrete(labels = model_plot_bi_y_labs) +
    scale_color_brewer(name = NULL, palette = "Dark2") +
    coord_cartesian(xlim = c(-1, 1)) +
    labs(y = "Parameters", x = "Estimates") +
    theme_minimal(base_family = "Times", base_size = 16) +
    model_theme_adj

posterior_poa %>%
  ggplot(., aes(y = parameters, x = estimate, color = metric)) +
    geom_vline(xintercept = 0, lty = 3) +
    stat_halfeyeh(position = position_dodgev(0.6)) +
    scale_y_discrete(labels = model_plot_poa_y_labs) +
    scale_color_brewer(name = NULL, palette = "Dark2") +
    coord_cartesian(xlim = c(-1, 1)) +
    labs(y = "Parameters", x = "Estimates") +
    theme_minimal(base_family = "Times", base_size = 16) +
    model_theme_adj
