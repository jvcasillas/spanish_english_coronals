# Plots -----------------------------------------------------------------------
#
# - Generate a single plot with all metrics
# - Include raw data and model estimates from posterior distributions
# - Save plots as .png and .pdf
#
# -----------------------------------------------------------------------------



# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "04a_vowel_analysis.R"))
source(here::here("scripts", "r", "production", "05a_mono_analysis.R"))
source(here::here("scripts", "r", "production", "06a_bi_analysis.R"))
source(here::here("scripts", "r", "production", "07a_bi_poa_analysis.R"))

posterior_vowels <-
  readRDS(here("data", "models", "posterior_vowels.rds"))

posterior_mono <-
  readRDS(here("data", "models", "posterior_mono.rds"))

posterior_bi <-
  readRDS(here("data", "models", "posterior_bi.rds"))

posterior_poa <-
  readRDS(here("data", "models", "posterior_poa_comp.rds"))

posterior_vowels_adj <-
  readRDS(here("data", "models", "posterior_vowels_adj.rds"))

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

# -----------------------------------------------------------------------------








# Vowel plot ------------------------------------------------------------------

posterior_summary <-
  bind_cols(
    posterior_vowels_adj %>%
      group_by(language) %>%
      summarize(f1_mean = mean(f1), f2_mean = mean(f2)),
    posterior_vowels_adj %>%
      group_by(language) %>%
      summarize_at(vars(f1, f2), p_funs)
  ) %>% select(-language1) %>%
  mutate(language = fct_recode(language, English = "english",
                                         Spanish = "spanish"))

vowel_all_metrics <- coronals_vowels %>%
  mutate(language = fct_recode(language, English = "english",
                                         Spanish = "spanish")) %>%
  ggplot(., aes(x = f2_std, y = f1_std, color = language)) +
    geom_point(alpha = 0.2) +
    stat_ellipse(type = "norm", show.legend = FALSE, geom = "polygon",
                 alpha = 0.05) +
    plot_posterior_vowel_summary() +
    coord_cartesian(xlim = c(-2.5, 2.5)) +
    scale_y_reverse() +
    scale_x_reverse(position = "top") +
    scale_color_manual(name = NULL, values = my_colors) +
    scale_fill_manual(name = NULL, values = my_colors) +
    labs(y = "F1 (std)", x = "F2 (std)") +
    theme_minimal(base_size = 16, base_family = "Times") +
    theme(legend.position = c(0.75, 0.25))

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
  x = language, color = phon)

poa_all_metrics <- plot_metrics(
  dataframe = poa_subj_item_means, posterior = posterior_poa_adj,
  x = place, color = language, color_labs =  c("English", "Spanish"),
  xlabs = c("Bilabial", "Coronal"))

# -----------------------------------------------------------------------------











# Posterior summary plots -----------------------------------------------------

vowel_summary <- model_summary_plot(posterior_vowels, model_plot_vowel_y_labs)
mono_summary  <- model_summary_plot(posterior_mono, model_plot_mono_y_labs)
bi_summary    <- model_summary_plot(posterior_bi, model_plot_bi_y_labs)
poa_summary   <- model_summary_plot(posterior_poa, model_plot_poa_y_labs)

# -----------------------------------------------------------------------------



# Save plots ------------------------------------------------------------------

devices        <- c('pdf', 'png')
path_vowel     <- file.path(here("figs"), "vowel_all_metrics.")
path_mono      <- file.path(here("figs"), "mono_all_metrics.")
path_bi        <- file.path(here("figs"), "bi_all_metrics.")
path_poa       <- file.path(here("figs"), "poa_all_metrics.")
path_vowel_sum <- file.path(here("figs"), "vowel_summary.")
path_mono_sum  <- file.path(here("figs"), "mono_summary.")
path_bi_sum    <- file.path(here("figs"), "bi_summary.")
path_poa_sum   <- file.path(here("figs"), "poa_summary.")

walk(devices, ~ ggsave(filename = glue(path_vowel, .x), plot = vowel_all_metrics,
                       device = .x, height = 7, width = 8, units = "in"))
walk(devices, ~ ggsave(filename = glue(path_mono, .x), plot = mono_all_metrics,
                       device = .x, height = 7, width = 14, units = "in"))
walk(devices, ~ ggsave(filename = glue(path_bi, .x), plot = bi_all_metrics,
                       device = .x, height = 7, width = 14, units = "in"))
walk(devices, ~ ggsave(filename = glue(path_poa, .x), plot = poa_all_metrics,
                       device = .x, height = 7, width = 14, units = "in"))

walk(devices, ~ ggsave(filename = glue(path_vowel_sum, .x), plot = vowel_summary,
                       device = .x, height = 6, width = 6, units = "in"))
walk(devices, ~ ggsave(filename = glue(path_mono_sum, .x), plot = mono_summary,
                       device = .x, height = 6, width = 6, units = "in"))
walk(devices, ~ ggsave(filename = glue(path_bi_sum, .x), plot = bi_summary,
                       device = .x, height = 6, width = 6, units = "in"))
walk(devices, ~ ggsave(filename = glue(path_poa_sum, .x), plot = poa_summary,
                       device = .x, height = 6, width = 6, units = "in"))

# -----------------------------------------------------------------------------
