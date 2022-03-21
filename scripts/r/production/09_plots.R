# Plots -----------------------------------------------------------------------
#
# Last update: 2020-06-16
#
# - Plot vowel analyses
# - Generate a single plot with all metrics
# - Include raw data and model estimates from posterior distributions
# - Generate model summary plots
# - Save plots as .png and .pdf
#
# -----------------------------------------------------------------------------




# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "04a_vowel_analysis.R"))
source(here::here("scripts", "r", "production", "05a_mono_analysis.R"))
source(here::here("scripts", "r", "production", "06a_bi_analysis.R"))
source(here::here("scripts", "r", "production", "07a_bi_poa_analysis.R"))

posterior_vowels <-
  readRDS(here("data", "models", "posterior_vowels.rds")) %>%
  mutate(parameters = fct_relevel(parameters, "b_Intercept", "b_language_sum",
    "b_phon_sum", "b_language_sum:phon_sum", "b_rep_n"))

posterior_mono <-
  readRDS(here("data", "models", "posterior_mono.rds")) %>%
  filter(!(parameters %in% c("b_rep_n", "b_Intercept"))) %>%
  mutate(parameters = fct_relevel(parameters, "b_group_sum", "b_phon_sum",
    "b_f1_cent", "b_f2_cent", "b_group_sum:phon_sum"),
    metric = fct_relevel(metric, "vot", "ri", "cog", "kt", "sd", "sk"))

posterior_bi <-
  readRDS(here("data", "models", "posterior_bi.rds")) %>%
  filter(!(parameters %in% c("b_rep_n", "b_Intercept"))) %>%
  mutate(parameters = fct_relevel(parameters, "b_language_sum", "b_phon_sum",
    "b_f1_cent", "b_f2_cent", "b_language_sum:phon_sum"),
    metric = fct_relevel(metric, "vot", "ri", "cog", "kt", "sd", "sk"))

posterior_poa <-
  readRDS(here("data", "models", "posterior_poa_comp.rds")) %>%
  filter(!(parameters %in% c("b_rep_n", "b_Intercept"))) %>%
  mutate(parameters = fct_relevel(parameters, "b_language_sum", "b_poa_sum",
    "b_f1_cent", "b_f2_cent", "b_language_sum:poa_sum"),
    metric = fct_relevel(metric, "vot", "ri", "cog", "kt", "sd", "sk"))


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




# Data transformations --------------------------------------------------------

#
# Plots to compare raw values with log transformed z scores
#

coronals_mono %>%
  pivot_longer(
    cols = c("f1_cent", "f2_cent", "vot", "ri", "cog", "sd", "sk", "kt",
             "f1_cent_std", "f2_cent_std", "vot_std", "ri_std", "cog_std",
             "sd_std", "sk_std", "kt_std"),
    values_to = "val") %>%
  mutate(type = str_detect(name, "_std"),
         name = fct_relevel(
           name, "f1_cent", "f2_cent", "vot", "ri", "sd", "cog", "kt", "sk",
           "f1_cent_std", "f2_cent_std", "vot_std", "ri_std", "sd_std",
           "cog_std", "kt_std")) %>%
  ggplot(., aes(x = val, fill = language)) +
  facet_wrap(. ~ name, scales = "free", nrow = 2, ncol = 8) +
  geom_histogram(color = "black") +
  scale_fill_manual(name = NULL, values = my_colors,
                    labels = c("English", "Spanish")) +
  theme_minimal() +
  my_theme_adj()


coronals_bi %>%
  pivot_longer(
    cols = c("f1_cent", "f2_cent", "vot", "ri", "cog", "sd", "sk", "kt",
             "f1_cent_std", "f2_cent_std", "vot_std", "ri_std", "cog_std",
             "sd_std", "sk_std", "kt_std"),
    values_to = "val") %>%
  mutate(type = str_detect(name, "_std"),
         name = fct_relevel(
           name, "f1_cent", "f2_cent", "vot", "ri", "sd", "cog", "kt", "sk",
           "f1_cent_std", "f2_cent_std", "vot_std", "ri_std", "sd_std",
           "cog_std", "kt_std")) %>%
  ggplot(., aes(x = val, fill = language)) +
  facet_wrap(. ~ name, scales = "free", nrow = 2, ncol = 8) +
  geom_histogram(color = "black") +
  scale_fill_manual(name = NULL, values = my_colors,
                    labels = c("English", "Spanish")) +
  theme_minimal() +
  my_theme_adj()


poa_bi %>%
  pivot_longer(
    cols = c("f1_cent", "f2_cent", "vot", "ri", "cog", "sd", "sk", "kt",
             "f1_cent_std", "f2_cent_std", "vot_std", "ri_std", "cog_std",
             "sd_std", "sk_std", "kt_std"),
    values_to = "val") %>%
  mutate(type = str_detect(name, "_std"),
         name = fct_relevel(
           name, "f1_cent", "f2_cent", "vot", "ri", "sd", "cog", "kt", "sk",
           "f1_cent_std", "f2_cent_std", "vot_std", "ri_std", "sd_std",
           "cog_std", "kt_std")) %>%
  ggplot(., aes(x = val, fill = language)) +
  facet_wrap(. ~ name, scales = "free", nrow = 2, ncol = 8) +
  geom_histogram(color = "black") +
  scale_fill_manual(name = NULL, values = my_colors,
                    labels = c("English", "Spanish")) +
  theme_minimal() +
  my_theme_adj()

# -----------------------------------------------------------------------------




# Vowel plots -----------------------------------------------------------------

posterior_summary <-
  left_join(
    posterior_vowels_adj %>%
      group_by(language, phon) %>%
      summarize(f1_mean = mean(f1), f2_mean = mean(f2), .groups = "drop"),
    posterior_vowels_adj %>%
      group_by(language, phon) %>%
      summarize_at(vars(f1, f2), p_funs) %>%
      ungroup(.),
    by = c("language", "phon")) %>%
  unite(col = "lang_phon", language, phon, sep = " ")

vowel_all_metrics <- coronals_vowels %>%
  unite(col = "lang_phon", language, phon, sep = " ") %>%
  ggplot(., aes(x = f2_std, y = f1_std, color = lang_phon, shape = lang_phon,
                fill = lang_phon)) +
  geom_point(alpha = 0, show.legend = F) +
  geom_point(inherit.aes = F, aes(x = f2_std, y = f1_std, shape = lang_phon),
    data = unite(coronals_vowels, col = "lang_phon", language, phon, sep = " "),
    alpha = 0.3, fill = "grey", color = "grey50", show.legend = F) +
  stat_ellipse(type = "norm", show.legend = FALSE, geom = "polygon",
    alpha = 0.2) +
  plot_posterior_vowel_summary() +
  scale_y_reverse() +
  scale_x_reverse() +
  scale_color_manual(name = NULL, values = my_colors0, labels = vowel_leg) +
  scale_fill_manual(name = NULL, values = my_colors0, labels = vowel_leg) +
  scale_shape_manual(name = NULL, values = 21:24, labels = vowel_leg) +
  labs(y = "F1 (std)", x = "F2 (std)") +
  theme_bw(base_size = 12, base_family = "Times") +
  theme(legend.position = c(0.85, 0.25),
        panel.grid.major = element_line(colour = 'grey90', size = 0.15),
        panel.grid.minor = element_line(colour = 'grey90', size = 0.15),
        legend.background = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA))

vowel_all_metrics_marginal <- ggMarginal(
  vowel_all_metrics, type = "density",
  groupColour = TRUE,
  groupFill = TRUE,
  size = 4
)

# -----------------------------------------------------------------------------




# Plot raw data prep ----------------------------------------------------------

# Average over item reps for mono, bi, and poa data
# This will provide by-subj means for next plot
mono_subj_means <-
  plot_prep(coronals_mono, grouping_var = group_sum, color_var = phon_sum)
bi_subj_means <-
  plot_prep(coronals_bi, grouping_var = language_sum, color_var = phon_sum)
poa_subj_means <-
  plot_prep(poa_bi, grouping_var = language_sum, color_var = poa_sum, poa = T)

# -----------------------------------------------------------------------------




# Plot raw data with posterior summaries --------------------------------------

mono_all_metrics <- plot_metrics(
  dataframe = mono_subj_means, posterior = posterior_mono_adj,
  x = language, color = phon)

bi_all_metrics <- plot_metrics(
  dataframe = bi_subj_means, posterior = posterior_bi_adj,
  x = language, color = phon)

poa_all_metrics <- plot_metrics(
  dataframe = poa_subj_means, posterior = posterior_poa_adj,
  x = place, color = language, color_labs =  c("English", "Spanish"),
  xlabs = c("Bilabial", "Coronal"))

# -----------------------------------------------------------------------------




# Posterior summary plots -----------------------------------------------------

vowel_summary <- model_summary_vowels_plot(posterior_vowels, y_labels_vowels)
mono_summary  <- model_summary_plot(posterior_mono, facet_labels_mono)
bi_summary    <- model_summary_plot(posterior_bi, facet_labels_bi)
poa_summary   <- model_summary_plot(posterior_poa, facet_labels_poa)

# -----------------------------------------------------------------------------




# Save plots ------------------------------------------------------------------

devices        <- c('pdf', 'png')
path_vowel     <- file.path(here("figs"), "vowel_all_metrics_marginal.")
path_mono      <- file.path(here("figs"), "mono_all_metrics.")
path_bi        <- file.path(here("figs"), "bi_all_metrics.")
path_poa       <- file.path(here("figs"), "poa_all_metrics.")
path_vowel_sum <- file.path(here("figs"), "vowel_summary.")
path_mono_sum  <- file.path(here("figs"), "mono_summary.")
path_bi_sum    <- file.path(here("figs"), "bi_summary.")
path_poa_sum   <- file.path(here("figs"), "poa_summary.")

walk(devices, ~ ggsave(filename = glue(path_vowel, .x), plot = vowel_all_metrics_marginal,
                       device = .x, height = 5, width = 9, units = "in"))
walk(devices, ~ ggsave(filename = glue(path_mono, .x), plot = mono_all_metrics,
                       device = .x, height = 5, width = 7.5, units = "in"))
walk(devices, ~ ggsave(filename = glue(path_bi, .x), plot = bi_all_metrics,
                       device = .x, height = 5, width = 7.5, units = "in"))
walk(devices, ~ ggsave(filename = glue(path_poa, .x), plot = poa_all_metrics,
                       device = .x, height = 5, width = 7.5, units = "in"))

walk(devices, ~ ggsave(filename = glue(path_vowel_sum, .x), plot = vowel_summary,
                       device = .x, height = 3, width = 9, units = "in"))
walk(devices, ~ ggsave(filename = glue(path_mono_sum, .x), plot = mono_summary,
                       device = .x, height = 5.7, width = 9, units = "in"))
walk(devices, ~ ggsave(filename = glue(path_bi_sum, .x), plot = bi_summary,
                       device = .x, height = 5.7, width = 9, units = "in"))
walk(devices, ~ ggsave(filename = glue(path_poa_sum, .x), plot = poa_summary,
                       device = .x, height = 5.7, width = 9, units = "in"))

# -----------------------------------------------------------------------------
