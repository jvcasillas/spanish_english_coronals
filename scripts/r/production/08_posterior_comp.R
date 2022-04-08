# Post-hoc comparisons --------------------------------------------------------
#
# Last update: 2022-03-20
#
# Questions of interest:
# - Can any of the metrics distinguish between English and Spanish for
#   coronal stops?
# - We want to show that X metric can distinguish between dental and alveolars
#   (english vs. spanish), but the same between-lang difference should not
#   exist for bilabials
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




# Post-hoc analyses: Monolinguals ---------------------------------------------

# Comparison of short-lag stops: monolinguals
d_t_post_mono <- posterior_mono_adj %>%
  filter(metric == "vot", language == "english" & phon == "d" |
                          language == "spanish" & phon == "t") %>%
  select(-metric, -language) %>%
  group_by(phon) %>%
  mutate(grouped_id = row_number()) %>%
  pivot_wider(everything(), names_from = "phon", values_from = "val") %>%
  mutate(diff = d - t)

mono_post_hoc_dt <- plot_posterior(
  posterior = d_t_post_mono, parameter = diff, rope = c(-0.1, 0.1),
  hdi = 0.95, xpos = -0.48, ypos = c(1.2, 1, 0.8))

# Within language comparisons of spectral moments and RI
mono_within_lang_dt_comp_posterior <- posterior_mono_adj %>%
  filter(metric != "vot") %>%
  group_by(language, phon) %>%
  mutate(grouped_id = row_number()) %>%
  pivot_wider(everything(), names_from = "phon", values_from = "val") %>%
  mutate(diff = d - t) %>%
  select(-d, -t, -grouped_id) %>%
  group_by(language, metric) %>%
  mutate(grouped_id = row_number()) %>%
  pivot_wider(names_from = c("language", "metric"), values_from = diff) %>%
  select(-grouped_id)

# Plot RI and spectral moments
mono_post_hoc_sm <- mono_within_lang_dt_comp_posterior %>%
  pivot_longer(everything(), names_to = "metric", values_to = "estimate") %>%
  separate(metric, c("lang", "metric"), sep = "_") %>%
  mutate(metric = toupper(metric), lang = str_to_title(lang)) %>%
  ggplot(., aes(x = estimate, y = lang))  +
    geom_rect(data = tibble(xmin = -0.1, xmax = 0.1), inherit.aes = FALSE,
      aes(xmin = -0.1, xmax = 0.1, ymin = -Inf, ymax = Inf),
      fill = "lightblue", color = "white", alpha = 0.2) +
    geom_vline(xintercept = 0, lty = 3) +
    stat_halfeye(aes(slab_fill = metric, point_fill = metric, shape = metric),
      position = position_dodgev(0.6), slab_alpha = 0.5, slab_color = "white",
      point_color = "white", point_size = 5, stroke = 0.3) +
    scale_fill_manual(aesthetics = "slab_fill", name = NULL,
      values = viridis::viridis(5, option = "C", begin = 0.2, end = 0.9)) +
    scale_fill_manual(aesthetics = "point_fill", name = NULL,
      values = viridis::viridis(5, option = "C", begin = 0.2, end = 0.9)) +
    scale_shape_manual(name = NULL, values = c(21:25)) +
    theme_minimal(base_family = "Times", base_size = 16) +
    labs(x = "/d/ - /t/ difference estimates", y = NULL) +
    model_theme_adj()

# -----------------------------------------------------------------------------




# Post-hoc analyses: Bilinguals -----------------------------------------------

# Comparison of short-lag stops: monolinguals
d_t_post_bi <- posterior_bi_adj %>%
  filter(metric == "vot", language == "english" & phon == "d" |
                          language == "spanish" & phon == "t") %>%
  select(-metric, -language) %>%
  group_by(phon) %>%
  mutate(grouped_id = row_number()) %>%
  pivot_wider(everything(), names_from = "phon", values_from = "val") %>%
  mutate(diff = d - t)

bi_post_hoc_dt <- plot_posterior(
  posterior = d_t_post_bi, parameter = diff, rope = c(-0.1, 0.1),
  hdi = 0.95, xpos = -0.65, ypos = c(1.2, 1, 0.8))

# Within language comparisons of spectral moments and RI
bi_within_lang_dt_comp_posterior <- posterior_bi_adj %>%
  filter(metric != "vot") %>%
  group_by(language, phon) %>%
  mutate(grouped_id = row_number()) %>%
  pivot_wider(everything(), names_from = "phon", values_from = "val") %>%
  mutate(diff = d - t) %>%
  select(-d, -t, -grouped_id) %>%
  group_by(language, metric) %>%
  mutate(grouped_id = row_number()) %>%
  pivot_wider(names_from = c("language", "metric"), values_from = diff) %>%
  select(-grouped_id)

# Plot RI and spectral moments
bi_post_hoc_sm <- bi_within_lang_dt_comp_posterior %>%
  pivot_longer(everything(), names_to = "metric", values_to = "estimate") %>%
  separate(metric, c("lang", "metric"), sep = "_") %>%
  mutate(metric = toupper(metric), lang = str_to_title(lang)) %>%
  ggplot(., aes(x = estimate, y = lang))  +
    geom_rect(data = tibble(xmin = -0.1, xmax = 0.1), inherit.aes = FALSE,
      aes(xmin = -0.1, xmax = 0.1, ymin = -Inf, ymax = Inf),
      fill = "lightblue", color = "white", alpha = 0.2) +
    geom_vline(xintercept = 0, lty = 3) +
    stat_halfeye(aes(slab_fill = metric, point_fill = metric, shape = metric),
      position = position_dodgev(0.4), slab_alpha = 0.5, slab_color = "white",
      point_color = "white", point_size = 5, stroke = 0.3) +
    scale_fill_manual(aesthetics = "slab_fill", name = NULL,
      values = viridis::viridis(5, option = "C", begin = 0.2, end = 0.9)) +
    scale_fill_manual(aesthetics = "point_fill", name = NULL,
      values = viridis::viridis(5, option = "C", begin = 0.2, end = 0.9)) +
    scale_shape_manual(name = NULL, values = c(21:25)) +
    theme_minimal(base_family = "Times", base_size = 16) +
    labs(x = "/d/ - /t/ difference estimates", y = NULL) +
    model_theme_adj()

# -----------------------------------------------------------------------------




# Comparison of POA - en vs. sp: ----------------------------------------------

# We are doing for a between language comparison of voiceless stops.
# This means we look at English vs. Spanish for bilabials (expected difference
# in VOT only) and coronals (expected difference in all metrics) because of
# place differences.
# So, if our acoustic measurements account for place differences we should see
# them in the coronals and not the bilabials.
# If you get confused about this in the future and are rereading this, use
# figure 'poa_all_metrics.pdf' to help you remember.
# You are subtracting the orange triangles from the purple circles (En - Sp)
# for bilabials and coronals (for each metric).

# All metrics (because we want the same comparison)
poa_between_lang_place_comp_posterior <- posterior_poa_adj %>%
  group_by(metric, language, place) %>%
  mutate(grouped_id = row_number()) %>%
  pivot_wider(names_from = language, values_from = val) %>%
  mutate(diff = english - spanish) %>%
  select(-english, -spanish, -grouped_id) %>%
  group_by(place, metric) %>%
  mutate(grouped_id = row_number()) %>%
  pivot_wider(names_from = c("place", "metric"), values_from = diff) %>%
  select(-grouped_id)

# Plot all
metric_y_labels <- c("Skewness", "Standard\ndeviation", "Kurtosis",
  "COG", "Relative\nintensity", "VOT")

poa_post_hoc_all <- poa_between_lang_place_comp_posterior %>%
  pivot_longer(everything(), names_to = "metric", values_to = "estimate") %>%
  separate(metric, c("place", "metric"), sep = "_") %>%
  mutate(place = str_to_title(place),
    metric = fct_relevel(metric, "vot", "ri", "cog", "kt", "sd", "sk")) %>%
  ggplot(., aes(x = estimate, y = metric)) +
    geom_rect(data = tibble(xmin = -0.1, xmax = 0.1), inherit.aes = FALSE,
      aes(xmin = -0.1, xmax = 0.1, ymin = -Inf, ymax = Inf),
      fill = "lightblue", color = "white", alpha = 0.2) +
    geom_vline(xintercept = 0, lty = 3) +
    stat_halfeye(aes(slab_fill = place, point_fill = place, shape = place),
      position = position_dodgev(0.4), slab_alpha = 0.5, slab_color = "white",
      point_color = "white", point_size = 4, stroke = 0.3) +
    scale_fill_manual(aesthetics = "slab_fill", name = NULL,
      values = viridis::viridis(2, option = "C", begin = 0.3, end = 0.7)) +
    scale_fill_manual(aesthetics = "point_fill", name = NULL,
      values = viridis::viridis(2, option = "C", begin = 0.3, end = 0.7)) +
    scale_shape_manual(name = NULL, values = c(21, 25)) +
    scale_y_discrete(labels = metric_y_labels, limits = rev) +
    theme_minimal(base_family = "Times", base_size = 16) +
    labs(x = "Between-language difference estimates", y = NULL) +
    theme(
      axis.title.y = element_text(size = rel(.9), hjust = 0.95),
      axis.title.x = element_text(size = rel(.9), hjust = 0.95),
      panel.grid.major = element_line(colour = 'grey90', size = 0.25),
      panel.grid.minor = element_line(colour = 'grey90', size = 0.25),
      legend.position = c(0.95, 0.27), legend.justification = c(1, 1),
      legend.key = element_blank(),
      axis.text.y = element_text(hjust = 0)
      )

# -----------------------------------------------------------------------------




# Combine comparison summaries ------------------------------------------------

bind_rows(
  bind_rows(
  d_t_post_mono$diff %>% make_model_table(name_v = "vot") %>%
    mutate(Parameter = "mono_dt_between"),
  mono_within_lang_dt_comp_posterior %>%
    imap_dfr(make_model_table) %>%
    mutate(Parameter = "mono_dt_win_lang")
  ),
  bind_rows(
  d_t_post_bi$diff %>% make_model_table(name_v = "vot") %>%
    mutate(Parameter = "bi_dt_between"),
  bi_within_lang_dt_comp_posterior %>%
    imap_dfr(make_model_table) %>%
    mutate(Parameter = "bi_dt_win_lang")
  ),
  poa_between_lang_place_comp_posterior %>%
    imap_dfr(make_model_table) %>%
    mutate(Parameter = "bi_poa_between_lang")
  ) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  mutate(HDI = glue("[{hdi_lo}, {hdi_hi}]"),
    Metric = toupper(column)) %>%
  mutate_at(c("Estimate", "HDI"), str_replace_all,
            pattern = "-", replacement = "&minus;") %>%
  unite(col = "identifier", Metric, Parameter, sep = "_", remove = F) %>%
  transmute(identifier, Metric, Parameter, Estimate, HDI, ROPE, MPE) %>%
  saveRDS(here("data", "models", "post_hoc_analyses.rds"))

# -----------------------------------------------------------------------------




# Save plots ------------------------------------------------------------------

devices         <- c('pdf', 'png')
path_mono_ph_dt <- file.path(here("figs"), "mono_post_hoc_dt.")
path_mono_ph_sm <- file.path(here("figs"), "mono_post_hoc_sm.")
path_bi_ph_dt   <- file.path(here("figs"), "bi_post_hoc_dt.")
path_bi_ph_sm   <- file.path(here("figs"), "bi_post_hoc_sm.")
path_poa_ph_all <- file.path(here("figs"), "poa_post_hoc_all.")

walk(devices, ~ ggsave(filename = glue(path_mono_ph_dt, .x),
                       plot = mono_post_hoc_dt,
                       device = .x, height = 4, width = 8, units = "in"))
walk(devices, ~ ggsave(filename = glue(path_mono_ph_sm, .x),
                       plot = mono_post_hoc_sm,
                       device = .x, height = 4, width = 8, units = "in"))

walk(devices, ~ ggsave(filename = glue(path_bi_ph_dt, .x),
                       plot = bi_post_hoc_dt,
                       device = .x, height = 4, width = 8, units = "in"))
walk(devices, ~ ggsave(filename = glue(path_bi_ph_sm, .x),
                       plot = bi_post_hoc_sm,
                       device = .x, height = 4, width = 8, units = "in"))

walk(devices, ~ ggsave(filename = glue(path_poa_ph_all, .x),
                       plot = poa_post_hoc_all,
                       device = .x, height = 4, width = 8, units = "in"))

# -----------------------------------------------------------------------------
