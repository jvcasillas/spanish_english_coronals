# Posterior distributions: monolinguals ---------------------------------------
#
# - Get posterior distributions of all 'mono' models and
#   combine them into a single dataframe for plotting and
#   post-hoc scrutiny
#
# -----------------------------------------------------------------------------


# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "04a_vowel_analysis.R"))

# -----------------------------------------------------------------------------




# Combined posterior ----------------------------------------------------------

posterior_samples(mod_f1f2_mv_mono_full) %>%
  select(starts_with("b_")) %>%
  pivot_longer(cols = everything(), values_to = "estimate") %>%
  separate(col = name, into = c("pt1", "metric", "pt2", "pt3"), sep = "_") %>%
  unite(col = "parameters", pt1, pt2, pt3, sep = "_") %>%
  mutate(metric = str_replace(metric, "std", ""),
         metric = str_replace(metric, "f", "F"),
         parameters = str_replace(parameters, "_NA", ""),
         parameters = fct_relevel(parameters,
          "b_rep_n", "b_phon_sum", "b_language_sum", "b_Intercept")) %>%
  select(metric, parameters, estimate) %>%
  arrange(metric) %>%
  saveRDS(., here("data", "models", "posterior_vowels.rds"))

# -----------------------------------------------------------------------------




# Adjusted posterior distributions --------------------------------------------

# Key info:
# phon_sum = if_else(phon == "d", 1, -1),
# language_sum = if_else(language == "english", 1, -1),

posterior_samples(mod_f1f2_mv_mono_full) %>%
  select(starts_with("b_")) %>%
  vowel_prep %>%
  pivot_longer(cols = everything(), names_to = "metric", values_to = "val") %>%
  separate(metric, into = c("metric", "language", "phon"), sep = "_") %>%
  arrange(metric, language, phon) %>%
  group_by(metric, language, phon) %>%
  mutate(draw = seq_along(phon)) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  saveRDS(., here("data", "models", "posterior_vowels_adj.rds"))

# -----------------------------------------------------------------------------
