# Posterior distributions: vowels ---------------------------------------------
#
# Last update: 2022-03-16
#
# - Get posterior distributions of mv 'mono' model and
#   prep it for plotting and post-hoc scrutiny
#
# -----------------------------------------------------------------------------


# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "04a_vowel_analysis.R"))

# -----------------------------------------------------------------------------




# Tidy posterior in long format -----------------------------------------------

as_tibble(mod_f1f2_mv_mono_full) %>%
  select(starts_with("b_")) %>%
  pivot_longer(cols = everything(), values_to = "estimate") %>%
  separate(col = name, into = c("pt1", "metric", "pt2"), sep = c(1, 8)) %>%
  unite(col = "parameters", pt1, pt2, sep = "_") %>%
  mutate(metric = str_replace(metric, "std_", ""),
         metric = str_replace(metric, "_f", "F"),
         parameters = fct_relevel(parameters,
          "b_rep_n", "b_language_sum:phon_sum", "b_phon_sum",
          "b_language_sum", "b_Intercept")) %>%
  select(metric, parameters, estimate) %>%
  arrange(metric) %>%
  saveRDS(here("data", "models", "posterior_vowels.rds"))

# -----------------------------------------------------------------------------




# Adjusted posterior distribution ---------------------------------------------

# Key info:
# phon_sum = if_else(phon == "d", 1, -1),
# language_sum = if_else(language == "english", 1, -1),

as_tibble(mod_f1f2_mv_mono_full) %>%
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
