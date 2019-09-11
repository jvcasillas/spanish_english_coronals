# Posterior distributions: monolinguals ---------------------------------------
#
# - Get posterior distributions of all 'mono' models and
#   combine them into a single dataframe for plotting and
#   post-hoc scrutiny
#
# -----------------------------------------------------------------------------


# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "05a_vowel_analysis.R"))

# -----------------------------------------------------------------------------




# Combined posterior ----------------------------------------------------------

bind_rows(
  posterior_samples(mod_f1_mono_full) %>%
    select(starts_with("b_")) %>%
    mutate(metric = "f1"),
  posterior_samples(mod_f2_mono_full) %>%
    select(starts_with("b_")) %>%
    mutate(metric = "f2")) %>%
  gather(parameters, estimate, -metric) %>%
  mutate(parameters = fct_relevel(parameters,
         "b_rep_n", "b_phon_sum", "b_language_sum", "b_Intercept")) %>%
  saveRDS(., here("data", "models", "posterior_vowels.rds"))

# -----------------------------------------------------------------------------




# Adjusted posterior distributions --------------------------------------------

# Key info:
# phon_sum = if_else(phon == "d", 1, -1),
# language_sum = if_else(language == "english", 1, -1),

bind_cols(
  posterior_samples(mod_f1_mono_full) %>%
    select(starts_with("b_")) %>%
    vowel_prep %>%
    gather(language, f1) %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T),
  posterior_samples(mod_f2_mono_full) %>%
    select(starts_with("b_")) %>%
    vowel_prep %>%
    gather(language, f2) %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T)) %>%
  select(-language1, -phon1) %>%
  saveRDS(., here("data", "models", "posterior_vowels_adj.rds"))

# -----------------------------------------------------------------------------
