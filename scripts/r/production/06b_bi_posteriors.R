# Posterior distributions: bilinguals -----------------------------------------
#
# - Get posterior distributions of all 'bi' models and
#   combine them into a single dataframe for plotting and
#   post-hoc scrutiny
#
# -----------------------------------------------------------------------------


# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "06a_bi_analysis.R"))

# -----------------------------------------------------------------------------




# Combined posterior ----------------------------------------------------------

bind_rows(
  posterior_samples(mod_coronals_vot_bi_full) %>%
    select(starts_with("b_")) %>%
    mutate(metric = "VOT"),
  posterior_samples(mod_coronals_ri_bi_full) %>%
    select(starts_with("b_")) %>%
    mutate(metric = "RI"),
  posterior_samples(mod_coronals_cog_bi_full) %>%
    select(starts_with("b_")) %>%
    mutate(metric = "COG"),
  posterior_samples(mod_coronals_sd_bi_full) %>%
    select(starts_with("b_")) %>%
    mutate(metric = "SD"),
  posterior_samples(mod_coronals_sk_bi_full) %>%
    select(starts_with("b_")) %>%
    mutate(metric = "SK"),
  posterior_samples(mod_coronals_kt_bi_full) %>%
    select(starts_with("b_")) %>%
    mutate(metric = "KT")) %>%
  gather(parameters, estimate, -metric) %>%
  mutate(metric = fct_relevel(metric, "VOT", "RI"),
         parameters = fct_relevel(parameters,
          "b_language_sum:phon_sum", "b_rep_n", "b_f2_std", "b_f1_std",
          "b_phon_sum", "b_language_sum", "b_Intercept")) %>%
  saveRDS(., here("data", "models", "posterior_bi.rds"))

# -----------------------------------------------------------------------------




# Adjusted posterior distributions --------------------------------------------

# Key info:
# phon_sum = if_else(phon == "d", 1, -1),
# group_sum = if_else(group == "NEN", 1, -1),
# language_sum = if_else(language == "english", 1, -1),
# stress_sum = if_else(stress == "stressed", 1, -1))


posterior_bi_adj <-
  bind_rows(
    posterior_samples(mod_coronals_vot_bi_full) %>%
    select(starts_with("b_")) %>%
    bi_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "vot"),
  posterior_samples(mod_coronals_ri_bi_full) %>%
    select(starts_with("b_")) %>%
    bi_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "ri"),
  posterior_samples(mod_coronals_cog_bi_full) %>%
    select(starts_with("b_")) %>%
    bi_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "cog"),
  posterior_samples(mod_coronals_sd_bi_full) %>%
    select(starts_with("b_")) %>%
    bi_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "sd"),
  posterior_samples(mod_coronals_sk_bi_full) %>%
    select(starts_with("b_")) %>%
    bi_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "sk"),
  posterior_samples(mod_coronals_kt_bi_full) %>%
    select(starts_with("b_")) %>%
    bi_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "kt")
  ) %>%
  saveRDS(., here("data", "models", "posterior_bi_adj.rds"))

# -----------------------------------------------------------------------------
