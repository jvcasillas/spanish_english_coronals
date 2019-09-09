# Posterior distributions: monolinguals ---------------------------------------
#
# - Get posterior distributions of all 'mono' models and
#   combine them into a single dataframe for plotting and
#   post-hoc scrutiny
#
# -----------------------------------------------------------------------------


# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "04a_mono_analysis.R"))

# -----------------------------------------------------------------------------




# Combined posterior ----------------------------------------------------------

bind_rows(
  posterior_samples(mod_coronals_vot_mono_full) %>%
    select(starts_with("b_")) %>%
    mutate(metric = "VOT"),
  posterior_samples(mod_coronals_ri_mono_full) %>%
    select(starts_with("b_")) %>%
    mutate(metric = "RI"),
  posterior_samples(mod_coronals_cog_mono_full) %>%
    select(starts_with("b_")) %>%
    mutate(metric = "COG"),
  posterior_samples(mod_coronals_sd_mono_full) %>%
    select(starts_with("b_")) %>%
    mutate(metric = "SD"),
  posterior_samples(mod_coronals_sk_mono_full) %>%
    select(starts_with("b_")) %>%
    mutate(metric = "SK"),
  posterior_samples(mod_coronals_kt_mono_full) %>%
    select(starts_with("b_")) %>%
    mutate(metric = "KT")) %>%
  gather(parameters, estimate, -metric) %>%
  mutate(metric = fct_relevel(metric, "VOT", "RI"),
         parameters = fct_relevel(parameters,
          "b_group_sum:phon_sum", "b_rep_n", "b_phon_sum", "b_group_sum",
          "b_Intercept")) %>%
  saveRDS(., here("data", "models", "posterior_mono.rds"))


# -----------------------------------------------------------------------------




# Adjusted posterior distributions --------------------------------------------

# Key info:
# phon_sum = if_else(phon == "d", 1, -1),
# group_sum = if_else(group == "NEN", 1, -1),
# language_sum = if_else(language == "english", 1, -1),
# stress_sum = if_else(stress == "stressed", 1, -1))

posterior_mono_adj <-
  bind_rows(
    posterior_samples(mod_coronals_vot_mono_full) %>%
    select(starts_with("b_")) %>%
    mono_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "vot"),
  posterior_samples(mod_coronals_ri_mono_full) %>%
    select(starts_with("b_")) %>%
    mono_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "ri"),
  posterior_samples(mod_coronals_cog_mono_full) %>%
    select(starts_with("b_")) %>%
    mono_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "cog"),
  posterior_samples(mod_coronals_sd_mono_full) %>%
    select(starts_with("b_")) %>%
    mono_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "sd"),
  posterior_samples(mod_coronals_sk_mono_full) %>%
    select(starts_with("b_")) %>%
    mono_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "sk"),
  posterior_samples(mod_coronals_kt_mono_full) %>%
    select(starts_with("b_")) %>%
    mono_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "kt")
  ) %>%
  saveRDS(., here("data", "models", "posterior_mono_adj.rds"))

# -----------------------------------------------------------------------------
