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



# Get posterior distributions -------------------------------------------------

# Key info:
# phon_sum = if_else(phon == "d", 1, -1),
# group_sum = if_else(group == "NEN", 1, -1),
# language_sum = if_else(language == "english", 1, -1),
# stress_sum = if_else(stress == "stressed", 1, -1))

posterior_mono_all <-
  bind_rows(
    posterior_samples(mod_coronals_vot_mono_full) %>%
    select(starts_with("b_")) %>%
    mono_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon", "stress"),
             sep = "_", remove = T) %>%
    mutate(metric = "vot"),
  posterior_samples(mod_coronals_ri_mono_full) %>%
    select(starts_with("b_")) %>%
    mono_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon", "stress"),
             sep = "_", remove = T) %>%
    mutate(metric = "ri"),
  posterior_samples(mod_coronals_cog_mono_full) %>%
    select(starts_with("b_")) %>%
    mono_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon", "stress"),
             sep = "_", remove = T) %>%
    mutate(metric = "cog"),
  posterior_samples(mod_coronals_sd_mono_full) %>%
    select(starts_with("b_")) %>%
    mono_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon", "stress"),
             sep = "_", remove = T) %>%
    mutate(metric = "sd"),
  posterior_samples(mod_coronals_sk_mono_full) %>%
    select(starts_with("b_")) %>%
    mono_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon", "stress"),
             sep = "_", remove = T) %>%
    mutate(metric = "sk"),
  posterior_samples(mod_coronals_kt_mono_full) %>%
    select(starts_with("b_")) %>%
    mono_prep %>%
    gather(language, val) %>%
    separate(language, into = c("language", "phon", "stress"),
             sep = "_", remove = T) %>%
    mutate(metric = "kt")
  ) %>%
  saveRDS(., here("data", "models", "posterior_mono_all.rds"))

# -----------------------------------------------------------------------------
