# Posterior distributions: bilinguals -----------------------------------------
#
# - Get posterior distributions of all 'bi' models and
#   combine them into a single dataframe for plotting and
#   post-hoc scrutiny
#
# -----------------------------------------------------------------------------


# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "06a_bi_poa_analysis.R"))

# -----------------------------------------------------------------------------



# Get posterior distributions -------------------------------------------------

# Key info:
# language_sum = if_else(language == "english", 1, -1),
# group_sum = if_else(group == "BIL", 1, -1),
# stress_sum = if_else(stress == "stressed", 1, -1),
# poa_sum = if_else(phon == "t", 1, -1))


posterior_poa_all <-
  bind_rows(
    posterior_samples(mod_poa_comp_vot_full) %>%
    select(starts_with("b_")) %>%
    transmute(
      english_t_stressed = b_Intercept + b_language_sum + b_poa_sum +
        b_stress_sum + `b_language_sum:poa_sum` + `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      english_t_unstressed = b_Intercept + b_language_sum + b_poa_sum -
        b_stress_sum + `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` - `b_language_sum:poa_sum:stress_sum`,
      english_p_stressed = b_Intercept + b_language_sum - b_poa_sum +
        b_stress_sum - `b_language_sum:poa_sum` + `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      english_p_unstressed = b_Intercept + b_language_sum - b_poa_sum -
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_t_stressed = b_Intercept - b_language_sum + b_poa_sum +
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_t_unstressed = b_Intercept - b_language_sum + b_poa_sum -
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` - `b_language_sum:poa_sum:stress_sum`,
      spanish_p_stressed = b_Intercept - b_language_sum - b_poa_sum +
        b_stress_sum + `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_p_unstressed = b_Intercept - b_language_sum - b_poa_sum -
        b_stress_sum + `b_language_sum:poa_sum` + `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`) %>%
    gather(language, val) %>%
    separate(language, into = c("language", "place", "stress"),
             sep = "_", remove = T) %>%
    mutate(metric = "vot"),
  posterior_samples(mod_poa_comp_ri_full) %>%
    select(starts_with("b_")) %>%
    transmute(
      english_t_stressed = b_Intercept + b_language_sum + b_poa_sum +
        b_stress_sum + `b_language_sum:poa_sum` + `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      english_t_unstressed = b_Intercept + b_language_sum + b_poa_sum -
        b_stress_sum + `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` - `b_language_sum:poa_sum:stress_sum`,
      english_p_stressed = b_Intercept + b_language_sum - b_poa_sum +
        b_stress_sum - `b_language_sum:poa_sum` + `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      english_p_unstressed = b_Intercept + b_language_sum - b_poa_sum -
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_t_stressed = b_Intercept - b_language_sum + b_poa_sum +
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_t_unstressed = b_Intercept - b_language_sum + b_poa_sum -
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` - `b_language_sum:poa_sum:stress_sum`,
      spanish_p_stressed = b_Intercept - b_language_sum - b_poa_sum +
        b_stress_sum + `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_p_unstressed = b_Intercept - b_language_sum - b_poa_sum -
        b_stress_sum + `b_language_sum:poa_sum` + `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`) %>%
    gather(language, val) %>%
    separate(language, into = c("language", "place", "stress"),
             sep = "_", remove = T) %>%
    mutate(metric = "ri"),
  posterior_samples(mod_poa_comp_cog_full) %>%
    select(starts_with("b_")) %>%
    transmute(
      english_t_stressed = b_Intercept + b_language_sum + b_poa_sum +
        b_stress_sum + `b_language_sum:poa_sum` + `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      english_t_unstressed = b_Intercept + b_language_sum + b_poa_sum -
        b_stress_sum + `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` - `b_language_sum:poa_sum:stress_sum`,
      english_p_stressed = b_Intercept + b_language_sum - b_poa_sum +
        b_stress_sum - `b_language_sum:poa_sum` + `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      english_p_unstressed = b_Intercept + b_language_sum - b_poa_sum -
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_t_stressed = b_Intercept - b_language_sum + b_poa_sum +
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_t_unstressed = b_Intercept - b_language_sum + b_poa_sum -
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` - `b_language_sum:poa_sum:stress_sum`,
      spanish_p_stressed = b_Intercept - b_language_sum - b_poa_sum +
        b_stress_sum + `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_p_unstressed = b_Intercept - b_language_sum - b_poa_sum -
        b_stress_sum + `b_language_sum:poa_sum` + `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`) %>%
    gather(language, val) %>%
    separate(language, into = c("language", "place", "stress"),
             sep = "_", remove = T) %>%
    mutate(metric = "cog"),
  posterior_samples(mod_poa_comp_sd_full) %>%
    select(starts_with("b_")) %>%
    transmute(
      english_t_stressed = b_Intercept + b_language_sum + b_poa_sum +
        b_stress_sum + `b_language_sum:poa_sum` + `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      english_t_unstressed = b_Intercept + b_language_sum + b_poa_sum -
        b_stress_sum + `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` - `b_language_sum:poa_sum:stress_sum`,
      english_p_stressed = b_Intercept + b_language_sum - b_poa_sum +
        b_stress_sum - `b_language_sum:poa_sum` + `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      english_p_unstressed = b_Intercept + b_language_sum - b_poa_sum -
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_t_stressed = b_Intercept - b_language_sum + b_poa_sum +
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_t_unstressed = b_Intercept - b_language_sum + b_poa_sum -
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` - `b_language_sum:poa_sum:stress_sum`,
      spanish_p_stressed = b_Intercept - b_language_sum - b_poa_sum +
        b_stress_sum + `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_p_unstressed = b_Intercept - b_language_sum - b_poa_sum -
        b_stress_sum + `b_language_sum:poa_sum` + `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`) %>%
    gather(language, val) %>%
    separate(language, into = c("language", "place", "stress"),
             sep = "_", remove = T) %>%
    mutate(metric = "sd"),
  posterior_samples(mod_poa_comp_sk_full) %>%
    select(starts_with("b_")) %>%
    transmute(
      english_t_stressed = b_Intercept + b_language_sum + b_poa_sum +
        b_stress_sum + `b_language_sum:poa_sum` + `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      english_t_unstressed = b_Intercept + b_language_sum + b_poa_sum -
        b_stress_sum + `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` - `b_language_sum:poa_sum:stress_sum`,
      english_p_stressed = b_Intercept + b_language_sum - b_poa_sum +
        b_stress_sum - `b_language_sum:poa_sum` + `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      english_p_unstressed = b_Intercept + b_language_sum - b_poa_sum -
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_t_stressed = b_Intercept - b_language_sum + b_poa_sum +
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_t_unstressed = b_Intercept - b_language_sum + b_poa_sum -
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` - `b_language_sum:poa_sum:stress_sum`,
      spanish_p_stressed = b_Intercept - b_language_sum - b_poa_sum +
        b_stress_sum + `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_p_unstressed = b_Intercept - b_language_sum - b_poa_sum -
        b_stress_sum + `b_language_sum:poa_sum` + `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`) %>%
    gather(language, val) %>%
    separate(language, into = c("language", "place", "stress"),
             sep = "_", remove = T) %>%
    mutate(metric = "sk"),
  posterior_samples(mod_poa_comp_kt_full) %>%
    select(starts_with("b_")) %>%
    transmute(
      english_t_stressed = b_Intercept + b_language_sum + b_poa_sum +
        b_stress_sum + `b_language_sum:poa_sum` + `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      english_t_unstressed = b_Intercept + b_language_sum + b_poa_sum -
        b_stress_sum + `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` - `b_language_sum:poa_sum:stress_sum`,
      english_p_stressed = b_Intercept + b_language_sum - b_poa_sum +
        b_stress_sum - `b_language_sum:poa_sum` + `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      english_p_unstressed = b_Intercept + b_language_sum - b_poa_sum -
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_t_stressed = b_Intercept - b_language_sum + b_poa_sum +
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_t_unstressed = b_Intercept - b_language_sum + b_poa_sum -
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` - `b_language_sum:poa_sum:stress_sum`,
      spanish_p_stressed = b_Intercept - b_language_sum - b_poa_sum +
        b_stress_sum + `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_p_unstressed = b_Intercept - b_language_sum - b_poa_sum -
        b_stress_sum + `b_language_sum:poa_sum` + `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`) %>%
    gather(language, val) %>%
    separate(language, into = c("language", "place", "stress"),
             sep = "_", remove = T) %>%
    mutate(metric = "kt")
  ) %>%
  saveRDS(., here("data", "models", "posterior_poa_comp_all.rds"))

# -----------------------------------------------------------------------------
