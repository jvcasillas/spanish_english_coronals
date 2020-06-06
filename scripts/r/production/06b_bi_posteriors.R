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
    pivot_longer(cols = everything(), values_to = "estimate") %>%
    mutate(metric = "vot",
           name = str_replace(name, "cent_std", "cent"),
           name = str_replace(name, ":phon_sum", ":phon"),
           name = fct_relevel(name,
            "b_language_sum:phon", "b_rep_n", "b_f1_cent", "b_f2_cent",
            "b_phon_sum", "b_language_sum", "b_Intercept")) %>%
    select(metric, parameters = name, estimate) %>%
    arrange(metric),
  posterior_samples(mod_coronals_mv_bi_full) %>%
    select(starts_with("b_")) %>%
    pivot_longer(cols = everything(), values_to = "estimate") %>%
    separate(col = name, into = c("pt1", "metric", "pt2", "pt3"), sep = "_") %>%
    unite(col = "parameters", pt1, pt2, pt3, sep = "_") %>%
    mutate(metric = str_replace(metric, "std", ""),
           parameters = str_replace(parameters, "_NA", ""),
           parameters = fct_relevel(parameters,
            "b_language_sum:phon", "b_rep_n", "b_f1_cent", "b_f2_cent",
            "b_phon_sum", "b_language_sum", "b_Intercept")) %>%
    select(metric, parameters, estimate) %>%
    arrange(metric)) %>%
  saveRDS(., here("data", "models", "posterior_bi.rds"))

# -----------------------------------------------------------------------------




# Adjusted posterior distributions --------------------------------------------

# Key info:
# phon_sum = if_else(phon == "d", 1, -1),
# language_sum = if_else(language == "english", 1, -1),

bind_rows(
  posterior_samples(mod_coronals_vot_bi_full) %>%
    select(starts_with("b_")) %>%
    bi_vot_prep %>%
    pivot_longer(cols = everything(), names_to = "language",
                 values_to = "val") %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "vot"),
  posterior_samples(mod_coronals_mv_bi_full) %>%
    select(starts_with("b_")) %>%
    bi_mv_prep %>%
    pivot_longer(cols = everything(), names_to = "language",
                 values_to = "val") %>%
    separate(language, into = c("metric", "language", "phon"),
             sep = "_", remove = T) %>%
    select(language, phon, val, metric)) %>%
  saveRDS(., here("data", "models", "posterior_bi_adj.rds"))

# -----------------------------------------------------------------------------
