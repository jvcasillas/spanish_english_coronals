# Posterior distributions: bilinguals -----------------------------------------
#
# - Get posterior distributions of all 'bi' models and
#   combine them into a single dataframe for plotting and
#   post-hoc scrutiny
#
# -----------------------------------------------------------------------------


# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "07a_bi_poa_analysis.R"))

# -----------------------------------------------------------------------------




# Combined posterior ----------------------------------------------------------

bind_rows(
  posterior_samples(mod_poa_comp_vot_full) %>%
    select(starts_with("b_")) %>%
    pivot_longer(cols = everything(), values_to = "estimate") %>%
    mutate(metric = "vot",
           name = str_replace(name, "cent_std", "cent"),
           name = str_replace(name, ":poa_sum", ":poa"),
           name = fct_relevel(name,
            "b_language_sum:poa", "b_rep_n", "b_f1_cent", "b_f2_cent",
            "b_poa_sum", "b_language_sum", "b_Intercept")) %>%
    select(metric, parameters = name, estimate) %>%
    arrange(metric),
  posterior_samples(mod_poa_comp_mv_full) %>%
    select(starts_with("b_")) %>%
    pivot_longer(cols = everything(), values_to = "estimate") %>%
    separate(col = name, into = c("pt1", "metric", "pt2", "pt3"), sep = "_") %>%
    unite(col = "parameters", pt1, pt2, pt3, sep = "_") %>%
    mutate(metric = str_replace(metric, "std", ""),
           parameters = str_replace(parameters, "_NA", ""),
           parameters = fct_relevel(parameters,
            "b_language_sum:poa", "b_rep_n", "b_f2_cent", "b_f1_cent",
            "b_poa_sum", "b_language_sum", "b_Intercept")) %>%
    select(metric, parameters, estimate) %>%
    arrange(metric)) %>%
  saveRDS(., here("data", "models", "posterior_poa_comp.rds"))

# -----------------------------------------------------------------------------




# Adjusted posterior distributions --------------------------------------------

# Key info:
# language_sum = if_else(language == "english", 1, -1),
# poa_sum = if_else(phon == "t", 1, -1))

bind_rows(
  posterior_samples(mod_poa_comp_vot_full) %>%
    select(starts_with("b_")) %>%
    poa_vot_prep %>%
    pivot_longer(cols = everything(), names_to = "language",
                 values_to = "val") %>%
    separate(language, into = c("language", "place"),
             sep = "_", remove = T) %>%
    mutate(metric = "vot"),
  posterior_samples(mod_poa_comp_mv_full) %>%
    select(starts_with("b_")) %>%
    poa_mv_prep %>%
    pivot_longer(cols = everything(), names_to = "language",
                 values_to = "val") %>%
    separate(language, into = c("metric", "language", "place"),
             sep = "_", remove = T) %>%
    select(language, place, val, metric)) %>%
  saveRDS(., here("data", "models", "posterior_poa_comp_adj.rds"))

# -----------------------------------------------------------------------------
