# Posterior distributions: bilinguals -----------------------------------------
#
# Last update: 2022-03-17
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
  as_tibble(mod_poa_comp_vot_full) %>%
    select(starts_with("b_")) %>%
    pivot_longer(cols = everything(), values_to = "estimate") %>%
    mutate(metric = "vot",
           name = str_replace(name, "cent_std", "cent"),
           name = fct_relevel(name,
            "b_language_sum:poa_sum", "b_rep_n", "b_f1_cent", "b_f2_cent",
            "b_poa_sum", "b_language_sum", "b_Intercept")) %>%
    select(metric, parameters = name, estimate) %>%
    arrange(metric),
  as_tibble(mod_poa_comp_mv_full) %>%
    select(starts_with("b_")) %>%
    pivot_longer(cols = everything(), values_to = "estimate") %>%
    separate(col = name, into = c("pt1", "metric", "pt2"), sep = c(2, 8)) %>%
    unite(col = "parameters", pt1, pt2, sep = "_") %>%
    mutate(metric = str_replace(metric, "_", ""),
      metric = str_remove(metric, "std"),
      parameters = str_replace(parameters, "_", ""),
      parameters = str_replace(parameters, "__", "_"),
      parameters = str_replace_all(parameters, "_std", ""),
      parameters = fct_relevel(parameters,
            "b_language_sum:poa_sum", "b_rep_n", "b_f2_cent", "b_f1_cent",
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
  as_tibble(mod_poa_comp_vot_full) %>%
    select(starts_with("b_")) %>%
    poa_vot_prep %>%
    pivot_longer(cols = everything(), names_to = "language",
                 values_to = "val") %>%
    separate(language, into = c("language", "place"),
             sep = "_", remove = T) %>%
    mutate(metric = "vot"),
  as_tibble(mod_poa_comp_mv_full) %>%
    select(starts_with("b_")) %>%
    sample_n(4000) %>%
    poa_mv_prep %>%
    pivot_longer(cols = everything(), names_to = "language",
                 values_to = "val") %>%
    separate(language, into = c("metric", "language", "place"),
             sep = "_", remove = T) %>%
    select(language, place, val, metric)) %>%
  saveRDS(., here("data", "models", "posterior_poa_comp_adj.rds"))

# -----------------------------------------------------------------------------
