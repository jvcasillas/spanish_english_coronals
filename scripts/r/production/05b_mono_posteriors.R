# Posterior distributions: monolinguals ---------------------------------------
#
# - Get posterior distributions of all 'mono' models and
#   combine them into a single dataframe for plotting and
#   post-hoc scrutiny
#
# -----------------------------------------------------------------------------


# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "05a_mono_analysis.R"))

# -----------------------------------------------------------------------------




# Combined posterior ----------------------------------------------------------

bind_rows(
  posterior_samples(mod_coronals_vot_mono_full) %>%
    select(starts_with("b_")) %>%
    pivot_longer(cols = everything(), values_to = "estimate") %>%
    mutate(metric = "vot",
           name = str_replace(name, "cent_std", "cent"),
           name = str_replace(name, ":phon_sum", ":phon"),
           name = fct_relevel(name,
            "b_group_sum:phon", "b_rep_n", "b_f1_cent", "b_f2_cent",
            "b_phon_sum", "b_group_sum", "b_Intercept")) %>%
    select(metric, parameters = name, estimate) %>%
    arrange(metric),
  posterior_samples(mod_coronals_mv_mono_full) %>%
  select(starts_with("b_")) %>%
    pivot_longer(cols = everything(), values_to = "estimate") %>%
    separate(col = name, into = c("pt1", "metric", "pt2", "pt3"), sep = "_") %>%
    unite(col = "parameters", pt1, pt2, pt3, sep = "_") %>%
    mutate(metric = str_replace(metric, "std", ""),
           parameters = str_replace(parameters, "_NA", ""),
           parameters = fct_relevel(parameters,
            "b_group_sum:phon", "b_rep_n", "b_f1_cent", "b_f2_cent",
            "b_phon_sum", "b_group_sum", "b_Intercept")) %>%
    select(metric, parameters, estimate) %>%
    arrange(metric)
  ) %>%
  saveRDS(., here("data", "models", "posterior_mono.rds"))

# -----------------------------------------------------------------------------




# Adjusted posterior distributions --------------------------------------------

# Key info:
# phon_sum = if_else(phon == "d", 1, -1),
# group_sum = if_else(group == "NEN", 1, -1),

bind_rows(
  posterior_samples(mod_coronals_vot_mono_full) %>%
    select(starts_with("b_")) %>%
    mono_vot_prep %>%
    pivot_longer(cols = everything(), names_to = "language",
                 values_to = "val") %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "vot"),
  posterior_samples(mod_coronals_mv_mono_full) %>%
    select(starts_with("b_")) %>%
    mono_mv_prep %>%
    pivot_longer(cols = everything(), names_to = "language",
                 values_to = "val") %>%
    separate(language, into = c("metric", "language", "phon"),
             sep = "_", remove = T) %>%
    select(language, phon, val, metric)) %>%
  saveRDS(., here("data", "models", "posterior_mono_adj.rds"))

# -----------------------------------------------------------------------------
