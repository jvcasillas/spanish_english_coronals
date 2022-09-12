# Posterior distributions: bilinguals -----------------------------------------
#
# Last update: 2022-09-11
#
# - Get posterior distributions of all 'poa' models and
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
           name = str_remove(name, "_cent_std"),
           name = fct_relevel(name,
            "b_languagespanish:phonk", "b_languagespanish:phonp", "b_rep_n",
            "b_f1", "b_f2", "b_phonk", "b_phonp", "b_languagespanish",
            "b_Intercept")) %>%
    select(metric, parameters = name, estimate) %>%
    arrange(metric),
  as_tibble(mod_poa_comp_mv_full) %>%
    select(starts_with("b_")) %>%
    pivot_longer(cols = everything(), values_to = "estimate") %>%
    separate(col = name, into = c("pt1", "metric", "pt2"), sep = "_", extra = "merge") %>%
    unite(col = "parameters", pt1, pt2, sep = "_") %>%
    mutate(
      metric = str_remove(metric, "std"),
      parameters = str_remove(parameters, "_cent_std"),
      parameters = fct_relevel(parameters,
        "b_languagespanish:phonk", "b_languagespanish:phonp", "b_rep_n",
        "b_f1", "b_f2", "b_phonk", "b_phonp", "b_languagespanish",
        "b_Intercept")) %>%
    select(metric, parameters, estimate) %>%
    arrange(metric)
  ) %>%
  saveRDS(., here("data", "models", "posterior_poa_comp.rds"))

# -----------------------------------------------------------------------------




# Adjusted posterior distributions --------------------------------------------

# Key info:
# language = english is baseline
# phon = t is baseline

bind_rows(
  as_tibble(mod_poa_comp_vot_full) %>%
    select(starts_with("b_")) %>%
    poa_vot_prep %>%
    pivot_longer(cols = everything(), names_to = "language",
                 values_to = "val") %>%
    separate(language, into = c("language", "phon"),
             sep = "_", remove = T) %>%
    mutate(metric = "vot"),
  as_tibble(mod_poa_comp_mv_full) %>%
    select(starts_with("b_")) %>%
    sample_n(4000) %>%
    poa_mv_prep %>%
    pivot_longer(cols = everything(), names_to = "language",
                 values_to = "val") %>%
    separate(language, into = c("metric", "language", "phon"),
             sep = "_", remove = T) %>%
    select(language, phon, val, metric)
  ) %>%
  saveRDS(., here("data", "models", "posterior_poa_comp_adj.rds"))

# Chapuza for plot
poa_vot_cond_effects <- conditional_effects(mod_poa_comp_vot_full, "language:phon")
poa_mv_cond_effects <- conditional_effects(mod_poa_comp_mv_full, "language:phon")

bind_rows(
  poa_vot_cond_effects$`language:phon` %>%
    mutate(metric = "vot") %>%
    select(metric, language:phon, estimate__:upper__),
  poa_mv_cond_effects$ri %>%
    mutate(metric = "ri") %>%
    select(metric, language:phon, estimate__:upper__),
  poa_mv_cond_effects$cog %>%
    mutate(metric = "cog") %>%
    select(metric, language:phon, estimate__:upper__),
  poa_mv_cond_effects$kt %>%
    mutate(metric = "kt") %>%
    select(metric, language:phon, estimate__:upper__),
  poa_mv_cond_effects$sd %>%
    mutate(metric = "sd") %>%
    select(metric, language:phon, estimate__:upper__),
  poa_mv_cond_effects$sk %>%
    mutate(metric = "sk") %>%
    select(metric, language:phon, estimate__:upper__)
) %>%
  pivot_longer(cols = c("estimate__", "se__", "lower__", "upper__"),
               names_to = "thing", values_to = "val") %>%
  mutate(lang = language,
         thing = str_remove_all(thing, "__"),
         place = case_when(
           phon == "p" ~ "bilabial",
           phon == "t" ~ "coronal",
           phon == "k" ~ "velar"
         ),
         place = fct_relevel(place, "bilabial", "coronal", "velar"),
         metric = fct_relevel(metric, "vot", "ri")) %>%
  pivot_wider(names_from = "thing", values_from = "val") %>%
  saveRDS(., here("data", "models", "posterior_poa_comp_adj_2.rds"))


# -----------------------------------------------------------------------------
