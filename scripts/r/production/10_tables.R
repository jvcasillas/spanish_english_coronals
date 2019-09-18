# Source vowel models ---------------------------------------------------------

source(here::here("scripts", "r", "production", "04a_vowel_analysis.R"))

# -----------------------------------------------------------------------------

fix_vowel_params <- . %>%
  str_replace("b_Intercept", "Intercept") %>%
  str_replace("b_language_sum", "Language") %>%
  str_replace("b_phon_sum", "Phoneme") %>%
  str_replace("b_rep_n", "Item rep.")


f1_f2_table <-
bind_rows(
  bind_cols(
    tibble(Metric = c("F1", "", "", "")),
    mod_f1_mono_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_f1_mono_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_f1_mono_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_f1_mono_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("F2", "", "", "")),
    mod_f2_mono_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_f2_mono_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_f2_mono_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_f2_mono_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3)
  ) %>%
  mutate(Parameter = fix_vowel_params(Parameter)) %>%
  write_csv(here("data", "tidy", "table_vowel_model_summary.csv"))

# -----------------------------------------------------------------------------















# Source monolingual models ---------------------------------------------------

source(here::here("scripts", "r", "production", "05a_mono_analysis.R"))

# -----------------------------------------------------------------------------

fix_mono_params <- . %>%
  str_replace("b_Intercept", "Intercept") %>%
  str_replace("b_group_sum", "Language") %>%
  str_replace("b_phon_sum", "Phoneme") %>%
  str_replace("b_f1_std", "F1") %>%
  str_replace("b_f2_std", "F2") %>%
  str_replace("b_rep_n", "Item rep.") %>%
  str_replace(":phon_sum", " x Phoneme")


mono_table <-
bind_rows(
  bind_cols(
    tibble(Metric = c("VOT", "", "", "", "", "", "")),
    mod_coronals_vot_mono_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_coronals_vot_mono_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_coronals_vot_mono_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_coronals_vot_mono_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("RI", "", "", "", "", "", "")),
    mod_coronals_ri_mono_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_coronals_ri_mono_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_coronals_ri_mono_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_coronals_ri_mono_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("COG", "", "", "", "", "", "")),
    mod_coronals_cog_mono_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_coronals_cog_mono_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_coronals_cog_mono_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_coronals_cog_mono_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("Kurtosis", "", "", "", "", "", "")),
    mod_coronals_kt_mono_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_coronals_kt_mono_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_coronals_kt_mono_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_coronals_kt_mono_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("SD", "", "", "", "", "", "")),
    mod_coronals_sd_mono_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_coronals_sd_mono_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_coronals_sd_mono_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_coronals_sd_mono_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("Skewness", "", "", "", "", "", "")),
    mod_coronals_sk_mono_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_coronals_sk_mono_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_coronals_sk_mono_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_coronals_sk_mono_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3)

  ) %>%
  mutate(Parameter = fix_mono_params(Parameter)) %>%
  write_csv(here("data", "tidy", "table_mono_model_summary.csv"))

# -----------------------------------------------------------------------------



















# Source bilingual models -----------------------------------------------------

source(here::here("scripts", "r", "production", "06a_bi_analysis.R"))

# -----------------------------------------------------------------------------

fix_bi_params <- . %>%
  str_replace("b_Intercept", "Intercept") %>%
  str_replace("b_language_sum", "Language") %>%
  str_replace("b_phon_sum", "Phoneme") %>%
  str_replace("b_f1_std", "F1") %>%
  str_replace("b_f2_std", "F2") %>%
  str_replace("b_rep_n", "Item rep.") %>%
  str_replace(":phon_sum", " x Phoneme")


bi_table <-
bind_rows(
  bind_cols(
    tibble(Metric = c("VOT", "", "", "", "", "", "")),
    mod_coronals_vot_bi_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_coronals_vot_bi_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_coronals_vot_bi_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_coronals_vot_bi_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("RI", "", "", "", "", "", "")),
    mod_coronals_ri_bi_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_coronals_ri_bi_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_coronals_ri_bi_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_coronals_ri_bi_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("COG", "", "", "", "", "", "")),
    mod_coronals_cog_bi_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_coronals_cog_bi_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_coronals_cog_bi_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_coronals_cog_bi_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("Kurtosis", "", "", "", "", "", "")),
    mod_coronals_kt_bi_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_coronals_kt_bi_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_coronals_kt_bi_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_coronals_kt_bi_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("SD", "", "", "", "", "", "")),
    mod_coronals_sd_bi_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_coronals_sd_bi_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_coronals_sd_bi_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_coronals_sd_bi_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("Skewness", "", "", "", "", "", "")),
    mod_coronals_sk_bi_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_coronals_sk_bi_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_coronals_sk_bi_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_coronals_sk_bi_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3)

  ) %>%
  mutate(Parameter = fix_bi_params(Parameter)) %>%
  write_csv(here("data", "tidy", "table_bi_model_summary.csv"))

# -----------------------------------------------------------------------------

















# Source bilingual models -----------------------------------------------------

source(here::here("scripts", "r", "production", "07a_bi_poa_analysis.R"))

# -----------------------------------------------------------------------------

fix_bi_poa_params <- . %>%
  str_replace("b_Intercept", "Intercept") %>%
  str_replace("b_language_sum", "Language") %>%
  str_replace("b_poa_sum", "Place") %>%
  str_replace("b_f1_std", "F1") %>%
  str_replace("b_f2_std", "F2") %>%
  str_replace("b_rep_n", "Item rep.") %>%
  str_replace(":poa_sum", " x Place")


bi_poa_table <-
bind_rows(
  bind_cols(
    tibble(Metric = c("VOT", "", "", "", "", "", "")),
    mod_poa_comp_vot_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_poa_comp_vot_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_poa_comp_vot_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_poa_comp_vot_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("RI", "", "", "", "", "", "")),
    mod_poa_comp_ri_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_poa_comp_ri_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_poa_comp_ri_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_poa_comp_ri_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("COG", "", "", "", "", "", "")),
    mod_poa_comp_cog_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_poa_comp_cog_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_poa_comp_cog_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_poa_comp_cog_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("Kurtosis", "", "", "", "", "", "")),
    mod_poa_comp_kt_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_poa_comp_kt_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_poa_comp_kt_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_poa_comp_kt_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("SD", "", "", "", "", "", "")),
    mod_poa_comp_sd_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_poa_comp_sd_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_poa_comp_sd_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_poa_comp_sd_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3),

  bind_cols(
    tibble(Metric = c("Skewness", "", "", "", "", "", "")),
    mod_poa_comp_sk_full %>%
      tidy() %>%
        filter(stringr::str_detect(term, "b_") == TRUE) %>%
        select(Parameter = term, estimate),
    bayestestR::hdi(mod_poa_comp_sk_full, ci = 0.95) %>% as_tibble %>%
      mutate_if(is.numeric, round, digits = 3) %>%
      unite(HDI, CI_low, CI_high, sep = ", ") %>%
      mutate(HDI = paste0("[", HDI, "]")),
    bayestestR::rope(mod_poa_comp_sk_full, ci = 0.95) %>% as_tibble %>%
      select(Parameter, ROPE = ROPE_Percentage),
    bayestestR::p_direction(mod_poa_comp_sk_full) %>% as_tibble) %>%
    select(Metric, Parameter, Estimate = estimate, HDI, ROPE, MPE = pd) %>%
    mutate_if(is.numeric, round, digits = 3)

  ) %>%
  mutate(Parameter = fix_bi_poa_params(Parameter)) %>%
  write_csv(here("data", "tidy", "table_bi_poa_model_summary.csv"))

# -----------------------------------------------------------------------------
