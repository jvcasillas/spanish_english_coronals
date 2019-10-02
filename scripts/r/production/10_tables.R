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
    mod_f1_mono_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("F2", "", "", "")),
    mod_f2_mono_full %>% make_model_table())
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
  str_replace(".phon_sum", " x Phoneme")

mono_table <-
bind_rows(
  bind_cols(
    tibble(Metric = c("VOT", "", "", "", "", "", "")),
    mod_coronals_vot_mono_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("RI", "", "", "", "", "", "")),
    mod_coronals_ri_mono_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("COG", "", "", "", "", "", "")),
    mod_coronals_cog_mono_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("Kurtosis", "", "", "", "", "", "")),
    mod_coronals_kt_mono_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("SD", "", "", "", "", "", "")),
    mod_coronals_sd_mono_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("Skewness", "", "", "", "", "", "")),
    mod_coronals_sk_mono_full %>% make_model_table())
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
  str_replace(".phon_sum", " x Phoneme")


bi_table <-
bind_rows(
  bind_cols(
    tibble(Metric = c("VOT", "", "", "", "", "", "")),
    mod_coronals_vot_bi_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("RI", "", "", "", "", "", "")),
    mod_coronals_ri_bi_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("COG", "", "", "", "", "", "")),
    mod_coronals_cog_bi_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("Kurtosis", "", "", "", "", "", "")),
    mod_coronals_kt_bi_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("SD", "", "", "", "", "", "")),
    mod_coronals_sd_bi_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("Skewness", "", "", "", "", "", "")),
    mod_coronals_sk_bi_full %>% make_model_table())
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
  str_replace(".poa_sum", " x Place")


bi_poa_table <-
bind_rows(
  bind_cols(
    tibble(Metric = c("VOT", "", "", "", "", "", "")),
    mod_poa_comp_vot_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("RI", "", "", "", "", "", "")),
    mod_poa_comp_ri_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("COG", "", "", "", "", "", "")),
    mod_poa_comp_cog_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("Kurtosis", "", "", "", "", "", "")),
    mod_poa_comp_kt_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("SD", "", "", "", "", "", "")),
    mod_poa_comp_sd_full %>% make_model_table()),
  bind_cols(
    tibble(Metric = c("Skewness", "", "", "", "", "", "")),
    mod_poa_comp_sk_full %>% make_model_table())
  ) %>%
  mutate(Parameter = fix_bi_poa_params(Parameter)) %>%
  write_csv(here("data", "tidy", "table_bi_poa_model_summary.csv"))

# -----------------------------------------------------------------------------
