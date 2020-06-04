# Create model summary tables -------------------------------------------------
#
# - use model objects to get summaries
# - combine in large data frames
# - edit as necessary
# - save as .csv
# - to be loaded in project report and converted to pandoc table
#
# -----------------------------------------------------------------------------


# Source  models --------------------------------------------------------------

source(here::here("scripts", "r", "production", "04a_vowel_analysis.R"))
source(here::here("scripts", "r", "production", "05a_mono_analysis.R"))
source(here::here("scripts", "r", "production", "06a_bi_analysis.R"))
source(here::here("scripts", "r", "production", "07a_bi_poa_analysis.R"))

# -----------------------------------------------------------------------------


# F1/F2 table -----------------------------------------------------------------

f1_f2_table <- posterior_samples(mod_f1f2_mv_mono_full) %>%
  select(starts_with("b_")) %>%
  imap_dfr(make_model_table) %>%
  arrange(column) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  mutate_if(is.numeric, format, nsmall = 3) %>%
  mutate(hdi_lo = str_replace(hdi_lo, " ", ""),
         hdi_hi = str_replace(hdi_hi, " ", "")) %>%
  unite(HDI, hdi_lo, hdi_hi, sep = ", ") %>%
  separate(column, into = c("Metric", "Parameter"), sep = 8) %>%
  mutate(
    HDI = paste0("[", HDI, "]"),
    Metric = if_else(Metric == "b_f1std_", "F1", "F2"),
    Parameter = case_when(
      Parameter == "Intercept" ~ "Intercept",
      Parameter == "language_sum" ~ "Language",
      Parameter == "phon_sum" ~ "Phoneme",
      TRUE ~ "Item rep.")) %>%
  mutate_at(c("Estimate", "HDI"), str_replace,
            pattern = "-", replacement = "&minus;") %>%
  unite(col = "identifier", Metric, Parameter, sep = "_", remove = F) %>%
  write_csv(here("data", "tidy", "table_vowel_model_summary.csv"))

# -----------------------------------------------------------------------------







# Monolingual table -----------------------------------------------------------

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


# Bilingual table -------------------------------------------------------------

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


# Bilingual POA table ---------------------------------------------------------

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
