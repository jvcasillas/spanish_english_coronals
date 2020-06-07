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

posterior_samples(mod_f1f2_mv_mono_full) %>%
  select(starts_with("b_")) %>%
  imap_dfr(make_model_table) %>%
  arrange(column) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  mutate_if(is.numeric, format, nsmall = 3) %>%
  mutate(hdi_lo = str_replace(hdi_lo, " ", ""),
         hdi_hi = str_replace(hdi_hi, " ", ""),
         Estimate = str_replace(Estimate, " ", "")) %>%
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
  mutate_at(c("Estimate", "HDI"), str_replace_all,
            pattern = "-", replacement = "&minus;") %>%
  unite(col = "identifier", Metric, Parameter, sep = "_", remove = F) %>%
  write_csv(here("data", "tidy", "table_vowel_model_summary.csv"))

# -----------------------------------------------------------------------------







# Monolingual table -----------------------------------------------------------

bind_rows(
  posterior_samples(mod_coronals_vot_mono_full) %>%
    select(starts_with("b_")) %>%
    imap_dfr(make_model_table) %>%
    mutate(Metric = "VOT",
           Parameter = case_when(
             column == "b_Intercept" ~ "Intercept",
             column == "b_group_sum" ~ "Group",
             column == "b_phon_sum" ~ "Phoneme",
             column == "b_f1_cent_std" ~ "F1",
             column == "b_f2_cent_std" ~ "F2",
             column == "b_rep_n" ~ "Item rep.",
             TRUE ~ "Group x Phoneme")) %>%
    mutate_if(is.numeric, round, digits = 3) %>%
    mutate_if(is.numeric, format, nsmall = 3) %>%
    mutate(hdi_lo = str_replace(hdi_lo, " ", ""),
           hdi_hi = str_replace(hdi_hi, " ", ""),
           Estimate = str_replace(Estimate, " ", "")) %>%
    unite(HDI, hdi_lo, hdi_hi, sep = ", ") %>%
    mutate(HDI = paste0("[", HDI, "]")) %>%
    mutate_at(c("Estimate", "HDI"), str_replace_all,
              pattern = "-", replacement = "&minus;") %>%
    unite(col = "identifier", Metric, Parameter, sep = "_", remove = F) %>%
    select(identifier, Metric, Parameter, Estimate, HDI, ROPE, MPE),

  posterior_samples(mod_coronals_mv_mono_full) %>%
    select(starts_with("b_")) %>%
    imap_dfr(make_model_table) %>%
    arrange(column) %>%
    separate(column, into = c("p0", "Metric", "p1", "p2", "p3"), sep = "_") %>%
    unite(col = "Parameter", p1, p2, sep = "_") %>%
    mutate_if(is.numeric, round, digits = 3) %>%
    mutate_if(is.numeric, format, nsmall = 3) %>%
    mutate(hdi_lo = str_replace(hdi_lo, " ", ""),
           hdi_hi = str_replace(hdi_hi, " ", ""),
           Estimate = str_replace(Estimate, " ", "")) %>%
    unite(HDI, hdi_lo, hdi_hi, sep = ", ") %>%
    mutate(
      HDI = paste0("[", HDI, "]"),
      Metric = str_replace(Metric, "std", ""),
      Metric = toupper(Metric),
      Parameter = case_when(
      Parameter == "Intercept_NA" ~ "Intercept",
      Parameter == "f1_cent" ~ "F1",
      Parameter == "f2_cent" ~ "F2",
      Parameter == "phon_sum" ~ "Phoneme",
      Parameter == "rep_n" ~ "Item rep.",
      Parameter == "group_sum:phon" ~ "Group x Phoneme",
      TRUE ~ "Group"),
      Parameter = fct_relevel(Parameter, "Intercept", "Group", "Phoneme",
                              "F1", "F2", "Item rep.")) %>%
      mutate_at(c("Estimate", "HDI"), str_replace_all,
                pattern = "-", replacement = "&minus;") %>%
      unite(col = "identifier", Metric, Parameter, sep = "_", remove = F) %>%
      select(-p0, -p3) %>%
      arrange(Metric, Parameter)) %>%
  write_csv(here("data", "tidy", "table_mono_model_summary.csv"))

# -----------------------------------------------------------------------------


# Bilingual table -------------------------------------------------------------

bind_rows(
  posterior_samples(mod_coronals_vot_bi_full) %>%
    select(starts_with("b_")) %>%
    imap_dfr(make_model_table) %>%
    mutate(Metric = "VOT",
           Parameter = case_when(
             column == "b_Intercept" ~ "Intercept",
             column == "b_language_sum" ~ "Language",
             column == "b_phon_sum" ~ "Phoneme",
             column == "b_f1_cent_std" ~ "F1",
             column == "b_f2_cent_std" ~ "F2",
             column == "b_rep_n" ~ "Item rep.",
             TRUE ~ "Language x Phoneme")) %>%
    mutate_if(is.numeric, round, digits = 3) %>%
    mutate_if(is.numeric, format, nsmall = 3) %>%
    mutate(hdi_lo = str_replace(hdi_lo, " ", ""),
           hdi_hi = str_replace(hdi_hi, " ", ""),
           Estimate = str_replace(Estimate, " ", "")) %>%
    unite(HDI, hdi_lo, hdi_hi, sep = ", ") %>%
    mutate(HDI = paste0("[", HDI, "]")) %>%
    mutate_at(c("Estimate", "HDI"), str_replace_all,
              pattern = "-", replacement = "&minus;") %>%
    unite(col = "identifier", Metric, Parameter, sep = "_", remove = F) %>%
    select(identifier, Metric, Parameter, Estimate, HDI, ROPE, MPE),

  posterior_samples(mod_coronals_mv_bi_full) %>%
    select(starts_with("b_")) %>%
    imap_dfr(make_model_table) %>%
    arrange(column) %>%
    separate(column, into = c("p0", "Metric", "p1", "p2", "p3"), sep = "_") %>%
    unite(col = "Parameter", p1, p2, sep = "_") %>%
    mutate_if(is.numeric, round, digits = 3) %>%
    mutate_if(is.numeric, format, nsmall = 3) %>%
    mutate(hdi_lo = str_replace(hdi_lo, " ", ""),
           hdi_hi = str_replace(hdi_hi, " ", ""),
           Estimate = str_replace(Estimate, " ", "")) %>%
    unite(HDI, hdi_lo, hdi_hi, sep = ", ") %>%
    mutate(
      HDI = paste0("[", HDI, "]"),
      Metric = str_replace(Metric, "std", ""),
      Metric = toupper(Metric),
      Parameter = case_when(
        Parameter == "Intercept_NA" ~ "Intercept",
        Parameter == "f1_cent" ~ "F1",
        Parameter == "f2_cent" ~ "F2",
        Parameter == "phon_sum" ~ "Phoneme",
        Parameter == "rep_n" ~ "Item rep.",
        Parameter == "language_sum:phon" ~ "Language x Phoneme",
        TRUE ~ "Language"),
      Parameter = fct_relevel(Parameter, "Intercept", "Language", "Phoneme",
                              "F1", "F2", "Item rep.")) %>%
    mutate_at(c("Estimate", "HDI"), str_replace_all,
              pattern = "-", replacement = "&minus;") %>%
    unite(col = "identifier", Metric, Parameter, sep = "_", remove = F) %>%
    select(-p0, -p3) %>%
    arrange(Metric, Parameter)) %>%
  write_csv(here("data", "tidy", "table_bi_model_summary.csv"))

# -----------------------------------------------------------------------------


# Bilingual POA table ---------------------------------------------------------

bind_rows(
  posterior_samples(mod_poa_comp_vot_full) %>%
    select(starts_with("b_")) %>%
    imap_dfr(make_model_table) %>%
    mutate(Metric = "VOT",
           Parameter = case_when(
             column == "b_Intercept" ~ "Intercept",
             column == "b_language_sum" ~ "Language",
             column == "b_poa_sum" ~ "Place",
             column == "b_f1_cent_std" ~ "F1",
             column == "b_f2_cent_std" ~ "F2",
             column == "b_rep_n" ~ "Item rep.",
             TRUE ~ "Language x Place")) %>%
    mutate_if(is.numeric, round, digits = 3) %>%
    mutate_if(is.numeric, format, nsmall = 3) %>%
    mutate(hdi_lo = str_replace(hdi_lo, " ", ""),
           hdi_hi = str_replace(hdi_hi, " ", ""),
           Estimate = str_replace(Estimate, " ", "")) %>%
    unite(HDI, hdi_lo, hdi_hi, sep = ", ") %>%
    mutate(HDI = paste0("[", HDI, "]")) %>%
    mutate_at(c("Estimate", "HDI"), str_replace_all,
              pattern = "-", replacement = "&minus;") %>%
    unite(col = "identifier", Metric, Parameter, sep = "_", remove = F) %>%
    select(identifier, Metric, Parameter, Estimate, HDI, ROPE, MPE),

  posterior_samples(mod_poa_comp_mv_full) %>%
    select(starts_with("b_")) %>%
    imap_dfr(make_model_table) %>%
    arrange(column) %>%
    separate(column, into = c("p0", "Metric", "p1", "p2", "p3"), sep = "_") %>%
    unite(col = "Parameter", p1, p2, sep = "_") %>%
    mutate_if(is.numeric, round, digits = 3) %>%
    mutate_if(is.numeric, format, nsmall = 3) %>%
    mutate(hdi_lo = str_replace(hdi_lo, " ", ""),
           hdi_hi = str_replace(hdi_hi, " ", ""),
           Estimate = str_replace(Estimate, " ", "")) %>%
    unite(HDI, hdi_lo, hdi_hi, sep = ", ") %>%
    mutate(
      HDI = paste0("[", HDI, "]"),
      Metric = str_replace(Metric, "std", ""),
      Metric = toupper(Metric),
      Parameter = case_when(
        Parameter == "Intercept_NA" ~ "Intercept",
        Parameter == "f1_cent" ~ "F1",
        Parameter == "f2_cent" ~ "F2",
        Parameter == "poa_sum" ~ "Place",
        Parameter == "rep_n" ~ "Item rep.",
        Parameter == "language_sum:poa" ~ "Language x Place",
        TRUE ~ "Language"),
      Parameter = fct_relevel(Parameter, "Intercept", "Language", "Place",
                              "F1", "F2", "Item rep.")) %>%
    mutate_at(c("Estimate", "HDI"), str_replace_all,
              pattern = "-", replacement = "&minus;") %>%
    unite(col = "identifier", Metric, Parameter, sep = "_", remove = F) %>%
    select(-p0, -p3) %>%
    arrange(Metric, Parameter)) %>%
  write_csv(here("data", "tidy", "table_bi_poa_model_summary.csv"))

# -----------------------------------------------------------------------------
