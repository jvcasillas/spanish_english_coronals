# Small data for reporting analyses -------------------------------------------
#
# Last update: 2022-04-05
#

# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "03_load_data.R"))

# -----------------------------------------------------------------------------




# Software --------------------------------------------------------------------

r_version    <- glue("version ",
                     sessionInfo()[["R.version"]][["major"]],
                     ".",
                     sessionInfo()[["R.version"]][["minor"]])
brms_version <- glue("version ",
                     sessionInfo()[["otherPkgs"]][["brms"]][["Version"]])

# -----------------------------------------------------------------------------




# Sample sizes ----------------------------------------------------------------

sample_size <- bind_rows(
  coronals %>%
    group_by(group) %>%
    summarize(n = n_distinct(id)),
  bilabials %>%
    mutate(group = "POA") %>%
    group_by(group) %>%
    summarize(n = n_distinct(id))
  )

mono_sp_n     <- sample_size %>% filter(group == "NSP") %>% pull(n)
mono_en_n     <- sample_size %>% filter(group == "NEN") %>% pull(n)
bi_coronals_n <- sample_size %>% filter(group == "BIL") %>% pull(n)
bi_poa_n      <- sample_size %>% filter(group == "POA") %>% pull(n)
exp_n         <- sample_size %>% pull(n) %>% sum

# -----------------------------------------------------------------------------



# Load tables -----------------------------------------------------------------

# Vowel analysis
f1_f2_table <- read_csv(here("data", "tidy", "table_vowel_model_summary.csv"))

# Monolingual analyses
mono_table <- read_csv(here("data", "tidy", "table_mono_model_summary.csv"))

# Bilingual analyses
bi_table <- read_csv(here("data", "tidy", "table_bi_model_summary.csv"))

# POA analyses
bi_poa_table <- read_csv(here("data", "tidy", "table_bi_poa_model_summary.csv"))

# Post-hoc comparisons
post_hoc <- readRDS(here("data", "models", "post_hoc_analyses.rds"))

# -----------------------------------------------------------------------------




# Dataset descriptives --------------------------------------------------------

# Coronals
n_coronal_items_en <- coronals %>%
                      filter(language == "english") %>%
                      pull(item) %>% unique %>% length
n_coronal_items_sp <- coronals %>%
                      filter(language == "spanish") %>%
                      pull(item) %>% unique %>% length
n_coronal_items    <- n_coronal_items_en + n_coronal_items_sp
n_coronal_reps     <- 3
n_coronal_tokens   <- nrow(coronals)
n_coronal_possible <- 24 * 2 * 3 * (17 + 8)
n_discarded        <- n_coronal_possible - n_coronal_tokens

blps_coronals_modules <- coronals_blp %>%
  select(-id, -c(spanish:dom_std)) %>%
  summarize(across(everything(), list(mean = mean, sd = sd))) %>%
  pivot_longer(cols = everything(), names_to = "metric", values_to = "val") %>%
  separate(metric, into = c("component", "lang", "metric"), sep = "_") %>%
  pivot_wider(names_from = metric, values_from = val) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(mean_sd = glue("{mean} (SD = {sd})")) %>%
  select(-mean, -sd) %>%
  pivot_wider(names_from = lang, values_from = mean_sd)

blp_coronals_dominance <- summarize(coronals_blp,
                            dom_avg = mean(dominance), dom_sd = sd(dominance))





# Bilabials
n_bilabial_items_en <- bilabials %>%
                       filter(language == "english") %>%
                       pull(item) %>% unique %>% length
n_bilabial_items_sp <- bilabials %>%
                       filter(language == "spanish") %>%
                       pull(item) %>% unique %>% length
n_bilabial_items    <- n_bilabial_items_en + n_bilabial_items_sp
n_bilabial_reps     <- 1
n_bilabial_tokens   <- nrow(bilabials)

# -----------------------------------------------------------------------------

