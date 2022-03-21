# Small data for reporting analyses -------------------------------------------
#
# Last update: 2022-03-17
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




# Vowel analysis --------------------------------------------------------------

f1_f2_table <- read_csv(here("data", "tidy", "table_vowel_model_summary.csv"))

# -----------------------------------------------------------------------------




# Monolingual analyses --------------------------------------------------------

mono_table <- read_csv(here("data", "tidy", "table_mono_model_summary.csv"))

# -----------------------------------------------------------------------------




# Bilingual analyses ----------------------------------------------------------

bi_table <- read_csv(here("data", "tidy", "table_bi_model_summary.csv"))

# -----------------------------------------------------------------------------




# POA analyses ----------------------------------------------------------------

bi_poa_table <- read_csv(here("data", "tidy", "table_bi_poa_model_summary.csv"))

# -----------------------------------------------------------------------------




# Post-hoc comparisons --------------------------------------------------------

post_hoc <- readRDS(here("data", "models", "post_hoc_analyses.rds"))

# -----------------------------------------------------------------------------
