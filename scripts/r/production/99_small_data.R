# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "03_load_data.R"))

models_path <- here("data", "models")

mod_names <- list.files(models_path, pattern = ".rds") %>%
  stringr::str_remove(".rds")

all_models <- fs::dir_ls(models_path, regexp = "\\.rds$") %>%
  map(readRDS) %>%
  set_names(mod_names)

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

f1_rope <- all_models$mod_f1_mono_full %>% get_rope()
f2_rope <- all_models$mod_f2_mono_full %>% get_rope()
f1_mpe  <- all_models$mod_f1_mono_full %>% get_mpe()
f2_mpe  <- all_models$mod_f2_mono_full %>% get_mpe()

# -----------------------------------------------------------------------------


# Monolingual analyses --------------------------------------------------------

mono_vot_rope <- all_models$mod_coronals_vot_mono %>% get_rope()
mono_vot_mpe  <- all_models$mod_coronals_vot_mono %>% get_mpe()

# -----------------------------------------------------------------------------


# Bilingual analyses ----------------------------------------------------------


# -----------------------------------------------------------------------------


# POA analyses ----------------------------------------------------------------


# -----------------------------------------------------------------------------
