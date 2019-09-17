# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "03_load_data.R"))

models_path <- here("data", "models")

mod_names <- list.files(models_path, pattern = ".rds") %>%
  stringr::str_remove(".rds")

all_models <- fs::dir_ls(models_path, regexp = "\\.rds$") %>%
  map(readRDS) %>%
  set_names(mod_names)

# -----------------------------------------------------------------------------

exp_n
mono_sp_n
mono_en_n
bi_coronals_n
bi_poa_n



