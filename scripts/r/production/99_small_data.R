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

# VOT
mono_vot_rope <- all_models$mod_coronals_vot_mono %>% get_rope()
mono_vot_mpe  <- all_models$mod_coronals_vot_mono %>% get_mpe()
mono_comp_d_t <- glue::glue("(&beta; = {mean}; CI = {hdi}; ROPE = {rope}; MPE = {mpe})",
                   mean = pull(all_models$post_hoc_analyses[1, 2]),
                   hdi = pull(all_models$post_hoc_analyses[1, 3]),
                   rope = pull(all_models$post_hoc_analyses[1, 4]),
                   mpe = pull(all_models$post_hoc_analyses[1, 5]))
# RI
mono_ri_rope  <- all_models$mod_coronals_ri_mono %>% get_rope()
mono_ri_mpe   <- all_models$mod_coronals_ri_mono %>% get_mpe()
# COG
mono_cog_rope <- all_models$mod_coronals_cog_mono %>% get_rope()
mono_cog_mpe  <- all_models$mod_coronals_cog_mono %>% get_mpe()
# KT
mono_kt_rope  <- all_models$mod_coronals_kt_mono %>% get_rope()
mono_kt_mpe   <- all_models$mod_coronals_kt_mono %>% get_mpe()
# SD
mono_sd_rope  <- all_models$mod_coronals_sd_mono %>% get_rope()
mono_sd_mpe   <- all_models$mod_coronals_sd_mono %>% get_mpe()
# SK
mono_sk_rope  <- all_models$mod_coronals_sk_mono %>% get_rope()
mono_sk_mpe   <- all_models$mod_coronals_sk_mono %>% get_mpe()

# -----------------------------------------------------------------------------





# Bilingual analyses ----------------------------------------------------------

# VOT
bi_vot_rope <- all_models$mod_coronals_vot_bi %>% get_rope()
bi_vot_mpe  <- all_models$mod_coronals_vot_bi %>% get_mpe()
bi_comp_d_t <- glue::glue("(&beta; = {mean}; CI = {hdi}; ROPE = {rope}; MPE = {mpe})",
                 mean = pull(all_models$post_hoc_analyses[2, 2]),
                 hdi = pull(all_models$post_hoc_analyses[2, 3]),
                 rope = pull(all_models$post_hoc_analyses[2, 4]),
                 mpe = pull(all_models$post_hoc_analyses[2, 5]))
# RI
bi_ri_rope  <- all_models$mod_coronals_ri_bi %>% get_rope()
bi_ri_mpe   <- all_models$mod_coronals_ri_bi %>% get_mpe()
# COG
bi_cog_rope <- all_models$mod_coronals_cog_bi %>% get_rope()
bi_cog_mpe  <- all_models$mod_coronals_cog_bi %>% get_mpe()
# KT
bi_kt_rope  <- all_models$mod_coronals_kt_bi %>% get_rope()
bi_kt_mpe   <- all_models$mod_coronals_kt_bi %>% get_mpe()
# SD
bi_sd_rope  <- all_models$mod_coronals_sd_bi %>% get_rope()
bi_sd_mpe   <- all_models$mod_coronals_sd_bi %>% get_mpe()
# SK
bi_sk_rope  <- all_models$mod_coronals_sk_bi %>% get_rope()
bi_sk_mpe   <- all_models$mod_coronals_sk_bi %>% get_mpe()

# -----------------------------------------------------------------------------






# POA analyses ----------------------------------------------------------------


# -----------------------------------------------------------------------------
