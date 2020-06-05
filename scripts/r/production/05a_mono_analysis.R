# Analysis 1: monolinguals ----------------------------------------------------
#
# - VOT
# - RI
# - Spectral moments
#    - sd
#    - cog
#    - sk
#    - kt
# -----------------------------------------------------------------------------




# Load libraries, helpers and data --------------------------------------------

source(here::here("scripts", "r", "production", "03_load_data.R"))

# -----------------------------------------------------------------------------




# General model setup ---------------------------------------------------------
#
# - create subset (only mono, exclude errors)
# - code variables (sum code group, phon, language, and stress)

coronals_mono <- coronals %>%
  filter(., group %in% c("NEN", "NSP"), is.na(label)) %>%
  mutate(across(c("cog", "kt", "sk"),
                .fns = list(std = ~smart_scale(.)))) %>%
  mutate(across(c("f1_cent", "f2_cent", "ri", "vot", "sd"),
                .fns = list(std = ~simple_scale(.)))) %>%
  mutate(phon_sum = if_else(phon == "d", 1, -1),
         group_sum = if_else(group == "NEN", 1, -1),
         language_sum = if_else(language == "english", 1, -1))

# Use all available cores for parallel computing
options(mc.cores = parallel::detectCores())

# Regularizing, weakly informative priors
priors <- c(
  set_prior("normal(0, 2)", class = "Intercept",
    resp = c("ristd", "sdstd", "cogstd", "ktstd", "skstd")),
  set_prior("normal(0, 2)", class = "b")
  )


# -----------------------------------------------------------------------------





# Fit models ------------------------------------------------------------------

# VOT
mod_coronals_vot_mono_full <- brm(
  formula = vot_std ~ 1 + group_sum * phon_sum + f1_std + f2_std + rep_n +
    (1 + phon_sum + f1_std + f2_std + rep_n | id) +
    (1 + rep_n | item),
  prior = c(set_prior("normal(0, 2)", class = "Intercept"),
            set_prior("normal(0, 2)", class = "b")),
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_mono,
  file = here("data", "models", "mod_coronals_vot_mono_full")
)

# Spectral moments
mv_mono_model_formula <- bf(
  mvbind(ri_std, cog_std, sd_std, sk_std, kt_std) ~ 1 +
     group_sum * phon_sum +
     f1_cent_std + f2_cent_std + rep_n +
     (1 + phon_sum |p| id) +
     (1 |q| item)
  ) + set_rescor(rescor = TRUE)

mod_coronals_mv_mono_full <- brm(
  formula = mv_mono_model_formula,
  prior = priors,
  warmup = 1000, iter = 4000, chains = 6, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  data = coronals_mono,
  file = here("data", "models", "mod_coronals_mv_mono_full")
)

# -----------------------------------------------------------------------------
