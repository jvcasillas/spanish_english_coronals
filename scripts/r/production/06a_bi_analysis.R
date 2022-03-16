# Analysis 2: bilinguals ------------------------------------------------------
#
# Last update: 2020-06-16
#
# This script will fit two separate models:
# - VOT ~ language and phoneme
# - Spectral moments and RI
#    - (RI, COG, SD, SK, KT) ~ language and phoneme
#
# The purpose of these analyses is to determine if there is a difference
# in VOT between languages for coronal stops (yes).
# We then ask if we can also capture coronal stop burst differences using
# relative intensity and spectral moments.
# Importantly, we respond to the second question using a multivariate model
# in order to assess not only how relative intensity and spectral moments
# of the stop burst vary as function of language, but also to model the
# residual covariance between them.
# -----------------------------------------------------------------------------




# Load libraries, helpers and data --------------------------------------------

source(here::here("scripts", "r", "production", "03_load_data.R"))

# -----------------------------------------------------------------------------




# General model setup ---------------------------------------------------------
#
# - create subset (only bilinguals, exclude errors)
# - code variables (sum code group, phon, language, and stress)

coronals_bi <- coronals %>%
  filter(., group == "BIL", is.na(label)) %>%
  mutate(across(c("cog", "kt", "sk"),
                .fns = list(std = ~smart_scale(.)))) %>%
  mutate(across(c("f1_cent", "f2_cent", "ri", "vot", "sd"),
                .fns = list(std = ~simple_scale(.)))) %>%
  mutate(phon_sum = if_else(phon == "d", 1, -1),
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
vot_bi_model_formula <- bf(
  vot_std ~ 1 +
    language_sum * phon_sum +
    f1_cent_std + f2_cent_std + rep_n +
    (1 + phon_sum | id) +
    (1 | item))

mod_coronals_vot_bi_full <- brm(
  formula = vot_bi_model_formula,
  prior = c(set_prior("normal(0, 2)", class = "Intercept"),
            set_prior("normal(0, 2)", class = "b")),
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_bi,
  file = here("data", "models", "mod_coronals_vot_bi_full")
)

# mod_2 <- update(mod_coronals_vot_mono_full, formula = vot_mono_model_formula_2)
# loo1 <- loo(mod_coronals_vot_mono_full, save_psis = T)
# loo2 <- loo(mod_2, save_psis = T)
# loo_compare(loo1, loo2)




# Spectral moments
mv_bi_model_formula <- bf(
  mvbind(ri_std, cog_std, sd_std, sk_std, kt_std) ~ 1 +
    language_sum * phon_sum +
    f1_cent_std + f2_cent_std + rep_n +
    (1 + phon_sum |p| id) +
    (1 |q| item)
) + set_rescor(rescor = TRUE)

mod_coronals_mv_bi_full <- brm(
  formula = mv_bi_model_formula,
  prior = priors,
  warmup = 1000, iter = 4000, chains = 6, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  data = coronals_bi,
  file = here("data", "models", "mod_coronals_mv_bi_full")
)

# -----------------------------------------------------------------------------
