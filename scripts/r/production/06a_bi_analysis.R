# Analysis 2: bilinguals ------------------------------------------------------
#
# Last update: 2022-03-16
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
         language_sum = if_else(language == "english", 1, -1)) %>%
  filter(!is.na(ri_std)) # Remove 2 rows with NA for ri_std

# Use all available cores for parallel computing
options(mc.cores = parallel::detectCores())

# -----------------------------------------------------------------------------





# Fit models ------------------------------------------------------------------

# VOT
vot_bi_model_formula <- bf(
  vot_std ~ 1 +
    language_sum * phon_sum +
    f1_cent_std + f2_cent_std + rep_n +
    (1 + phon_sum + f1_cent_std + f2_cent_std + rep_n | id) +
    (1 | item))

# Get prior
get_prior(
  formula = vot_bi_model_formula,
  family = gaussian(),
  data = coronals_bi
  ) %>%
  as_tibble() %>%
  select(prior, class, coef, group) %>%
  as.data.frame()

# Regularizing, weakly informative priors
priors_vot <- c(
  prior(normal(0, 1), class = "Intercept"),
  prior(normal(0, 1), class = "b"),
  prior(cauchy(0, 0.2), class = "sd"),
  prior(cauchy(0, 0.5), class = "sigma"),
  prior(lkj(2), class = "cor")
  )

# Fit VOT model
mod_coronals_vot_bi_full <- brm(
  formula = vot_bi_model_formula,
  prior = priors_vot,
  warmup = 1000, iter = 2000, chains = 4, cores = 4,
  family = gaussian(),
  control = list(adapt_delta = 0.9),
  backend = "cmdstanr",
  data = coronals_bi,
  file = here("data", "models", "mod_coronals_vot_bi_full")
)


# Spectral moments
mv_bi_model_formula <- bf(
  mvbind(ri_std, cog_std, sd_std, sk_std, kt_std) ~ 1 +
    language_sum * phon_sum +
    f1_cent_std + f2_cent_std + rep_n +
    (1 + phon_sum + f1_cent_std + f2_cent_std + rep_n |p| id) +
    (1 |q| item)
) + set_rescor(rescor = TRUE)

# Get prior
get_prior(
  formula = mv_bi_model_formula,
  family = gaussian(),
  data = coronals_bi
  ) %>%
  as_tibble() %>%
  select(prior, class, coef, group) %>%
  as.data.frame()

# Set priors
priors_bi_spectral_moments <- c(
  prior(normal(0, 1), class = "Intercept", resp = "ristd"),
  prior(normal(0, 1), class = "Intercept", resp = "cogstd"),
  prior(normal(0, 1), class = "Intercept", resp = "sdstd"),
  prior(normal(0, 1), class = "Intercept", resp = "skstd"),
  prior(normal(0, 1), class = "Intercept", resp = "ktstd"),
  prior(normal(0, 1), class = "b", resp = "ristd"),
  prior(normal(0, 1), class = "b", resp = "cogstd"),
  prior(normal(0, 1), class = "b", resp = "sdstd"),
  prior(normal(0, 1), class = "b", resp = "skstd"),
  prior(normal(0, 1), class = "b", resp = "ktstd"),
  prior(cauchy(0, 0.2), class = "sd", resp = "ristd"),
  prior(normal(0, 0.2), class = "sd", resp = "cogstd"),
  prior(normal(0, 0.2), class = "sd", resp = "sdstd"),
  prior(normal(0, 0.2), class = "sd", resp = "skstd"),
  prior(normal(0, 0.2), class = "sd", resp = "ktstd"),
  prior(cauchy(0, 0.5), class = "sigma", resp = "ristd"),
  prior(cauchy(0, 0.5), class = "sigma", resp = "cogstd"),
  prior(cauchy(0, 0.5), class = "sigma", resp = "sdstd"),
  prior(cauchy(0, 0.5), class = "sigma", resp = "skstd"),
  prior(cauchy(0, 0.5), class = "sigma", resp = "ktstd"),
  prior(lkj(2), class = "cor"),
  prior(lkj(2), class = "rescor")
  )

# Fit MV model
mod_coronals_mv_bi_full <- brm(
  formula = mv_bi_model_formula,
  prior = priors_bi_spectral_moments,
  warmup = 1000, iter = 2000, chains = 4, cores = 4,
  family = gaussian(),
  backend = "cmdstanr",
  data = coronals_bi,
  file = here("data", "models", "mod_coronals_mv_bi_full")
)

# -----------------------------------------------------------------------------
