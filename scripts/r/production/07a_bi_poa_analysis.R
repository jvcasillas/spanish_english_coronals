# Analysis 3: bilinguals POA --------------------------------------------------
#
# Last update: 2022-03-16
#
# This script will fit two separate models:
# - VOT ~ language and phoneme
# - Spectral moments and RI
#    - (RI, COG, SD, SK, KT) ~ language and poa
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

poa_bi <-
  bind_rows(
    coronals %>%
      filter(group == "BIL", is.na(label), phon == "t"),
    bilabials) %>%
  mutate(across(c("cog", "kt", "sk"),
                .fns = list(std = ~smart_scale(.)))) %>%
  mutate(across(c("f1_cent", "f2_cent", "ri", "vot", "sd"),
                .fns = list(std = ~simple_scale(.)))) %>%
  mutate(poa_sum = if_else(phon == "t", 1, -1),
         language_sum = if_else(language == "english", 1, -1)) %>%
  filter(!is.na(ri))

# -----------------------------------------------------------------------------





# Fit models ------------------------------------------------------------------

# VOT model formula
vot_poa_model_formula <- bf(
  vot_std ~ 1 +
    language_sum * poa_sum +
    f1_cent_std + f2_cent_std + rep_n +
    (1 + poa_sum + f1_cent_std + f2_cent_std + rep_n | id) +
    (1 | item))

# Use formula and data to get possible priors
get_prior(
  formula = vot_poa_model_formula,
  family = gaussian(),
  data = poa_bi
  ) %>%
  as_tibble() %>%
  select(prior, class, coef, group) %>%
  as.data.frame()

# Set regularizing, weakly informative priors
priors_vot <- c(
  prior(normal(0, 1), class = "Intercept"),
  prior(normal(0, 1), class = "b"),
  prior(cauchy(0, 0.2), class = "sd"),
  prior(cauchy(0, 0.5), class = "sigma"),
  prior(lkj(2), class = "cor")
  )

# Fit VOT model
mod_poa_comp_vot_full <- brm(
  formula = vot_poa_model_formula,
  prior = priors_vot,
  warmup = 1000, iter = 2000, chains = 4, cores = 4,
  family = gaussian(),
  control = list(adapt_delta = 0.90),
  backend = "cmdstanr",
  data = poa_bi,
  file = here("data", "models", "mod_poa_comp_vot_full")
)


# Spectral moments model formula
mv_poa_model_formula <- bf(
  mvbind(ri_std, cog_std, sd_std, sk_std, kt_std) ~ 1 +
    language_sum * poa_sum +
    f1_cent_std + f2_cent_std + rep_n +
    (1 + poa_sum + f1_cent_std + f2_cent_std + rep_n |p| id) +
    (1 |q| item)
) + set_rescor(rescor = TRUE)

# Get priors
get_prior(
  formula = mv_poa_model_formula,
  family = gaussian(),
  data = poa_bi
  ) %>%
  as_tibble() %>%
  select(prior, class, coef, group) %>%
  as.data.frame()

# Set priors
priors_poa_spectral_moments <- c(
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
  prior(cauchy(0, 0.5), class = "sd", resp = "ristd"),
  prior(normal(0, 0.5), class = "sd", resp = "cogstd"),
  prior(normal(0, 0.5), class = "sd", resp = "sdstd"),
  prior(normal(0, 0.5), class = "sd", resp = "skstd"),
  prior(normal(0, 0.5), class = "sd", resp = "ktstd"),
  prior(cauchy(0, 0.5), class = "sigma", resp = "ristd"),
  prior(cauchy(0, 0.5), class = "sigma", resp = "cogstd"),
  prior(cauchy(0, 0.5), class = "sigma", resp = "sdstd"),
  prior(cauchy(0, 0.5), class = "sigma", resp = "skstd"),
  prior(cauchy(0, 0.5), class = "sigma", resp = "ktstd"),
  prior(lkj(2), class = "cor"),
  prior(lkj(2), class = "rescor")
  )

# Fit MV model
mod_poa_comp_mv_full <- brm(
  formula = mv_poa_model_formula,
  prior = priors_poa_spectral_moments,
  warmup = 1000, iter = 12000, chains = 4, cores = 4, thin = 10,
  family = gaussian(),
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  backend = "cmdstanr",
  data = poa_bi,
  file = here("data", "models", "mod_poa_comp_mv_full")
)

# -----------------------------------------------------------------------------
