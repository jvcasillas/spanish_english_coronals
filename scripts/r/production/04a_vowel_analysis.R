# Analysis 2: Vowels ----------------------------------------------------------
#
# Last update: 2022-03-16
#
# - Multivariate bayesian regression
# - F1 and F2 ~ language (spanish, english) and phon (d, t)
# - Goal is compare the production of /a/ from both languages to see if
#   they are different
# - If they are acoustically different from each other then one would question
#   how much coarticulatory differences may be contribution to spectral
#   measurements of the preceding burst
#
# -----------------------------------------------------------------------------




# Load libraries, helpers and data --------------------------------------------

source(here::here("scripts", "r", "production", "03_load_data.R"))

# -----------------------------------------------------------------------------




# General model setup ---------------------------------------------------------
#
# - create subset (only mono, exclude errors)
# - code variables (sum code group, phon, language, and stress)
# - standardize F1 and F2 N(mu = 0, sigma = 1)
# - establish a ROPE of 0.05 (suggestion from Kruchske 2018 for regression
#   with standardized variables)

coronals_vowels <- coronals %>%
  filter(group %in% c("NEN", "NSP"), is.na(label)) %>%
  mutate(language_sum = if_else(language == "english", 1, -1),
         phon_sum = if_else(phon == "d", 1, -1),
         f1_std = (f1_cent - mean(f1_cent)) / sd(f1_cent),
         f2_std = (f2_cent - mean(f2_cent)) / sd(f2_cent))


# Use all available cores for parallel computing
options(mc.cores = parallel::detectCores())

# -----------------------------------------------------------------------------






# F1/F2 multivariate model  ---------------------------------------------------

# Model formula
mv_vowel_model <- bf(
  mvbind(f1_std, f2_std) ~ 1 + language_sum * phon_sum + rep_n +
        (1 + phon_sum + rep_n |p| id) +
        (1 + rep_n |q| item)
) + set_rescor(TRUE)

# Get prior
get_prior(
  formula = mv_vowel_model,
  family = gaussian(),
  data = coronals_vowels
) %>%
  as_tibble() %>%
  select(prior, class, coef, group, resp) %>%
  as.data.frame()

# Regularizing, weakly informative priors
priors <- c(
  prior(normal(0, 1), class = "Intercept", resp = "f1std"),
  prior(normal(0, 1), class = "Intercept", resp = "f2std"),
  prior(normal(0, 1), class = "b", resp = "f1std"),
  prior(normal(0, 1), class = "b", resp = "f2std"),
  prior(cauchy(0, 0.2), class = "sd", resp = "f1std"),
  prior(cauchy(0, 0.2), class = "sd", resp = "f2std"),
  prior(cauchy(0, 0.2), class = "sigma", resp = "f1std"),
  prior(cauchy(0, 0.2), class = "sigma", resp = "f2std"),
  prior(lkj(2), class = "cor"),
  prior(lkj(2), class = "rescor")
)

# Fit model
mod_f1f2_mv_mono_full <- brm(
  formula = mv_vowel_model,
  prior = priors,
  warmup = 1000, iter = 10000, chains = 4, cores = 4, thin = 10,
  family = gaussian(),
  data = coronals_vowels,
  file = here("data", "models", "mod_f1f2_mv_mono_full")
)

# -----------------------------------------------------------------------------
