# Analysis 2: Vowels ----------------------------------------------------------
#
# - F1 and F2 ~ language
# -
#
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
  mutate(., #f1_std,
            #f2_std,
            language_sum = if_else(language == "english", 1, -1))

# Use all available cores for parallel computing
options(mc.cores = parallel::detectCores())

# Regularizing, weakly informative priors
priors <- c(
  set_prior("normal(0, 5)", class = "Intercept"),
  set_prior("normal(0, 5)", class = "b")
)

# -----------------------------------------------------------------------------





# Fit models ------------------------------------------------------------------

# F1
mod_f1_mono_full <- brm(
  formula = f1 ~ 1 + language_sum + rep_n +
    (1 + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_mono,
  file = here("data", "models", "mod_f1_mono_full")
)


# F2
mod_f2_mono_full <- brm(
  formula = f2 ~ 1 + language_sum + rep_n +
    (1 + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_mono,
  file = here("data", "models", "mod_f2_mono_full")
)

# -----------------------------------------------------------------------------
