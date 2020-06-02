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

coronals_vowels <- coronals %>%
  filter(., group %in% c("NEN", "NSP"), is.na(label)) %>%
  mutate(., language_sum = if_else(language == "english", 1, -1),
            phon_sum = if_else(phon == "d", 1, -1),
            f1_std = (f1_cent - mean(f1_cent)) / sd(f1_cent),
            f2_std = (f2_cent - mean(f2_cent)) / sd(f2_cent),
            mean_f1 = mean(f1_std), mean_f2 = mean(f2_std),
            euc_dist = sqrt((0 - 0)^2 + (f1_std - f2_std)^2))


# Use all available cores for parallel computing
options(mc.cores = parallel::detectCores())

# Regularizing, weakly informative priors
priors <- c(
  set_prior("normal(0, 2)", class = "Intercept"),
  set_prior("normal(0, 2)", class = "b")
)

# -----------------------------------------------------------------------------






# F1/F2 multivariate model  ---------------------------------------------------

mv_vowel_model <- bf(
  mvbind(f1_std, f2_std) ~ 1 + language_sum + phon_sum + rep_n +
        (1 + phon_sum + rep_n |p| id) +
        (1 + rep_n |q| item)
)

mod_f1f2_mv_mono_full <- brm(
  formula = mv_vowel_model,
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_vowels,
  file = here("data", "models", "mod_f1f2_mv_mono_full")
)

# -----------------------------------------------------------------------------
