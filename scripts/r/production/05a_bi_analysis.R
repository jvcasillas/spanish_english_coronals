# Analysis 2: bilinguals ------------------------------------------------------
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
# - code variables (sum code group, phon, and language)

coronals_bi <- coronals %>%
  filter(., group == "BIL", is.na(label)) %>%
  mutate(., vot_std = (vot - mean(vot)) / sd(vot),
            ri_std = (ri - mean(ri, na.rm = T)) / sd(ri, na.rm = T),
            cog_std = (cog - mean(cog, na.rm = T)) / sd(cog, na.rm = T),
            sd_std = (sd - mean(sd, na.rm = T)) / sd(sd, na.rm = T),
            sk_std = (sk - mean(sk, na.rm = T)) / sd(sk, na.rm = T),
            kt_std = (kt - mean(kt, na.rm = T)) / sd(kt, na.rm = T),
            phon_sum = if_else(phon == "d", 1, -1),
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

# VOT
mod_coronals_vot_bi_full <- brm(
  formula = vot_std ~ 1 + language_sum * phon_sum + rep_n +
    (1 + language_sum * phon_sum + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4,
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_bi,
  file = here("data", "models", "mod_coronals_vot_bi_full")
)


# RI
mod_coronals_ri_bi_full <- brm(
  formula = ri_std ~ 1 + language_sum * phon_sum + rep_n +
    (1 + language_sum * phon_sum + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4,
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_bi,
  file = here("data", "models", "mod_coronals_ri_bi_full")
)


# Spectral moments

# COG
mod_coronals_cog_bi_full <- brm(
  formula = cog_std ~ 1 + language_sum * phon_sum + rep_n +
    (1 + language_sum * phon_sum + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4,
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_bi,
  file = here("data", "models", "mod_coronals_cog_bi_full")
)


# SD
mod_coronals_sd_bi_full <- brm(
  formula = sd_std ~ 1 + language_sum * phon_sum + rep_n +
    (1 + language_sum * phon_sum + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4,
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_bi,
  file = here("data", "models", "mod_coronals_sd_bi_full")
)


# Skewness
mod_coronals_sk_bi_full <- brm(
  formula = sk_std ~ 1 + language_sum * phon_sum + rep_n +
    (1 + language_sum * phon_sum + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4,
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_bi,
  file = here("data", "models", "mod_coronals_sk_bi_full")
)


# Kurtosis
mod_coronals_kt_bi_full <- brm(
  formula = kt_std ~ 1 + language_sum * phon_sum + rep_n +
    (1 + language_sum * phon_sum + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4,
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_bi,
  file = here("data", "models", "mod_coronals_kt_bi_full")
)

# -----------------------------------------------------------------------------







# Posterior prep --------------------------------------------------------------

post_samples_bi_vot <- posterior_samples(mod_coronals_vot_bi_full) %>%
  as_tibble(.) %>%
  select(starts_with("b_"))

post_samples_bi_ri <- posterior_samples(mod_coronals_ri_bi_full) %>%
  as_tibble(.) %>%
  select(starts_with("b_"))

post_samples_bi_cog <- posterior_samples(mod_coronals_cog_bi_full) %>%
  as_tibble(.) %>%
  select(starts_with("b_"))





# -----------------------------------------------------------------------------
