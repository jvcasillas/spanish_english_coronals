# Analysis 3: bilinguals POA --------------------------------------------------
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
# - create subset (only bilinguals, exclude errors)
# - code variables (sum code group, phon, language, and stress)

poa_bi <- bind_rows(
  coronals %>%
    filter(., group == "BIL", is.na(label), phon == "t"),
  bilabials) %>%
  mutate(., vot_std = (vot - mean(vot)) / sd(vot),
            ri_std = (ri - mean(ri, na.rm = T)) / sd(ri, na.rm = T),
            cog_std = (cog - mean(cog, na.rm = T)) / sd(cog, na.rm = T),
            sd_std = (sd - mean(sd, na.rm = T)) / sd(sd, na.rm = T),
            sk_std = (sk - mean(sk, na.rm = T)) / sd(sk, na.rm = T),
            kt_std = (kt - mean(kt, na.rm = T)) / sd(kt, na.rm = T),
            language_sum = if_else(language == "english", 1, -1),
            group_sum = if_else(group == "BIL", 1, -1),
            stress_sum = if_else(stress == "stressed", 1, -1),
            poa_sum = if_else(phon == "t", 1, -1))



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
mod_vot_poa_comp_full <- brm(
  formula = vot_std ~ 1 + language_sum * poa_sum * stress_sum + rep_n +
    (1 + language_sum + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = poa_bi,
  file = here("data", "models", "mod_vot_poa_comp_full")
)


# RI
mod_ri_poa_comp_full <- brm(
  formula = ri_std ~ 1 + language_sum * poa_sum * stress_sum + rep_n +
    (1 + language_sum + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = poa_bi,
  file = here("data", "models", "mod_ri_poa_comp_full")
)


# Spectral moments

# COG
mod_cog_poa_comp_full <- brm(
  formula = cog_std ~ 1 + language_sum * poa_sum * stress_sum + rep_n +
    (1 + language_sum + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = poa_bi,
  file = here("data", "models", "mod_cog_poa_comp_full")
)


# SD
mod_sd_poa_comp_full <- brm(
  formula = sd_std ~ 1 + language_sum * poa_sum * stress_sum + rep_n +
    (1 + language_sum + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = poa_bi,
  file = here("data", "models", "mod_sd_poa_comp_full")
)


# Skewness
mod_sk_poa_comp_full <- brm(
  formula = sk_std ~ 1 + language_sum * poa_sum * stress_sum + rep_n +
    (1 + language_sum + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = poa_bi,
  file = here("data", "models", "mod_sk_poa_comp_full")
)


# Kurtosis
mod_kt_poa_comp_full <- brm(
  formula = kt_std ~ 1 + language_sum * poa_sum * stress_sum + rep_n +
    (1 + language_sum + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = poa_bi,
  file = here("data", "models", "mod_kt_poa_comp_full")
)

# -----------------------------------------------------------------------------





