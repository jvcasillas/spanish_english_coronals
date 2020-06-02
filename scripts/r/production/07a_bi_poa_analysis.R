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
            cog_std = (cog - mean(cog)) / sd(cog),
            sd_std = (sd - mean(sd)) / sd(sd),
            sk_std = (sk - mean(sk)) / sd(sk),
            #kt_std = (kt - mean(kt)) / sd(kt),
            f1_std = (f1_cent - mean(f1_cent)) / sd(f1_cent),
            f2_std = (f2_cent - mean(f2_cent)) / sd(f2_cent),
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
mod_poa_comp_vot_full <- brm(
  formula = vot_std ~ 1 + language_sum * poa_sum + f1_std + f2_std + rep_n +
    (1 + language_sum + f1_std + f2_std + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = poa_bi,
  file = here("data", "models", "mod_poa_comp_vot_full")
)


# RI
mod_poa_comp_ri_full <- brm(
  formula = ri_std ~ 1 + language_sum * poa_sum + f1_std + f2_std + rep_n +
    (1 + language_sum + f1_std + f2_std + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = poa_bi,
  file = here("data", "models", "mod_poa_comp_ri_full")
)


# Spectral moments

# COG
mod_poa_comp_cog_full <- brm(
  formula = cog_std ~ 1 + language_sum * poa_sum + f1_std + f2_std + rep_n +
    (1 + language_sum + f1_std + f2_std + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = poa_bi,
  file = here("data", "models", "mod_poa_comp_cog_full")
)


# SD
mod_poa_comp_sd_full <- brm(
  formula = sd_std ~ 1 + language_sum * poa_sum + f1_std + f2_std + rep_n +
    (1 + language_sum + f1_std + f2_std + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = poa_bi,
  file = here("data", "models", "mod_poa_comp_sd_full")
)


# Skewness
mod_poa_comp_sk_full <- brm(
  formula = sk_std ~ 1 + language_sum * poa_sum + f1_std + f2_std + rep_n +
    (1 + language_sum + f1_std + f2_std + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = poa_bi,
  file = here("data", "models", "mod_poa_comp_sk_full")
)


# Kurtosis
mod_poa_comp_kt_full <- brm(
  formula = kt_std ~ 1 + language_sum * poa_sum + f1_std + f2_std + rep_n +
    (1 + language_sum + f1_std + f2_std + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = filter(poa_bi, kt >= 0) %>%
         mutate(kt_log = log(kt), kt_std = (kt_log - mean(kt_log)) / sd(kt_log)),
  file = here("data", "models", "mod_poa_comp_kt_full")
)

# -----------------------------------------------------------------------------
