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
  mutate(., vot_std = (vot - mean(vot)) / sd(vot),
            ri_std = (ri - mean(ri, na.rm = T)) / sd(ri, na.rm = T),
            cog_std = (cog - mean(cog, na.rm = T)) / sd(cog, na.rm = T),
            sd_std = (sd - mean(sd, na.rm = T)) / sd(sd, na.rm = T),
            sk_std = (sk - mean(sk, na.rm = T)) / sd(sk, na.rm = T),
            #kt_std = (kt - mean(kt, na.rm = T)) / sd(kt, na.rm = T),
            f1_std = (f1_cent - mean(f1_cent)) / sd(f1_cent),
            f2_std = (f2_cent - mean(f2_cent)) / sd(f2_cent),
            phon_sum = if_else(phon == "d", 1, -1),
            group_sum = if_else(group == "NEN", 1, -1),
            language_sum = if_else(language == "english", 1, -1),
            stress_sum = if_else(stress == "stressed", 1, -1))


coronals_mono %>%
  select(group, language, f1_cent, f2_cent, vot, ri, cog, sd, sk, kt) %>%
  mutate(across(c("cog", "kt", "sk"),
                .fns = list(std = ~smart_scale(.)))) %>%
  mutate(across(c("f1_cent", "f2_cent", "ri", "vot", "sd"),
                .fns = list(std = ~simple_scale(.)))) %>%
  pivot_longer(
    cols = c("f1_cent", "f2_cent", "vot", "ri", "cog", "sd", "sk", "kt",
             "f1_cent_std", "f2_cent_std", "vot_std", "ri_std", "cog_std",
             "sd_std", "sk_std", "kt_std"),
    values_to = "val") %>%
  mutate(type = str_detect(name, "_std"),
         name = fct_relevel(
           name, "f1_cent", "f2_cent", "vot", "cog", "kt", "sk", "ri", "sd",
           "f1_cent_std", "f2_cent_std", "vot_std", "cog_std", "kt_std",
           "sk_std", "ri_std")) %>%
  ggplot(., aes(x = val, color = language)) +
  facet_wrap(. ~ name, scales = "free", nrow = 2, ncol = 8) +
  geom_histogram() +
  theme_bw(base_size = 6)











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
mod_coronals_vot_mono_full <- brm(
  formula = vot_std ~ 1 + group_sum * phon_sum + f1_std + f2_std + rep_n +
    (1 + phon_sum + f1_std + f2_std + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_mono,
  file = here("data", "models", "mod_coronals_vot_mono_full")
)


# RI
mod_coronals_ri_mono_full <- brm(
  formula = ri_std ~ 1 + group_sum * phon_sum + f1_std + f2_std + rep_n +
    (1 + phon_sum + f1_std + f2_std + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_mono,
  file = here("data", "models", "mod_coronals_ri_mono_full")
)


# Spectral moments

# COG
mod_coronals_cog_mono_full <- brm(
  formula = cog_std ~ 1 + group_sum * phon_sum + f1_std + f2_std + rep_n +
    (1 + phon_sum + f1_std + f2_std + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_mono,
  file = here("data", "models", "mod_coronals_cog_mono_full")
)


# SD
mod_coronals_sd_mono_full <- brm(
  formula = sd_std ~ 1 + group_sum * phon_sum + f1_std + f2_std + rep_n +
    (1 + phon_sum + f1_std + f2_std + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_mono,
  file = here("data", "models", "mod_coronals_sd_mono_full")
)


# Skewness
mod_coronals_sk_mono_full <- brm(
  formula = sk_std ~ 1 + group_sum * phon_sum + f1_std + f2_std + rep_n +
    (1 + phon_sum + f1_std + f2_std + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_mono,
  file = here("data", "models", "mod_coronals_sk_mono_full")
)


# Kurtosis
mod_coronals_kt_mono_full <- brm(
  formula = kt_std ~ 1 + group_sum * phon_sum + f1_std + f2_std + rep_n +
    (1 + phon_sum + f1_std + f2_std + rep_n | id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 4, cores = parallel::detectCores(),
  family = gaussian(),
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = filter(coronals_mono, kt >= 0) %>%
         mutate(kt_log = log(kt), kt_std = (kt_log - mean(kt_log)) / sd(kt_log)),
  file = here("data", "models", "mod_coronals_kt_mono_full")
)

# -----------------------------------------------------------------------------


mod_coronals_mv_mono_full <- brm(
  formula = mvbind(ri_std, cog_std, sd_std, sk_std) ~ 1 +
    group_sum + phon_sum + rep_n +
    (1 + phon_sum + f1_std + f2_std + rep_n |2| id) +
    (1 + rep_n | item),
  prior = priors,
  warmup = 1000, iter = 4000, chains = 6, cores = parallel::detectCores(),
  family = gaussian(),
  #control = list(adapt_delta = 0.999, max_treedepth = 15),
  data = coronals_mono,
  file = here("data", "models", "mod_coronals_mv_mono_full")
)




