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
  mutate(., language_sum = if_else(language == "english", 1, -1)) %>%
  mutate(., f1_std = (f1_mp - mean(f1_mp)) / sd(f1_mp),
            f2_std = (f2_mp - mean(f2_mp)) / sd(f2_mp))

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




vowel_means <- coronals_vowels %>%
  group_by(language) %>%
  summarize(f1_mean = mean(f1_std), f1_sd = sd(f1_std),
            f2_mean = mean(f2_std), f2_sd = sd(f2_std))

coronals_vowels %>%
  ggplot(., aes(x = f2_std, y = f1_std, color = language)) +
    geom_point(alpha = 0.2) +
    stat_ellipse(type = "norm", show.legend = FALSE, geom = "polygon",
                 alpha = 0.05) +
    geom_errorbar(data = vowel_means, inherit.aes = F,
      aes(x = f2_mean, ymin = f1_mean - f1_sd, ymax = f1_mean + f1_sd),
      color = "grey10", size = 1, width = 0.15) +
    geom_errorbarh(data = vowel_means, inherit.aes = F,
      aes(xmin = f2_mean - f2_sd, xmax = f2_mean + f2_sd, y = f1_mean),
      color = "grey10", size = 1, width = 0.15) +
    geom_point(data = vowel_means,
               aes(x = f2_mean, y = f1_mean, fill = language),
               color = "black", size = 5, pch = 21, stroke = 1) +
    scale_y_reverse() +
    scale_x_reverse(position = "top") +
    scale_color_manual(name = NULL, values = my_colors) +
    scale_fill_manual(name = NULL, values = my_colors) +
    labs(y = "F1 (std)", x = "F2 (std)") +
    theme_minimal(base_size = 16, base_family = "Times")

