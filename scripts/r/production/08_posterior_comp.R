# Post-hoc comparisons --------------------------------------------------------
#
# Questions of interest:
# - Can any of the metrics distinguish between english and spanish for
#   coronal stops?
# - We want to show that X metric can distinguish between dental and alveolars
#   (english vs. spanish), but the same between-lang difference should not
#   exist for bilabials
#
# -----------------------------------------------------------------------------


# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "04a_vowel_analysis.R"))
source(here::here("scripts", "r", "production", "05a_mono_analysis.R"))
source(here::here("scripts", "r", "production", "06a_bi_analysis.R"))
source(here::here("scripts", "r", "production", "07a_bi_poa_analysis.R"))

posterior_vowels <-
  readRDS(here("data", "models", "posterior_vowels.rds"))

posterior_mono <-
  readRDS(here("data", "models", "posterior_mono.rds"))

posterior_bi <-
  readRDS(here("data", "models", "posterior_bi.rds"))

posterior_poa <-
  readRDS(here("data", "models", "posterior_poa_comp.rds"))

posterior_vowels_adj <-
  readRDS(here("data", "models", "posterior_vowels_adj.rds"))

posterior_mono_adj <-
  readRDS(here("data", "models", "posterior_mono_adj.rds")) %>%
    mutate(metric = fct_relevel(metric, "vot", "ri"))

posterior_bi_adj <-
  readRDS(here("data", "models", "posterior_bi_adj.rds")) %>%
    mutate(metric = fct_relevel(metric, "vot", "ri"))

posterior_poa_adj <-
  readRDS(here("data", "models", "posterior_poa_comp_adj.rds")) %>%
    mutate(place = if_else(place == "t", "coronal", "bilabial"),
           metric = fct_relevel(metric, "vot", "ri"))

# -----------------------------------------------------------------------------





# Comparison of short-lag stops in Engish (d) and Spanish (t) -----------------

d_t_post <- posterior_mono_adj %>%
  filter(metric == "vot", language == "english" & phon == "d" |
                          language == "spanish" & phon == "t") %>%
  select(-metric, -language) %>%
  group_by(phon) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(phon, val) %>%
  mutate(diff = d - t)

plot_posterior(posterior = d_t_post, parameter = diff, rope = c(-0.1, 0.1),
               hdi = 0.95, xpos = -0.45, ypos = c(1.2, 1, 0.8))

# -----------------------------------------------------------------------------








# Comparison of English d and Spanish t: COG ----------------------------------

poa_cog_post <- posterior_poa_adj %>%
  filter(metric == "cog", place == "coronal") %>%
  select(-metric, -place) %>%
  group_by(language) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(language, val) %>%
  mutate(diff = english - spanish)

hold <- plot_posterior(
  posterior = poa_cog_post, parameter = diff, rope = c(-0.1, 0.1), color = 2,
  hdi = 0.95, xpos = 0.7, ypos = c(2, 1.75, 1.5))

hold[["plot_env"]][["summary_vals"]]

# -----------------------------------------------------------------------------


