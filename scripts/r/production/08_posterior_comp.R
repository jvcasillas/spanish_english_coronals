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





# Comparison of short-lag stops: monolinguals ---------------------------------

d_t_post_mono <- posterior_mono_adj %>%
  filter(metric == "vot", language == "english" & phon == "d" |
                          language == "spanish" & phon == "t") %>%
  select(-metric, -language) %>%
  group_by(phon) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(phon, val) %>%
  mutate(diff = d - t)

d_t_mono_comp <- plot_posterior(
  posterior = d_t_post_mono, parameter = diff, rope = c(-0.1, 0.1),
  hdi = 0.95, xpos = -0.45, ypos = c(1.2, 1, 0.8))

d_t_mono_comp[["plot_env"]][["summary_vals"]]

# -----------------------------------------------------------------------------




# Comparison of short-lag stops: bilinguals -----------------------------------

d_t_post_bi <- posterior_bi_adj %>%
  filter(metric == "vot", language == "english" & phon == "d" |
                          language == "spanish" & phon == "t") %>%
  select(-metric, -language) %>%
  group_by(phon) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(phon, val) %>%
  mutate(diff = d - t)

d_t_bi_comp <- plot_posterior(
  posterior = d_t_post_bi, parameter = diff, rope = c(-0.1, 0.1), hdi = 0.95,
  xpos = -0.45, ypos = c(1.2, 1, 0.8))

# -----------------------------------------------------------------------------








# Comparison of POA - en vs. sp: ----------------------------------------------

# VOT
poa_coronal_vot_post <- posterior_poa_adj %>%
  filter(metric == "vot", place == "coronal") %>%
  select(-metric, -place) %>%
  group_by(language) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(language, val) %>%
  mutate(diff = english - spanish)

poa_bilabial_vot_post <- posterior_poa_adj %>%
  filter(metric == "vot", place == "bilabial") %>%
  select(-metric, -place) %>%
  group_by(language) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(language, val) %>%
  mutate(diff = english - spanish)

poa_coronal_vot_comp <- plot_posterior(
  posterior = poa_coronal_vot_post, parameter = diff, rope = c(-0.1, 0.1),
  color = 2, hdi = 0.95, xpos = 1.0, ypos = c(2, 1.75, 1.5))

poa_bilabial_vot_comp <- plot_posterior(
  posterior = poa_bilabial_vot_post, parameter = diff, rope = c(-0.1, 0.1),
  color = 2, hdi = 0.95, xpos = 0.85, ypos = c(1.2, 1.1, 1))

# RI
poa_coronal_ri_post <- posterior_poa_adj %>%
  filter(metric == "ri", place == "coronal") %>%
  select(-metric, -place) %>%
  group_by(language) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(language, val) %>%
  mutate(diff = english - spanish)

poa_bilabial_ri_post <- posterior_poa_adj %>%
  filter(metric == "ri", place == "bilabial") %>%
  select(-metric, -place) %>%
  group_by(language) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(language, val) %>%
  mutate(diff = english - spanish)

poa_coronal_ri_comp <- plot_posterior(
  posterior = poa_coronal_ri_post, parameter = diff, rope = c(-0.1, 0.1),
  color = 2, hdi = 0.95, xpos = 0.5, ypos = c(2, 1.75, 1.5))

poa_bilabial_ri_comp <- plot_posterior(
  posterior = poa_bilabial_ri_post, parameter = diff, rope = c(-0.1, 0.1),
  color = 2, hdi = 0.95, xpos = -0.3, ypos = c(1.2, 1, 0.8))

# COG
poa_coronal_cog_post <- posterior_poa_adj %>%
  filter(metric == "cog", place == "coronal") %>%
  select(-metric, -place) %>%
  group_by(language) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(language, val) %>%
  mutate(diff = english - spanish)

poa_bilabial_cog_post <- posterior_poa_adj %>%
  filter(metric == "cog", place == "bilabial") %>%
  select(-metric, -place) %>%
  group_by(language) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(language, val) %>%
  mutate(diff = english - spanish)


poa_coronal_cog_comp <- plot_posterior(
  posterior = poa_coronal_cog_post, parameter = diff, rope = c(-0.1, 0.1),
  color = 2, hdi = 0.95, xpos = 0.7, ypos = c(2, 1.75, 1.5))

poa_bilabial_cog_comp <- plot_posterior(
  posterior = poa_bilabial_cog_post, parameter = diff, rope = c(-0.1, 0.1),
  color = 2, hdi = 0.95, xpos = -0.6, ypos = c(1.2, 1.1, 1))

# KT
poa_coronal_kt_post <- posterior_poa_adj %>%
  filter(metric == "kt", place == "coronal") %>%
  select(-metric, -place) %>%
  group_by(language) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(language, val) %>%
  mutate(diff = english - spanish)

poa_bilabial_kt_post <- posterior_poa_adj %>%
  filter(metric == "kt", place == "bilabial") %>%
  select(-metric, -place) %>%
  group_by(language) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(language, val) %>%
  mutate(diff = english - spanish)


poa_coronal_kt_comp <- plot_posterior(
  posterior = poa_coronal_kt_post, parameter = diff, rope = c(-0.1, 0.1),
  color = 2, hdi = 0.95, xpos = -1.15, ypos = c(2, 1.75, 1.5))

poa_bilabial_kt_comp <- plot_posterior(
  posterior = poa_bilabial_kt_post, parameter = diff, rope = c(-0.1, 0.1),
  color = 2, hdi = 0.95, xpos = -0.5, ypos = c(1.4, 1.2, 1))

# SD
poa_coronal_sd_post <- posterior_poa_adj %>%
  filter(metric == "sd", place == "coronal") %>%
  select(-metric, -place) %>%
  group_by(language) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(language, val) %>%
  mutate(diff = english - spanish)

poa_bilabial_sd_post <- posterior_poa_adj %>%
  filter(metric == "sd", place == "bilabial") %>%
  select(-metric, -place) %>%
  group_by(language) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(language, val) %>%
  mutate(diff = english - spanish)


poa_coronal_sd_comp <- plot_posterior(
  posterior = poa_coronal_sd_post, parameter = diff, rope = c(-0.1, 0.1),
  color = 2, hdi = 0.95, xpos = 0.5, ypos = c(2, 1.75, 1.5))

poa_bilabial_sd_comp <- plot_posterior(
  posterior = poa_bilabial_sd_post, parameter = diff, rope = c(-0.1, 0.1),
  color = 2, hdi = 0.95, xpos = -0.5, ypos = c(1.4, 1.2, 1))

# SK
poa_coronal_sk_post <- posterior_poa_adj %>%
  filter(metric == "sk", place == "coronal") %>%
  select(-metric, -place) %>%
  group_by(language) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(language, val) %>%
  mutate(diff = english - spanish)

poa_bilabial_sk_post <- posterior_poa_adj %>%
  filter(metric == "sk", place == "bilabial") %>%
  select(-metric, -place) %>%
  group_by(language) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(language, val) %>%
  mutate(diff = english - spanish)

poa_coronal_sk_comp <- plot_posterior(
  posterior = poa_coronal_sk_post, parameter = diff, rope = c(-0.1, 0.1),
  color = 2, hdi = 0.95, xpos = -0.85, ypos = c(2, 1.75, 1.5))

poa_bilabial_sk_comp <- plot_posterior(
  posterior = poa_bilabial_sk_post, parameter = diff, rope = c(-0.1, 0.1),
  color = 2, hdi = 0.95, xpos = -0.6, ypos = c(1.4, 1.2, 1))

# -----------------------------------------------------------------------------







# Combine comparison summaries ------------------------------------------------

bind_rows(
  d_t_mono_comp[["plot_env"]][["summary_vals"]] %>%
    select(-density) %>%
    mutate(comp = "d_t_mono"),
  d_t_bi_comp[["plot_env"]][["summary_vals"]] %>%
    select(-density) %>%
    mutate(comp = "d_t_bi"),
  poa_coronal_vot_comp[["plot_env"]][["summary_vals"]] %>%
    select(-density) %>%
    mutate(comp = "poa_coronal_vot"),
  poa_bilabial_vot_comp[["plot_env"]][["summary_vals"]] %>%
    select(-density) %>%
    mutate(comp = "poa_bilabial_vot"),
  poa_coronal_ri_comp[["plot_env"]][["summary_vals"]] %>%
    select(-density) %>%
    mutate(comp = "poa_coronal_ri"),
  poa_bilabial_ri_comp[["plot_env"]][["summary_vals"]] %>%
    select(-density) %>%
    mutate(comp = "poa_bilabial_ri"),
  poa_coronal_cog_comp[["plot_env"]][["summary_vals"]] %>%
    select(-density) %>%
    mutate(comp = "poa_coronal_cog"),
  poa_bilabial_cog_comp[["plot_env"]][["summary_vals"]] %>%
    select(-density) %>%
    mutate(comp = "poa_bilabial_cog"),
  poa_coronal_kt_comp[["plot_env"]][["summary_vals"]] %>%
    select(-density) %>%
    mutate(comp = "poa_coronal_kt"),
  poa_bilabial_kt_comp[["plot_env"]][["summary_vals"]] %>%
    select(-density) %>%
    mutate(comp = "poa_bilabial_kt"),
  poa_coronal_sd_comp[["plot_env"]][["summary_vals"]] %>%
    select(-density) %>%
    mutate(comp = "poa_coronal_sd"),
  poa_bilabial_sd_comp[["plot_env"]][["summary_vals"]] %>%
    select(-density) %>%
    mutate(comp = "poa_bilabial_sd"),
  poa_coronal_sk_comp[["plot_env"]][["summary_vals"]] %>%
    select(-density) %>%
    mutate(comp = "poa_coronal_sk"),
  poa_bilabial_sk_comp[["plot_env"]][["summary_vals"]] %>%
    select(-density) %>%
    mutate(comp = "poa_bilabial_sk")
  ) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  saveRDS(here("data", "models", "post_hoc_analyses.rds"))

# -----------------------------------------------------------------------------

