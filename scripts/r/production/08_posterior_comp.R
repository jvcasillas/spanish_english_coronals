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


d_t_post %>%
  ggplot(., aes(x = diff)) +
    geom_density(color = "white", fill = my_colors[2], alpha = 0.5) +
    geom_segment(x = hdi(d_t_post$diff, ci = 0.95)$CI_low,
                 xend = hdi(d_t_post$diff, ci = 0.95)$CI_high,
                 y = 0.1, yend = 0.1, size = 1.5) +
    geom_segment(x = -0.1, xend = 0.1, y = 0, yend = 0, size = 1,
                 color = my_colors[1]) +
    geom_point(data = tibble(diff = mean(d_t_post$diff), density = 0.1),
               aes(x = diff, y = density), size = 6, pch = 21, fill = "white") +
    geom_text(
      data = tibble(
        diff = -0.4, density = 1,
        text = glue::glue("MPE = ", round(p_direction(d_t_post$diff), 3))),
      aes(x = diff, y = density, label = text)) +
    theme_minimal()



mean(d_t_post$diff)
mean(d_t_post$diff > 0)
hdi(d_t_post$diff, ci = 0.95)
rope(d_t_post$diff, range = c(-0.1, 0.1), ci = 0.95)
equivalence_test(d_t_post$diff, range = c(-0.1, 0.1), ci = 0.95)
p_direction(d_t_post$diff)
p_direction(mod_coronals_cog_bi_full)










plot_posterior(posterior = d_t_post, parameter = diff, rope = c(-0.1, 0.1),
               hdi = 0.95, labs = "hi", xpos = -0.45, ypos = c(1.2, 1, 0.8))

plot_posterior(posterior_samples(mod_coronals_vot_mono_full), rope = c(-0.1, 0.1),
               `b_group_sum:phon_sum`, xpos = -0.05, ypos = c(2, 1.5, 1))


