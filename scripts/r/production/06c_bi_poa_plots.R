# Plots: bilinguals poa -------------------------------------------------------
#
# -----------------------------------------------------------------------------



# Average over items and reps
item_means <- poa_bi %>%
  select(id, language_sum, group_sum, poa_sum, stress_sum, contains("_std")) %>%
  gather(metric, val, -language_sum, -group_sum, -poa_sum, -stress_sum, -id) %>%
  group_by(id, language_sum, group_sum, poa_sum, stress_sum, metric) %>%
  summarize(val = mean(val))

# Plot raw data
poa_bi %>%
  select(language_sum, group_sum, poa_sum, stress_sum, contains("_std")) %>%
  gather(metric, val, -language_sum, -group_sum, -poa_sum, -stress_sum) %>%
  ggplot(., aes(x = factor(language_sum), y = val, shape = factor(stress_sum))) +
    facet_wrap(~ metric, scales = "free_x") +
    geom_beeswarm(data = item_means,
                  aes(color = factor(poa_sum), shape = factor(stress_sum)),
                  dodge.width = 0.5, alpha = 0.3) +
    stat_summary(fun.data = mean_cl_boot,
                 aes(color = factor(poa_sum), shape = factor(stress_sum)),
                 geom = "pointrange", position = position_dodge(0.5),
                 size = 1) +
    scale_color_brewer(palette = "Dark2", name = NULL,
                       labels = c("Coronal", "Bilabial")) +
    scale_shape_manual(name = NULL, values = c(21, 24),
                       breaks = c(1, -1), labels = c("Stressed", "Unstressed")) +
    scale_x_discrete(labels = c("Spanish", "English")) +
    labs(y = "Metric (std. units)", x = "Language") +
    coord_flip() +
    theme_bw()




# STOPED HERE




# Add fitted draws from each model to raw data

bi_grid <- coronals_bi %>%
    data_grid(id, item, phon_sum, language_sum, rep_n)

if(F) {

fits_bi <-
  bind_rows(
    bi_grid %>%
      add_fitted_draws(mod_coronals_vot_bi_full, n = 10) %>%
      mutate(metric = "vot_std"),
    bi_grid %>%
      add_fitted_draws(mod_coronals_ri_bi_full, n = 10) %>%
      mutate(metric = "ri_std"),
    bi_grid %>%
      add_fitted_draws(mod_coronals_cog_bi_full, n = 10) %>%
      mutate(metric = "cog_std"),
    bi_grid %>%
      add_fitted_draws(mod_coronals_sd_bi_full, n = 10) %>%
      mutate(metric = "sd_std"),
    bi_grid %>%
      add_fitted_draws(mod_coronals_sk_bi_full, n = 10) %>%
      mutate(metric = "sk_std"),
    bi_grid %>%
      add_fitted_draws(mod_coronals_kt_bi_full, n = 10) %>%
      mutate(metric = "kt_std")) %>%
  write_csv(., here("data", "tidy", "fits_bi.csv"))

}

# Make plot
#mono_vot <-
  coronals_bi %>%
  select(id, item, language_sum, phon_sum, contains("_std")) %>%
  gather(metric, val, -language_sum, -phon_sum, -id, -item) %>%
  ggplot(aes(x = language_sum, y = val, fill = factor(phon_sum))) +
  facet_wrap(~ metric) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_beeswarm(dodge.width = 2, alpha = 0.1) +
  stat_pointinterval(data = fits_bi, aes(y = .value), .width = c(.80, .95),
                     position = position_dodge(2)) +
  stat_summary(data = fits_bi, aes(y = .value, shape = factor(phon_sum)),
               fun.y = median, geom = "point", position = position_dodge(2),
               size = 5) +
  scale_fill_brewer(palette = "Set1", guide = F) +
  scale_x_continuous(breaks = c(-1, 1), labels = c("Spanish", "English")) +
  scale_shape_manual(values = c(21, 23), labels = c("/t/", "/d/"), name = "") +
  labs(y = "Std. units", x = "") +
  coord_cartesian(ylim = c(-2, 2)) +
  theme_minimal(base_family = "Times", base_size = 16)

