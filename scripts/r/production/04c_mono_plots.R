# Plots: monolinguals ----------------------------------------------------
#
# - Plot raw data
# - Plot raw data w/ model estimates
#
# -----------------------------------------------------------------------------



# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "04a_mono_analysis.R"))
fits_mono <- read_csv(here("data", "tidy", "fits_mono.csv"))

# -----------------------------------------------------------------------------





# Plot raw data ---------------------------------------------------------------

# Average over item reps
item_means <- coronals_mono %>%
  filter(kt > 0) %>%
  mutate(kt_log = log(kt), kt_std = (kt_log - mean(kt_log)) / sd(kt_log)) %>%
  select(id, item, group_sum, phon_sum, stress_sum, contains("_std")) %>%
  gather(metric, val, -group_sum, -phon_sum, -stress_sum, -id, -item) %>%
  group_by(id, item, group_sum, phon_sum, stress_sum, metric) %>%
  summarize(val = mean(val)) %>%
  mutate(metric = fct_relevel(metric, "vot_std", "ri_std"))

# Plot raw data
coronals_mono %>%
  filter(kt > 0) %>%
  mutate(kt_log = log(kt), kt_std = (kt_log - mean(kt_log)) / sd(kt_log)) %>%
  select(group_sum, phon_sum, stress_sum, contains("_std")) %>%
  gather(metric, val, -group_sum, -phon_sum, -stress_sum) %>%
  mutate(metric = fct_relevel(metric, "vot_std", "ri_std")) %>%
  ggplot(., aes(x = factor(group_sum), y = val, fill = factor(phon_sum),
                shape = factor(stress_sum))) +
    facet_wrap(~ metric, scales = "free_y") +
    geom_beeswarm(data = item_means,
                  aes(color = factor(phon_sum), shape = factor(stress_sum)),
                  dodge.width = 0.5, alpha = 0.3) +
    stat_summary(fun.data = mean_cl_boot,
                 geom = "pointrange", position = position_dodge(0.5),
                 size = 1, show.legend = F) +
    scale_shape_manual(values = c(21, 23), name = "",
                       labels = c("Untressed", "stressed")) +
    scale_color_manual(values = my_colors, name = NULL,
                       labels = c("/t/", "/d/")) +
    scale_fill_manual(values= my_colors, name = NULL,
                      labels = c("/t/", "/d/")) +
    scale_x_discrete(labels = c("Spanish", "English")) +
    labs(y = "Metric(std. units)", x = NULL) +
    theme_grey(base_family = "Times", base_size = 16) +
    theme(legend.position = "bottom")

# -----------------------------------------------------------------------------



# Plot models -----------------------------------------------------------------

# Add fitted draws from each model to raw data

if(F) {

  mono_grid <- coronals_mono %>%
    data_grid(id, item, phon_sum, group_sum, stress_sum, rep_n)

  fits_mono <-
    bind_rows(
      mono_grid %>%
        add_fitted_draws(mod_coronals_vot_mono_full, n = 2) %>%
        mutate(metric = "vot_std"),
      mono_grid %>%
        add_fitted_draws(mod_coronals_ri_mono_full, n = 2) %>%
        mutate(metric = "ri_std"),
      mono_grid %>%
        add_fitted_draws(mod_coronals_cog_mono_full, n = 2) %>%
        mutate(metric = "cog_std"),
      mono_grid %>%
        add_fitted_draws(mod_coronals_sd_mono_full, n = 2) %>%
        mutate(metric = "sd_std"),
      mono_grid %>%
        add_fitted_draws(mod_coronals_sk_mono_full, n = 2) %>%
        mutate(metric = "sk_std"),
      mono_grid %>%
        add_fitted_draws(mod_coronals_kt_mono_full, n = 2) %>%
        mutate(metric = "kt_std")) %>%
  write_csv(., here("data", "tidy", "fits_mono.csv"))

}

# Make plot
#mono_vot <-
coronals_mono %>%
  filter(kt > 0) %>%
  mutate(kt_log = log(kt), kt_std = (kt_log - mean(kt_log)) / sd(kt_log)) %>%
  select(group_sum, phon_sum, stress_sum, contains("_std")) %>%
  gather(metric, val, -group_sum, -phon_sum, -stress_sum) %>%
  mutate(metric = fct_relevel(metric, "vot_std", "ri_std")) %>%
  ggplot(., aes(x = factor(group_sum), y = val, fill = factor(phon_sum),
                shape = factor(stress_sum))) +
    facet_wrap(~ metric, scales = "free_y") +
    geom_beeswarm(data = item_means,
                  aes(color = factor(phon_sum), shape = factor(stress_sum)),
                  dodge.width = 0.5, alpha = 0.2) +
    stat_pointinterval(
      data = fits_mono, aes(y = .value), show.legend = F,
      .width = c(.80, .95), position = position_dodge(0.5)) +
    stat_summary(
      data = fits_mono,
      aes(y = .value, color = factor(phon_sum), shape = factor(stress_sum)),
      fun.y = median, geom = "point", position = position_dodge(0.5),
      size = 4) +
    scale_shape_manual(values = c(21, 23), name = "",
                       labels = c("Untressed", "stressed")) +
    scale_color_manual(values = my_colors, name = NULL,
                       labels = c("/t/", "/d/")) +
    scale_fill_manual(values = my_colors, name = NULL,
                      labels = c("/t/", "/d/")) +
    scale_x_discrete(labels = c("Spanish", "English")) +
    labs(y = "Metric\n(std. units)", x = NULL) +
    theme_grey(base_family = "Times", base_size = 16) +
    my_theme_adj
