# Plots: monolinguals ----------------------------------------------------
#
# -----------------------------------------------------------------------------




item_means <- coronals_mono %>%
  select(id, item, group, phon, contains("_std")) %>%
  gather(metric, val, -group, -phon, -id, -item) %>%
  group_by(id, item, group, phon, metric) %>%
  summarize(val = mean(val))

coronals_mono %>%
  select(group, phon, contains("_std")) %>%
  gather(metric, val, -group, -phon) %>%
  ggplot(., aes(x = group, y = val, fill = phon)) +
    facet_wrap(~ metric, scales = "free") +
    geom_beeswarm(data = item_means,
                  aes(color = phon), dodge.width = 0.5, alpha = 0.3) +
    stat_summary(fun.data = mean_cl_boot,
                 geom = "pointrange", position = position_dodge(0.5),
                 size = 1, pch = 21) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    theme_minimal()

