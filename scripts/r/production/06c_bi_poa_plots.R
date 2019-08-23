# Plots: POA comparison -------------------------------------------------------
#
# - Generate a single plot with all metrics
# - Include raw data and model estimates from posterior distributions
# - Save plots as .png and .pdf
#
# -----------------------------------------------------------------------------



# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "06a_bi_poa_analysis.R"))
posterior_poa_all <-
  readRDS(here("data", "models", "posterior_poa_comp_all.rds")) %>%
    mutate(place = if_else(place == "t", "coronal", "bilabial"),
           metric = fct_relevel(metric, "vot", "ri"))

# -----------------------------------------------------------------------------





# Plot data -------------------------------------------------------------------

# Average over item reps
poa_subj_item_means <- poa_bi %>%
  filter(kt > 0) %>%
  mutate(kt_log = log(kt), kt_std = (kt_log - mean(kt_log)) / sd(kt_log)) %>%
  select(id, item, language_sum, poa_sum, stress_sum, contains("_std")) %>%
  gather(metric, val, -language_sum, -poa_sum, -stress_sum, -id, -item) %>%
  group_by(id, item, language_sum, poa_sum, stress_sum, metric) %>%
  summarize(val = mean(val)) %>%
  ungroup(.) %>%
  separate(metric, into = c("metric", "trash"), sep = "_", remove = T) %>%
  mutate(language = if_else(language_sum == 1, "english", "spanish"),
         place = if_else(poa_sum == 1, "coronal", "bilabial"),
         stress = if_else(stress_sum == 1, "stressed", "unstressed"),
         metric = fct_relevel(metric, "vot", "ri")) %>%
  select(-trash, -language_sum, -poa_sum, -stress_sum)

# Plot raw data with posterior summaries
poa_all_metrics <- poa_subj_item_means %>%
  ggplot(.) +
    aes(x = language, y = val, fill = place, color = place, shape = stress) +
    facet_wrap(~ metric, scales = "free_y",
               labeller = as_labeller(facet_labels)) +
    geom_beeswarm(dodge.width = 0.5, alpha = 0.3) +
    stat_pointinterval(data = posterior_poa_all, show.legend = F,
                       color = "black", .width = c(.80, .95),
                       position = position_dodge(0.5)) +
    stat_summary(data = posterior_poa_all, fun.y = mean, geom = "point",
                 position = position_dodge(0.5), size = 2, show.legend = F) +
    scale_color_manual(values = my_colors, name = NULL,
                       labels = c("bilabial", "coronal")) +
    scale_fill_manual(values = my_colors, name = NULL,
                      labels = c("bilabial", "coronal")) +
    scale_shape_discrete(name = NULL, labels = c("Stressed", "Unstressed")) +
    scale_x_discrete(labels = c("English", "Spanish")) +
    labs(y = "Metric(std. units)", x = NULL) +
    theme_grey(base_family = "Times", base_size = 16) +
    my_theme_adj()

path <- file.path(here("figs"), "poa_all_metrics.")
devices <- c('pdf', 'png')
walk(devices, ~ ggsave(filename = glue::glue(path, .x), device = .x),
     height = 6.43, width = 11.4, units = "in")

# -----------------------------------------------------------------------------
