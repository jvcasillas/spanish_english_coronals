# Source libraries ----------- ------------------------------------------------

source(here::here("scripts", "r", "production", "00_libraries.R"))

# -----------------------------------------------------------------------------



# Posterior prep --------------------------------------------------------------

mono_prep <- . %>%
  transmute(
      english_d_stressed = b_Intercept + b_group_sum + b_phon_sum +
        b_stress_sum + `b_group_sum:phon_sum` + `b_group_sum:stress_sum` +
        `b_phon_sum:stress_sum` + `b_group_sum:phon_sum:stress_sum`,
      english_d_unstressed = b_Intercept + b_group_sum + b_phon_sum -
        b_stress_sum + `b_group_sum:phon_sum` - `b_group_sum:stress_sum` -
        `b_phon_sum:stress_sum` - `b_group_sum:phon_sum:stress_sum`,
      english_t_stressed = b_Intercept + b_group_sum - b_phon_sum +
        b_stress_sum - `b_group_sum:phon_sum` + `b_group_sum:stress_sum` -
        `b_phon_sum:stress_sum` + `b_group_sum:phon_sum:stress_sum`,
      english_t_unstressed = b_Intercept + b_group_sum - b_phon_sum -
        b_stress_sum - `b_group_sum:phon_sum` - `b_group_sum:stress_sum` +
        `b_phon_sum:stress_sum` + `b_group_sum:phon_sum:stress_sum`,
      spanish_d_stressed = b_Intercept - b_group_sum + b_phon_sum +
        b_stress_sum - `b_group_sum:phon_sum` - `b_group_sum:stress_sum` +
        `b_phon_sum:stress_sum` + `b_group_sum:phon_sum:stress_sum`,
      spanish_d_unstressed = b_Intercept - b_group_sum + b_phon_sum -
        b_stress_sum - `b_group_sum:phon_sum` - `b_group_sum:stress_sum` -
        `b_phon_sum:stress_sum` - `b_group_sum:phon_sum:stress_sum`,
      spanish_t_stressed = b_Intercept - b_group_sum - b_phon_sum +
        b_stress_sum + `b_group_sum:phon_sum` - `b_group_sum:stress_sum` -
        `b_phon_sum:stress_sum` + `b_group_sum:phon_sum:stress_sum`,
      spanish_t_unstressed = b_Intercept - b_group_sum - b_phon_sum -
        b_stress_sum + `b_group_sum:phon_sum` + `b_group_sum:stress_sum` +
        `b_phon_sum:stress_sum` + `b_group_sum:phon_sum:stress_sum`)

bi_prep <- . %>%
  transmute(
      english_d_stressed = b_Intercept + b_language_sum + b_phon_sum +
        b_stress_sum + `b_language_sum:phon_sum` + `b_language_sum:stress_sum` +
        `b_phon_sum:stress_sum` + `b_language_sum:phon_sum:stress_sum`,
      english_d_unstressed = b_Intercept + b_language_sum + b_phon_sum -
        b_stress_sum + `b_language_sum:phon_sum` - `b_language_sum:stress_sum` -
        `b_phon_sum:stress_sum` - `b_language_sum:phon_sum:stress_sum`,
      english_t_stressed = b_Intercept + b_language_sum - b_phon_sum +
        b_stress_sum - `b_language_sum:phon_sum` + `b_language_sum:stress_sum` -
        `b_phon_sum:stress_sum` + `b_language_sum:phon_sum:stress_sum`,
      english_t_unstressed = b_Intercept + b_language_sum - b_phon_sum -
        b_stress_sum - `b_language_sum:phon_sum` - `b_language_sum:stress_sum` +
        `b_phon_sum:stress_sum` + `b_language_sum:phon_sum:stress_sum`,
      spanish_d_stressed = b_Intercept - b_language_sum + b_phon_sum +
        b_stress_sum - `b_language_sum:phon_sum` - `b_language_sum:stress_sum` +
        `b_phon_sum:stress_sum` + `b_language_sum:phon_sum:stress_sum`,
      spanish_d_unstressed = b_Intercept - b_language_sum + b_phon_sum -
        b_stress_sum - `b_language_sum:phon_sum` - `b_language_sum:stress_sum` -
        `b_phon_sum:stress_sum` - `b_language_sum:phon_sum:stress_sum`,
      spanish_t_stressed = b_Intercept - b_language_sum - b_phon_sum +
        b_stress_sum + `b_language_sum:phon_sum` - `b_language_sum:stress_sum` -
        `b_phon_sum:stress_sum` + `b_language_sum:phon_sum:stress_sum`,
      spanish_t_unstressed = b_Intercept - b_language_sum - b_phon_sum -
        b_stress_sum + `b_language_sum:phon_sum` + `b_language_sum:stress_sum` +
        `b_phon_sum:stress_sum` + `b_language_sum:phon_sum:stress_sum`)

poa_prep <- . %>%
  transmute(
      english_t_stressed = b_Intercept + b_language_sum + b_poa_sum +
        b_stress_sum + `b_language_sum:poa_sum` + `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      english_t_unstressed = b_Intercept + b_language_sum + b_poa_sum -
        b_stress_sum + `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` - `b_language_sum:poa_sum:stress_sum`,
      english_p_stressed = b_Intercept + b_language_sum - b_poa_sum +
        b_stress_sum - `b_language_sum:poa_sum` + `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      english_p_unstressed = b_Intercept + b_language_sum - b_poa_sum -
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_t_stressed = b_Intercept - b_language_sum + b_poa_sum +
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_t_unstressed = b_Intercept - b_language_sum + b_poa_sum -
        b_stress_sum - `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` - `b_language_sum:poa_sum:stress_sum`,
      spanish_p_stressed = b_Intercept - b_language_sum - b_poa_sum +
        b_stress_sum + `b_language_sum:poa_sum` - `b_language_sum:stress_sum` -
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`,
      spanish_p_unstressed = b_Intercept - b_language_sum - b_poa_sum -
        b_stress_sum + `b_language_sum:poa_sum` + `b_language_sum:stress_sum` +
        `b_poa_sum:stress_sum` + `b_language_sum:poa_sum:stress_sum`)

# -----------------------------------------------------------------------------








# Plotting functions ----------------------------------------------------------

# Average over repetitions and prep vars
plot_prep <- function(dataframe, grouping_var, color_var, poa = FALSE) {

  grouping_var <- enquo(grouping_var)
  color_var <- enquo(color_var)

  if (poa == F) {
    group_mutate <- . %>%
      mutate(., language = if_else(!!grouping_var == 1, "english", "spanish"),
         phon = if_else(phon_sum == 1, "d", "t"),
         stress = if_else(stress_sum == 1, "stressed", "unstressed"),
         metric = fct_relevel(metric, "vot", "ri"))
  } else {
     group_mutate <- . %>%
       mutate(language = if_else(language_sum == 1, "english", "spanish"),
         place = if_else(poa_sum == 1, "coronal", "bilabial"),
         stress = if_else(stress_sum == 1, "stressed", "unstressed"),
         metric = fct_relevel(metric, "vot", "ri"))
  }

  dataframe %>%
  filter(kt > 0) %>%
  mutate(kt_log = log(kt), kt_std = (kt_log - mean(kt_log)) / sd(kt_log)) %>%
  select(id, item, !!grouping_var, !!color_var, stress_sum, contains("_std")) %>%
  gather(metric, val, -!!grouping_var, -!!color_var, -stress_sum, -id, -item) %>%
  group_by(id, item, !!grouping_var, !!color_var, stress_sum, metric) %>%
  summarize(val = mean(val)) %>%
  ungroup(.) %>%
  separate(metric, into = c("metric", "trash"), sep = "_", remove = T) %>%
  group_mutate %>%
  select(-trash, -!!grouping_var, -!!color_var, -stress_sum)
}


# Named vector of facet labels
facet_labels <- c(
  `vot` = "VOT",
  `ri` = "Relative intensity",
  `cog` = "COG",
  `kt` = "Kurtosis",
  `sd` = "Standard deviation",
  `sk` = "Skewness"
  )

# Custom colors
my_colors <- c("#7A475D", "#2980B9", "#2C9286")

# Plot adjustments
my_theme_adj <- function() {
  list(
    theme(
      legend.position = "bottom",
      axis.title.y = element_text(size = rel(.9), hjust = 0.95),
      axis.title.x = element_text(size = rel(.9)),
      legend.key = element_blank()
      ),
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3)),
           shape = guide_legend(override.aes = list(size = 3)))
  )
}

# Generic plotting function
plot_metrics <- function(dataframe, posterior, x, color, shape,
                         color_labs = c("/d/", "/t/"),
                         shape_labs = c("Stressed", "Unstressed")) {
  x <- enquo(x)
  color <- enquo(color)
  shape <- enquo(shape)

  ggplot(dataframe) +
    aes(x = !!x, y = val, fill = !!color, color = !!color, shape = !!shape) +
    facet_wrap(~ metric, scales = "free_y",
               labeller = as_labeller(facet_labels)) +
    geom_beeswarm(dodge.width = 0.5, alpha = 0.3) +
    stat_pointinterval(data = posterior, show.legend = F,
                       color = "black", .width = c(.80, .95),
                       position = position_dodge(0.5)) +
    stat_summary(data = posterior, fun.y = mean, geom = "point",
                 position = position_dodge(0.5), size = 2, show.legend = F) +
    scale_color_manual(values = my_colors, name = NULL,
                       labels = color_labs) +
    scale_fill_manual(values = my_colors, name = NULL,
                      labels = color_labs) +
    scale_shape_discrete(name = NULL, labels = shape_labs) +
    scale_x_discrete(labels = c("English", "Spanish")) +
    labs(y = "Metric (std. units)", x = NULL) +
    theme_grey(base_family = "Times", base_size = 16) +
    my_theme_adj()
}

# -----------------------------------------------------------------------------
