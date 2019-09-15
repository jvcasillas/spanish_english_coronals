# Source libraries ----------- ------------------------------------------------

source(here::here("scripts", "r", "production", "00_libraries.R"))

# -----------------------------------------------------------------------------



# Posterior prep --------------------------------------------------------------

# Adjust posterior to match factor means
vowel_prep <- . %>%
  transmute(
    spanish_t = b_Intercept - b_language_sum - b_phon_sum,
    english_t = b_Intercept + b_language_sum - b_phon_sum,
    spanish_d = b_Intercept - b_language_sum + b_phon_sum,
    english_d = b_Intercept + b_language_sum + b_phon_sum)

mono_prep <- . %>%
  transmute(
    english_d = b_Intercept + b_group_sum + b_phon_sum + `b_group_sum:phon_sum`,
    english_t = b_Intercept + b_group_sum - b_phon_sum - `b_group_sum:phon_sum`,
    spanish_d = b_Intercept - b_group_sum + b_phon_sum - `b_group_sum:phon_sum`,
    spanish_t = b_Intercept - b_group_sum - b_phon_sum + `b_group_sum:phon_sum`)

bi_prep <- . %>%
  transmute(
    english_t = b_Intercept + b_language_sum - b_phon_sum - `b_language_sum:phon_sum`,
    english_d = b_Intercept + b_language_sum + b_phon_sum + `b_language_sum:phon_sum`,
    spanish_d = b_Intercept - b_language_sum + b_phon_sum - `b_language_sum:phon_sum`,
    spanish_t = b_Intercept - b_language_sum - b_phon_sum + `b_language_sum:phon_sum`)

poa_prep <- . %>%
  transmute(
    english_t = b_Intercept + b_language_sum + b_poa_sum + `b_language_sum:poa_sum`,
    english_p = b_Intercept + b_language_sum - b_poa_sum - `b_language_sum:poa_sum`,
    spanish_p = b_Intercept - b_language_sum - b_poa_sum + `b_language_sum:poa_sum`,
    spanish_t = b_Intercept - b_language_sum + b_poa_sum - `b_language_sum:poa_sum`)

# -----------------------------------------------------------------------------









# Vowel functions -------------------------------------------------------------

# Calculate quantiles of posterior
p <- c(0.025, 0.975, 0.1, 0.9)
p_names <- map_chr(p, ~paste0(.x*100, "%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
  set_names(nm = p_names)

# -----------------------------------------------------------------------------









# Plotting functions ----------------------------------------------------------

# Plot posterior summer for vowels
plot_posterior_vowel_summary <- function() {
  list(
    geom_errorbar(data = posterior_summary, inherit.aes = F,
      aes(x = f2_mean, ymin = `f1_2.5%`, ymax = `f1_97.5%`),
      color = "grey10", size = 0.9, width = 0),
    geom_errorbarh(data = posterior_summary, inherit.aes = F,
      aes(y = f1_mean, xmin = `f2_2.5%`, xmax = `f2_97.5%`),
      color = "grey10", size = 0.9, width = 0),
    geom_errorbar(data = posterior_summary, inherit.aes = F,
      aes(x = f2_mean, ymin = `f1_10%`, ymax = `f1_90%`),
      color = "grey10", size = 2, width = 0),
    geom_errorbarh(data = posterior_summary, inherit.aes = F,
      aes(y = f1_mean, xmin = `f2_10%`, xmax = `f2_90%`),
      color = "grey10", size = 2, width = 0),
    geom_point(data = posterior_summary,
               aes(x = f2_mean, y = f1_mean, fill = language),
               color = "black", size = 5, pch = 21, stroke = 1)
  )
}

# Average over repetitions and prep vars
plot_prep <- function(dataframe, grouping_var, color_var, poa = FALSE) {

  grouping_var <- enquo(grouping_var)
  color_var <- enquo(color_var)

  if (poa == F) {
    group_mutate <- . %>%
      mutate(., language = if_else(!!grouping_var == 1, "english", "spanish"),
         phon = if_else(phon_sum == 1, "d", "t"),
         metric = fct_relevel(metric, "vot", "ri"))
  } else {
     group_mutate <- . %>%
       mutate(language = if_else(language_sum == 1, "english", "spanish"),
         place = if_else(poa_sum == 1, "coronal", "bilabial"),
         metric = fct_relevel(metric, "vot", "ri"))
  }

  dataframe %>%
  filter(kt > 0) %>%
  mutate(kt_log = log(kt), kt_std = (kt_log - mean(kt_log)) / sd(kt_log)) %>%
  select(id, item, !!grouping_var, !!color_var, contains("_std"), -f1_std, -f2_std) %>%
  gather(metric, val, -!!grouping_var, -!!color_var, -id, -item) %>%
  group_by(id, item, !!grouping_var, !!color_var, metric) %>%
  summarize(val = mean(val)) %>%
  ungroup(.) %>%
  separate(metric, into = c("metric", "trash"), sep = "_", remove = T) %>%
  group_mutate %>%
  select(-trash, -!!grouping_var, -!!color_var)
}


# Named vector of facet labels
facet_labels <- c(
  `vot` = "VOT",
  `ri`  = "Relative intensity",
  `cog` = "COG",
  `kt`  = "Kurtosis",
  `sd`  = "Standard deviation",
  `sk`  = "Skewness"
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
plot_metrics <- function(dataframe, posterior, x, color,
                         color_labs = c("/d/", "/t/"),
                         xlabs = c("English", "Spanish")) {
  x <- enquo(x)
  color <- enquo(color)

  ggplot(dataframe) +
    aes(x = !!x, y = val, fill = !!color, color = !!color) +
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
    scale_x_discrete(labels = xlabs) +
    labs(y = "Metric (std. units)", x = NULL) +
    theme_grey(base_family = "Times", base_size = 16) +
    my_theme_adj()
}


# Y labs for model summary plots
model_plot_vowel_y_labs <-
  c("Item rep", "Phoneme", "Language", "Intercept")

model_plot_mono_y_labs <-
  c("Group x Phoneme", "Item rep", "F2", "F1", "Phoneme", "Group", "Intercept")

model_plot_bi_y_labs <-
  c("Language x Phoneme", "Item rep", "F2", "F1", "Phoneme", "Language",
    "Intercept")

model_plot_poa_y_labs <-
  c("Language x Place", "Item rep", "F2", "F1", "Place", "Language",
    "Intercept")

# Theme adjustment for model summary plots
model_theme_adj <-
    theme(
      axis.title.y = element_text(size = rel(.9), hjust = 0.95),
      axis.title.x = element_text(size = rel(.9), hjust = 0.95),
      legend.key = element_blank())

# Make model summary plot
model_summary_plot <- function(posterior, ylabs) {
  ggplot(posterior, aes(y = parameters, x = estimate, color = metric)) +
    geom_vline(xintercept = 0, lty = 3) +
    stat_halfeyeh(position = position_dodgev(0.6)) +
    scale_y_discrete(labels = ylabs) +
    scale_color_brewer(name = NULL, palette = "Dark2") +
    coord_cartesian(xlim = c(-1, 1)) +
    labs(y = "Parameters", x = "Estimates") +
    theme_minimal(base_family = "Times", base_size = 16) +
    model_theme_adj
}



# Function for plotting posterior distrubutions with descriptives
plot_posterior <- function(posterior, parameter, rope = c(-0.1, 0.1),
                           hdi = 0.95, labs = "Posterior summary",
                           colors, xpos = -0.4, ypos = c(1.2, 1, 0.8)) {

  # Quasi quotation for tidyeval
  param <- enquo(parameter)

  # Create column indicating whether posterior values are in or out of ROPE
  post <- mutate(posterior,
      region = case_when(
        !!param <= rope[1] ~ 0,
        !!param >= rope[2] ~ 2,
        !!param > rope[1] & !!param < rope[2] ~ 1))

  # Generate basic plot
  plot_hold <- ggplot(post, aes(x = !!param)) +
         geom_density(color = "grey75", fill = 'lightgrey', alpha = 0.4)

  # Get proto object and add ROPE region, in_out column
  plot_build <- ggplot_build(plot_hold)$data[[1]] %>%
    mutate(rank = percent_rank(y),
           region = case_when(
             x <= rope[1] ~ 0,
             x >= rope[2] ~ 2,
             x > rope[1] & x < rope[2] ~ 1),
           region = as.factor(region),
           in_out = if_else(region == 1, "in", "out"))

  # Calculate some descriptors of the posterior
  summary_vals <- posterior %>%
    summarize(
      density = 0, # This is a holder for posterior point mean estimate
      mean = mean(!!param),
      mpe = p_direction(!!param) %>% round(., 3),
      hdi_low = hdi(!!param, ci = hdi)$CI_low %>% round(., 3),
      hdi_high = hdi(!!param, ci = hdi)$CI_high %>% round(., 3),
      rope_p = rope(!!param, range = rope, ci = hdi)$ROPE_Percentage %>%
          round(., 3))

  # Summary tibble with summary_vals info and some text
  post_summary <- tibble(
    x = xpos, density = ypos,
    text = c(glue::glue("MPE = ", summary_vals$mpe),
             glue::glue("HDI = [{l}, {h}]", l = summary_vals$hdi_low,
                                            h = summary_vals$hdi_high),
             glue::glue("ROPE = ", summary_vals$rope_p)))

  # Plot final object
  plot_final <- plot_hold +
    geom_area(data = plot_build, show.legend = F,
              aes(x = x, y = y, fill = region)) +
    scale_fill_manual(name = "", values = c("grey80", "grey90", "grey80"),
                      labels = c("", "ROPE", "")) +
    geom_segment(x = summary_vals$hdi_low, xend = summary_vals$hdi_high,
                 y = 0, yend = 0, size = 1.5) +
    geom_point(data = summary_vals, size = 6, pch = 21, fill = "white",
               aes(x = mean, y = density)) +
    geom_text(data = post_summary, hjust = 0, size = 4,
        aes(x = x, y = density, label = text)) +
    labs(y = NULL, x = labs) +
    theme_minimal(base_size = 12, base_family = "Times")
  print(plot_final)

}

# -----------------------------------------------------------------------------
