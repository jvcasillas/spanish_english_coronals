# Source libraries ----------- ------------------------------------------------

source(here::here("scripts", "r", "production", "00_libraries.R"))

# -----------------------------------------------------------------------------



# Posterior prep --------------------------------------------------------------

# Adjust posterior to match factor means
vowel_prep <- . %>%
  transmute(
    f1_spanish_t = b_f1std_Intercept - b_f1std_language_sum - b_f1std_phon_sum,
    f2_spanish_t = b_f2std_Intercept - b_f2std_language_sum - b_f2std_phon_sum,
    f1_english_t = b_f1std_Intercept + b_f1std_language_sum - b_f1std_phon_sum,
    f2_english_t = b_f2std_Intercept + b_f2std_language_sum - b_f2std_phon_sum,
    f1_spanish_d = b_f1std_Intercept - b_f1std_language_sum + b_f1std_phon_sum,
    f2_spanish_d = b_f2std_Intercept - b_f2std_language_sum + b_f2std_phon_sum,
    f1_english_d = b_f1std_Intercept + b_f1std_language_sum + b_f1std_phon_sum,
    f2_english_d = b_f2std_Intercept + b_f2std_language_sum + b_f2std_phon_sum)

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
      legend.key = element_blank(),
      panel.grid.major = element_line(colour = 'grey90', size = 0.25),
      panel.grid.minor = element_line(colour = 'grey90', size = 0.25)
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
    geom_beeswarm(dodge.width = 0.5, alpha = 0.15) +
    stat_pointinterval(data = posterior, show.legend = F,
                       color = "black", .width = c(.80, .99),
                       position = position_dodge(0.5)) +
    stat_summary(data = posterior, fun.y = mean, geom = "point",
                 position = position_dodge(0.5), size = 2, show.legend = F) +
    scale_color_manual(values = my_colors, name = NULL,
                       labels = color_labs) +
    scale_fill_manual(values = my_colors, name = NULL,
                      labels = color_labs) +
    scale_x_discrete(labels = xlabs) +
    labs(y = "Metric (std. units)", x = NULL) +
    theme_minimal(base_family = "Times", base_size = 17) +
    my_theme_adj()
}


# Y labs for model summary plots
model_plot_vowel_y_labs <-
  c("Item rep", "Phoneme", "Language", "Intercept")

model_plot_mono_y_labs <-
  c("Group x\nPhoneme", "F2", "F1", "Phoneme", "Group", "Intercept")

model_plot_bi_y_labs <-
  c("Language x\nPhoneme", "F2", "F1", "Phoneme", "Language",
    "Intercept")

model_plot_poa_y_labs <-
  c("Language x\nPlace", "F2", "F1", "Place", "Language",
    "Intercept")

# Theme adjustment for model summary plots
model_theme_adj <- function() {
  list(
    theme(
      axis.title.y = element_text(size = rel(.9), hjust = 0.95),
      axis.title.x = element_text(size = rel(.9), hjust = 0.95),
      panel.grid.major = element_line(colour = 'grey90', size = 0.25),
      panel.grid.minor = element_line(colour = 'grey90', size = 0.25),
      legend.position = c(0.97, 0.5), legend.justification = c(1, 1),
      legend.key = element_blank())
  )
}

# Make model summary plot
model_summary_plot <- function(posterior, ylabs, rope = c(-0.1, 0.1)) {
  ggplot(posterior, aes(y = parameters, x = estimate, color = metric)) +
    geom_rect(data = tibble(xmin = rope[1], xmax = rope[2]), inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "lightblue", color = "white", alpha = 0.2) +
    geom_vline(xintercept = 0, lty = 3) +
    stat_pointintervalh(position = position_dodgev(0.5)) +
    scale_y_discrete(labels = ylabs) +
    scale_color_brewer(name = NULL, palette = "Dark2") +
    coord_cartesian(xlim = c(-1, 1)) +
    labs(y = "Parameters", x = "Estimates") +
    theme_minimal(base_family = "Times", base_size = 16) +
    model_theme_adj()
}



# Function for plotting posterior distrubutions with descriptives
plot_posterior <- function(posterior, parameter, rope = c(-0.1, 0.1),
                           hdi = 0.95, xpos = -0.4, ypos = c(1.2, 1, 0.8),
                           xlab = "Parameter value", ylab = "Density",
                           color = 2) {

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
         geom_density(color = "grey65", fill = 'white', alpha = 0.4)

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
      mean = median(!!param),
      mpe = p_direction(!!param) %>% round(., 3),
      hdi_low = bayestestR::hdi(!!param, ci = hdi)$CI_low %>% round(., 3),
      hdi_high = bayestestR::hdi(!!param, ci = hdi)$CI_high %>% round(., 3),
      rope_p = rope(!!param, range = rope, ci = hdi)$ROPE_Percentage %>%
          round(., 3))

  # Summary tibble with summary_vals info and some text
  post_summary <- tibble(
    x = xpos, density = ypos,
    text = c(glue("MPE = ", summary_vals$mpe),
             glue("HDI = [{l}, {h}]", l = summary_vals$hdi_low,
                                      h = summary_vals$hdi_high),
             glue("ROPE = ", summary_vals$rope_p)))

  # Plot final object
  plot_final <- plot_hold +
    geom_area(data = plot_build, show.legend = F, alpha = 0.4,
              aes(x = x, y = y, fill = region)) +
    scale_fill_manual(labels = c("", "ROPE", ""),
                      values = c(my_colors[color], "grey80", my_colors[color])) +
    geom_segment(x = summary_vals$hdi_low, xend = summary_vals$hdi_high,
                 y = 0, yend = 0, size = 2) +
    geom_point(data = summary_vals, size = 7, pch = 21, fill = "white",
               aes(x = mean, y = density)) +
    geom_text(data = post_summary, hjust = 0, size = 4, family = "Times",
        aes(x = x, y = density, label = text)) +
    labs(y = ylab, x = xlab) +
    theme_minimal(base_size = 16, base_family = "Times") +
    model_theme_adj()
  print(plot_final)

}

# -----------------------------------------------------------------------------





# Printing functions ----------------------------------------------------------


# Make table (take mod obj and tweak it)
make_model_table <- .  %>%
describe_posterior(centrality = "mean", ci = 0.95, rope_ci = 0.95,
                         test = c("rope", "p_direction")) %>%
        as_tibble %>%
        select(-CI, -ROPE_CI, -ROPE_low, -ROPE_high) %>%
        mutate_if(is.numeric, round, digits = 3) %>%
        mutate(CI_low = format(CI_low, nsmall = 3),
               CI_low = str_replace(CI_low, " ", ""),
               CI_high = format(CI_high, nsmall = 3),
               CI_high = str_replace(CI_high, " ", "")) %>%
        unite(HDI, CI_low, CI_high, sep = ", ") %>%
        mutate(HDI = paste0("[", HDI, "]")) %>%
        select(Parameter, Estimate = Mean, HDI, ROPE = ROPE_Percentage,
               MPE = pd)

# Round and format numbers to exactly N digits
round_exactly_n <- function(x, n = 3) {
  if (x < 1) {
    rounded_n <- format(round(x, digits = n), nsmall = n)
    out <- substr(as.character(rounded_n), start = 2, stop = n + 2)
  } else {
  out <- format(round(x, digits = n), nsmall = n)
  }
  return(out)
}

# Convert probability (0-1) to percent
convert_to_percent <- function(x) {
  out <- round(x * 100, digits = 2)
  return(out)
}

# Take model obj and calculate rope
get_rope <- . %>%
 rope(ci = 0.95) %>%
 select(Parameter, ROPE_Percentage) %>%
 spread(Parameter, ROPE_Percentage) %>%
 mutate_all(convert_to_percent)

# Take model obj and calculate MPE
get_mpe <- . %>%
  p_direction() %>%
  select(Parameter, pd) %>%
  spread(Parameter, pd) %>%
  mutate_all(round_exactly_n)

# -----------------------------------------------------------------------------
