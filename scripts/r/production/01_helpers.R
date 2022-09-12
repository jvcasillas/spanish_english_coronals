# Helper functions ------------------------------------------------------------
#
# Last update: 2022-09-11
#


# Source libraries ----------- ------------------------------------------------

source(here::here("scripts", "r", "production", "00_libraries.R"))

# -----------------------------------------------------------------------------




# Misc functions --------------------------------------------------------------

# Scale continuous variables
simple_scale <- function(x) {
  out   <- (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
  return(out)
}

# Smart scaling for skewed distributions with negative values
smart_scale <- function(x) {
  # Find min. val. and add its abs. val + 1
  # to every value in vector, then log transform and
  # standardize
  min   <- na.omit(x) %>% min()
  pos_x <- x + (abs(min) + 1)
  log_x <- log(pos_x)
  out   <- (log_x - mean(log_x, na.rm = T)) / sd(log_x, na.rm = T)
  return(out)
}

# -----------------------------------------------------------------------------









# Posterior prep --------------------------------------------------------------

# Adjust posterior to match factor means
# These adjusted posterior distributions are used for plotting

# Vowel model
vowel_prep <- . %>%
  transmute(
    f1_spanish_t = b_f1std_Intercept - b_f1std_language_sum - b_f1std_phon_sum + `b_f1std_language_sum:phon_sum`,
    f2_spanish_t = b_f2std_Intercept - b_f2std_language_sum - b_f2std_phon_sum + `b_f2std_language_sum:phon_sum`,
    f1_english_t = b_f1std_Intercept + b_f1std_language_sum - b_f1std_phon_sum - `b_f1std_language_sum:phon_sum`,
    f2_english_t = b_f2std_Intercept + b_f2std_language_sum - b_f2std_phon_sum - `b_f2std_language_sum:phon_sum`,
    f1_spanish_d = b_f1std_Intercept - b_f1std_language_sum + b_f1std_phon_sum - `b_f1std_language_sum:phon_sum`,
    f2_spanish_d = b_f2std_Intercept - b_f2std_language_sum + b_f2std_phon_sum - `b_f2std_language_sum:phon_sum`,
    f1_english_d = b_f1std_Intercept + b_f1std_language_sum + b_f1std_phon_sum + `b_f1std_language_sum:phon_sum`,
    f2_english_d = b_f2std_Intercept + b_f2std_language_sum + b_f2std_phon_sum + `b_f2std_language_sum:phon_sum`)

# Monolingual analyses
mono_vot_prep <- . %>%
  transmute(
    english_d = b_Intercept + b_group_sum + b_phon_sum + `b_group_sum:phon_sum`,
    english_t = b_Intercept + b_group_sum - b_phon_sum - `b_group_sum:phon_sum`,
    spanish_d = b_Intercept - b_group_sum + b_phon_sum - `b_group_sum:phon_sum`,
    spanish_t = b_Intercept - b_group_sum - b_phon_sum + `b_group_sum:phon_sum`)

mono_mv_prep <- . %>%
  transmute(
    cog_english_d = b_cogstd_Intercept + b_cogstd_group_sum + b_cogstd_phon_sum + `b_cogstd_group_sum:phon_sum`,
    cog_english_t = b_cogstd_Intercept + b_cogstd_group_sum - b_cogstd_phon_sum - `b_cogstd_group_sum:phon_sum`,
    cog_spanish_d = b_cogstd_Intercept - b_cogstd_group_sum + b_cogstd_phon_sum - `b_cogstd_group_sum:phon_sum`,
    cog_spanish_t = b_cogstd_Intercept - b_cogstd_group_sum - b_cogstd_phon_sum + `b_cogstd_group_sum:phon_sum`,
    ri_english_d = b_ristd_Intercept + b_ristd_group_sum + b_ristd_phon_sum + `b_ristd_group_sum:phon_sum`,
    ri_english_t = b_ristd_Intercept + b_ristd_group_sum - b_ristd_phon_sum - `b_ristd_group_sum:phon_sum`,
    ri_spanish_d = b_ristd_Intercept - b_ristd_group_sum + b_ristd_phon_sum - `b_ristd_group_sum:phon_sum`,
    ri_spanish_t = b_ristd_Intercept - b_ristd_group_sum - b_ristd_phon_sum + `b_ristd_group_sum:phon_sum`,
    sd_english_d = b_sdstd_Intercept + b_sdstd_group_sum + b_sdstd_phon_sum + `b_sdstd_group_sum:phon_sum`,
    sd_english_t = b_sdstd_Intercept + b_sdstd_group_sum - b_sdstd_phon_sum - `b_sdstd_group_sum:phon_sum`,
    sd_spanish_d = b_sdstd_Intercept - b_sdstd_group_sum + b_sdstd_phon_sum - `b_sdstd_group_sum:phon_sum`,
    sd_spanish_t = b_sdstd_Intercept - b_sdstd_group_sum - b_sdstd_phon_sum + `b_sdstd_group_sum:phon_sum`,
    sk_english_d = b_skstd_Intercept + b_skstd_group_sum + b_skstd_phon_sum + `b_skstd_group_sum:phon_sum`,
    sk_english_t = b_skstd_Intercept + b_skstd_group_sum - b_skstd_phon_sum - `b_skstd_group_sum:phon_sum`,
    sk_spanish_d = b_skstd_Intercept - b_skstd_group_sum + b_skstd_phon_sum - `b_skstd_group_sum:phon_sum`,
    sk_spanish_t = b_skstd_Intercept - b_skstd_group_sum - b_skstd_phon_sum + `b_skstd_group_sum:phon_sum`,
    kt_english_d = b_ktstd_Intercept + b_ktstd_group_sum + b_ktstd_phon_sum + `b_ktstd_group_sum:phon_sum`,
    kt_english_t = b_ktstd_Intercept + b_ktstd_group_sum - b_ktstd_phon_sum - `b_ktstd_group_sum:phon_sum`,
    kt_spanish_d = b_ktstd_Intercept - b_ktstd_group_sum + b_ktstd_phon_sum - `b_ktstd_group_sum:phon_sum`,
    kt_spanish_t = b_ktstd_Intercept - b_ktstd_group_sum - b_ktstd_phon_sum + `b_ktstd_group_sum:phon_sum`)



# Bilingual coronal analyses
bi_vot_prep <- . %>%
  transmute(
    english_t = b_Intercept + b_language_sum - b_phon_sum - `b_language_sum:phon_sum`,
    english_d = b_Intercept + b_language_sum + b_phon_sum + `b_language_sum:phon_sum`,
    spanish_d = b_Intercept - b_language_sum + b_phon_sum - `b_language_sum:phon_sum`,
    spanish_t = b_Intercept - b_language_sum - b_phon_sum + `b_language_sum:phon_sum`)

bi_mv_prep <- . %>%
  transmute(
    cog_english_d = b_cogstd_Intercept + b_cogstd_language_sum + b_cogstd_phon_sum + `b_cogstd_language_sum:phon_sum`,
    cog_english_t = b_cogstd_Intercept + b_cogstd_language_sum - b_cogstd_phon_sum - `b_cogstd_language_sum:phon_sum`,
    cog_spanish_d = b_cogstd_Intercept - b_cogstd_language_sum + b_cogstd_phon_sum - `b_cogstd_language_sum:phon_sum`,
    cog_spanish_t = b_cogstd_Intercept - b_cogstd_language_sum - b_cogstd_phon_sum + `b_cogstd_language_sum:phon_sum`,
    ri_english_d = b_ristd_Intercept + b_ristd_language_sum + b_ristd_phon_sum + `b_ristd_language_sum:phon_sum`,
    ri_english_t = b_ristd_Intercept + b_ristd_language_sum - b_ristd_phon_sum - `b_ristd_language_sum:phon_sum`,
    ri_spanish_d = b_ristd_Intercept - b_ristd_language_sum + b_ristd_phon_sum - `b_ristd_language_sum:phon_sum`,
    ri_spanish_t = b_ristd_Intercept - b_ristd_language_sum - b_ristd_phon_sum + `b_ristd_language_sum:phon_sum`,
    sd_english_d = b_sdstd_Intercept + b_sdstd_language_sum + b_sdstd_phon_sum + `b_sdstd_language_sum:phon_sum`,
    sd_english_t = b_sdstd_Intercept + b_sdstd_language_sum - b_sdstd_phon_sum - `b_sdstd_language_sum:phon_sum`,
    sd_spanish_d = b_sdstd_Intercept - b_sdstd_language_sum + b_sdstd_phon_sum - `b_sdstd_language_sum:phon_sum`,
    sd_spanish_t = b_sdstd_Intercept - b_sdstd_language_sum - b_sdstd_phon_sum + `b_sdstd_language_sum:phon_sum`,
    sk_english_d = b_skstd_Intercept + b_skstd_language_sum + b_skstd_phon_sum + `b_skstd_language_sum:phon_sum`,
    sk_english_t = b_skstd_Intercept + b_skstd_language_sum - b_skstd_phon_sum - `b_skstd_language_sum:phon_sum`,
    sk_spanish_d = b_skstd_Intercept - b_skstd_language_sum + b_skstd_phon_sum - `b_skstd_language_sum:phon_sum`,
    sk_spanish_t = b_skstd_Intercept - b_skstd_language_sum - b_skstd_phon_sum + `b_skstd_language_sum:phon_sum`,
    kt_english_d = b_ktstd_Intercept + b_ktstd_language_sum + b_ktstd_phon_sum + `b_ktstd_language_sum:phon_sum`,
    kt_english_t = b_ktstd_Intercept + b_ktstd_language_sum - b_ktstd_phon_sum - `b_ktstd_language_sum:phon_sum`,
    kt_spanish_d = b_ktstd_Intercept - b_ktstd_language_sum + b_ktstd_phon_sum - `b_ktstd_language_sum:phon_sum`,
    kt_spanish_t = b_ktstd_Intercept - b_ktstd_language_sum - b_ktstd_phon_sum + `b_ktstd_language_sum:phon_sum`)



# Bilingual POA analyses
poa_vot_prep <- . %>%
  transmute(
    english_p = b_Intercept + b_phonp,
    english_t = b_Intercept,
    english_k = b_Intercept + b_phonk,
    spanish_p = b_Intercept + b_languagespanish + b_phonp + `b_languagespanish:phonp`,
    spanish_t = b_Intercept + b_languagespanish,
    spanish_k = b_Intercept + b_languagespanish + b_phonk + `b_languagespanish:phonk`)

poa_mv_prep <- . %>%
  transmute(
    cog_english_p = b_cogstd_Intercept + b_cogstd_phonp,
    cog_english_t = b_cogstd_Intercept,
    cog_english_k = b_cogstd_Intercept + b_cogstd_phonk,
    cog_spanish_p = b_cogstd_Intercept + b_cogstd_languagespanish + b_cogstd_phonp + `b_cogstd_languagespanish:phonp`,
    cog_spanish_t = b_cogstd_Intercept + b_cogstd_languagespanish,
    cog_spanish_k = b_cogstd_Intercept + b_cogstd_languagespanish + b_cogstd_phonk + `b_cogstd_languagespanish:phonk`,

    ri_english_p = b_ristd_Intercept + b_ristd_phonp,
    ri_english_t = b_ristd_Intercept,
    ri_english_k = b_ristd_Intercept + b_ristd_phonk,
    ri_spanish_p = b_ristd_Intercept + b_ristd_languagespanish + b_ristd_phonp + `b_ristd_languagespanish:phonp`,
    ri_spanish_t = b_ristd_Intercept + b_ristd_languagespanish,
    ri_spanish_k = b_ristd_Intercept + b_ristd_languagespanish + b_ristd_phonk + `b_ristd_languagespanish:phonk`,

    sd_english_p = b_sdstd_Intercept + b_sdstd_phonp,
    sd_english_t = b_sdstd_Intercept,
    sd_english_k = b_sdstd_Intercept + b_sdstd_phonk,
    sd_spanish_p = b_sdstd_Intercept + b_sdstd_languagespanish + b_sdstd_phonp + `b_sdstd_languagespanish:phonp`,
    sd_spanish_t = b_sdstd_Intercept + b_sdstd_languagespanish,
    sd_spanish_k = b_sdstd_Intercept + b_sdstd_languagespanish + b_sdstd_phonk + `b_sdstd_languagespanish:phonk`,

    sk_english_p = b_skstd_Intercept + b_skstd_phonp,
    sk_english_t = b_skstd_Intercept,
    sk_english_k = b_skstd_Intercept + b_skstd_phonk,
    sk_spanish_p = b_skstd_Intercept + b_skstd_languagespanish + b_skstd_phonp + `b_skstd_languagespanish:phonp`,
    sk_spanish_t = b_skstd_Intercept + b_skstd_languagespanish,
    sk_spanish_k = b_skstd_Intercept + b_skstd_languagespanish + b_skstd_phonk + `b_skstd_languagespanish:phonk`,

    kt_english_p = b_ktstd_Intercept + b_ktstd_phonp,
    kt_english_t = b_ktstd_Intercept,
    kt_english_k = b_ktstd_Intercept + b_ktstd_phonk,
    kt_spanish_p = b_ktstd_Intercept + b_ktstd_languagespanish + b_ktstd_phonp + `b_ktstd_languagespanish:phonp`,
    kt_spanish_t = b_ktstd_Intercept + b_ktstd_languagespanish,
    kt_spanish_k = b_ktstd_Intercept + b_ktstd_languagespanish + b_ktstd_phonk + `b_ktstd_languagespanish:phonk`)

# -----------------------------------------------------------------------------









# Vowel functions -------------------------------------------------------------

# Calculate quantiles of posterior
p <- c(0.025, 0.975, 0.1, 0.9)
p_names <- map_chr(p, ~paste0(.x * 100, "%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
  set_names(nm = p_names)

# -----------------------------------------------------------------------------









# Plotting functions ----------------------------------------------------------

# Plot posterior summary for vowels
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
      color = "grey10", size = 1.5, width = 0),
    geom_errorbarh(data = posterior_summary, inherit.aes = F,
      aes(y = f1_mean, xmin = `f2_10%`, xmax = `f2_90%`),
      color = "grey10", size = 1.5, width = 0),
    geom_point(data = posterior_summary,
      aes(x = f2_mean, y = f1_mean, fill = lang_phon, shape = lang_phon),
      color = "black", size = 4, stroke = 0.5)
  )
}

# Legend labels for vowel plot
vowel_leg <- c("English /d/", "English /t/", "Spanish /d/", "Spanish /t/")


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
       mutate(lang = language,
         place = case_when(
           phon == "p" ~ "bilabial",
           phon == "t" ~ "coronal",
           phon == "k" ~ "velar"),
         metric = fct_relevel(metric, "vot", "ri"))
  }

  dataframe %>%
  select(id, item, !!grouping_var, !!color_var, contains("_std"),
         -f1_cent_std, -f2_cent_std) %>%
  gather(metric, val, -!!grouping_var, -!!color_var, -id, -item) %>%
  group_by(id, !!grouping_var, !!color_var, metric) %>%
  summarize(val = mean(val), .groups = "drop") %>%
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
my_colors0 <- c("#7A475D", "#2980B9", "#2C9286", "#dcefe5")
my_colors <- viridis::viridis(6, option = "C", begin = 0.2, end = 0.9)



# Plot adjustments
my_theme_adj <- function() {
  list(
    theme(
      legend.position = "bottom",
      axis.title.y = element_text(size = rel(.9), hjust = 0.95),
      axis.title.x = element_text(size = rel(.9)),
      legend.key = element_blank(),
      panel.grid.major = element_line(colour = 'grey90', size = 0.15),
      panel.grid.minor = element_line(colour = 'grey90', size = 0.15)
      ),
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3)),
           fill = guide_legend(override.aes = list(alpha = 1, size = 3)),
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
    aes(x = !!x, y = val) +
    facet_wrap(~ metric, nrow = 2, labeller = as_labeller(facet_labels)) +
    geom_hline(yintercept = 0, lty = 2, size = 0.25) +
    geom_point(alpha = 0.2, stroke = 0.3, color = "white",
      aes(fill = !!color, shape = !!color),
      position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.2)) +
    stat_pointinterval(data = posterior, point_size = 3, stroke = 0.25,
      aes(shape = !!color, point_fill = !!color), show.legend = F,
      .width = c(.80, .99), position = position_dodge(0.5), point_color = "white") +
    scale_color_manual(values = my_colors[c(2, 4)], name = NULL,
      labels = color_labs) +
    scale_fill_manual(values = my_colors[c(2, 4)], name = NULL,
      labels = color_labs) +
    scale_fill_manual(values = my_colors[c(2, 4)], name = NULL,
      labels = color_labs, aesthetics = "point_fill") +
    scale_shape_manual(values = c(21, 25), name = NULL, labels = color_labs) +
    scale_x_discrete(labels = xlabs) +
    coord_cartesian(ylim = c(-2, 2)) +
    labs(y = "Metric (std. units)", x = NULL) +
    theme_classic(base_family = "Times", base_size = 12) +
    my_theme_adj()
}


# Facet labs for model summary plots
y_labels_vowels <- c(
  "Item rep.", "Language\nx Voicing", "Voicing", "Language", "Intercept"
  )

facet_labels_mono <- c(
  `b_group_sum` = "Language",
  `b_phon_sum` = "Voicing",
  `b_f1_cent` = "F1",
  `b_f2_cent` = "F2",
  `b_group_sum:phon_sum`  = "Language x\nVoicing"
  )

facet_labels_bi <- c(
  `b_language_sum` = "Language",
  `b_phon_sum` = "Voicing",
  `b_f1_cent` = "F1",
  `b_f2_cent` = "F2",
  `b_language_sum:phon_sum`  = "Language x\nVoicing"
  )

# Named vector of facet labels
facet_labels_poa <- c(
  `b_languagespanish` = "Language\nSpanish",
  `b_phonp` = "Place\nBilabial",
  `b_phonk` = "Place\nVelar",
  `b_f1` = "F1",
  `b_f2` = "F2",
  `b_languagespanish:phonp`  = "Spanish\nbilabial",
  `b_languagespanish:phonk`  = "Spanish\nvelar"
  )

# Theme adjustment for model summary plots
model_theme_adj <- function(legend.position = c(0.97, 0.5)) {
  list(
    theme(
      axis.title.y = element_text(size = rel(.9), hjust = 0.95),
      axis.title.x = element_text(size = rel(.9), hjust = 0.95),
      panel.grid.major = element_line(colour = 'grey90', size = 0.25),
      panel.grid.minor = element_line(colour = 'grey90', size = 0.25),
      legend.position = legend.position, legend.justification = c(1, 1),
      legend.key = element_blank())
  )
}

# Make model summary plot
model_summary_plot <- function(
    posterior, ylabs, rope = c(-0.1, 0.1), xlim = c(-1, 1), legend.position = c(0.97, 0.5)
    ) {
  ggplot(posterior, aes(y = NULL, x = estimate)) +
    facet_grid(parameters ~ ., labeller = as_labeller(ylabs)) +
    geom_rect(data = tibble(xmin = rope[1], xmax = rope[2]), inherit.aes = FALSE,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      fill = "lightblue", color = "white", alpha = 0.2) +
    geom_vline(xintercept = 0, lty = 3) +
    stat_pointinterval(position = position_dodgev(0.7), point_size = 2,
      color = "black", aes(shape = metric)) +
    stat_pointinterval(position = position_dodgev(0.7), point_size = 1.25,
      aes(shape = metric, point_color = metric, point_fill = metric)) +
    scale_fill_manual(name = NULL, values = my_colors,
      labels = facet_labels, aesthetics = "point_fill") +
    scale_color_manual(name = NULL, values = my_colors,
      labels = facet_labels, aesthetics = "point_color") +
    scale_shape_manual(name = NULL, values = c(21:25, 8),
      labels = facet_labels) +
    coord_cartesian(xlim = xlim) +
    labs(y = "Parameters", x = "Estimates") +
    theme_minimal(base_family = "Times", base_size = 16) +
    model_theme_adj(legend.position = legend.position) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

# Make model summary plot - vowels
model_summary_vowels_plot <- function(posterior, ylabs, rope = c(-0.1, 0.1)) {
  ggplot(posterior, aes(y = parameters, x = estimate)) +
    geom_rect(data = tibble(xmin = rope[1], xmax = rope[2]), inherit.aes = FALSE,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      fill = "lightblue", color = "white", alpha = 0.2) +
    geom_vline(xintercept = 0, lty = 3) +
    stat_pointinterval(position = position_dodgev(0.7), point_size = 2,
      color = "black", aes(shape = metric)) +
    stat_pointinterval(position = position_dodgev(0.7), point_size = 1.25,
      aes(shape = metric, point_color = metric, point_fill = metric)) +
    scale_fill_manual(name = NULL, values = my_colors[c(2, 6)],
      labels = facet_labels, aesthetics = "point_fill") +
    scale_color_manual(name = NULL, values = my_colors[c(2, 6)],
      labels = facet_labels, aesthetics = "point_color") +
    scale_shape_manual(name = NULL, values = c(21:25, 8),
      labels = facet_labels) +
    coord_cartesian(xlim = c(-1, 1)) +
    scale_y_discrete(labels = ylabs) +
    labs(y = "Parameters", x = "Estimates") +
    theme_minimal(base_family = "Times", base_size = 16) +
    model_theme_adj()
}

# Function for plotting posterior distributions with descriptives
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
    stat_pointinterval(pch = 21, fill = "white", point_size = 7, stroke = 0.5,
      interval_size = c(20, 5)) +
    geom_text(data = post_summary, hjust = 0, size = 4, family = "Times",
        aes(x = x, y = density, label = text)) +
    labs(y = ylab, x = xlab) +
    coord_cartesian(ylim = c(0, NA)) +
    theme_minimal(base_size = 16, base_family = "Times") +
    model_theme_adj()
  print(plot_final)

}

# -----------------------------------------------------------------------------














# Printing functions ----------------------------------------------------------


# Make model summary table (give it a posterior dist.)
make_model_table <- function(v, name_v, ci = 0.95, rope = c(-0.1, 0.1)) {
  tibble(
    column   = name_v,
    Estimate = mean(v),
    hdi_lo   = hdi(v, .width = ci)[1],
    hdi_hi   = hdi(v, .width = ci)[2],
    ROPE     = rope(v, ci = ci, range = rope)$ROPE_Percentage,
    MPE      = p_direction(v, effects = "all")[1]
  )}


# Report posterior estimates, HDI, ROPE, and MPE in prose
report_posterior <- function(df, param, metric = NULL, prefix = NULL,
  supress = FALSE) {

  if (is.null(metric)) {
    # Extract wanted value from model output
    est  <- df[df$Parameter == param, "Estimate"]
    cis  <- df[df$Parameter == param, "HDI"]
    rope <- df[df$Parameter == param, "ROPE"]
    mpe  <- df[df$Parameter == param, "MPE"]
  } else {
    # Extract wanted value from model output
    est  <- df[df$Parameter == param & df$Metric == metric, "Estimate"]
    cis  <- df[df$Parameter == param & df$Metric == metric, "HDI"]
    rope <- df[df$Parameter == param & df$Metric == metric, "ROPE"]
    mpe  <- df[df$Parameter == param & df$Metric == metric, "MPE"]
  }

  if(supress == FALSE) {
  capture.output(
    paste0("(", prefix, "&beta; = ", est, ", HDI = ", cis, ", ROPE = ", rope,
           ", MPE = ", mpe, ")", "\n") %>%
      cat()) %>%
    paste()
  } else {
  capture.output(
    paste0(prefix, "&beta; = ", est, ", HDI = ", cis, ", ROPE = ", rope,
           ", MPE = ", mpe, "\n") %>%
      cat()) %>%
    paste()
  }
}

# -----------------------------------------------------------------------------
