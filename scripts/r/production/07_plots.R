# Plots -----------------------------------------------------------------------
#
# - Generate a single plot with all metrics
# - Include raw data and model estimates from posterior distributions
# - Save plots as .png and .pdf
#
# -----------------------------------------------------------------------------



# Source files ----------------------------------------------------------------

source(here::here("scripts", "r", "production", "04a_mono_analysis.R"))
source(here::here("scripts", "r", "production", "05a_bi_analysis.R"))
source(here::here("scripts", "r", "production", "06a_bi_poa_analysis.R"))

posterior_mono_all <-
  readRDS(here("data", "models", "posterior_mono_all.rds")) %>%
    mutate(metric = fct_relevel(metric, "vot", "ri"))

posterior_bi_all <-
  readRDS(here("data", "models", "posterior_bi_all.rds")) %>%
    mutate(metric = fct_relevel(metric, "vot", "ri"))

posterior_poa_all <-
  readRDS(here("data", "models", "posterior_poa_comp_all.rds")) %>%
    mutate(place = if_else(place == "t", "coronal", "bilabial"),
           metric = fct_relevel(metric, "vot", "ri"))

# -----------------------------------------------------------------------------





# Average over item reps ------------------------------------------------------

mono_subj_item_means <- coronals_mono %>%
  filter(kt > 0) %>%
  mutate(kt_log = log(kt), kt_std = (kt_log - mean(kt_log)) / sd(kt_log)) %>%
  select(id, item, group_sum, phon_sum, stress_sum, contains("_std")) %>%
  gather(metric, val, -group_sum, -phon_sum, -stress_sum, -id, -item) %>%
  group_by(id, item, group_sum, phon_sum, stress_sum, metric) %>%
  summarize(val = mean(val)) %>%
  ungroup(.) %>%
  separate(metric, into = c("metric", "trash"), sep = "_", remove = T) %>%
  mutate(language = if_else(group_sum == 1, "english", "spanish"),
         phon = if_else(phon_sum == 1, "d", "t"),
         stress = if_else(stress_sum == 1, "stressed", "unstressed"),
         metric = fct_relevel(metric, "vot", "ri")) %>%
  select(-trash, -group_sum, -phon_sum, -stress_sum)

bi_subj_item_means <- coronals_bi %>%
  filter(kt > 0) %>%
  mutate(kt_log = log(kt), kt_std = (kt_log - mean(kt_log)) / sd(kt_log)) %>%
  select(id, item, language_sum, phon_sum, stress_sum, contains("_std")) %>%
  gather(metric, val, -language_sum, -phon_sum, -stress_sum, -id, -item) %>%
  group_by(id, item, language_sum, phon_sum, stress_sum, metric) %>%
  summarize(val = mean(val)) %>%
  ungroup(.) %>%
  separate(metric, into = c("metric", "trash"), sep = "_", remove = T) %>%
  mutate(language = if_else(language_sum == 1, "english", "spanish"),
         phon = if_else(phon_sum == 1, "d", "t"),
         stress = if_else(stress_sum == 1, "stressed", "unstressed"),
         metric = fct_relevel(metric, "vot", "ri")) %>%
  select(-trash, -language_sum, -phon_sum, -stress_sum)

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

# -----------------------------------------------------------------------------


# Plot raw data with posterior summaries --------------------------------------

mono_all_metrics <- plot_metrics(
  dataframe = mono_subj_item_means, posterior = posterior_mono_all,
  x = language, color = phon, shape = stress)

bi_all_metrics <- plot_metrics(
  dataframe = bi_subj_item_means, posterior = posterior_bi_all,
  x = language, color = phon, shape = stress)

poa_all_metrics <- plot_metrics(
  dataframe = poa_subj_item_means, posterior = posterior_poa_all,
  x = language, color = place, shape = stress,
  color_labs =  c("Bilabial", "Coronal"))

# -----------------------------------------------------------------------------



# Save plots ------------------------------------------------------------------

paths <- glue(here("figs"), "/{name}_all_metrics.",
           name = c("mono", "bi", "poa"))
devices <- c('pdf', 'png')

for (path in paths) {
  for (device in devices) {
   ggsave(glue(path, device), height = 6.43, width = 11.4, units = "in")
  }
}

# -----------------------------------------------------------------------------
