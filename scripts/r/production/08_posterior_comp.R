posterior_mono_adj %>%
  filter(metric == "vot", language == "english" & phon == "d" |
                          language == "spanish" & phon == "t") %>%
  select(-metric, -language) %>%
  group_by(phon) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup(.) %>%
  spread(phon, val) %>%
  mutate(diff = d - t) %>%
  ggplot(., aes(x = diff)) +
    geom_histogram(color = "black", fill = "grey")
