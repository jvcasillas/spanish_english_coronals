library(tidyverse)
library(here)


# read data
# Calculate mean and SD of VOT for each participant as a function of language 
# and segment. 
# Create column 'mean_2sd' which gives the value 2 sd's above each mean
temp <- read_csv(here("scripts", "r", "production", "coronals_clean.csv")) %>% 
  group_by(participant, group, lang, phon) %>% 
  summarize(mean_vot = mean(vot), 
            sd_vot = sd(vot), 
            mean_2sd = mean_vot + (sd_vot * 2)) %>% 
  ungroup(.) %>% 
  select(-mean_vot, -sd_vot) %>% 
  spread(phon, mean_2sd)


# For monolingual English speakers: 
#   - use the value of English /d/
nen <- temp %>% 
  filter(., group == "NEN") %>% 
  select(participant, group, cutoff = d)

# For monolingual Spanish speakers:
#   - use the value of Spanish /t/
nsp <- temp %>% 
  filter(., group == "NSP") %>% 
  select(participant, group, cutoff = t)

# For bilingual speakers: 
#   - Use whichever value is larger between Spanish /t/ and english /d/
bil <- temp %>% 
  filter(., group %in% c("biEng", "biEsp")) %>% 
  gather(., phon, vot, -participant, -group, -lang) %>% 
  filter(., lang == "english" & phon == "d" | 
            lang == "spanish" & phon == "t") %>% 
  group_by(., participant) %>% 
  summarize(., cutoff = max(vot)) %>% 
  mutate(., group = "BIL") %>% 
  select(., participant, group, cutoff)

# Combine data frames, round to nearest hundredth and save as csv
bind_rows(nen, nsp, bil) %>% 
  mutate_if(., is.numeric, round, digits = 2) %>% 
  #knitr::kable(.)
  write_csv(here("scripts", "r", "production", "vot_cutoffs.csv"))
