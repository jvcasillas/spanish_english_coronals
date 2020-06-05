# Source libraries and helpers ------------------------------------------------

source(here::here("scripts", "r", "production", "00_libraries.R"))
source(here("scripts", "r", "production", "01_helpers.R"))

# -----------------------------------------------------------------------------


# Tidy coronal data -----------------------------------------------------------

# Store column names as vector
headers <- c("id", "f1_start", "f2_start", "f1_cent", "f2_cent", "vot", "ri",
             "cog", "sd", "sk", "kt", "label")

# Targets with stressed initial syllables
raw_prod_df$item %>% unique

initial_stress <- c(
  "daba", "dado", "daga", "dagger", "dama", "damage", "damper", "dancing",
  "dano",  "danza", "dapper", "dazzle", "tabard", "tabla", "tabloid",
  "tacit", "tackle", "taco", "tactics", "tanker", "tanque", "tanto",
  "tantrum", "tamper", "taza", "tablet")

# Load data, set column names
# create group, id, item, and language variables from id col
# clean up levels of language that include numbers
raw_prod_df <- read_csv(
  file = here("data", "raw", "data_20200605.csv"),
  col_names = headers) %>%
  separate(id, into = c("group", "id", "item", "language", "misc"),
              sep = "_") %>%
  group_by(id, group, item) %>%
  mutate(rep_n = seq_along(item)) %>%
  ungroup() %>%
  mutate(
    ri = as.numeric(ri),
    phon = substr(item, start = 1, stop = 1),
    voicing = if_else(phon == "d", "voiced", "voiceless"),
    language = case_when(
     language %in% c("english", "english1", "english2", "english3") ~ "english",
     TRUE ~ "spanish"),
    stress = if_else(item %in% initial_stress, "stressed", "unstressed")) %>%
  write_csv(., path = here("data", "tidy", "tidy_coronals.csv"))

# -----------------------------------------------------------------------------




# Tidy bilabial data from Aldrich (2019) --------------------------------------

# Store column names as a vector
headers_aldrich <- c('id', 'item', 'n_intervals', 'status', 'f1_start',
                     'f2_start', 'f1_mp', 'f2_mp', 'v_dur', 'vot', 'ri',
                     'cog', 'sd', 'sk', 'kt', 'notes')

# Targets with stressed initial syllables
initial_stress_aldrich <- c(
  "pacto", "paso", "pata", "patria", "paño", "pano", "paja", "panza", "paco",
  "tanque", "tanto", "taza", "tacha", "tambo", "tapa", "pacto", "tacto",
  "tajo", "casa", "cambio", "campo", "canto", "capa", "casco", "caso",
  "cancha", "cana", "pacify", "package", "paddle", "padlock", "pageant",
  "pancake", "pander", "passable", "patty", "passion", "tabernacle",
  "tabloid", "tacit", "tackle", "tandem", "tamper", "tantrum", "tangent",
  "tacky", "taffy", "cabbage", "canopy", "capsize", "capsule", "captain",
  "caption", "captive", "casket", "casting", "castrate")

# Load data, set column names
# create cols to match coronal data
raw_aldrich_df <- read_csv(here("data", "raw", "bl_data_aldrich2019.csv"),
         col_names = headers_aldrich) %>%
  filter(status == "hit", is.na(notes)) %>%
  separate(col = id, into = c("id", "language"), sep = c(3, 4), remove = T) %>%
  separate(col = item, into = "phon", sep = 1, remove = F) %>%
  mutate(
    group = "bi_aldrich",
    language = if_else(language == "e", "english", "spanish"),
    phon = case_when(phon == "c" ~ "k", phon == "t" ~ "t", phon == "p" ~ "p"),
    voicing = "voiceless",
    rep_n = 1,
    stress = if_else(item %in% initial_stress_aldrich,
                     "stressed", "unstressed")) %>%
  rename(misc = n_intervals) %>%
  select(group, id, item, language, misc, starts_with("f"), vot, ri, cog, sd,
         sk, kt, label = notes, rep_n, phon, voicing, stress) %>%
  filter(phon == "p") %>%
  write_csv(., path = here("data", "tidy", "tidy_bilabials.csv"))

 # ----------------------------------------------------------------------------
