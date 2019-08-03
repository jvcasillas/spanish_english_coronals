# Source libraries and helpers ------------------------------------------------

source(here::here("scripts", "r", "production", "00_libraries.R"))
source(here("scripts", "r", "production", "01_helpers.R"))

# -----------------------------------------------------------------------------


# Tidy data -------------------------------------------------------------------

# Store column names as vector
headers <- c("id", "f1_start", "f2_start", "f1_mp", "f2_mp", "vot", "ri",
             "cog", "sd", "sk", "kt", "label")

# Load data, set column names
# create group, id, item, and language variables from id col
# clean up levels of language that include numbers
raw_prod_df <- read_csv(
  file = here("data", "raw", "data_20190803.csv"),
  col_names = headers) %>%
  separate(., id, into = c("group", "id", "item", "language", "misc"),
              sep = "_") %>%
  mutate(., language = case_when(
    language %in% c("english", "english1", "english2", "english3") ~ "english",
    TRUE ~ "spanish")) %>%
  write_csv(., path = here("data", "tidy", "tidy_coronals.csv"))

# -----------------------------------------------------------------------------
