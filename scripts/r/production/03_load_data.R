# Load tidy data --------------------------------------------------------------
#
# Last update: 2022-09-10
#
# Change log:
#  - We no longer import just the bilabial data from Aldrich 2018 (all stops)
#  - old: tidy_bilabials.csv; new: tidy_aldrich.csv
#  - renamed 'bilabials' to 'aldrich'
#
# -----------------------------------------------------------------------------

# Source libs and helpers -----------------------------------------------------

source(here::here("scripts", "r", "production", "00_libraries.R"))
source(here("scripts", "r", "production", "01_helpers.R"))

# -----------------------------------------------------------------------------



# Load data -------------------------------------------------------------------

# Load coronals
coronals <- read_csv(here("data", "tidy", "tidy_coronals.csv"))
coronals_blp <- read_csv(here("data", "tidy", "blp.csv"))

# Load aldrich data
aldrich <- read_csv(here("data", "tidy", "tidy_aldrich.csv"))

# -----------------------------------------------------------------------------
