# Load tidy data --------------------------------------------------------------
#
# Last update: 2020-06-16
#

# Source libs and helpers -----------------------------------------------------

source(here::here("scripts", "r", "production", "00_libraries.R"))
source(here("scripts", "r", "production", "01_helpers.R"))

# -----------------------------------------------------------------------------



# Load data -------------------------------------------------------------------

# Load coronals
coronals <- read_csv(here("data", "tidy", "tidy_coronals.csv"))

# Load aldrich data
bilabials <- read_csv(here("data", "tidy", "tidy_bilabials.csv"))

# -----------------------------------------------------------------------------
