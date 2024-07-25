#
# I need to clean up the MIC dataset prior to deployment by:
#
# - Fixing variable names
# - Adding new variables
#


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)


# Read in data ------------------------------------------------------------

dt <- read_rds(
  here(
    "_data",
    "raw-mic-dataset.rds"
  )
)


# Data cleaning -----------------------------------------------------------

## Keep only the columns I need
dt |>
  # using transmute lets me rename columns along the way
  transmute(
    conflict_name = micname,
    year_started = styear,
    years_duration = 1 + endyear - styear,
    minimum_fatalities = fatalmin,
    maximum_fatalities = fatalmax,
    hostility_level = hostlev,
    total_belligerents = numa + numb,
    belligerent_population = wbpopest_sum,
    belligerent_military_spending = milex_sum,
    belligerent_troops = milper_sum,
    mean_belligerent_democracy = polity2_mean
  ) -> sm_dt


# Save the cleaned data ---------------------------------------------------

write_csv(
  sm_dt,
  here(
    "dashboard_data.csv"
  )
)
