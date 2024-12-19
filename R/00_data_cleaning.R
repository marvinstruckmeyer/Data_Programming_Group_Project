### Data Programming - Group Assignment

# Packages ----------------------------------------------------------------
# Load required libraries
library(tidyverse)
library(lubridate)  # For date handling


# Preliminaries -----------------------------------------------------------
# information about the data sets
(election_data %>% is.na() %>% sum()) / (dim(election_data)[1] * dim(election_data)[2])

# reduce size of data objects
election_data_short <- election_data[1:(nrow(election_data) * 0.4),]
write.csv(election_data_short, "election_data_short.csv")


# Data Cleaning -----------------------------------------------------------
# do the data cleaning for the surveys object

# Clean historical surveys
surveys_clean <- surveys %>%
  # Convert date columns to proper date format
  mutate(
    date_elec = as.Date(date_elec),
    field_date_from = as.Date(field_date_from),
    field_date_to = as.Date(field_date_to)
  ) %>%
  # Calculate fieldwork days
  mutate(
    fieldwork_days = as.numeric(field_date_to - field_date_from)
  ) %>%
  # Apply all filtering conditions
  filter(
    # Elections from 2018 onwards
    date_elec >= as.Date("2018-01-01"),
    
    # Not exit polls
    !exit_poll,
    
    # Sample size >= 750 and not unknown (NA)
    !is.na(size) & size >= 750,
    
    # More than 1 day of fieldwork
    fieldwork_days > 1
  )

# Print summary to check the results
summary(surveys_clean)
