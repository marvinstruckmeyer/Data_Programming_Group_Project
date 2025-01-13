library(dplyr)
library(ggplot2)

# Load the .rds files
data_surveys <- readRDS("~/Data_Programming_Group_Project/data/processed/surveys.rds")
data_election <- readRDS("~/Data_Programming_Group_Project/data/processed/election_data.rds")

# Create a mapping table to align party names
party_mapping <- data.frame(
  surveys_name = c("ucd", "ap", "ciu", "erc", "eh", "vox", "psoe"),
  election_name = c("other", "other", "convergència i unió", 
                    "esquerra republicana de catalunya", 
                    "eh-bildu", "vox", "psoe")
)

# Standardize `party_name` in data_surveys
data_surveys <- data_surveys %>%
  left_join(party_mapping, by = c("party_name" = "surveys_name")) %>%
  mutate(party_name = ifelse(is.na(election_name), party_name, election_name)) %>%
  select(-election_name)

# Create `date_elec` in data_election
data_election <- data_election %>%
  mutate(date_elec = as.Date(paste(anno, mes, "01", sep = "-")))

# Convert `date_elec` to Date format in data_surveys
data_surveys <- data_surveys %>%
  mutate(date_elec = as.Date(date_elec))

# Merge the datasets on `party_name` and `date_elec`
comparison_data <- merge(
  data_surveys,
  data_election,
  by = c("party_name", "date_elec"),
  all.x = TRUE
)

# Aggregate vote intentions and actual votes
comparison_data <- comparison_data %>%
  group_by(party_name) %>%
  summarise(
    total_vote_intention = sum(vote_intention, na.rm = TRUE),
    total_actual_votes = sum(num_votos, na.rm = TRUE)
  ) %>%
  mutate(
    vote_share_intention = total_vote_intention / sum(total_vote_intention) * 100,
    vote_share_actual = total_actual_votes / sum(total_actual_votes) * 100
  )

# Create the bar chart comparing poll predictions and actual results
ggplot(comparison_data, aes(x = reorder(party_name, -vote_share_actual))) +
  geom_bar(aes(y = vote_share_intention, fill = "Poll Prediction"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = vote_share_actual, fill = "Actual Result"), stat = "identity", position = "dodge") +
  labs(
    title = "Comparison of Poll Predictions and Actual Results",
    x = "Party",
    y = "Vote Share (%)",
    fill = "Legend"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
