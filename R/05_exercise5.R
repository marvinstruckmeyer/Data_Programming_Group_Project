library(dplyr)
library(ggplot2)

# Load the data
data_surveys <- readRDS("surveys.rds")
data_election <- readRDS("election_data.rds")

# Set target dates - Changed to November election
target_date_survey <- as.Date("2019-11-10")  # November election date
target_date_election <- as.Date("2019-11-01")  # First of November

# Create party name mapping with display names and colors
party_mapping <- data.frame(
  survey_name = c(
    "CIUDADANOS",
    "PARTIDO NACIONALISTA VASCO",
    "PARTIDO POPULAR",
    "PARTIDO SOCIALISTA OBRERO ESPAÑOL",
    "UNIDAS PODEMOS - IU",
    "ESQUERRA REPUBLICANA DE CATALUNYA",
    "EH-BILDU",
    "VOX",
    "OTHER"
  ),
  election_name = c(
    "CS",
    "EAJ-PNV",
    "PP",
    "PSOE",
    "UP",
    "ERC",
    "EH-BILDU",
    "VOX",
    "OTHER"
  ),
  display_name = c(
    "CS",
    "EAJ-PNV",
    "PP",
    "PSOE",
    "UP",
    "ERC",
    "EH-BILDU",
    "VOX",
    "OTHER"
  ),
  party_color = c(
    "#E9521D",  # CS
    "#04B82B",  # EAJ-PNV
    "#41A4F5",  # PP
    "#F93D46",  # PSOE
    "#7C316E",  # UP
    "#FF9E33",  # ERC
    "#03CFB5",  # EH-BILDU
    "#53FF53",  # VOX
    "#C0C0C0"   # OTHER
  )
)

# Process survey data
data_surveys_filtered <- data_surveys %>%
  filter(date_elec == target_date_survey) %>%
  group_by(party_name) %>%
  summarise(
    vote_intention = mean(vote_intention, na.rm = TRUE),
    n_polls = n()
  ) %>%
  filter(!is.na(vote_intention)) %>%
  mutate(vote_share_intention = vote_intention / sum(vote_intention) * 100) %>%
  left_join(party_mapping, by = c("party_name" = "survey_name"))

# Process election data
data_election <- data_election %>%
  mutate(date_elec = as.Date(paste(anno, mes, "01", sep = "-")))

data_election_filtered <- data_election %>%
  filter(date_elec == target_date_election) %>%
  group_by(siglas) %>%
  summarise(
    total_votes = sum(num_votos, na.rm = TRUE)
  ) %>%
  mutate(vote_share_actual = total_votes / sum(total_votes) * 100)

# Merge datasets using the mapped names
comparison_data <- data_surveys_filtered %>%
  left_join(data_election_filtered, by = c("election_name" = "siglas"))

# Calculate errors
comparison_data <- comparison_data %>%
  mutate(
    absolute_error = vote_share_intention - vote_share_actual,
    relative_error = (vote_share_intention - vote_share_actual) / vote_share_actual * 100
  )

# Calculate overall error metrics
error_metrics <- comparison_data %>%
  filter(!is.na(absolute_error)) %>%
  summarise(
    mae = mean(abs(absolute_error)),
    rmse = sqrt(mean(absolute_error^2)),
    mean_error = mean(absolute_error)
  )

# Create error visualization with custom colors
error_plot <- ggplot(
  comparison_data %>% filter(!is.na(absolute_error)), 
  aes(x = reorder(display_name, -vote_share_actual))
) +
  geom_col(aes(y = absolute_error), fill = comparison_data$party_color[!is.na(comparison_data$absolute_error)]) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Polling Error by Party - November 2019 Election",  # Updated title
    subtitle = paste("Mean Absolute Error:", round(error_metrics$mae, 2), "%"),
    x = "Party",
    y = "Error in Vote Share (Prediction - Actual) %"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

print(error_plot)

# Print detailed results
print("\nDetailed Error Analysis:")
print(comparison_data %>% 
        select(display_name, vote_share_intention, vote_share_actual, absolute_error, relative_error))

print("\nOverall Error Metrics:")
print(error_metrics)

