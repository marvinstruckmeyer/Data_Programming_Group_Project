# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)  # For uncount()

# Define custom colors and abbreviations for parties
party_colors <- c(
  "CS" = "#E9521D",
  "CIU" = "#1A3788",
  "EH-BILDU" = "#03CFB5",
  "ERC" = "#FF9E33",
  "PP" = "#41A4F5",
  "PSOE" = "#F93D46",
  "VOX" = "#53FF53",
  "OTHER" = "#C0C0C0"
)

# Filter data for the 2016 election year
election_data <- election_data %>%
  filter(anno == 2016)  # Include only rows for 2016

# Categorize turnout based on participacion_1 (assuming it's the turnout rate in percentage)
election_data <- election_data %>%
  mutate(
    turnout_rate = participacion_1 / 100,  # Adjust if already a proportion
    turnout_category = ifelse(turnout_rate < 0.5, "Low", "High")  # Define low vs. high turnout
  )

# Summarize votes under Low Turnout using partido_2
low_turnout_data <- election_data %>%
  filter(turnout_category == "Low") %>%
  group_by(partido_2) %>%
  summarise(total_votes = sum(num_votos, na.rm = TRUE), .groups = "drop") %>%
  mutate(percentage = total_votes / sum(total_votes) * 100)

# Recode partido_2 to abbreviations for plotting
low_turnout_data <- low_turnout_data %>%
  mutate(
    partido_2 = recode(partido_2,
                       "CIUDADANOS" = "CS",
                       "CONVERGÈNCIA I UNIÓ" = "CIU",
                       "EH-BILDU" = "EH-BILDU",
                       "ESQUERRA REPUBLICANA DE CATALUNYA" = "ERC",
                       "PARTIDO POPULAR" = "PP",
                       "PSOE" = "PSOE",
                       "VOX" = "VOX",
                       .default = "OTHER")  # Assign "OTHER" to any unmatched parties
  )

# Set total seats for the parliament
total_seats <- 350
low_turnout_data <- low_turnout_data %>%
  mutate(seats = round((total_votes / sum(total_votes)) * total_seats))

# Expand data to one row per seat
seat_positions <- low_turnout_data %>%
  uncount(seats) %>%
  mutate(seat_id = row_number())  # Unique ID for each seat

# Assign seats to rows and distribute radially by party
num_rows <- 12  # Number of rows for the semicircle
seat_positions <- seat_positions %>%
  arrange(partido_2) %>%  # Group by party for radial distribution
  mutate(
    row = ((seat_id - 1) %% num_rows) + 1,  # Assign rows cyclically
    pos_in_row = ((seat_id - 1) %/% num_rows) + 1  # Position within the row for radial grouping
  )

# Calculate x, y coordinates for radial alignment
seat_positions <- seat_positions %>%
  mutate(
    row_radius = num_rows - row + 1,  # Outer rows have larger radii
    angle = (pos_in_row - 0.5) / max(pos_in_row) * pi,  # Spread seats evenly in radial wedges
    x = row_radius * cos(angle),  # X-coordinate
    y = row_radius * sin(angle)   # Y-coordinate
  )

# Plot the parliamentary diagram
ggplot(seat_positions, aes(x = x, y = y, fill = partido_2)) +
  geom_point(shape = 21, size = 3) +
  scale_fill_manual(values = party_colors) +  # Apply custom party colors
  coord_fixed() +
  labs(
    title = "Spanish Parliamentary Representation Under Low Turnout (2016)",
    subtitle = paste("Proportional allocation of", total_seats, "seats"),
    x = NULL,
    y = NULL,
    fill = "Party"
  ) +
  theme_void() +
  theme(
    legend.position = "right",  # Move legend to the right
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
