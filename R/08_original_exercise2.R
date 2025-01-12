# load necessary libraries
library(tidyverse)
library(magrittr)

# load the data
election_data <- readRDS("R/data/processed/election_data.rds")

# focus on the November election because that's when Vox was even more
# successful than in the April one

# first, let's create municipality_summary for November 2019 (using both month and year)
municipality_summary_2019 <- election_data |> 
  filter(anno == 2019, mes == 11) |>   # specifying November 2019
  group_by(municipio) |> 
  slice(1) |> 
  summarise(
    censo = censo,
    total_votes = votos_candidaturas + votos_blancos + votos_nulos,
    valid_votes = votos_candidaturas,
    turnout = (total_votes / censo) * 100) |> 
  filter(!is.na(turnout), turnout <= 100, turnout > 0)

# then calculate party votes for the same election
party_votes_2019 <- election_data |> 
  filter(anno == 2019, mes == 11) |>   # again, specifying November 2019
  mutate(
    party_group = case_when(
      partido_2 == "VOX" ~ "VOX",
      TRUE ~ "OTHER")) |> 
  group_by(municipio, party_group) |> 
  summarise(
    votes = sum(num_votos, na.rm = TRUE),
    .groups = 'drop') |> 
  pivot_wider(
    names_from = party_group,
    values_from = votes,
    values_fill = 0)

# join the two objects
final_analysis <- municipality_summary_2019 |> 
  left_join(party_votes_2019, by = "municipio") |> 
  mutate(
    vox_share = (VOX / valid_votes) * 100) |> 
  select(municipio, censo, turnout, vox_share)

# some ggplot
ggplot(final_analysis) +
  geom_point(aes(x = turnout, y = vox_share), alpha = 0.3, color = "#52BE80", size = 2) +
  geom_smooth(aes(x = turnout, y = vox_share), method = "lm", se = TRUE, color = "#52BE80") +
  labs(
    title = "Relationship between Turnout and VOX Vote Share (November 2019)",
    subtitle = "Analysis by Municipality",
    x = "Voter Turnout (%)",
    y = "VOX Vote Share (%)"
  ) +
  theme_minimal()

# calculate the correlation
cor(final_analysis$turnout, final_analysis$vox_share)

# we tested for different sizes of municipalities by adding a population
# threshold; the correlations were more or less the same

