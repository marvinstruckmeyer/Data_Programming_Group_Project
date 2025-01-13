# To solve the exercise we need to:
# 1. Filter the data: Focus on municipalities with more than 100,000 inhabitants (big_municipalities).
# 2. Group and summarize votes: Summarize the total votes for each party by municipality and election year.
# 3. Identify first- and second-place parties: For each municipality and year, rank the parties based on total votes.
# 4. Filter based on first-place party: Select cases where the first-place party is either PSOE or PP, and extract the corresponding second-place party.

# Identify winners and second-place parties
ranked_parties <- big_municipalities |> 
  group_by(codigo_municipio, anno, siglas) |> 
  summarise(total_votes = sum(num_votos, na.rm = TRUE), .groups = "drop") |> 
  group_by(codigo_municipio, anno) |> 
  arrange(desc(total_votes)) |> 
  mutate(rank = row_number()) |> 
  ungroup()
# ranked_parties groups the data by municipality and year - and it sums up votes for each party and ranks them by total votes.

# Filter for cases where the first-place party is PSOE or PP
second_places <- ranked_parties |> 
  filter(rank <= 2) |> 
  group_by(codigo_municipio, anno) |> 
  summarise(
    first_place = siglas[rank == 1],
    second_place = siglas[rank == 2],
    .groups = "drop"
  ) |> 
  filter(first_place %in% c("PSOE", "PARTIDO POPULAR"))
#second_places filters to include only the first- and second-ranked parties for each municipality - and it identifies cases where the first-place party is PSOE or PP and retrieves the corresponding second-place party.

# View results
print(second_places)

# define colors
party_colors <- c(
  "PSOE" = "#F93D46",
  "PP" = "#41A4F5",
  "VOX" = "#53FF53",
  "ERC" = "#FF9E33",
  "EAJ-PNV" = "#04B82B",
  "OTHER" = "#C0C0C0"
)

# Plot results
ggplot(second_places, aes(x = first_place, fill = second_place)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = party_colors) +
  labs(title = "Second-Place Parties When First is PSOE or PP",
       x = "First-Place Party", y = "Number of Municipalities", fill = "Second-Place Party") +
  theme_minimal()
# the bar chart shows the distribution of second-place parties when the first is PSOE or PP

#### over the years:
# Summarize second-place parties over the years
second_places_year <- ranked_parties |> 
  filter(rank <= 2) |> 
  group_by(codigo_municipio, anno) |> 
  summarise(
    first_place = siglas[rank == 1],
    second_place = siglas[rank == 2],
    .groups = "drop"
  ) |> 
  filter(first_place %in% c("PARTIDO SOCIALISTA OBRERO ESPAÑOL", "PARTIDO POPULAR"))

# View the summarized results
print(second_places_year)

# # Plot results: Stacked bar plot with years
# ggplot(second_places_year, aes(x = anno, fill = second_place)) +
#   geom_bar(position = "stack") +
#   facet_wrap(~ first_place, scales = "free_y") +
#   scale_fill_manual(values = party_colors) +
#   labs(
#     title = "Second-Place Parties Over Time When First is PSOE or PP",
#     x = "Year of Election", y = "Number of Municipalities",
#     fill = "Second-Place Party"
#   ) +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "RobotoCondensed")  # Use Roboto Condensed font
#   )

# Plot results: Grouped bar plot with years
ggplot(second_places_year, aes(x = factor(anno), fill = second_place)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ first_place, scales = "free_y") +
  scale_fill_manual(values = party_colors) +
  labs(
    title = "Second-Place Parties Over Time When First is PSOE or PP",
    x = "Year of Election", y = "Number of Municipalities",
    fill = "Second-Place Party"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "RobotoCondensed"),  # Use Roboto Condensed font
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )
