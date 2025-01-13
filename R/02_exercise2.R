### Which party was the second when the first was the PSOE? And when the first was the PP?
# To solve the exercise we need to:
# 1. Filter the data in order to focus on big_municipalities (more than 100,000 inhabitants)
# 2. Group and summarize summarize the total votes for each party by municipality and election year
# 3. Identify first- and second-place parties for each municipality and year: rank the parties based on total votes
# 4. Filter based on first-place party: select cases where the first-place party is either PSOE or PP, and extract the corresponding second-place party

# Identify winners and second-place parties
ranked_parties <- big_municipalities |> 
  group_by(codigo_municipio, anno, partido_2) |> 
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
    first_place = partido_2[rank == 1],
    second_place = partido_2[rank == 2],
    .groups = "drop"
  ) |> 
  filter(first_place %in% c("PARTIDO SOCIALISTA OBRERO ESPAÑOL", "PARTIDO POPULAR"))
#second_places filters to include only the first- and second-ranked parties for each municipality - and it identifies cases where the first-place party is PSOE or PP and retrieves the corresponding second-place party.

# View results
print(second_places)

party_colors <- c(
  "PARTIDO SOCIALISTA OBRERO ESPAÑOL" = "#F93D46",
  "PARTIDO POPULAR" = "#41A4F5",
  "VOX" = "#53FF53",
  "ERC" = "#FF9E33",
  "EAJ-PNV" = "#04B82B",
  "OTHER" = "#C0C0C0"
)

sysfonts::font_add_google("Roboto Condensed", family = "rob_cond")
showtext::showtext_auto()

# Plot results
# the bar chart shows the distribution of second-place parties when the first is PSOE or PP
ggplot(second_places, aes(x = first_place, fill = second_place)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = party_colors) +
  labs(title = "Second-Place Parties When First is PSOE or PP",
       x = "First-Place Party", y = "Number of Municipalities", fill = "Second-Place Party") +
  theme_minimal() +
  theme(
    text = element_text(family = "RobotoCondensed")  # Apply the Roboto Condensed font to all text elements
  )



####### TAKING YEARS INTO CONSIDERATION:
# Summarize second-place parties over the years
second_places_year <- ranked_parties |> 
  filter(rank <= 2) |> 
  group_by(codigo_municipio, anno) |> 
  summarise(
    first_place = partido_2[rank == 1],
    second_place = partido_2[rank == 2],
    .groups = "drop"
  ) |> 
  filter(first_place %in% c("PARTIDO SOCIALISTA OBRERO ESPAÑOL", "PARTIDO POPULAR"))

# View the summarized results
print(second_places_year)

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
  theme_minimal()
 )

## otherwise, but I don't like it:
## Plot results: Stacked bar plot with years
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