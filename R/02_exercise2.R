# To solve the exercise we need to:
# 1. Filter the data: Focus on municipalities with more than 100,000 inhabitants (big_municipalities).
# 2. Group and summarize votes: Summarize the total votes for each party by municipality and election year.
# 3. Identify first- and second-place parties: For each municipality and year, rank the parties based on total votes.
# 4. Filter based on first-place party: Select cases where the first-place party is either PSOE or PP, and extract the corresponding second-place party.

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
  filter(first_place %in% c("PSOE", "PARTIDO POPULAR"))
#second_places filters to include only the first- and second-ranked parties for each municipality - and it identifies cases where the first-place party is PSOE or PP and retrieves the corresponding second-place party.

# View results
print(second_places)

# Plot results
ggplot(second_places, aes(x = first_place, fill = second_place)) +
  geom_bar(position = "dodge") +
  labs(title = "Second-Place Parties When First is PSOE or PP",
       x = "First-Place Party", y = "Number of Municipalities", fill = "Second-Place Party") +
  theme_minimal()
# the bar chart shows the distribution of second-place parties when the first is PSOE or PP