## How to analyze the relationship between census and vote? Is it true that certain parties win in rural areas?

# To solve the exercise we follow different steps:
# 1. Define urban vs. rural: we define a threshold for rural and urban areas based on census size
## we can consider: rural = municipalities with a census size < 10,000; urban = census size ≥ 10,000
# 2. Summarize voting patterns by census category:
## a) we aggregate voting data by party and categorize municipalities as rural or urban
## b) we calculate the proportion of votes each party receives in rural and urban areas
# 3. Visualize the Results: we use bar plots or line plots to compare voting patterns in rural vs. urban areas
# 4. Statistical analysis: to examine whether the relationship between census size and voting patterns is significant, 
## we perform statistical tests (chi-squared tests or correlation analysis)

library(tidyverse)

# Define rural and urban areas
election_data <- election_data |> 
  mutate(area_type = case_when(
    censo < 10000 ~ "Rural",
    censo >= 10000 ~ "Urban"
  ))

# Summarize votes by area type and party
area_summary <- election_data |> 
  group_by(area_type, siglas) |> 
  summarise(total_votes = sum(num_votos, na.rm = TRUE),
            total_municipalities = n(), .groups = "drop") |> 
  mutate(vote_share = total_votes / sum(total_votes))

# DEFINE COLOURS
party_colors <- c(
  "PSOE" = "#F93D46",
  "PP" = "#41A4F5",
  "VOX" = "#53FF53",
  "ERC" = "#FF9E33",
  "EAJ-PNV" = "#04B82B",
  "OTHER" = "#C0C0C0"
)

# palette
color_palette <-  c(
  "#E69F00",  # orange
  "#56B4E9",  # light blue
  "#009E73",  # green
  "#F0E442",  # yellow
  "#0072B2",  # dark blue
  "#D55E00",  # red
  "#CC79A7",  # pink
  "#999999",  # grey
  "#44AA99",  # teal
  "#332288",  # indigo
  "#AA4499",  # purple
  "#117733")   # forest green

# Visualize voting patterns in rural vs. urban areas
ggplot(area_summary, aes(x = siglas, y = vote_share, fill = area_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = color_palette) +
  labs(title = "Vote Share by Party in Rural vs. Urban Areas",
       x = "Party", y = "Vote Share", fill = "Area Type") +
  theme_minimal()
# the bar plot showing the vote share of each party in rural and urban areas

# --> with colours: add "scale_fill_manual(values = party_colors) +" between geom_bar and labs


# Analyze relationship between census size and vote share
census_vote <- election_data |> 
  group_by(censo, siglas) |> 
  summarise(total_votes = sum(num_votos, na.rm = TRUE), .groups = "drop")

# Correlation analysis
correlation_results <- census_vote |> 
  group_by(siglas) |> 
  summarise(correlation = cor(censo, total_votes, use = "complete.obs"))

print(correlation_results)

## I have interpreted the coefficient in detail on another file.
## The key result is that the weak correlations for some parties
## (like EH-BILDU and CONVERGÈNCIA I UNIÓ) might indicate that they
## perform relatively better in rural areas.

# Additional analysis: Top parties in rural areas
rural_winners <- election_data |> 
  filter(area_type == "Rural") |> 
  group_by(codigo_municipio, anno, siglas) |> 
  summarise(total_votes = sum(num_votos, na.rm = TRUE), .groups = "drop") |> 
  group_by(codigo_municipio, anno) |> 
  slice_max(total_votes, n = 1)

print(rural_winners)
# a table listing the most successful parties in rural municipalities


## Dominant Parties in Rural Areas: The table shows that PSOE and PARTIDO POPULAR
## frequently appear as winners in rural municipalities. This indicates these parties
## are competitive in rural areas.
## The presence of OTHER in later years (e.g., 2015, 2016, 2019) suggests that smaller
## parties or coalitions have started gaining traction in rural areas.

## In addition, by examining the data for a single municipality, we can see shifts in party dominance over time

# We can take the analysis further and:
# 1. aggregate the data to count how often each party wins in rural areas:
rural_party_summary <- rural_winners |> 
  group_by(siglas) |> 
  summarise(wins = n(), .groups = "drop") |> 
  arrange(desc(wins))

print(rural_party_summary)
# 2. Visualize Trends with a bar chart (to visualize which parties dominate in rural areas):
ggplot(rural_party_summary, aes(x = reorder(siglas, wins), y = wins, fill = siglas)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = party_colors) +
  labs(title = "Winning Parties in Rural Areas", x = "Party", y = "Number of Wins") +
  theme_minimal()

## We could also perform a similar analysis for urban areas (area_type == "Urban")
## and compare results to see if certain parties are more dominant in rural vs. urban regions.
## or explore regional variations adding a regional breakdown (codigo_ccaa) to identify
## if certain parties are more dominant in specific regions.



##### other graphs:
# # HISTOGRAM (SKIP)
# # Prepare the Data: calculate the percentage of votes for each party in rural
# # vs. urban areas and ensure the data is structured for a histogram:
# # Calculate vote percentage for each party in rural areas
# vote_percentage <- election_data |> 
#   mutate(vote_percentage = num_votos / censo * 100) |> 
#   filter(area_type == "Rural") |>  # Focus on rural areas
#   group_by(siglas) |> 
#   summarise(mean_percentage = mean(vote_percentage, na.rm = TRUE), 
#             .groups = "drop")
# # Create a Histogram: focused on vote percentages in rural areas
# ggplot(election_data, aes(x = num_votos / censo * 100, fill = siglas)) +
#   geom_histogram(binwidth = 2, position = "stack", color = "black") +
#   labs(title = "Vote Distribution in Rural Areas by Party",
#        subtitle = "Analyzing vote percentages across parties in rural areas",
#        x = "Percentage of Votes",
#        y = "Frequency",
#        fill = "Party") +
#   theme_minimal()
# # Compare Urban and Rural Directly: You can modify the histogram to include
# # both urban and rural distributions by faceting or adding area_type as a grouping variable:
# ggplot(election_data, aes(x = num_votos / censo * 100, fill = partido_2)) +
#   geom_histogram(binwidth = 2, position = "stack", color = "black") +
#   facet_wrap(~area_type) +
#   labs(title = "Vote Distribution in Rural vs. Urban Areas",
#        x = "Percentage of Votes",
#        y = "Frequency",
#        fill = "Party") +
#   theme_minimal()
# 
# # Separate Density Plots for Each Party: (SKIP)
# #for visualizing overlap and comparing patterns without stacking.
# ggplot(election_data, aes(x = num_votos / censo * 100, color = siglas, fill = siglas)) +
#   geom_density(alpha = 0.3) +
#   labs(title = "Vote Percentage Distribution in Rural Areas by Party",
#        subtitle = "Density of vote percentages across parties in rural areas",
#        x = "Percentage of Votes",
#        y = "Density",
#        color = "Party", fill = "Party") +
#   theme_minimal()

# !! Faceted Histograms
# split the histogram into separate panels for each party using facet_wrap.
ggplot(election_data, aes(x = num_votos / censo * 100, fill = siglas)) +
  geom_histogram(binwidth = 2, color = "white") +
  facet_wrap(~ siglas, scales = "free_y") +
  scale_fill_manual(values = party_colors) +
  labs(title = "Vote Percentage Distribution by Party in Rural Areas",
       x = "Percentage of Votes",
       y = "Frequency") +
  theme_minimal()


## !! Overlapping Line Charts (Frequency Polygons)
ggplot(election_data, aes(x = num_votos / censo * 100, color = siglas)) +
  geom_freqpoly(binwidth = 2, size = 1) +
  scale_color_manual(values = party_colors) +
  labs(title = "Vote Percentage Distribution by Party in Rural Areas",
       x = "Percentage of Votes",
       y = "Frequency",
       color = "Party") +
  theme_minimal()