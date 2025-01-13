# load necessary libraries
library(tidyverse)
library(magrittr)

# Analyze VOX's electoral performance in the Nov 2019 general election by studying
# how voter turnout relates to VOX's electoral success across municipalities, 
# analysing the geographic concentration in the support of Vox, and studying
# electoral fragmentation

# load the data
election_data <- readRDS("R/data/processed/election_data.rds")

## analysing how voter turnout relates to Vox"s electoral success
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

# visualise
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


## analysing the geographic concentration in the support of Vox
regional_analysis <- election_data |> 
  filter(anno == 2019, mes == 11) |> 
  group_by(codigo_provincia) |>  
  summarise(total_votes = sum(num_votos, na.rm = TRUE), 
            vox_share = sum(num_votos[partido_2 == "VOX"], na.rm = TRUE) / total_votes * 100)

# get geospatial data for Spanish provinces
provinces <- esp_get_prov() |> 
  mutate(
    provincia = prov.shortname.en,
    codigo_provincia = as.character(cpro)) |> 
  select(provincia, codigo_provincia)

# transform to Mercator projection
provinces <- st_transform(provinces, 3857)

# get population data
pop_prov <- mapSpain::pobmun19 |> 
  rename(codigo_provincia = cpro) |> 
  mutate(codigo_provincia = as.character(codigo_provincia)) |> 
  group_by(codigo_provincia) |> 
  summarise(n_pop = sum(pob19))

# join all data
final_data <- provinces %>%
  left_join(pop_prov, by = "codigo_provincia") %>%
  left_join(regional_analysis, by = "codigo_provincia")

# set up font
sysfonts::font_add_google("Roboto Condensed", family = "rob_cond")
showtext::showtext_auto()

# create color palette for VOX vote share
vox_colors <- colorRampPalette(c("#FFFFFF", "#53FF53"))(5)

# create Dorling cartogram
cartogram_plot <- cartogram_dorling(final_data, weight = "n_pop")

# create the final plot
ggplot(cartogram_plot) +
  geom_sf(aes(fill = vox_share)) +
  scale_fill_gradientn(
    colors = vox_colors,
    name = "VOX Vote Share (%)",
    limits = c(0, max(regional_analysis$vox_share)),
    breaks = seq(0, max(regional_analysis$vox_share), length.out = 5),
    labels = scales::label_number(accuracy = 0.1)) +
  theme_void() +
  labs(
    title = "VOX Vote Share by Province (November 2019)",
    subtitle = "Circle size represents province population") +
  theme(
    plot.title = element_text(
      face = "bold",
      margin = unit(c(0.6, 0, 0.2, 0), "cm"),
      size = 18,
      family = "rob_cond"),
    plot.subtitle = element_text(
      family = "rob_cond",
      size = 14),
    legend.title = element_text(
      face = "bold",
      family = "rob_cond",
      size = 15),
    legend.text = element_text(
      family = "rob_cond",
      size = 12))

## studying electoral fragmentation
fragmentation_analysis <- election_data |> 
  filter(anno == 2019, mes == 11) |> 
  group_by(municipio) |> 
  summarise(
    total_votes = sum(num_votos, na.rm = TRUE),
    vox_share = sum(num_votos[partido_2 == "VOX"], na.rm = TRUE) / total_votes * 100,
    pp_share = sum(num_votos[partido_2 == "PARTIDO POPULAR"], na.rm = TRUE) / total_votes * 100,
    psoe_share = sum(num_votos[partido_2 == "PARTIDO SOCIALISTA OBRERO ESPAÑOL"], na.rm = TRUE) / total_votes * 100,
    traditional_share = pp_share + psoe_share,  # combined share of traditional parties
    effective_parties = 1 / sum((num_votos/total_votes)^2, na.rm = TRUE))

# first visualisation: scatter plot of VOX share vs effective number of parties
ggplot(fragmentation_analysis) +
  geom_point(aes(x = effective_parties, y = vox_share), 
             alpha = 0.4, color = "#52BE80") +
  geom_smooth(aes(x = effective_parties, y = vox_share), 
              method = "lm", color = "#2E86C1") +
  labs(
    title = "VOX Support vs Electoral Fragmentation",
    subtitle = "November 2019 Elections",
    x = "Effective Number of Parties",
    y = "VOX Vote Share (%)"
  ) +
  theme_minimal()

# second visualisation: scatter plot with traditional party share
ggplot(fragmentation_analysis) +
  geom_point(aes(x = traditional_share, y = vox_share), 
             alpha = 0.4, color = "#E74C3C") +
  geom_smooth(aes(x = traditional_share, y = vox_share), 
              method = "lm", color = "#8E44AD") +
  labs(
    title = "VOX Support vs Traditional Party Support",
    subtitle = "November 2019 Elections",
    x = "Combined PP + PSOE Vote Share (%)",
    y = "VOX Vote Share (%)") +
  theme_minimal()

# calculate correlations
cor_analysis <- fragmentation_analysis |> 
  summarise(
    cor_fragmentation = cor(effective_parties, vox_share, use = "complete.obs"),
    cor_traditional = cor(traditional_share, vox_share, use = "complete.obs"))

print("Correlation Analysis:")
print(cor_analysis)


