# load required packages
library(tidyverse)
library(readxl)

# load the election data
election_data <- readRDS("R/data/processed/election_data.rds")

# read GDP and Population data
# source: REGDATA_v70_55_23.xlsx from :
# https://www.bbvaresearch.com/en/publicaciones/spain-long-series-of-regional-economic-and-demographic-aggregates-2023-update/
gdp_data <- read_excel("R/data/raw/External_GDP_Pop_Data.xlsx", sheet = "GDP")
pop_data <- read_excel("R/data/raw/External_GDP_Pop_Data.xlsx", sheet = "Population")

# convert both datasets to long format
gdp_long <- gdp_data |> 
  pivot_longer(
    cols = -'...1',
    names_to = "year",
    values_to = "gdp") |> 
  rename(region = '...1')

pop_long <- pop_data |> 
  pivot_longer(
    cols = -'...1',
    names_to = "year",
    values_to = "population" ) |> 
  rename(region = '...1')

# join GDP and population data
combined_data <- gdp_long |> 
  left_join(pop_long, by = c("region", "year"))

# calculate GDP per capita
gdp_per_capita <- combined_data |> 
  mutate(gdp_per_capita = gdp / population) |> 
  arrange(region, year)

# calculate growth rates (1999-2019)
growth_rates <- gdp_per_capita |> 
  group_by(region) |> 
  summarize(
    gdp_per_capita_1999 = first(gdp_per_capita),
    gdp_per_capita_2019 = last(gdp_per_capita),
    total_growth = (gdp_per_capita_2019 / gdp_per_capita_1999 - 1) * 100) |> 
  arrange(desc(total_growth))

# calculate national average growth
national_avg_growth <- mean(growth_rates$total_growth)

# add relative growth compared to national average
growth_rates <- growth_rates |> 
  mutate(
    relative_growth = total_growth - national_avg_growth,
    growth_category = if_else(relative_growth >= 0, "Above Average", "Below Average"))

# view results
print("GDP per capita growth rates by region (1999-2019):")
print(growth_rates)

print(paste("National average growth rate:", round(national_avg_growth, 2), "%"))

# calculate VOX results by CCAA for November 2019
vox_results_2019 <- election_data |> 
  filter(anno == 2019, mes == 11) |> 
  mutate(
    party_group = case_when(
      partido_2 == "VOX" ~ "VOX",
      TRUE ~ "OTHER")) |> 
  group_by(codigo_ccaa, party_group) |> 
  summarise(
    votes = sum(num_votos, na.rm = TRUE),
    .groups = 'drop') |> 
  pivot_wider(
    names_from = party_group,
    values_from = votes,
    values_fill = 0) |> 
  mutate(
    total_votes = VOX + OTHER,
    vox_percentage = (VOX / total_votes) * 100)

# create lookup table with official CCAA codes
# this is based on https://www.ine.es/en/daco/daco42/codmun/cod_ccaa_en.htm
ccaa_lookup <- tibble(
  codigo_ccaa = c(paste0("0", 1:9), as.character(10:17), "18", "19"),
  region = c("AND", "ARA", "AST", "BAL", "CAN", "CANT", "CYL", "CLM", 
             "CAT", "VAL", "EXT", "GAL", "MAD", "MUR", "NAV", "PV", 
             "RIO", "CyMel", "CyMel"))

# join CCAA codes with results and combine Ceuta and Melilla
vox_results_2019_named <- vox_results_2019 |> 
  left_join(ccaa_lookup, by = "codigo_ccaa") |> 
  group_by(region) |> 
  summarise(
    VOX = sum(VOX),
    OTHER = sum(OTHER),
    total_votes = sum(total_votes),
    vox_percentage = (sum(VOX) / sum(total_votes)) * 100) |> 
  arrange(region)

# now join with GDP growth data
combined_analysis <- growth_rates |> 
  filter(region != "total") |> # Remove the 'total' row
  left_join(vox_results_2019_named, by = "region") |> 
  select(region, relative_growth, vox_percentage, growth_category)

# print combined results
print("\nCombined GDP growth and VOX support analysis:")
print(combined_analysis)

# calculate correlation (excluding NA values)
correlation <- cor(combined_analysis$relative_growth, 
                   combined_analysis$vox_percentage,
                   use = "complete.obs")  # this will exclude NA values
print(paste("\nCorrelation between relative GDP growth and VOX support:", round(correlation, 3)))

# create scatter plot
ggplot(combined_analysis, aes(x = relative_growth, y = vox_percentage)) +
  geom_point(aes(color = growth_category), size = 3) +
  geom_text(aes(label = region), vjust = -0.5, size = 3) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
  labs(
    title = "Relationship between Economic Growth and VOX Support",
    subtitle = "By Autonomous Community (1999-2019)",
    x = "Relative GDP per Capita Growth (%)",
    y = "VOX Vote Share (%)",
    color = "Growth Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"))
