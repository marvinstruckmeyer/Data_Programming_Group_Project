library(tidyverse)
library(ggplot2)

Sys.setlocale("LC_ALL", "en_US.UTF-8")

setwd("C:/Users/Casper/Desktop/group (1)/group")

gert::git_pull()

election_data <- read.csv("C:/Users/Casper/Desktop/group (1)/group/Data_Programming_Group_Project.git/group/election_data_short.csv")

big_municipalities <- election_data %>%
  filter(censo > 100000)


big_municipalities<- big_municipalities %>%
  pivot_longer(
    cols = starts_with("PARTIDO"), 
    names_to = "party",
    values_to = "votes"
  )

big_municipalities <- big_municipalities  %>%
  mutate(party = case_when(
    grepl("SOCIALISTA", party, ignore.case = TRUE) ~ "PSOE",
    grepl("POPULAR", party, ignore.case = TRUE) ~ "PP",
    grepl("PODEMOS|UNIDAS", party, ignore.case = TRUE) ~ "PODEMOS",
    grepl("CIUDADANOS", party, ignore.case = TRUE) ~ "CIUDADANOS",
    grepl("VOX", party, ignore.case = TRUE) ~ "VOX",
    grepl("ESQUERRA", party, ignore.case = TRUE) ~ "ERC",
    grepl("EUSKO", party, ignore.case = TRUE) ~ "PNV",
    grepl("BLOQUE.NACIONALISTA", party, ignore.case = TRUE) ~ "BNG",
    TRUE ~ "OTHER"
  ))


winners <- big_municipalities %>%
  group_by(codigo_municipio, anno, party) %>%
  summarise(total_votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  group_by(codigo_municipio, anno) %>%
  slice_max(total_votes, n = 1)  

print(winners)

winners_plot <- ggplot(winners, aes(x = anno, fill = party)) +
  geom_bar(position = "dodge") +
  labs(title = "Winning Parties in Municipalities",
       x = "Year", y = "Number of Municipalities", fill = "Party") +
  theme_minimal()

print(winners_plot)

gert::git_add(".")
