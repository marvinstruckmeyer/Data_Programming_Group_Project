library(tidyverse)
library(ggplot2)

Sys.setlocale("LC_ALL", "en_US.UTF-8")

setwd("C:/Users/Casper/Desktop/group (1)/group/Data_Programming_Group_Project.git")

gert::git_pull()

election_data <- readRDS("C:/Users/Casper/Desktop/group (1)/group/Data_Programming_Group_Project.git/data/processed/election_data.rds")

big_municipalities <- election_data|> 
  filter(censo > 100000)





winners <- big_municipalities |> 
  group_by(codigo_municipio, anno, partido_2) |> 
  summarise(total_votes = sum(num_votos, na.rm = TRUE), .groups = "drop") %>%
  group_by(codigo_municipio, anno) |> 
  slice_max(total_votes, n = 1)  

print(winners)

winners_plot <- ggplot(winners, aes(x = anno, fill = partido_2)) +
  geom_bar(position = "dodge") +
  labs(title = "Winning Parties in Municipalities",
       x = "Year", y = "Number of Municipalities", fill = "Party") +
  theme_minimal()


print(winners_plot)

#maybe we can add regional analysis
regional_summary <- big_municipalities |> 
  group_by(codigo_ccaa, partido_2, anno) |> 
  summarise(total_municipalities = n(), .groups = "drop") |> 
  arrange(desc(total_municipalities))


ggplot(regional_summary, aes(x = anno, y = total_municipalities, color = partido_2)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~codigo_ccaa) + 
  labs(title = "Regional Performance of Parties Over Time",
       x = "Year", y = "Number of Municipalities", color = "Party") +
  theme_minimal()


gert::git_add(".")

gert::git_commit(message = "I uploaded first question")

gert::git_push()
