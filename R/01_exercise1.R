library(tidyverse)
library(ggplot2)


gert::git_status()
gert::git_pull()


big_municipalities <- election_data|> 
  filter(censo > 100000)



winners <- big_municipalities |> 
  group_by(codigo_municipio, anno, siglas) |> 
  summarise(total_votes = sum(num_votos, na.rm = TRUE), .groups = "drop") %>%
  group_by(codigo_municipio, anno) |> 
  slice_max(total_votes, n = 1)  

print(winners)

party_colors <- c(
  "PSOE" = "#F93D46",
  "PP" = "#41A4F5",
  "VOX" = "#53FF53",
  "ERC" = "#FF9E33",
  "EAJ-PNV" = "#04B82B",
  "OTHER" = "#C0C0C0")

winners_plot <- ggplot(winners, aes(x = anno, fill = siglas)) +
  geom_bar(position = "dodge") + scale_fill_manual(
    values = c( "PSOE" = "#F93D46",
                "PP" = "#41A4F5",
                "VOX" = "#53FF53",
                "ERC" = "#FF9E33",
                "EAJ-PNV" = "#04B82B",
                "OTHER" = "#C0C0C0"))+
  labs(title = "Winning Parties in Municipalities",
       x = "Year", y = "Number of Municipalities", fill = "Party") +
  theme_minimal()


print(winners_plot)


#regional analysis
regional_summary <- big_municipalities |> 
  group_by(codigo_ccaa, partido_2, anno) |> 
  summarise(total_municipalities = n(), .groups = "drop") |> 
  arrange(desc(total_municipalities))


ggplot(regional_summary, aes(x = anno, y = total_municipalities, color = partido_2)) +
  geom_line(linewidth = 1.2) + scale_fill_manual(values = c(scale_fill_manual(
    values = c( #E69F00",  # orange
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
      "#117733"))))+
  facet_wrap(~codigo_ccaa) + 
  labs(title = "Regional Performance of Parties Over Time",
       x = "Year", y = "Number of Municipalities", color = "Party") +
  theme_minimal()+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

gert::git_add(".")


gert::git_push()
