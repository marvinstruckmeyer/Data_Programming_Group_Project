# ORIGINAL QUESTION - 1

Most elections map usually show the most voted party per electoral district. These type of maps can be a bit misleading because some electoral districts have a really big area but in reality have a very small population. Such is the case of Spain in provinces such as Cáceres, Ciudad Real or Burgos just to say a few. Think an alternative way to create a map to solve this issue.

SOLUTION: Dorling cartogram

-   Each circle represents a province.

-   The size o each circle varies according to the population of each province.

-   The color of each circle represents the most voted party in each province.

![](images/15734246152160-01.jpg)

## Example with 10N Elections:

Libraries:

```{r}
# Github
library(gert) 

# Data cleaning
library(tidyverse)

# Maps & Geospatial data
library(mapSpain)
library(sf)
library(cartogram)

# Font setting
library(sysfonts) 
library(showtext) 
```

First I load the proccesdata:

```{r}
election_data <- readRDS(file = "data/processed/election_data.rds")
```

Now I filter the data for the 10N elections and calculate the most voted party in each province:

```{r}
elections_1911 <- election_data |> 
  filter(anno == 2019 & mes == 11) |> 
  select(municipio, cod_mun, codigo_provincia, num_votos, siglas, partido, partido_2)

winners_prov <- elections_1911 |> 
  group_by(codigo_provincia, siglas) |>  
  summarise(votos_totales = sum(num_votos, na.rm = TRUE)) |> 
  slice_max(votos_totales)
```

Now I retrieve geospatial data of Spain from the `mapSpain` package that also includes a dataset with population data of every municipality (`pobmun19`).

```{r}
# Geospatial data:

provinces <- esp_get_prov() |> 
  mutate(provincia = prov.shortname.en) |> 
  mutate(codigo_provincia = cpro) |> 
  select(provincia, codigo_provincia)

# Set Mercator projection:

provinces <- st_transform(provinces, 3857)

# Population data:

pop_prov <- mapSpain::pobmun19 |> 
  rename(codigo_provincia = cpro) |> 
  group_by(codigo_provincia) |> 
  summarise(n_pop = sum(pob19))

# Join all dataframes:

pop_prov <- provinces |> 
  left_join(pop_prov, by = "codigo_provincia") |> 
  left_join(winners_prov, by = "codigo_provincia")
```

To create the cartogram, fist I will set a color palette for each party and a font:

```{r}
party_colors <- c(
  "PSOE" = "#F93D46",
  "PP" = "#41A4F5",
  "VOX" = "#53FF53",
  "ERC" = "#FF9E33",
  "EAJ-PNV" = "#04B82B",
  "OTHER" = "#C0C0C0"
)

sysfonts::font_add_google("Roboto Condensed", family = "rob_cond")
showtext::showtext_auto()
```

```{r}
pop_prov_cartog <- cartogram_dorling(pop_prov, 
                                     weight = "n_pop")

pop_prov_cartog <- ggplot(pop_prov_cartog) +
  geom_sf(aes(fill = siglas)) +
  scale_fill_manual(values = party_colors,
                    name = "Party",
                    breaks = c("PSOE", "PP", "VOX", "ERC", "EAJ-PNV", "OTHER")) +
  theme_void() +
  labs(title = "2019 November Elections: Most voted party by province",
       subtitle = "(The size of each circle represents the population of each province)") +
  theme(plot.title = element_text(face = "bold",
                                  margin = unit(c(0.6, 0, 0.2, 0), "cm"),
                                  size = 18,
                                  family = "rob_cond"),
        plot.subtitle = element_text(family = "rob_cond",
                                     size = 14),
        legend.title = element_text(face = "bold",
                                    family = "rob_cond", 
                                    size = 15),
        legend.text = element_text(family = "rob_cond", 
                                    size = 12))


pop_prov_cartog
```



