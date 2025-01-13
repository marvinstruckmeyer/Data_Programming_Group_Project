

gert::git_pull()

library(tidyverse)
library(ggplot2)



gert::git_pull()

library(tidyverse)
library(ggplot2)
library(stringr)

election_data_clean <- election_data |>
  filter(!is.na(partido_2) & !is.na(censo) & !is.na(num_votos)) |>
  filter(censo > 0 & num_votos >= 0) |>
  mutate(partido_2 = str_to_lower(str_trim(partido_2)))

surveys_clean <- surveys |>
  filter(!is.na(vote_intention) & !is.na(turnout)) |>
  mutate(
    vote_intention = ifelse(vote_intention > 1, vote_intention / 100, vote_intention),
    turnout = ifelse(turnout > 1, turnout / 100, turnout),
    party_name = str_to_lower(str_trim(party_name))
  )



surveys_clean <- surveys_clean |>
  group_by(party_name, pollster) |>
  summarise(
    avg_vote_intention = mean(vote_intention, na.rm = TRUE),
    avg_turnout = mean(turnout, na.rm = TRUE),
    .groups = "drop"
  )


merged_data <- election_data_clean|>
  inner_join(surveys_clean, by = c("partido_2" = "party_name"),relationship = "many-to-many")

merged_data<- merged_data |>
  inner_join(election_data_grouped, by = "partido_2")


merged_data <- merged_data |>
  mutate(
    errors = abs(avg_vote_intention * 100 - (num_votos / censo * 100))
  )



pollster_performance <- merged_data%>%
  group_by(pollster) %>%
  summarise(
    mean_error = mean(errors, na.rm = TRUE),
    max_error = max(errors, na.rm = TRUE),
    total_error = sum(errors, na.rm = TRUE),
    .groups = "drop"
  )



best_pollsters <- pollster_performance |>
  arrange(mean_error) |>
  slice(1:5)

worst_pollsters <- pollster_performance |>
  arrange(desc(mean_error))|>
  slice(1:5)


combined_pollsters <- bind_rows(
  best_pollsters |> mutate(category = "Best"),
  worst_pollsters |> mutate(category = "Worst")
)


combined_pollsters <- combined_pollsters %>%
  arrange(mean_error) 



ggplot(combined_pollsters, aes(x = reorder(pollster, desc(mean_error)), y = mean_error)) +
  geom_bar(stat = "identity", position = "dodge",fill= "blue") +
  labs(
    title = "Best and Worst Pollsters by Mean Error",
    x = "Pollster",
    y = "Mean Error (Percentage Points)",
  )  + theme_minimal()+
  coord_flip() 



gert::git_add(".")

gert::git_commit("hi")

gert::git_push()



