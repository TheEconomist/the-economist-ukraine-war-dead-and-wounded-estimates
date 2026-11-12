library(dplyr)
library(readr)

men <- read_csv("source-data/comparators/russia_2021_from_un_pop_projections_2024.csv")

age_groups <- men %>%
  filter(Sex == "Male", AgeStart >= 15) %>%
  mutate(age_group = if_else(AgeStart >= 65, "65+", paste0(5 * (AgeStart %/% 5), "–", 5 * (AgeStart %/% 5) + 4))) %>%
  group_by(age_group) %>%
  summarise(population = round(sum(Value)), .groups = "drop")

men_18_49 <- men %>%
  filter(Sex == "Male", AgeStart >= 18, AgeStart <= 49) %>%
  summarise(age_group = "18–49 total", population = round(sum(Value)))

men_18_59 <- men %>%
  filter(Sex == "Male", AgeStart >= 18, AgeStart <= 59) %>%
  summarise(age_group = "18–59 total", population = round(sum(Value)))

write_csv(bind_rows(age_groups, men_18_49, men_18_59), "source-data/comparators/russian-men-by-age-group.csv")
