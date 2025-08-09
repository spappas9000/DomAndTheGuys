
library(tidyverse)

fantasydata = list(
  passing = read_csv("data/Fantasy Projections (2025-26) - Passing.csv"),
  receiving = read_csv("data/Fantasy Projections (2025-26) - Receiving.csv"),
  rushing = read_csv("data/Fantasy Projections (2025-26) - Rushing.csv")
) %>% reduce(merge, by = c("Player", "ID", "Pos", "Season", "Team", "Age"), all = T)

fantasy_yty = list(
  data2024 = fantasydata %>% filter(Season == 2024) %>% rename_with(~ paste0(., "24")) %>%
    rename(Player = Player24, ID = ID24, Pos = Pos24) %>% select(-c(Season24, Team24)),
  data2023 = fantasydata %>% filter(Season == 2023) %>% rename_with(~ paste0(., "23")) %>%
    rename(Player = Player23, ID = ID23, Pos = Pos23) %>% select(-c(Season23, Team23)),
  data2022 = fantasydata %>% filter(Season == 2022) %>% rename_with(~ paste0(., "22")) %>%
    rename(Player = Player22, ID = ID22, Pos = Pos22) %>% select(-c(Season22, Team22))
) %>% reduce(merge, by = c("Player", "ID", "Pos"))
