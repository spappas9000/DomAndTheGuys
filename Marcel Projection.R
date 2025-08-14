
library(tidyverse)
library(rvest)

lgaverage = list(
  lg1 = fantasydata,
  lg2 = fantasydata %>%
    select(Season, PassAtt, PassYds, PassTD, Int, Rec, RecYds, RecTD, RushAtt, RushYds, RushTD) %>%
    group_by(Season) %>%
    summarise_all(sum, na.rm = T) %>%
    rename_with(~ paste0(., "_lg")) %>%
    rename(Season = Season_lg)
) %>% reduce(merge, by = "Season")

marcel_yty = list(
  data2024 = lgaverage %>% filter(Season == 2024) %>% rename_with(~ paste0(., "24")) %>%
    rename(Player = Player24, ID = ID24, Pos = Pos24) %>% select(-c(Season24, Team24)),
  data2023 = lgaverage %>% filter(Season == 2023) %>% rename_with(~ paste0(., "23")) %>%
    rename(Player = Player23, ID = ID23, Pos = Pos23) %>% select(-c(Season23, Team23)),
  data2022 = lgaverage %>% filter(Season == 2022) %>% rename_with(~ paste0(., "22")) %>%
    rename(Player = Player22, ID = ID22, Pos = Pos22) %>% select(-c(Season22, Team22))
) %>% reduce(merge, by = c("Player", "ID", "Pos"), all = T) %>%
  replace(is.na(.), 0)

marceldata = marcel_yty %>%
  group_by(Player, ID, Pos, Age24) %>%
  reframe(
    PassAtt24 = PassAtt24, Rec24 = Rec24, RushAtt24 = RushAtt24,
    PassAttWt = (PassAtt24 * 5 + PassAtt23 * 4 + PassAtt22 * 3)/(PassAtt24 * 5 + PassAtt23 * 4 + PassAtt22 * 3 + 1000),
    PassYds_r = (PassYds24 * 5 + PassYds23 * 4 + PassYds22 * 3)/(PassAtt24 * 5 + PassAtt23 * 4 + PassAtt22 * 3),
    PassYds_lg = (PassYds_lg24 * 5 + PassYds_lg23 * 4 + PassYds_lg22 * 3)/(PassAtt_lg24 * 5 + PassAtt_lg23 * 4 + PassAtt_lg22 * 3),
    pPassYds_r = (PassYds_r * PassAttWt) + (PassYds_lg * (1 - PassAttWt)),
    PassTD_r = (PassTD24 * 5 + PassTD23 * 4 + PassTD22 * 3)/(PassAtt24 * 5 + PassAtt23 * 4 + PassAtt22 * 3),
    PassTD_lg = (PassTD_lg24 * 5 + PassTD_lg23 * 4 + PassTD_lg22 * 3)/(PassAtt_lg24 * 5 + PassAtt_lg23 * 4 + PassAtt_lg22 * 3),
    pPassTD_r = (PassTD_r * PassAttWt) + (PassTD_lg * (1 - PassAttWt)),
    Int_r = (Int24 * 5 + Int23 * 4 + Int22 * 3)/(PassAtt24 * 5 + PassAtt23 * 4 + PassAtt22 * 3),
    Int_lg = (Int_lg24 * 5 + Int_lg23 * 4 + Int_lg22 * 3)/(PassAtt_lg24 * 5 + PassAtt_lg23 * 4 + PassAtt_lg22 * 3),
    pInt_r = (Int_r * PassAttWt) + (Int_lg * (1 - PassAttWt)),
    RecWt = (Rec24 * 5 + Rec23 * 4 + Rec22 * 3)/(Rec24 * 5 + Rec23 * 4 + Rec22 * 3 + 200),
    RecYds_r = (RecYds24 * 5 + RecYds23 * 4 + RecYds22 * 3)/(Rec24 * 5 + Rec23 * 4 + Rec22 * 3),
    RecYds_lg = (RecYds_lg24 * 5 + RecYds_lg23 * 4 + RecYds_lg22 * 3)/(Rec_lg24 * 5 + Rec_lg23 * 4 + Rec_lg22 * 3),
    pRecYds_r = (RecYds_r * RecWt) + (RecYds_lg * (1 - RecWt)),
    RecTD_r = (RecTD24 * 5 + RecTD23 * 4 + RecTD22 * 3)/(Rec24 * 5 + Rec23 * 4 + Rec22 * 3),
    RecTD_lg = (RecTD_lg24 * 5 + RecTD_lg23 * 4 + RecTD_lg22 * 3)/(Rec_lg24 * 5 + Rec_lg23 * 4 + Rec_lg22 * 3),
    pRecTD_r = (RecTD_r * RecWt) + (RecTD_lg * (1 - RecWt)),
    RushAttWt = (RushAtt24 * 5 + RushAtt23 * 4 + RushAtt22 * 3)/(RushAtt24 * 5 + RushAtt23 * 4 + RushAtt22 * 3 + 400),
    RushYds_r = (RushYds24 * 5 + RushYds23 * 4 + RushYds22 * 3)/(RushAtt24 * 5 + RushAtt23 * 4 + RushAtt22 * 3),
    RushYds_lg = (RushYds_lg24 * 5 + RushYds_lg23 * 4 + RushYds_lg22 * 3)/(RushAtt_lg24 * 5 + RushAtt_lg23 * 4 + RushAtt_lg22 * 3),
    pRushYds_r = (RushYds_r * RushAttWt) + (RushYds_lg * (1 - RushAttWt)),
    RushTD_r = (RushTD24 * 5 + RushTD23 * 4 + RushTD22 * 3)/(RushAtt24 * 5 + RushAtt23 * 4 + RushAtt22 * 3),
    RushTD_lg = (RushTD_lg24 * 5 + RushTD_lg23 * 4 + RushTD_lg22 * 3)/(RushAtt_lg24 * 5 + RushAtt_lg23 * 4 + RushAtt_lg22 * 3),
    pRushTD_r = (RushTD_r * RushAttWt) + (RushTD_lg * (1 - RushAttWt))
  )

playingtime = list(
  pff = read_csv("C:/Users/sppap/Downloads/projections.csv") %>%
    mutate(Pos = str_to_upper(position), Rec_pff = recvReceptions/2, PassAtt_pff = passAtt/2, 
           RushAtt_pff = rushAtt/2) %>%
    rename(Player = playerName, Team = teamName) %>%
    select(Player, Pos, Team, PassAtt_pff, Rec_pff, RushAtt_pff),
  marcel = fantasy_yty %>%
    group_by(Player, Pos, ID) %>%
    reframe(PassAtt_marcel = PassAtt24 * 0.5 + PassAtt23 * 0.1 + 150, 
            Rec_marcel = Rec24 * 0.5 + Rec23 * 0.1 + 50,
            RushAtt_marcel = RushAtt24 * 0.5 + RushAtt23 * 0.1 + 75)
) %>% reduce(merge, by = c("Player", "Pos")) %>%
  replace(is.na(.), 0) %>%
  select(Player, ID, Pos, Team, everything())

marcel = list(
  playingtime %>%
    select(Player, ID, Pos, Team, PassAtt_pff, Rec_pff, RushAtt_pff),
  marceldata %>%
    select(Player, ID, Pos, Age24, pPassYds_r, pPassTD_r, pInt_r, pRecYds_r, pRecTD_r, pRushYds_r, pRushTD_r)
) %>% reduce(merge, by = c("Player", "ID", "Pos")) %>%
  replace(is.na(.), 0) %>%
  select(Player, ID, Pos, Team, Age24, everything()) %>%
  mutate(
    Age_adj = ifelse(Age24 + 1 <= 28, 1 + (28 - Age24 + 1) * 0.008, 1 + (28 - Age24 + 1) * 0.004),
    pPassYds = pPassYds_r * PassAtt_pff * Age_adj, 
    pPassTD = pPassTD_r * PassAtt_pff * Age_adj,
    pInt = pInt_r * PassAtt_pff * Age_adj,
    pRecYds = pRecYds_r * Rec_pff * Age_adj,
    pRecTD = pRecTD_r * Rec_pff * Age_adj,
    pRushYds = pRushYds_r * RushAtt_pff * Age_adj,
    pRushTD = pRushTD_r * RushAtt_pff * Age_adj,
    pFantasyPts = pPassYds * 0.04 + pPassTD * 4 + pInt * -2 + pRecYds * 0.1 + Rec_pff * 0.5 + pRecTD * 6 + 
                  pRushYds * 0.1 + pRushTD * 6
  ) %>%
  arrange(desc(pFantasyPts))

adp = read_html("https://www.fantasypros.com/nfl/adp/half-point-ppr-overall.php") %>%
  html_table() %>%
  bind_rows() %>%
  rename(Player = `Player Team (Bye)`) %>%
  separate(col = Player, into = c("First", "Last", "Misc"), sep = " ") %>%
  unite(Player, First, Last, sep = " ") %>%
  mutate(Player = case_when(Player == "DJ Moore" ~ "D.J. Moore", 
                            Player == "Kenneth Walker" ~ "Kenneth Walker III",
                            Player == "Brian Robinson" ~ "Brian Robinson Jr.",
                            Player == "AJ Dillon" ~ "A.J. Dillon",
                            Player == "Amon-Ra St." ~ "Amon-Ra St. Brown",
                            Player == "Marvin Harrison" ~ "Marvin Harrison Jr.",
                            Player == "DK Metcalf" ~ "D.K. Metcalf",
                            Player == "DeMario Douglas" ~ "Demario Douglas",
                            Player == "Michael Pittman" ~ "Michael Pittman Jr.",
                            Player == "Chig Okonkwo" ~ "Chigoziem Okonkwo",
                            Player == "Calvin Austin" ~ "Calvin Austin III",
                            Player == "Joshua Palmer" ~ "Josh Palmer",
                            Player == "Joshua Reynolds" ~ "Josh Reynolds",
                            Player == "Tyrone Tracy" ~ "Tyrone Tracy Jr.",
                            .default = Player))

marcel2 = list(
  marcel %>%
    select(Player, Pos, Team, pFantasyPts, pPassYds, pPassTD, pInt, Rec_pff, pRecYds, pRecTD, pRushYds, pRushTD, 
           PassAtt_pff, Rec_pff, RushAtt_pff),
  adp %>%
    select(Player, AVG)
) %>% reduce(merge, by = "Player", all = T) %>%
  arrange(desc(pFantasyPts)) %>%
  group_by(Pos) %>%
  mutate(Rank_pos = order(pFantasyPts, decreasing = T)) %>%
  ungroup() %>%
  mutate(Rank = row_number(), Rank_diff = AVG - Rank) %>%
  filter(pFantasyPts >= 50)

write_csv(marcel2, "C:/Users/sppap/Downloads/FantasyMarcel.csv")








  