
library(tidyverse)

lgaverage = list(
  lg1 = fantasydata,
  lg2 = fantasydata %>%
    select(Season, PassAtt, PassYds, PassTD, Int, Tgt, RecYds, RecTD, RushAtt, RushYds, RushTD) %>%
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
) %>% reduce(merge, by = c("Player", "ID", "Pos"))

marcel = marcel_yty %>%
  group_by(Player, ID, Pos) %>%
  reframe(
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
    TgtWt = (Tgt24 * 5 + Tgt23 * 4 + Tgt22 * 3)/(Tgt24 * 5 + Tgt23 * 4 + Tgt22 * 3 + 300),
    RecYds_r = (RecYds24 * 5 + RecYds23 * 4 + RecYds22 * 3)/(Tgt24 * 5 + Tgt23 * 4 + Tgt22 * 3),
    RecYds_lg = (RecYds_lg24 * 5 + RecYds_lg23 * 4 + RecYds_lg22 * 3)/(Tgt_lg24 * 5 + Tgt_lg23 * 4 + Tgt_lg22 * 3),
    pRecYds_r = (RecYds_r * TgtWt) + (RecYds_lg * (1 - TgtWt)),
    RecTD_r = (RecTD24 * 5 + RecTD23 * 4 + RecTD22 * 3)/(Tgt24 * 5 + Tgt23 * 4 + Tgt22 * 3),
    RecTD_lg = (RecTD_lg24 * 5 + RecTD_lg23 * 4 + RecTD_lg22 * 3)/(Tgt_lg24 * 5 + Tgt_lg23 * 4 + Tgt_lg22 * 3),
    pRecTD_r = (RecTD_r * TgtWt) + (RecTD_lg * (1 - TgtWt)),
    RushAttWt = (RushAtt24 * 5 + RushAtt23 * 4 + RushAtt22 * 3)/(RushAtt24 * 5 + RushAtt23 * 4 + RushAtt22 * 3 + 400),
    RushYds_r = (RushYds24 * 5 + RushYds23 * 4 + RushYds22 * 3)/(RushAtt24 * 5 + RushAtt23 * 4 + RushAtt22 * 3),
    RushYds_lg = (RushYds_lg24 * 5 + RushYds_lg23 * 4 + RushYds_lg22 * 3)/(RushAtt_lg24 * 5 + RushAtt_lg23 * 4 + RushAtt_lg22 * 3),
    pRushYds_r = (RushYds_r * RushAttWt) + (RushYds_lg * (1 - RushAttWt)),
    RushTD_r = (RushTD24 * 5 + RushTD23 * 4 + RushTD22 * 3)/(RushAtt24 * 5 + RushAtt23 * 4 + RushAtt22 * 3),
    RushTD_lg = (RushTD_lg24 * 5 + RushTD_lg23 * 4 + RushTD_lg22 * 3)/(RushAtt_lg24 * 5 + RushAtt_lg23 * 4 + RushAtt_lg22 * 3),
    pRushTD_r = (RushTD_r * RushAttWt) + (RushTD_lg * (1 - RushAttWt))
  ) %>%
  select(Player, ID, Pos, pPassYds_r, pPassTD_r, pInt_r, pRecYds_r, pRecTD_r, pRushYds_r, pRushTD_r)

playingtime = list(
  pff = read_csv("C:/Users/sppap/Downloads/projections.csv") %>%
    mutate(Pos = str_to_upper(position)) %>%
    rename(Player = playerName, PassAtt_pff = passAtt, Tgt_pff = recvTargets, RushAtt_pff = rushAtt) %>%
    select(Player, Pos, PassAtt_pff, Tgt_pff, RushAtt_pff),
  marcel = fantasy_yty %>%
    group_by(Player, Pos) %>%
    reframe(PassAtt_marcel = PassAtt24 * 0.5 + PassAtt23 * 0.1 + 150, 
            Tgt_marcel = Tgt24 * 0.5 + Tgt23 * 0.1 + 50,
            RushAtt_marcel = RushAtt24 * 0.5 + RushAtt23 * 0.1 + 75)
) %>% reduce(merge, by = c("Player", "Pos")) %>%
  replace(is.na(.), 0)








  