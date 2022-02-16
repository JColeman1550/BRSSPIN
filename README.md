# BRSSPIN
library(tidyverse)
library(xml2)
library(rvest)



spin_direction_leaderboard <- function(num_pitches, pitch_type = "ALL",throwing_hand="All",team) {
  if(missing(team)) {
    if(throwing_hand != "All") {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=2021&min=",num_pitches,"&sort=0&sortDir=desc&pitch_type=",pitch_type,"&throws=",throwing_hand,"&playerName=&team=&csv=true")
    } else {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=2021&min=",num_pitches,"&sort=0&sortDir=desc&pitch_type=",pitch_type,"&throws=&playerName=&team=&csv=true")
    }
  }
  else {
    if(throwing_hand != "All") {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=2021&min=",num_pitches,"&sort=0&sortDir=desc&pitch_type=",pitch_type,"&throws=",throwing_hand,"&playerName=&team=",team,"&csv=true")
    } else {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=2021&min=",num_pitches,"&sort=0&sortDir=desc&pitch_type=",pitch_type,"&throws=&playerName=&team=",team,"&csv=true")
    }
  }
  
 payload <- read_csv(url) 
 
 return(payload)
}

spin_axis_data <- spin_direction_leaderboard(1)

write_csv(spin_axis_data,"spin_axis_data.csv")

BOS_data <- BOS_data %>% mutate(zone_loc = case_when(zone == 1 | zone == 2 | zone == 3 ~ "High",
                                                     zone == 4 | zone == 5 | zone == 6 ~ "Medium",
                                                     zone == 7 | zone == 8 | zone == 9 ~ "Low",
                                                     plate_z >= 3.5 ~ "High",
                                                     zone == 13 | zone == 14 ~ "Low",
                                                     plate_z <= 1.6 ~ "Low",
                                                     TRUE ~ NA_character_))

BOS_data <- left_join(BOS_data,spin_axis_data,by=c("pitcher"="player_id","pitch_name"="api_pitch_name"))

BOS_data %>% dplyr::filter(api_pitch_type == "SI", zone_loc == "Low") %>%
  dplyr::group_by(hawkeye_measured_clock_label,p_throws) %>% dplyr::summarise(n_pitches = n(),
                                                                            spin_rate = mean(release_spin_rate,na.rm=T)) %>%

ggplot() +
geom_bar(aes(hawkeye_measured_clock_label,n_pitches,fill = spin_rate),
         position = "dodge", stat = "identity")
theme_classic() +
ggtitle("Spin Direction Distribution on Low Sinkers")
facet_wrap(~p_throws)


         
