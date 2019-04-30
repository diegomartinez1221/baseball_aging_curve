library(tidyverse)
library(readr)
library(dplyr)
library(Lahman)
library(robustHD)
library(lubridate)
library(fs)

#get WAR data from this source

download.file("https://www.baseball-reference.com/data/war_daily_bat.txt", dest ="bbref.csv", mode = "wb")

#I get all the dat from the Lahman library in R, except the People file is not
#included. So I downloded from the web

download.file("https://github.com/chadwickbureau/baseballdatabank/archive/v2019.2.zip", 
              destfile = "Lahman_Data.zip",
              mode = "wb")
untar("Lahman_Data.zip")
People<-read_csv("./baseballdatabank-2019.2/core/People.csv")

# no longer need these files 


file_delete(c("baseballdatabank-2019.2/", "Lahman_Data.zip"))

#read in data that includes WAR 

bbref <- read_csv("bbref.csv", col_types = cols(WAR = col_number(), salary = col_number()))


#creating rookie year variable. 

rookie_year<-People%>% 
  group_by(playerID) %>% 
  mutate(rookie = year(debut))%>%
  ungroup()

#only needs these variables for my analysis 

debut<-rookie_year %>%
  select(playerID, rookie, birthCountry, birthState, height, weight)

#combine with the master that has IDs compatible to baseball reference dataset

rookie_only<-left_join(debut, Master, by= "playerID")%>%
  filter(!is.na(rookie))%>%
  select(playerID,bbrefID,nameFirst, nameLast, height.y, weight.y,bats,throws,rookie,birthCountry.y,birthState.y)


# different players have played different positions across there careers. Thus
# here, I am assigning each player to the one positons he played the majority of
# his games at

positions<-Fielding%>%
  group_by(playerID)%>%
  mutate(total_games = sum(G))%>%
  ungroup()%>%
  group_by(playerID, POS)%>%
  mutate(games= sum(G))%>%
  ungroup()%>%
  group_by(playerID)%>%
  top_n(1, games)%>%
  filter(total_games > 162)%>%
  distinct(playerID, POS)

#this is all the data i need from Lahman datasets combined with positions 


Lahman_needed<-rookie_only%>%left_join(positions, by="playerID")

#the salary data I have only goes back to 1950 so I only wanted to include
#players that were rookies in 1950 onward to 2018

Lahman_all<-Lahman_needed%>%filter(rookie>=1950 & !is.na(rookie))%>%
  select(playerID,POS,bbrefID,height.y,weight.y,bats,throws,rookie,birthCountry.y,birthState.y)%>% ungroup()



batters<-bbref%>%filter(pitcher == "N")
batter<- batters%>%
  group_by(year_ID)%>%
  mutate(bbrefID = player_ID)

active<-batter%>% group_by(bbrefID)%>%mutate(last_year = max(year_ID),active = last_year==2019)%>%distinct(bbrefID, active)

Hitters_data<-batter%>%inner_join(Lahman_all, by= "bbrefID")%>%filter(!is.na(POS), year_ID<2019)%>%inner_join(active, by="bbrefID")



players_totals<-Hitters_data%>%
  group_by(player_ID)%>%
  mutate(total_war = sum(WAR), years= n())%>%
  filter(years>1 | total_war>0, G>90)%>%
  mutate(avg_war = total_war/years, median_war = median(WAR))%>%
  distinct(player_ID, total_war, years, avg_war, median_war)


mean<- mean(players_totals$avg_war)
sd<- sd(players_totals$avg_war)



players_totals<-players_totals%>%mutate(z_score= (avg_war-mean)/sd)


top_average<- players_totals%>% filter(z_score > 2)

second_tier<-players_totals%>% filter(z_score >1 & z_score<2)

third_tier<-players_totals%>% filter(z_score > 0 & z_score<1)

fourth_tier<-players_totals%>%filter(z_score> -1 & z_score<0)

fifth_tier<-players_totals%>%filter(z_score< -1)


totals<- Hitters_data%>%left_join(players_totals, by= "player_ID")


top_average<-top_average%>% inner_join(Hitters_data, by= "player_ID")%>%mutate(tier = as.character(1))

second_tier<- second_tier %>% inner_join(Hitters_data, by = "player_ID")%>%mutate(tier= as.character(2))

third_tier<- third_tier %>% inner_join(Hitters_data, by = "player_ID")%>% mutate(tier= as.character(3))

fourth_tier<- fourth_tier %>% inner_join(Hitters_data, by = "player_ID")%>% mutate(tier= as.character(4))

fifth_tier<- fifth_tier %>% inner_join(Hitters_data, by = "player_ID")%>% mutate(tier= as.character(5))

complete_dataset<- bind_rows(top_average, second_tier, third_tier, fourth_tier, fifth_tier)%>%select(-(runs_bat:runs_above_avg_def), -(teamRpG:TB_lg))

salary_dataset<-complete_dataset%>%filter(!is.na(salary))%>%group_by(year_ID)%>%mutate(mean_salary = mean(salary), 
                                                                                       sd_salary = sd(salary),
                                                                                       standard_salary= (salary-mean_salary)/sd_salary)


complete_dataset<-complete_dataset%>%group_by(player_ID)%>%mutate(war_prior= lag(WAR, 1, default = 0), war_prior_2= lag(WAR,2, default = 0))

write_csv(complete_dataset, path = "./Baseball_Aging_Curve//complete_dataset.csv")

write_csv(salary_dataset, path = "./Baseball_Aging_Curve//salary_dataset.csv")

