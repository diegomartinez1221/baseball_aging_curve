library(tidyverse)
library(readr)
library(dplyr)
library(Lahman)
library(robustHD)
library(lubridate)

download.file("https://www.baseball-reference.com/data/war_daily_bat.txt", dest ="bbref.csv", mode = "wb")



People <- read_csv("People.csv")

bbref <- read_csv("bbref.csv", col_types = cols(WAR = col_number(), salary = col_number()))

rookie_year<-People%>% group_by(playerID) %>% mutate(rookie = year(debut))%>%ungroup()


debut<-rookie_year %>%select(playerID, rookie, birthCountry, birthState, height, weight)


rookie_only<-left_join(debut, Master, by= "playerID")

rookie_only<-rookie_only%>%filter(!is.na(rookie))%>%select(playerID,bbrefID,nameFirst, nameLast, height.y, weight.y,bats,throws,rookie,birthCountry.y,birthState.y)




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



Lahman_needed<-rookie_only%>%left_join(positions, by="playerID")

Lahman_all<-Lahman_needed%>%filter(rookie>=1950 & !is.na(rookie))%>%
  select(playerID,POS,bbrefID,height.y,weight.y,bats,throws,rookie,birthCountry.y,birthState.y)%>% ungroup()



batters<-bbref%>%filter(pitcher == "N")
batter<- batters%>%
  group_by(year_ID)%>%
  mutate(bbrefID = player_ID)

Hitters_data<-batter%>%inner_join(Lahman_all, by= "bbrefID")%>%filter(!is.na(POS), year_ID<2019)


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

complete_dataset<- bind_rows(top_average, second_tier, third_tier, fourth_tier, fifth_tier)%>%select(-(runs_bat:runs_above_avg_def), -(teamRpG:TB_lg))%>%mutate(active = year_ID==2019, 
                                                                                                                                                                OF= as.numeric(POS == "OF"))
mutate(active = year_ID==2019)

salary_dataset<-complete_dataset%>%filter(!is.na(salary))%>%group_by(year_ID)%>%mutate(mean_salary = mean(salary), 
                                                                                       sd_salary = sd(salary),
                                                                                       standard_salary= (salary-mean_salary)/sd_salary)

library(moderndiven)
attach(top_average)
top_average<-top_average%>%mutate(age = as.numeric(age))
fit=lm(top_average$WAR~top_average$age+I(top_average$age^2)+POS)
summary(fit)


plot(top_average$age,top_average$WAR)
ord= order(top_average$age)
lines(top_average$age[ord],predict(fit)[ord],col="red")

plot(mydata$mpg,mydata$price)
ord=order(mydata$mpg)
lines(mydata$mpg[ord],predict(fit)[ord],col="red")
plot(top_average$age,top_average$WAR)
abline(fit,col="Red")



top_average%>%ungroup()%>%distinct(player_ID, POS)%>%count(POS)

complete_dataset<-complete_dataset%>%group_by(player_ID)%>%mutate(war_prior= lag(WAR, 1, default = 0), war_prior_2= lag(WAR,2, default = 0))

write_csv(complete_dataset, path = "./Baseball_Aging_Curve//complete_dataset.csv")

write_csv(salary_dataset, path = "./Baseball_Aging_Curve//salary_dataset.csv")

