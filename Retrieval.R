#Stattleship Data Retrieval

#install retrieval packages
install.packages(c("httr", "jsonlite", "lubridate"))
library("httr", lib.loc="~/R/win-library/3.4")
library("jsonlite", lib.loc="~/R/win-library/3.4")
library("lubridate", lib.loc="~/R/win-library/3.4")
options(stringsAsFactors = FALSE)

install.packages("devtools")
devtools::install_github("stattleship/stattleship-r")

## Load the stattleshipR package
library(stattleshipR)

## Get your free token from www.stattleship.com
set_token("9a0051b400b0f4c347963658a5cfe236")

sport <- 'football'
league <- 'nfl'
ep <- 'game_logs'

#
#
#
#grab game stats for 2015-2016
q_body <- list(season_id='nfl-2015-2016')

gls <- ss_get_result(sport=sport, league=league, query=q_body, ep=ep, walk=TRUE)  
gls_2015 <- gls
game_logs_2015<-do.call('rbind', lapply(gls_2015, function(x) x$games))
game_logs_2015 <- game_logs_2015[!duplicated(game_logs_2015$id),]
game_logs_2015 <- game_logs_2015[,c("id","attendance","away_team_outcome","away_team_score","daytime","duration","home_team_outcome","home_team_score","humidity","interval_number","interval_type","label","name","on","score_differential","scoreline","slug","temperature","temperature_unit","timestamp","weather_conditions","wind_direction","wind_speed","home_team_id","away_team_id","season_id","venue_id","official_ids")]

#Make a dataframe of teams with important information
#Load Excel of missing Team Locations
library(readxl)
Team_Locations <- read_excel("~/LEVEL/Stattleship/Team_Locations.xlsx")
Team_Locations$latitude <- as.numeric(Team_Locations$latitude)

#Read in Teams from Stattleship
teams_2015 <- do.call('rbind', lapply(gls_2015, function(x) x$teams))
#get rid of duplicates
teams_2015 <- teams_2015[!duplicated(teams_2015$id),c("id","color","location","name","nickname","latitude","longitude","division_id","league_id")]
#Merge with dataframe of missing values
teams_2015 <- merge(teams_2015,Team_Locations, by.x = 'nickname', by.y = 'nickname', all.x = T)
#Filter out nas and make new columns
teams_2015[is.na(teams_2015)] <- 0
teams_2015$latitude <- teams_2015$latitude.x + teams_2015$latitude.y
teams_2015$longitude <- teams_2015$longitude.x + teams_2015$longitude.y
teams_2015 <- teams_2015[,c("nickname","id","color","location","name","division_id","league_id","latitude","longitude")]

#bring in venues
venues_2015 <- do.call('rbind', lapply(gls_2015, function(x) x$venues))
venues_2015 <- venues_2015[!duplicated(venues_2015$id),c("id","capacity","city","country","field_type","name","state","time_zone")]
venues_2015$stateabb <- setNames(state.abb, state.name)[venues_2015$state]
#add missing lat, and long
library(readr)
zip_codes_states <- read_csv("~/LEVEL/Stattleship/zip_codes_states.csv")
venues_2015[is.na(venues_2015)] <- 'UK'
venues_2015$state <- venues_2015$stateabb
Venue_Location_2015 <- merge(venues_2015, zip_codes_states, by.x = c('city','state'), by.y = c('city','state'), all.x = T)
Venue_Location_2015 <- Venue_Location_2015[!duplicated(Venue_Location_2015$name),c("city","state","id","capacity","field_type","name","time_zone","latitude","longitude")]

#Merge results with info on teams and venues
game_logs_2015 <- merge( game_logs_2015, teams_2015, by.x = c('home_team_id'), by.y = c('id'))
game_logs_2015 <- renameCol(game_logs_2015, c('nickname', 'color', 'location', 'name.y', 'division_id', 'league_id', 'latitude', 'longitude'), c('h.nickname', 'h.color','h.location', 'h.name', 'h.division_id', 'h.league_id', 'h.latitude', 'h.longitude'))
game_logs_2015 <- merge( game_logs_2015, teams_2015, by.x = c('away_team_id'), by.y = c('id'))
game_logs_2015 <- renameCol(game_logs_2015, c('nickname', 'color', 'location', 'name', 'division_id', 'league_id', 'latitude', 'longitude'), c('a.nickname', 'a.color','a.location', 'a.name', 'a.division_id', 'a.league_id', 'a.latitude', 'a.longitude'))
game_logs_2015 <- merge(game_logs_2015, Venue_Location_2015, by.x = c('venue_id'), by.y = c('id'), all.x = T)
game_logs_2015 <- renameCol(game_logs_2015, c("city","state","capacity","field_type","name","time_zone","latitude","longitude"),c("v.city","v.state","v.capacity","v.field_type","v.name","v.time_zone","v.latitude","v.longitude"))
game_logs_2015 <- game_logs_2015[,c("venue_id","away_team_id","home_team_id","id","attendance","away_team_outcome","away_team_score","daytime","duration","home_team_score","humidity","interval_number","label","name.x","on","score","score_differential","scoreline","temperature","timestamp","weather_conditions","wind_direction","wind_speed","winning_team_id","season_id","official_ids","h.nickname","h.location","h.name","h.division_id","h.league_id","h.latitude","h.longitude","a.nickname","a.location","a.name","a.division_id","a.league_id","a.latitude","a.longitude","v.city","v.state","v.capacity","v.field_type","v.name","v.time_zone","v.latitude", "v.longitude")]

#
#
#

#
#
#
#grab game stats for 2014 - 2015
q_body_2014 <- list(season_id='nfl-2014-2015')

gls_2014 <- ss_get_result(sport=sport, league=league, query=q_body_2014, ep=ep, walk=TRUE)  
game_logs_2014<-do.call('rbind', lapply(gls_2014, function(x) x$games))
game_logs_2014 <- game_logs_2014[!duplicated(game_logs_2014$id),]
game_logs_2014 <- game_logs_2014[,c("id","attendance","away_team_outcome","away_team_score","daytime","duration","home_team_outcome","home_team_score","humidity","interval_number","interval_type","label","name","on","score_differential","scoreline","slug","temperature","temperature_unit","timestamp","weather_conditions","wind_direction","wind_speed","home_team_id","away_team_id","season_id","venue_id","official_ids")]

#Make a dataframe of teams with important information
#Load Excel of missing Team Locations
library(readxl)
Team_Locations <- read_excel("~/LEVEL/Stattleship/Team_Locations.xlsx")
Team_Locations$latitude <- as.numeric(Team_Locations$latitude)

#Read in Teams from Stattleship
teams_2014 <- do.call('rbind', lapply(gls_2014, function(x) x$teams))
#get rid of duplicates
teams_2014 <- teams_2014[!duplicated(teams_2014$id),c("id","color","location","name","nickname","latitude","longitude","division_id","league_id")]
#Merge with dataframe of missing values
teams_2014 <- merge(teams_2014,Team_Locations, by.x = 'nickname', by.y = 'nickname', all.x = T)
#Filter out nas and make new columns
teams_2014[is.na(teams_2014)] <- 0
teams_2014$latitude <- teams_2014$latitude.x + teams_2014$latitude.y
teams_2014$longitude <- teams_2014$longitude.x + teams_2014$longitude.y
teams_2014 <- teams_2014[,c("nickname","id","color","location","name","division_id","league_id","latitude","longitude")]

#bring in venues
venues_2014 <- do.call('rbind', lapply(gls_2014, function(x) x$venues))
venues_2014 <- venues_2014[!duplicated(venues_2014$id),c("id","capacity","city","country","field_type","name","state","time_zone")]
venues_2014$stateabb <- setNames(state.abb, state.name)[venues_2014$state]
#add missing lat, and long
library(readr)
zip_codes_states <- read_csv("~/LEVEL/Stattleship/zip_codes_states.csv")
venues_2014[is.na(venues_2014)] <- 'UK'
venues_2014$state <- venues_2014$stateabb
Venue_Location_2014 <- merge(venues_2014, zip_codes_states, by.x = c('city','state'), by.y = c('city','state'), all.x = T)
Venue_Location_2014 <- Venue_Location_2014[!duplicated(Venue_Location_2014$name),c("city","state","id","capacity","field_type","name","time_zone","latitude","longitude")]

#Merge results with info on teams and venues
game_logs_2014 <- merge( game_logs_2014, teams_2014, by.x = c('home_team_id'), by.y = c('id'))
game_logs_2014 <- renameCol(game_logs_2014, c('nickname', 'color', 'location', 'name.y', 'division_id', 'league_id', 'latitude', 'longitude'), c('h.nickname', 'h.color','h.location', 'h.name', 'h.division_id', 'h.league_id', 'h.latitude', 'h.longitude'))
game_logs_2014 <- merge( game_logs_2014, teams_2014, by.x = c('away_team_id'), by.y = c('id'))
game_logs_2014 <- renameCol(game_logs_2014, c('nickname', 'color', 'location', 'name', 'division_id', 'league_id', 'latitude', 'longitude'), c('a.nickname', 'a.color','a.location', 'a.name', 'a.division_id', 'a.league_id', 'a.latitude', 'a.longitude'))
game_logs_2014 <- merge(game_logs_2014, Venue_Location_2014, by.x = c('venue_id'), by.y = c('id'), all.x = T)
game_logs_2014 <- renameCol(game_logs_2014, c("city","state","capacity","field_type","name","time_zone","latitude","longitude"),c("v.city","v.state","v.capacity","v.field_type","v.name","v.time_zone","v.latitude","v.longitude"))
game_logs_2014 <- game_logs_2014[,c("venue_id","away_team_id","home_team_id","id","attendance","away_team_outcome","away_team_score","daytime","duration","home_team_score","humidity","interval_number","label","name.x","on","score","score_differential","scoreline","temperature","timestamp","weather_conditions","wind_direction","wind_speed","winning_team_id","season_id","official_ids","h.nickname","h.location","h.name","h.division_id","h.league_id","h.latitude","h.longitude","a.nickname","a.location","a.name","a.division_id","a.league_id","a.latitude","a.longitude","v.city","v.state","v.capacity","v.field_type","v.name","v.time_zone","v.latitude", "v.longitude")]

#
#
#

#
#
#
#grab game stats for 2013 - 2014
q_body_2013 <- list(season_id='nfl-2013-2014')

gls_2013 <- ss_get_result(sport=sport, league=league, query=q_body_2013, ep=ep, walk=TRUE)  
game_logs_2013 <- do.call('rbind', lapply(gls_2013, function(x) x$games))
game_logs_2013 <- game_logs_2013[!duplicated(game_logs_2013$id),]
game_logs_2013 <- game_logs_2013[,c("id","attendance","away_team_outcome","away_team_score","daytime","duration","home_team_outcome","home_team_score","humidity","interval_number","interval_type","label","name","on","score_differential","scoreline","slug","temperature","temperature_unit","timestamp","weather_conditions","wind_direction","wind_speed","home_team_id","away_team_id","season_id","venue_id","official_ids")]

#Make a dataframe of teams with important information
#Load Excel of missing Team Locations
library(readxl)
Team_Locations <- read_excel("~/LEVEL/Stattleship/Team_Locations.xlsx")
Team_Locations$latitude <- as.numeric(Team_Locations$latitude)

#Read in Teams from Stattleship
teams_2013 <- do.call('rbind', lapply(gls_2013, function(x) x$teams))
#get rid of duplicates
teams_2013 <- teams_2013[!duplicated(teams_2013$id),c("id","color","location","name","nickname","latitude","longitude","division_id","league_id")]
#Merge with dataframe of missing values
teams_2013 <- merge(teams_2013,Team_Locations, by.x = 'nickname', by.y = 'nickname', all.x = T)
#Filter out nas and make new columns
teams_2013[is.na(teams_2013)] <- 0
teams_2013$latitude <- teams_2013$latitude.x + teams_2013$latitude.y
teams_2013$longitude <- teams_2013$longitude.x + teams_2013$longitude.y
teams_2013 <- teams_2013[,c("nickname","id","color","location","name","division_id","league_id","latitude","longitude")]

#bring in venues
venues_2013 <- do.call('rbind', lapply(gls_2013, function(x) x$venues))
venues_2013 <- venues_2013[!duplicated(venues_2013$id),c("id","capacity","city","country","field_type","name","state","time_zone")]
venues_2013$stateabb <- setNames(state.abb, state.name)[venues_2013$state]
#add missing lat, and long
library(readr)
zip_codes_states <- read_csv("~/LEVEL/Stattleship/zip_codes_states.csv")
venues_2013[is.na(venues_2013)] <- 'UK'
venues_2013$state <- venues_2013$stateabb
Venue_Location_2013 <- merge(venues_2013, zip_codes_states, by.x = c('city','state'), by.y = c('city','state'), all.x = T)
Venue_Location_2013 <- Venue_Location_2013[!duplicated(Venue_Location_2013$name),c("city","state","id","capacity","field_type","name","time_zone","latitude","longitude")]
#check for foreign venues and update zip_codes_states

#Merge results with info on teams and venues
game_logs_2013 <- merge( game_logs_2013, teams_2013, by.x = c('home_team_id'), by.y = c('id'))
game_logs_2013 <- renameCol(game_logs_2013, c('nickname', 'color', 'location', 'name.y', 'division_id', 'league_id', 'latitude', 'longitude'), c('h.nickname', 'h.color','h.location', 'h.name', 'h.division_id', 'h.league_id', 'h.latitude', 'h.longitude'))
game_logs_2013 <- merge( game_logs_2013, teams_2013, by.x = c('away_team_id'), by.y = c('id'))
game_logs_2013 <- renameCol(game_logs_2013, c('nickname', 'color', 'location', 'name', 'division_id', 'league_id', 'latitude', 'longitude'), c('a.nickname', 'a.color','a.location', 'a.name', 'a.division_id', 'a.league_id', 'a.latitude', 'a.longitude'))
game_logs_2013 <- merge(game_logs_2013, Venue_Location_2013, by.x = c('venue_id'), by.y = c('id'), all.x = T)
game_logs_2013 <- renameCol(game_logs_2013, c("city","state","capacity","field_type","name","time_zone","latitude","longitude"),c("v.city","v.state","v.capacity","v.field_type","v.name","v.time_zone","v.latitude","v.longitude"))
#game_logs_2013 <- game_logs_2013[,c("venue_id","away_team_id","home_team_id","id","attendance","away_team_outcome","away_team_score","daytime","duration","home_team_score","humidity","interval_number","label","name.x","on","score","score_differential","scoreline","temperature","timestamp","weather_conditions","wind_direction","wind_speed","winning_team_id","season_id","official_ids","h.nickname","h.location","h.name","h.division_id","h.league_id","h.latitude","h.longitude","a.nickname","a.location","a.name","a.division_id","a.league_id","a.latitude","a.longitude","v.city","v.state","v.capacity","v.field_type","v.name","v.time_zone","v.latitude", "v.longitude")]

#
#
#

#
#
#
#grab game stats for 2012 - 2013
q_body_2012 <- list(season_id='nfl-2012-2013')

gls_2012 <- ss_get_result(sport=sport, league=league, query=q_body_2012, ep=ep, walk=TRUE)  
game_logs_2012 <- do.call('rbind', lapply(gls_2012, function(x) x$games))
game_logs_2012 <- game_logs_2012[!duplicated(game_logs_2012$id),]
game_logs_2012 <- game_logs_2012[,c("id","attendance","away_team_outcome","away_team_score","daytime","duration","home_team_outcome","home_team_score","humidity","interval_number","interval_type","label","name","on","score_differential","scoreline","slug","temperature","temperature_unit","timestamp","weather_conditions","wind_direction","wind_speed","home_team_id","away_team_id","season_id","venue_id","official_ids")]

#Make a dataframe of teams with important information
#Load Excel of missing Team Locations
library(readxl)
Team_Locations <- read_excel("~/LEVEL/Stattleship/Team_Locations.xlsx")
Team_Locations$latitude <- as.numeric(Team_Locations$latitude)

#Read in Teams from Stattleship
teams_2012 <- do.call('rbind', lapply(gls_2012, function(x) x$teams))
#get rid of duplicates
teams_2012 <- teams_2012[!duplicated(teams_2012$id),c("id","color","location","name","nickname","latitude","longitude","division_id","league_id")]
#Merge with dataframe of missing values
teams_2012 <- merge(teams_2012,Team_Locations, by.x = 'nickname', by.y = 'nickname', all.x = T)
#Filter out nas and make new columns
teams_2012[is.na(teams_2012)] <- 0
teams_2012$latitude <- teams_2012$latitude.x + teams_2012$latitude.y
teams_2012$longitude <- teams_2012$longitude.x + teams_2012$longitude.y
teams_2012 <- teams_2012[,c("nickname","id","color","location","name","division_id","league_id","latitude","longitude")]

#bring in venues
venues_2012 <- do.call('rbind', lapply(gls_2012, function(x) x$venues))
venues_2012 <- venues_2012[!duplicated(venues_2012$id),c("id","capacity","city","country","field_type","name","state","time_zone")]
venues_2012$stateabb <- setNames(state.abb, state.name)[venues_2012$state]
#add missing lat, and long
library(readr)
zip_codes_states <- read_csv("~/LEVEL/Stattleship/zip_codes_states.csv")
venues_2012[is.na(venues_2012)] <- 'UK'
venues_2012$state <- venues_2012$stateabb
Venue_Location_2012 <- merge(venues_2012, zip_codes_states, by.x = c('city','state'), by.y = c('city','state'), all.x = T)
Venue_Location_2012 <- Venue_Location_2012[!duplicated(Venue_Location_2012$name),c("city","state","id","capacity","field_type","name","time_zone","latitude","longitude")]
#check for foreign venues and update zip_codes_states

#Merge results with info on teams and venues
game_logs_2012 <- merge( game_logs_2012, teams_2012, by.x = c('home_team_id'), by.y = c('id'))
game_logs_2012 <- renameCol(game_logs_2012, c('nickname', 'color', 'location', 'name.y', 'division_id', 'league_id', 'latitude', 'longitude'), c('h.nickname', 'h.color','h.location', 'h.name', 'h.division_id', 'h.league_id', 'h.latitude', 'h.longitude'))
game_logs_2012 <- merge( game_logs_2012, teams_2012, by.x = c('away_team_id'), by.y = c('id'))
game_logs_2012 <- renameCol(game_logs_2012, c('nickname', 'color', 'location', 'name', 'division_id', 'league_id', 'latitude', 'longitude'), c('a.nickname', 'a.color','a.location', 'a.name', 'a.division_id', 'a.league_id', 'a.latitude', 'a.longitude'))
game_logs_2012 <- merge(game_logs_2012, Venue_Location_2012, by.x = c('venue_id'), by.y = c('id'), all.x = T)
game_logs_2012 <- renameCol(game_logs_2012, c("city","state","capacity","field_type","name","time_zone","latitude","longitude"),c("v.city","v.state","v.capacity","v.field_type","v.name","v.time_zone","v.latitude","v.longitude"))
#game_logs_2012 <- game_logs_2012[,c("venue_id","away_team_id","home_team_id","id","attendance","away_team_outcome","away_team_score","daytime","duration","home_team_score","humidity","interval_number","label","name.x","on","score","score_differential","scoreline","temperature","timestamp","weather_conditions","wind_direction","wind_speed","winning_team_id","season_id","official_ids","h.nickname","h.location","h.name","h.division_id","h.league_id","h.latitude","h.longitude","a.nickname","a.location","a.name","a.division_id","a.league_id","a.latitude","a.longitude","v.city","v.state","v.capacity","v.field_type","v.name","v.time_zone","v.latitude", "v.longitude")]

#
#
#

#
#
#
#grab game stats for 2016 - 2017
q_body_2016 <- list(season_id='nfl-2016-2017')

gls_2016 <- ss_get_result(sport=sport, league=league, query=q_body_2016, ep=ep, walk=TRUE)  
game_logs_2016 <- do.call('rbind', lapply(gls_2016, function(x) x$games))
game_logs_2016 <- game_logs_2016[!duplicated(game_logs_2016$id),]
game_logs_2016 <- game_logs_2016[,c("id","attendance","away_team_outcome","away_team_score","daytime","duration","home_team_outcome","home_team_score","humidity","interval_number","interval_type","label","name","on","score_differential","scoreline","slug","temperature","temperature_unit","timestamp","weather_conditions","wind_direction","wind_speed","home_team_id","away_team_id","season_id","venue_id","official_ids")]

#Make a dataframe of teams with important information
#Load Excel of missing Team Locations
library(readxl)
Team_Locations <- read_excel("~/LEVEL/Stattleship/Team_Locations.xlsx")
Team_Locations$latitude <- as.numeric(Team_Locations$latitude)

#Read in Teams from Stattleship
teams_2016 <- do.call('rbind', lapply(gls_2016, function(x) x$teams))
#get rid of duplicates
teams_2016 <- teams_2016[!duplicated(teams_2016$id),c("id","color","location","name","nickname","latitude","longitude","division_id","league_id")]
#Merge with dataframe of missing values
teams_2016 <- merge(teams_2016,Team_Locations, by.x = 'nickname', by.y = 'nickname', all.x = T)
#Filter out nas and make new columns
teams_2016[is.na(teams_2016)] <- 0
teams_2016$latitude <- teams_2016$latitude.x + teams_2016$latitude.y
teams_2016$longitude <- teams_2016$longitude.x + teams_2016$longitude.y
teams_2016 <- teams_2016[,c("nickname","id","color","location","name","division_id","league_id","latitude","longitude")]

#bring in venues
venues_2016 <- do.call('rbind', lapply(gls_2016, function(x) x$venues))
venues_2016 <- venues_2016[!duplicated(venues_2016$id),c("id","capacity","city","country","field_type","name","state","time_zone")]
venues_2016$stateabb <- setNames(state.abb, state.name)[venues_2016$state]
#add missing lat, and long
library(readr)
zip_codes_states <- read_csv("~/LEVEL/Stattleship/zip_codes_states.csv")
venues_2016[is.na(venues_2016)] <- 'UK'
venues_2016$state <- venues_2016$stateabb
Venue_Location_2016 <- merge(venues_2016, zip_codes_states, by.x = c('city','state'), by.y = c('city','state'), all.x = T)
Venue_Location_2016 <- Venue_Location_2016[!duplicated(Venue_Location_2016$name),c("city","state","id","capacity","field_type","name","time_zone","latitude","longitude")]
#check for foreign venues and update zip_codes_states

#Merge results with info on teams and venues
game_logs_2016 <- merge( game_logs_2016, teams_2016, by.x = c('home_team_id'), by.y = c('id'))
game_logs_2016 <- renameCol(game_logs_2016, c('nickname', 'color', 'location', 'name.y', 'division_id', 'league_id', 'latitude', 'longitude'), c('h.nickname', 'h.color','h.location', 'h.name', 'h.division_id', 'h.league_id', 'h.latitude', 'h.longitude'))
game_logs_2016 <- merge( game_logs_2016, teams_2016, by.x = c('away_team_id'), by.y = c('id'))
game_logs_2016 <- renameCol(game_logs_2016, c('nickname', 'color', 'location', 'name', 'division_id', 'league_id', 'latitude', 'longitude'), c('a.nickname', 'a.color','a.location', 'a.name', 'a.division_id', 'a.league_id', 'a.latitude', 'a.longitude'))
game_logs_2016 <- merge(game_logs_2016, Venue_Location_2016, by.x = c('venue_id'), by.y = c('id'), all.x = T)
game_logs_2016 <- renameCol(game_logs_2016, c("city","state","capacity","field_type","name","time_zone","latitude","longitude"),c("v.city","v.state","v.capacity","v.field_type","v.name","v.time_zone","v.latitude","v.longitude"))
#game_logs_2016 <- game_logs_2016[,c("venue_id","away_team_id","home_team_id","id","attendance","away_team_outcome","away_team_score","daytime","duration","home_team_score","humidity","interval_number","label","name.x","on","score","score_differential","scoreline","temperature","timestamp","weather_conditions","wind_direction","wind_speed","winning_team_id","season_id","official_ids","h.nickname","h.location","h.name","h.division_id","h.league_id","h.latitude","h.longitude","a.nickname","a.location","a.name","a.division_id","a.league_id","a.latitude","a.longitude","v.city","v.state","v.capacity","v.field_type","v.name","v.time_zone","v.latitude", "v.longitude")]

#
#
#


#grab game stats for 2017-2018

#gls2017 <- ss_get_result(sport=sport, league=league, ep=ep, walk=TRUE)  
#gls_2017 <- gls2017
#
#
#
#grab game stats for 2017 - 2018
q_body_2017 <- list(season_id='nfl-2017-2018')

gls_2017 <- ss_get_result(sport=sport, league=league, query=q_body_2017, ep=ep, walk=TRUE) 
#gls_2017 <- gls2017 
game_logs_2017 <- do.call('rbind', lapply(gls_2017, function(x) x$games))
game_logs_2017 <- game_logs_2017[!duplicated(game_logs_2017$id),]
game_logs_2017 <- game_logs_2017[,c("id","attendance","away_team_outcome","away_team_score","daytime","duration","home_team_outcome","home_team_score","humidity","interval_number","interval_type","label","name","on","score_differential","scoreline","slug","temperature","temperature_unit","timestamp","weather_conditions","wind_direction","wind_speed","home_team_id","away_team_id","season_id","venue_id","official_ids")]

#Make a dataframe of teams with important information
#Load Excel of missing Team Locations
library(readxl)
Team_Locations <- read_excel("~/LEVEL/Stattleship/Team_Locations.xlsx")
Team_Locations$latitude <- as.numeric(Team_Locations$latitude)

#Read in Teams from Stattleship
teams_2017 <- do.call('rbind', lapply(gls_2017, function(x) x$teams))
#get rid of duplicates
teams_2017 <- teams_2017[!duplicated(teams_2017$id),c("id","color","location","name","nickname","latitude","longitude","division_id","league_id")]
#Merge with dataframe of missing values
teams_2017 <- merge(teams_2017,Team_Locations, by.x = 'nickname', by.y = 'nickname', all.x = T)
#Filter out nas and make new columns
teams_2017[is.na(teams_2017)] <- 0
teams_2017$latitude <- teams_2017$latitude.x + teams_2017$latitude.y
teams_2017$longitude <- teams_2017$longitude.x + teams_2017$longitude.y
teams_2017 <- teams_2017[,c("nickname","id","color","location","name","division_id","league_id","latitude","longitude")]

#bring in venues
venues_2017 <- do.call('rbind', lapply(gls_2017, function(x) x$venues))
venues_2017 <- venues_2017[!duplicated(venues_2017$id),c("id","capacity","city","country","field_type","name","state","time_zone")]
#venues_2017$stateabb <- setNames(state.abb, state.name)[venues_2017$state]
#add missing lat, and long
library(readr)
zip_codes_states <- read_csv("~/LEVEL/Stattleship/zip_codes_states.csv")
venues_2017[is.na(venues_2017)] <- 'UK'
#venues_2017$state <- venues_2017$stateabb
Venue_Location_2017 <- merge(venues_2017, zip_codes_states, by.x = c('city','state'), by.y = c('city','state'), all.x = T)
Venue_Location_2017 <- Venue_Location_2017[!duplicated(Venue_Location_2017$name),c("city","state","id","capacity","field_type","name","time_zone","latitude","longitude")]
#check for foreign venues and update zip_codes_states

#Merge results with info on teams and venues
game_logs_2017 <- merge( game_logs_2017, teams_2017, by.x = c('home_team_id'), by.y = c('id'))
game_logs_2017 <- renameCol(game_logs_2017, c('nickname', 'color', 'location', 'name.y', 'division_id', 'league_id', 'latitude', 'longitude'), c('h.nickname', 'h.color','h.location', 'h.name', 'h.division_id', 'h.league_id', 'h.latitude', 'h.longitude'))
game_logs_2017 <- merge( game_logs_2017, teams_2017, by.x = c('away_team_id'), by.y = c('id'))
game_logs_2017 <- renameCol(game_logs_2017, c('nickname', 'color', 'location', 'name', 'division_id', 'league_id', 'latitude', 'longitude'), c('a.nickname', 'a.color','a.location', 'a.name', 'a.division_id', 'a.league_id', 'a.latitude', 'a.longitude'))
game_logs_2017 <- merge(game_logs_2017, Venue_Location_2017, by.x = c('venue_id'), by.y = c('id'), all.x = T)
game_logs_2017 <- renameCol(game_logs_2017, c("city","state","capacity","field_type","name","time_zone","latitude","longitude"),c("v.city","v.state","v.capacity","v.field_type","v.name","v.time_zone","v.latitude","v.longitude"))
#game_logs_2017 <- game_logs_2017[,c("venue_id","away_team_id","home_team_id","id","attendance","away_team_outcome","away_team_score","daytime","duration","home_team_score","humidity","interval_number","label","name.x","on","score","score_differential","scoreline","temperature","timestamp","weather_conditions","wind_direction","wind_speed","winning_team_id","season_id","official_ids","h.nickname","h.location","h.name","h.division_id","h.league_id","h.latitude","h.longitude","a.nickname","a.location","a.name","a.division_id","a.league_id","a.latitude","a.longitude","v.city","v.state","v.capacity","v.field_type","v.name","v.time_zone","v.latitude", "v.longitude")]

#
#
#


#combine data for years 2012-2017
gamelogs <- rbind(game_logs_2012, game_logs_2013, game_logs_2014, game_logs_2015, game_logs_2016, game_logs_2017)

gamelogs$point_total <- gamelogs$away_team_score + gamelogs$home_team_score

#add identifier
gamelogs$identifier <- paste('At', gamelogs$h.name) 
gamelogs$on <- gsub("on ","", gamelogs$on)
gamelogs$on <- gsub("^.*?,","", gamelogs$on)
gamelogs$on <- gsub(" ","", gamelogs$on)
#gamelogs <- gamelogs[!(is.na(gamelogs$id)),]



### Vegas Odds Section of Code did not end up being used in this assignment
#You can skip over it


#bring in Vegas odds
setwd("~/LEVEL/Stattleship")
library(readxl)
X2016_Vegas_Lines1 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx")
Vegas_Lines1 <- as.data.frame(X2016_Vegas_Lines1)
X2016_Vegas_Lines2 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                 sheet = "Week2")
Vegas_Lines2 <- as.data.frame(X2016_Vegas_Lines2)
X2016_Vegas_Lines3 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                 sheet = "Week3")
Vegas_Lines3 <- as.data.frame(X2016_Vegas_Lines3)
X2016_Vegas_Lines4 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                 sheet = "Week4")
Vegas_Lines4 <- as.data.frame(X2016_Vegas_Lines4)
X2016_Vegas_Lines5 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                 sheet = "Week5")
Vegas_Lines5 <- as.data.frame(X2016_Vegas_Lines5)
X2016_Vegas_Lines6 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                 sheet = "Week6")
Vegas_Lines6 <- as.data.frame(X2016_Vegas_Lines6)
X2016_Vegas_Lines7 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                 sheet = "Week7")
Vegas_Lines7 <- as.data.frame(X2016_Vegas_Lines7)
X2016_Vegas_Lines8 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                 sheet = "Week8")
Vegas_Lines8 <- as.data.frame(X2016_Vegas_Lines8)
X2016_Vegas_Lines9 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                 sheet = "Week9")
Vegas_Lines9 <- as.data.frame(X2016_Vegas_Lines9)
X2016_Vegas_Lines10 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                  sheet = "Week10")
Vegas_Lines10 <- as.data.frame(X2016_Vegas_Lines10)
X2016_Vegas_Lines11 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                  sheet = "Week11")
Vegas_Lines11 <- as.data.frame(X2016_Vegas_Lines11)
X2016_Vegas_Lines12 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                  sheet = "Week12")
Vegas_Lines12 <- as.data.frame(X2016_Vegas_Lines12)
X2016_Vegas_Lines13 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                  sheet = "Week13")
Vegas_Lines13 <- as.data.frame(X2016_Vegas_Lines13)
X2016_Vegas_Lines14 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                  sheet = "Week14")
Vegas_Lines14 <- as.data.frame(X2016_Vegas_Lines14)
X2016_Vegas_Lines15 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                  sheet = "Week15")
Vegas_Lines15 <- as.data.frame(X2016_Vegas_Lines15)
X2016_Vegas_Lines16 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                  sheet = "Week16")
Vegas_Lines16 <- as.data.frame(X2016_Vegas_Lines16)
X2016_Vegas_Lines17 <- read_excel("~/LEVEL/Stattleship/2016_Vegas_Lines.xlsx", 
                                sheet = "week17")
Vegas_Lines17 <- as.data.frame(X2016_Vegas_Lines17)

#Form one dataframe of odds
Vegas_Lines <-rbind(Vegas_Lines1, Vegas_Lines2, Vegas_Lines3, Vegas_Lines4, Vegas_Lines5, Vegas_Lines6, Vegas_Lines7, Vegas_Lines8, Vegas_Lines9, Vegas_Lines10, Vegas_Lines11, Vegas_Lines12, Vegas_Lines13, Vegas_Lines14, Vegas_Lines15, Vegas_Lines16, Vegas_Lines17)
Vegas_Lines$identifier <- paste(Vegas_Lines$Favorite,',', Vegas_Lines$Underdog,',')
Vegas_Lines$identifier <- gsub("^.*?At ", "At ", Vegas_Lines$identifier)
Vegas_Lines$identifier <- substring(Vegas_Lines$identifier, 0, regexpr(',', Vegas_Lines$identifier)[]-1)
Vegas_Lines$identifier <- gsub("^.*?At ", "", Vegas_Lines$identifier)#Add an identifier for merge
Vegas_Lines$identifier <- trimws(Vegas_Lines$identifier, "r")

#Find Distances the teams Traveled

install.packages("geosphere")
library(geosphere)

p1 <- cbind(gamelogs$a.longitude, gamelogs$a.latitude)
p2 <- cbind(gamelogs$v.longitude, gamelogs$v.latitude)
gamelogs$away.team.trav <- distHaversine(p1,p2)

h1<- cbind(gamelogs$h.longitude, gamelogs$h.latitude)
gamelogs$home.team.trav <- distHaversine(h1,p2)
gamelogs$total.travel <- gamelogs$away.team.trav + gamelogs$home.team.trav
#lat1 <- gamelogs$a.latitude
#lon1 <- gamelogs$a.longitude
#lat2 <- gamelogs$v.latitude
#lon2 <- gamelogs$v.longitude
#gamelogs$away.team.trav <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(lon2-lon1)) * 6371000

#Merge gamelogs and Vegas odds
#gamelogs$on <- as.numeric(gamelogs$on)
#gamelogs$merge <- paste(gamelogs$on, gamelogs$interval_number, gamelogs$h.name)
#gamelogs$merge <- gsub(" ","", gamelogs$merge)
#Vegas_Lines$merge <- paste(Vegas_Lines$YEAR, Vegas_Lines$Week, Vegas_Lines$identifier)
#Vegas_Lines$merge <- gsub(" ","", Vegas_Lines$merge)

#Nfl_game_info <- merge( gamelogs, Vegas_Lines, by.x = c('identifier''on', 'interval_number'), by.y = c('identifier', 'YEAR', 'Week'))
#Nfl_game_info <- merge( gamelogs, Vegas_Lines, by.x = c('merge'), by.y = c('merge'))

#Find over or under
#Nfl_game_info$over.under <- Nfl_game_info$point_total - Nfl_game_info$Total

#Find attendance Percentage
#Nfl_game_info$att_pc <- (Nfl_game_info$attendance)/(Nfl_game_info$v.capacity)


#Same division game
#Nfl_game_info$same_div <- (Nfl_game_info$h.division_id == Nfl_game_info$a.division_id)

#Nfl <- Nfl_game_info[!duplicated(Nfl_game_info$id),]

#Nfl_tomerge <- Nfl



###
###Run Big_Plays_** script for years 12-17

#combine big play info into one data frame 2017
merge_12_17 <- rbind(merge_17,merge_16,merge_15,merge_14,merge_13,merge_12)

Nfl_12_17 <- merge(gamelogs, merge_12_17, by.x = 'id', by.y = 'game_id')

###Run some Plotting scripts
#Run humidity_wind_plot script
#Run Fieldtype boxplots
#Run Weather Replacements
#
#