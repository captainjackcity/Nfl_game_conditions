# Nfl_game_conditions
Analyze game conditions for the last 5 seasons. See if grass vs turf field has an impact on scoring. See if weather conditions have an impact on scoring. Create new metrics for skill plays including passing, receiving and rushing.

## Prerequisites
Download R and an IDE such as R studio
download Stattleship package from github ('stattleship/stattleship-r')
get API token from www.stattleship.com

## Install some packages for R 
install.packages("devtools")
install.packages("doBy")

## Load data from stattleship
run Retreival.R

add missing data from games including locations of stadiums

## Make new metrics and scale stats
Run Big_Plays.R for years 12-17

## Plot data to find abnormalities
run fieldtype_boxplot.R

format weather observations Weather_replacements.R
plot weather observations
fantasy_weather.R

## T-test for snow games
run t_distribution.R

Snow games have a higher mean of fantasy scores than other weather conditions
p= 0.026
