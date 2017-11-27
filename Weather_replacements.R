weather.cond.names <- unique(Nfl_12_17$weather_conditions)
weather.cond.names


Nfl_12_17$weather_conditions <- gsub('Partly Cloudy ', 'partly-cloudy', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('partly-cloudy skies', 'partly-cloudy', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('Mist', 'light-rain', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('Light rain', 'light-rain', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('Heavy rain', 'heavy-rain', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('Sunny', 'sunny', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('Sunny skies', 'sunny', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('Clear ', 'clear', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('Overcast skies', 'Overcast', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('Patchy light snow', 'Light snow', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('Patchy light rain with thunder', 'light-rain', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('Clear', 'clear', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('Partly cloudy skies', 'partly-cloudy', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('Partly cloudy', 'partly-cloudy', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('sunny skies', 'sunny', Nfl_12_17$weather_conditions)
Nfl_12_17$weather_conditions <- gsub('Patchy light rain', 'light-rain', Nfl_12_17$weather_conditions)

weather.cond.names <- unique(Nfl_12_17$weather_conditions)
weather.cond.names

boxplot( fantasy ~ weather_conditions, data = Nfl_12_17)
abline(h = mean(Nfl_12_17$fantasy))
#snow
boxplot( passing ~ weather_conditions, data = Nfl_12_17)
#light snow
boxplot( recieving ~ weather_conditions, data = Nfl_12_17)
#light snow
boxplot( rushing ~ weather_conditions, data = Nfl_12_17)
#snow
boxplot( puntreturns ~ weather_conditions, data = Nfl_12_17)
boxplot( kickreturns ~ weather_conditions, data = Nfl_12_17)
boxplot (bigplays ~ weather_conditions, data = Nfl_12_17 )

#Grass vs. Turf
field_types <- unique(Nfl_12_17$v.field_type)
field_types
Nfl_12_17$v.field_type <- gsub('turf', 'Turf', Nfl_12_17$v.field_type)
Nfl_12_17$v.field_type <- gsub('artificial', 'Artificial', Nfl_12_17$v.field_type)
Nfl_12_17$v.field_type <- gsub('FieldTurf', 'Artificial', Nfl_12_17$v.field_type)
Nfl_12_17$v.field_type <- gsub('Turf', 'Artificial', Nfl_12_17$v.field_type)
Nfl_12_17$v.field_type <- gsub('FieldArtificial', 'Artificial', Nfl_12_17$v.field_type)

boxplot( fantasy ~ v.field_type, data = Nfl_12_17)
boxplot( passing ~ v.field_type, data = Nfl_12_17 )
