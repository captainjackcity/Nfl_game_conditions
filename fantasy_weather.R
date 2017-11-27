#Fantasy and Weather Conditions

FWeatherPlot <- ggplot(Nfl_12_17, aes(x = weather_conditions, y = fantasy, color=weather_conditions, fill = weather_conditions)) + 
  geom_boxplot()
FWeatherPlot <- FWeatherPlot + theme_classic()
FWeatherPlot <- FWeatherPlot + scale_y_continuous(name = "Fantasy Score (standardized metric)")
FWeatherPlot <- FWeatherPlot + scale_x_discrete(name = "Weather Condition")
FWeatherPlot <- FWeatherPlot + scale_fill_manual(values=c('white','white','white','white','white','white','white','white','royalblue3','white','white'), guide = 'none')
FWeatherPlot <- FWeatherPlot + theme(axis.text.x  = element_text(angle=90, vjust=0.5))
FWeatherPlot <- FWeatherPlot + ggtitle('Weather Conditions on Fantasy Scoring')
FWeatherPlot <- FWeatherPlot + theme(panel.background = element_rect(fill='lightblue'))
FWeatherPlot <- FWeatherPlot + geom_line(aes(colour = 'white')) + geom_hline(yintercept= 0, linetype= 'dotted')
FWeatherPlot

#Passing and Weather Conditions

PassWeatherPlot <- ggplot(Nfl_12_17, aes(x = weather_conditions, y = passing, color=weather_conditions, fill = weather_conditions)) +
  geom_boxplot()
  theme_classic()
PassWeatherPlot <- PassWeatherPlot + scale_y_continuous(name = "Passing Score (standardized metric)")
PassWeatherPlot <- PassWeatherPlot + scale_x_discrete(name = "Weather Condition")
PassWeatherPlot <- PassWeatherPlot + scale_fill_manual(values=c('white','white','white','white','white','black','white','white','white','white','white'), guide = 'none')
PassWeatherPlot <- PassWeatherPlot + theme(axis.text.x  = element_text(angle=90, vjust=0.5))
PassWeatherPlot <- PassWeatherPlot + ggtitle('Weather Conditions on Passing')
PassWeatherPlot <- PassWeatherPlot + theme(panel.background = element_rect(fill='azure'))
PassWeatherPlot <- PassWeatherPlot + geom_line(aes(colour = 'white')) + geom_hline(yintercept= 0, linetype= 'dotted')
PassWeatherPlot

#Receiving and Weather

RecievingWeatherPlot <- ggplot(Nfl_12_17, aes(x = weather_conditions, y = recieving, color=weather_conditions, fill = weather_conditions)) +
  geom_boxplot()
RecievingWeatherPlot <- RecievingWeatherPlot + theme_classic()
RecievingWeatherPlot <- RecievingWeatherPlot + scale_y_continuous(name = "Recieving Score (standardized metric)")
RecievingWeatherPlot <- RecievingWeatherPlot + scale_x_discrete(name = "Weather Condition")
RecievingWeatherPlot <- RecievingWeatherPlot + scale_fill_manual(values=c('white','white','white','white','white','black','white','white','white','white','white'), guide = 'none')
RecievingWeatherPlot <- RecievingWeatherPlot + theme(axis.text.x  = element_text(angle=90, vjust=0.5))
RecievingWeatherPlot <- RecievingWeatherPlot + ggtitle('Weather Conditions on Recieving')
RecievingWeatherPlot <- RecievingWeatherPlot + theme(panel.background = element_rect(fill='aliceblue'))
RecievingWeatherPlot <- RecievingWeatherPlot + geom_line(aes(colour = 'white')) + geom_hline(yintercept= 0, linetype= 'dotted')
RecievingWeatherPlot

#Rushing and Weather

rushingWeatherPlot <- ggplot(Nfl_12_17, aes(x = weather_conditions, y = rushing, color=weather_conditions, fill = weather_conditions)) +
  geom_boxplot()
rushingWeatherPlot <- rushingWeatherPlot + theme_classic()
rushingWeatherPlot <- rushingWeatherPlot + scale_y_continuous(name = "Rushing Score (standardized metric)")
rushingWeatherPlot <- rushingWeatherPlot + scale_x_discrete(name = "Weather Condition")
rushingWeatherPlot <- rushingWeatherPlot + scale_fill_manual(values=c('indianred2','white','white','steelblue','white','white','white','white','seagreen3','white','white'), guide = 'none')
rushingWeatherPlot <- rushingWeatherPlot + theme(axis.text.x  = element_text(angle=90, vjust=0.5))
rushingWeatherPlot <- rushingWeatherPlot + ggtitle('Weather Conditions on Rushing')
rushingWeatherPlot <- rushingWeatherPlot + theme(panel.background = element_rect(fill='aliceblue'))
rushingWeatherPlot <- rushingWeatherPlot + geom_line(aes(colour = 'white')) + geom_hline(yintercept= 0, linetype= 'dotted')
rushingWeatherPlot


#make df with big play data (only 16 and 17)
bigplaydata <- rbind(Compare_16, Compare_17)
#filter weather
bigplaydata$weather_conditions <- gsub('Partly Cloudy ', 'partly-cloudy', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('partly-cloudy skies', 'partly-cloudy', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('Mist', 'light-rain', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('Light rain', 'light-rain', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('Heavy rain', 'heavy-rain', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('Sunny', 'sunny', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('Sunny skies', 'sunny', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('Clear ', 'clear', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('Overcast skies', 'Overcast', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('Patchy light snow', 'Light snow', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('Patchy light rain with thunder', 'light-rain', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('Clear', 'clear', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('Partly cloudy skies', 'partly-cloudy', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('Partly cloudy', 'partly-cloudy', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('sunny skies', 'sunny', bigplaydata$weather_conditions)
bigplaydata$weather_conditions <- gsub('Patchy light rain', 'light-rain', bigplaydata$weather_conditions)

weathercond.names <- unique(bigplaydata$weather_conditions)
weathercond.names

# Big Plays and Weather

bigplayWeatherPlot <- ggplot(bigplaydata, aes(x = weather_conditions, y = bigplays, color=weather_conditions, fill = weather_conditions)) +
  geom_boxplot()
bigplayWeatherPlot <- bigplayWeatherPlot + theme_classic()
bigplayWeatherPlot <- bigplayWeatherPlot + scale_y_continuous(name = "Bigplay Score (standardized metric)")
bigplayWeatherPlot <- bigplayWeatherPlot + scale_x_discrete(name = "Weather Condition")
bigplayWeatherPlot <- bigplayWeatherPlot + scale_fill_manual(values=c('white','white','white','white','white','white','white','white','white','white','white'), guide = 'none')
bigplayWeatherPlot <- bigplayWeatherPlot + theme(axis.text.x  = element_text(angle=90, vjust=0.5))
bigplayWeatherPlot <- bigplayWeatherPlot + ggtitle('Weather Conditions on Bigplays')
bigplayWeatherPlot <- bigplayWeatherPlot + theme(panel.background = element_rect(fill='aliceblue'))
bigplayWeatherPlot <- bigplayWeatherPlot + geom_line(aes(colour = 'white')) + geom_hline(yintercept= 0, linetype= 'dotted')
bigplayWeatherPlot


