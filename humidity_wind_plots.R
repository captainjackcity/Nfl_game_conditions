#Wind Speed and Humidity Plots

plot(Nfl_12_17$wind_speed, Nfl_12_17$fantasy)
summary(lm(fantasy ~ wind_speed, data= Nfl_12_17))$r.squared
plot(Nfl_12_17$wind_speed, Nfl_12_17$passing)
summary(lm(passing ~ wind_speed, data= Nfl_12_17))$r.squared
plot(Nfl_12_17$wind_speed, Nfl_12_17$recieving)
summary(lm(recieving ~ wind_speed, data= Nfl_12_17))$r.squared
plot(Nfl_12_17$wind_speed, Nfl_12_17$rushing)
summary(lm(rushing ~ wind_speed, data= Nfl_12_17))$r.squared

abline(h= mean(Nfl_12_17$fantasy))

#windplot facetwrap

Windplot <- Nfl_12_17[,c('id', 'wind_speed', 'passing', 'recieving', 'rushing', 'fantasy')]
df.new <- melt(Windplot, id = c('id','wind_speed'))

k <- ggplot(data = df.new, aes(x=wind_speed, y=value)) + 
  geom_point(aes())
k <- k + facet_wrap( ~ variable, scales="free")
k <- k + xlab('Wind Speed') + ylab('Value (Standardized Metric)') + ggtitle('Wind on Various Skill Metrics')
k <- k + theme_bw()
#k <- k + theme(strip.background=element_rect(fill="black"))
k <- k + theme(strip.text=element_text(color="black", face="bold"))
k <- k + geom_hline(yintercept = 0)
k

#Humidity facetwrap
Humidityplot <- Nfl_12_17[,c('id', 'humidity', 'passing', 'recieving', 'rushing', 'fantasy')]
df.new <- melt(Humidityplot, id = c('id','humidity'))

z <- ggplot(data = df.new, aes(x=humidity, y=value)) + 
  geom_point(aes())
z <- z + facet_wrap( ~ variable, scales="free")
z <- z + xlab('Humidity') + ylab('Value (Standardized Metric)') + ggtitle('Humidity on Various Skill Metrics')
z <- z + theme_bw()
#z <- z + theme(strip.background=element_rect(fill="black"))
z <- z + theme(strip.text=element_text(color="black", face="bold"))
z <- z + geom_hline(yintercept = 0)

z


plot(Nfl_12_17$humidity, Nfl_12_17$fantasy)
summary(lm(fantasy ~ humidity, data= Nfl_12_17))$r.squared
plot(Nfl_12_17$humidity, Nfl_12_17$passing)
summary(lm(passing ~ humidity, data= Nfl_12_17))$r.squared
plot(Nfl_12_17$humidity, Nfl_12_17$recieving)
summary(lm(recieving ~ humidity, data= Nfl_12_17))$r.squared
plot(Nfl_12_17$humidity, Nfl_12_17$rushing)
summary(lm(rushing ~ humidity, data= Nfl_12_17))$r.squared
