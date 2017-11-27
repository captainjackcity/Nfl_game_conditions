#test heavy rain and snow

rushing <- hist(Nfl_12_17$rushing, breaks = 30)
curve(dnorm(, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

g = Nfl_12_17$rushing
m<-mean(g)
std<-sqrt(var(g))
hist(g, breaks=30, prob=TRUE, 
     xlab="Rushing Value",  
     main="Rushing with T-distribution Curve",
     ylim= c(0, 0.2))
#curve(dnorm(x, mean=m, sd=std), 
      #col="darkblue", lwd=2, add=TRUE, yaxt="n")
snowrush <- Nfl_12_17[which(Nfl_12_17$weather_conditions == 'snow'),'rushing']
plot(function(x) dt(x, df = 12), -5, 5, ylim = c(0, 0.4),
     main = "T - Density", yaxs = "i", add = TRUE)
curve( dt(x, df=12), add=TRUE, col='blue' )
summary(snowrush)
sd(snowrush)
t_snowrush <- (mean(snowrush)-mean(Nfl_12_17$rushing))/(sd(snowrush)/sqrt(length(snowrush))) 
t_snowrush
1-pt(abs(t_snowrush), df= length(snowrush)-1)
m
lines(snowrush, dt(snowrush,df = 12)
 
#How to shade under a curve      
           
plot(function(x) dt(x, df = 12), -5, 5, ylim = c(0, 0.4),
           main = "T - Density", yaxs = "i")     
x <- seq(t_snowrush,5, len= 100)
y <- dt(x,12)
polygon(c(x[1], x, x[100]), c(dt(-5, 12), y, dt(5, 12)),
        col = "red", border = NA)

#
#
#
#Two sample t-test for fantasy score and snow

#first F test for sample variances
snowfantasy <- Nfl_12_17[which(Nfl_12_17$weather_conditions == 'snow'),'fantasy']

x <- snowfantasy
y <- Nfl_12_17$fantasy

var.test(x,y)

t.test(x,y, alternative = "greater", var.equal = TRUE, paired = FALSE)

qt(0.95, 12)


plot(function(x) dt(x, df = 12), -5, 5, ylim = c(0, 0.4),
     main = "T - Density plot", sub = 'Snow vs. all Weather', yaxs = "i", xlab = 't-value')     
x <- seq(1.9344,5, len= 100)
y <- dt(x,12)
polygon(c(x[1], x, x[100]), c(dt(-5, 12), y, dt(5, 12)),
        col = "red", border = NA)
