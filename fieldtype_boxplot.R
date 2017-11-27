#Make a boxplot comparing Artificial Turf and Grass for some of the Metrics of Interest

library(ggplot2)



Fieldtype <- Nfl_12_17[,c('id', 'v.field_type', 'passing', 'recieving', 'rushing', 'fantasy')]
df.new <- melt(Fieldtype, id = c('id','v.field_type'))

p <- ggplot(data = df.new, aes(x=v.field_type, y=value)) + 
  geom_boxplot(aes(fill=v.field_type))
p <- p + facet_wrap( ~ variable, scales="free")
p <- p + xlab('Venue Field Type') + ylab('Value (Standardized Metric)') + ggtitle('Field Type on Various Skill Metrics')
p <- p + theme_bw()
p <- p + theme(strip.background=element_rect(fill="black"))
p <- p + theme(strip.text=element_text(color="white", face="bold"))
p <- p + guides(fill=guide_legend('Field Type'))
p <- p + scale_fill_manual(values= c('greenyellow','springgreen4'))
p
