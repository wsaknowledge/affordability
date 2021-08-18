ciudades <- read.csv("ciudades.csv")
##must be run after running affordability script

library(ggplot2)
library(dplyr)
library(ggplot2)

## it would be better to do this with the normalized currency
ciudades_simple$ciudad1 <- as.factor(ciudades_simple$ciudad1)

library(lattice)
options(scipen=5)

itpcplot<- bwplot(ciudades_simple$itpc_usd_2018 ~ ciudades_simple$ciudad1, do.out = TRUE,
                  xlab='City', ylab='Income per capita in USD 2018 equivalent',
                  outer=TRUE, as.table=TRUE, horizontal=FALSE,
                  col='cyan4',
                  ylim=c(-20, 10000),
                  ##panel=panel.violin, ## this changes it from a boxplot to a violin plot.
                  par.settings = list(box.rectangle=list(col='black'),
                                      plot.symbol = list(pch='.', cex = 0.1)),
                  scales=list(x=list(rot=45, cex=0.5)))

#png("itpc_violin_plot.png", width = 1200, height = 600)
itpcplot
#dev.off()

ciudades_simple$ciudad1 <- as.factor(ciudades_simple$ciudad1)

ggplot(ciudades_simple, aes(x=ciudad1, y=itpc_usd_2018, color = pais_c)) + 
  ylim(0, 2000) +
  geom_violin(trim=FALSE, fill='lightgrey', color="cyan4")+
  geom_boxplot(width=0.1)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(title = "Income per capita in featured cities", x = "Cities", y = "Income per capita, USD 2018 equivalent", color = "Country")