ciudades <- read.csv("ciudades.csv")
library(ggplot2)
library(dplyr)

## it would be better to do this with the normalized currency
ciudadesitpc <- select(ciudades, ciudad1, pais_c, ing_hogar,itpc)
ciudadesitpc$ciudad1 = as.factor(ciudadesitpc$ciudad1)

library(lattice)
options(scipen=5)

#png("itpc_violin_plot.png", width = 1200, height = 600)
  bwplot(ciudadesitpc$itpc~ciudadesitpc$ciudad1,
       xlab='count', ylab='Income per capita in local currency',
       outer=TRUE, as.table=TRUE, horizontal=FALSE,
       col='cyan4',
       panel=panel.violin)
#dev.off()