install.packages("ggplot2")
install.packages("ggrepel")

library(ggplot2)
library(ggrepel)
city_locations  <-read.csv("worldcities_locations.csv")

world <- map_data("world")
lac<- subset(world,region %in% c("Bahamas", "Barbados", "Belize", "Bolivia", 
                                 "Costa Rica", "Dominican Republic", "Ecuador", 
                                 "El Salvador", "Guatemala", "Guyana", 
                                 "Haiti", "Honduras", "Jamaica", "Nicaragua", 
                                 "Panama", "Paraguay", "Suriname", "Trinidad and Tobago",
                                 "Uruguay", "Argentina", "Brazil", "Chile", "Colombia", 
                                 "Mexico", "Peru", "Venezuela"))
p<-ggplot(lac, aes(long, lat, group=group)) + 
  geom_polygon(fill = "white", colour = "grey50")+
  coord_equal()+
  geom_point(data = city_locations,aes(lng,lat), inherit.aes = FALSE, colour = "coral1")+
  theme(legend.position="none")+
  geom_label(data=city_locations, geom_label())
  ggtitle("Cities included in study")
p + geom_text(data = city_locations,mapping = aes(x= lng, y= lat, label = city),inherit.aes = FALSE
                , nudge_x = 3.5, nudge_y = -0.25, size = 3, check_overlap = T)  



png("cities.png")
p  
dev.off()


### Original group of cities

##city_locations_old <-subset(city_locations, !(city %in% c("Cali","Monterrey","Medellín","Guadalajara", "Rio de Janeiro")))

#png("cities_original.png")
#cities_original <- ggplot(lac, aes(long, lat, group=group)) + 
#  geom_polygon(fill = "white", colour = "grey50")+
#  coord_equal()+
#  theme(legend.position="none")+
#  geom_point(data = city_locations_old,aes(lng,lat), inherit.aes = FALSE, colour = "coral1")+
#  ggtitle("Cities included in study") 
#dev.off()


p <- ggplot(lac, aes(long, lat, group=group)) + 
  geom_polygon(fill = "white", colour = "grey50")+
  coord_equal()+
  geom_point(data = city_locations,aes(lng,lat), inherit.aes = FALSE, colour = "coral1")+
  theme(legend.position="none")+
  ggtitle("Cities included in study")

  

               
               
                 