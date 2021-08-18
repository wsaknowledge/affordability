## Affordability calculations
# Author darcia datshkovsky
# Contribuiters: jesse Libra, Holt Williamson and Maria Perez
## Created: Originally created February 2021
## Modified August 2021

rm(list=ls())
library(foreign)
library(dplyr)
library(tidyverse)
library(haven)
library(pryr)
library(ggplot2)
library(survey)
library(reshape2)
library(data.table)
library(gridExtra)
library(wesanderson)
library(ggsci)
library(devtools)
library(lattice)
library(cowplot)
library(writexl)


# This dataset adds to the original DF file and merges to the Jefes dataset used dor OLAS and avlidation, see code clean dataset for that"

##Create income
load(file="merged2.rda")

DF$ylm_ci<-ifelse(is.na(DF$ylm_ci),0,DF$ylm_ci)
DF$ylnm_ci<-ifelse(is.na(DF$ylnm_ci),0,DF$ylnm_ci)
DF$ynlm_ci<-ifelse(is.na(DF$ynlm_ci),0,DF$ynlm_ci)
DF$ynlnm_ci<-ifelse(is.na(DF$ynlnm_ci),0,DF$ynlnm_ci)

###ylm_ci ingreso laboral monetario individual
###ylnm_ci ingreso laboral no monetario individual
###ynlm_ci ingreso no laboral monetario individual
###ynlnm_ci ingreso no laboral no monetario individual




###generate total person income
table(is.na(DF$factor_ch),DF$pais_c)
DF$ing<- DF$ylm_ci+DF$ylnm_ci+DF$ynlm_ci+DF$ynlnm_ci

###Replace household fatcors with individual ones, as they are same it is just depends on base

DF$factor_ch <- ifelse(is.na(DF$factor_ch), DF$factor_ci, DF$factor_ch)
###create household income
ingreso<- DF %>%
  group_by(pais_c,idh_ch,nmiembros_ch,zona_c,region_c,factor_ch) %>%
  summarize(ing_hogar=sum(ing))


#Divide by number of people per household

ingreso<- ingreso%>%
  mutate(itpc=sum(ing_hogar/nmiembros_ch))
ingresoh<- ingreso%>%
  filter(jefe_ci)
# cREATE quintiles

##This part adds specific information missing from DF dataset, especially for geographic information at the city level
library(data.table)
fread

mexico<-read_dta("MEX_2018m8_m12_BID.dta")
jefemex <- mexico %>%
  filter(jefe_ci==1)
myvars<- c("idh_ch","ubica_geo","pais_c")
jefemex1<-jefemex[myvars]
rm(jefemex,mexico)

brasil<-read_dta("BRA_2019a_BID.dta")
jefebra <- brasil %>%
  filter(jefe_ci==1)
myvars<- c("idh_ch","rm_ride","pais_c")
jefebra1<-jefebra[myvars]
rm(jefebra,brasil)

colombia<-read_dta("COL_2018t3_BID.dta")
jefecol<- colombia %>%
  filter(jefe_ci==1)
myvars<- c("idh_ch","area","pais_c")
jefecol1<-jefecol[myvars]
rm(jefecol,colombia)




load("jefesmergedclean.rda")
##This merges to original jefes
jefe1<-merge(jefemex1,jefes,by=c("pais_c","idh_ch"), all=TRUE)
##rm(jefemex1)
jefe2<-merge(jefecol1,jefe1,by=c("pais_c","idh_ch"), all=TRUE)
jefe<-merge(jefebra1,jefe2,by=c("pais_c","idh_ch"), all=TRUE)
##rm(jefebra1)
##testing results
table(jefebra1$region_c)
table(jefe$pais_c,jefe$ubica_geo)

##This creates a list of important cities
jefe$ciudad1 <- ifelse(jefe$region_c==53,"Brasilia",
                ifelse(jefe$pais_c=="BRA" & jefe$rm_ride==35,"Sao Paulo",
                       ifelse(jefe$pais_c=="BRA" & jefe$rm_ride==33,"Rio de Janeiro",
                 ifelse(jefe$pais_c=="BOL" & jefe$zona_c==1 & jefe$region_c==2,"La Paz", 
                 ifelse(jefe$pais_c=="CHL" & jefe$region_c==13,"Santiago",
                 ifelse(jefe$pais_c=="CHL" & jefe$region_c==2 & jefe$zona_c==1,"Antofagasta",
                  ifelse(jefe$pais_c=="CHL" & jefe$region_c==4 & jefe$zona_c==1,"La Serena",
                  ifelse(jefe$pais_c=="COL" & jefe$region_c==11,"Bogota",
                  ifelse(jefe$pais_c=="COL" & jefe$area==05,"Medellin",
                  ifelse(jefe$pais_c=="COL" & jefe$area==76,"Cali",
                  ifelse(jefe$pais_c=="CRI" & jefe$region_c==1 & jefe$zona_c==1,"San Jose",
                  ifelse(jefe$pais_c=="ECU" & jefe$region_c==17 & jefe$zona_c==1, "Quito",
                 ifelse(jefe$pais_c=="HND" & jefe$region_c==8 & jefe$zona_c==1,"Tegucigalpa",
                 ifelse(jefe$pais_c=="MEX" & jefe$region_c==9,"CDMX",
                  ifelse(jefe$pais_c=="MEX" & jefe$ubica_geo==15033,"Ecatepec",
                  ifelse(jefe$pais_c=="MEX" & jefe$ubica_geo==15013,"Atizapan de Zaragoza",
                         ifelse(jefe$pais_c=="MEX" & jefe$ubica_geo==19039,"Monterrey",
                                ifelse(jefe$pais_c=="MEX" & jefe$ubica_geo==14039,"Guadalajara",                         
                  ifelse(jefe$pais_c=="PAN" & jefe$region_c==8 & jefe$zona_c==1,"Panama",
                  ifelse(jefe$pais_c=="PER" & jefe$region_c%in%c(15,7) & jefe$zona_c==1,"Lima",
                  ifelse(jefe$pais_c=="PRY" & jefe$region_c==1, "Asuncion",
                  ifelse(jefe$pais_c=="URY" & jefe$region_c==1, "Montevideo",0))))))))))))))))))))))

table(jefe$ciudad1)
##Filter cities
ciudades <- jefe %>%
  filter(ciudad1!=0)
write.csv(ciudades,"ciudades.csv")
##This part of code was an attempt to get the tariffds in R 
#tarifa<-as.data.frame(fread("income.csv"))
#ciudadtarifa<- merge(ciudades,tarifa,by="ciudad1", all=TRUE)

#ciudadtarifa$porciento<- ciudadtarifa$'`18 m3`'/4*ciudadtarifa$itpc

#ciudad <- ciudades %>%
  #summarize(city=count(ciudad1))

#lapaz<- ciudades %>%
 # filter(pais_c=="BOL")

#save(ciudades, file = "cities.Rda")
#table(ciudades$ciudad1)

## NORMALIZING INCOMES TO 2018 USD

  #### pulling from world bank and IMF data in excel workbook and merging
    install.packages("readxl")
    library(readxl)
    PPP<-read_excel("xchange.xls", sheet ="WB PPP 2018") ####this info is for 2018
    LCU<-read_excel("xchange.xls", sheet ="WB DOLLAR LCU 2018") ####this info is for 2018
    IMF<-read_excel("xchange.xls", sheet ="IMF xchange") ####this info is for 2018
    
    library(dplyr)
    LCU <- rename(LCU, lcu2018 = `2018`)
    PPP <- PPP %>% rename(ppp2018 = `2018`)
    IMF <- IMF %>% rename(imf2018 = xchange)
    
  #### world bank data is missing information for LCU (USD conversion), references IMF data for missing information, bolivia comes from xc.com
    xchangelcu<-merge(LCU, IMF, by="Country Code", all.x = TRUE)
    xchangelcu$xr<-ifelse(is.na(xchangelcu$lcu2018),xchangelcu$imf2018, xchangelcu$lcu2018)
    
  #simplify dataframe with only necessary fields
    xchangelcuimf <- xchangelcu[, c("Country Code", "lcu2018","imf2018", "xr")]
    
  
  # pull inflation figures
    infl<-read_excel("xchange.xls", sheet ="WB inflation")
    infl <- rename(infl, infl_2019 = `2019`, infl_2017 = `2017` )
  # simplify by only keeping necessary columns
    inflation <- infl[, c("Country Code", "infl_2019","infl_2017")]
  
  
  # deflate for 2019 data, inflate for 2017 data
    xchange<- merge(PPP,xchangelcuimf, by="Country Code", all.x = TRUE)
  
    economicstats<-merge(xchange,inflation, by= "Country Code", all.x = TRUE)
  
    ciudades <- left_join(ciudades, economicstats, by = c("pais_c" = "Country Code"))
  
  ##2018 inflation
    ciudades$year <-ifelse(ciudades$pais_c=="BRA",2019,
                           ifelse(ciudades$pais_c %in% c("CHL","ECU","PRY"),2017,
                                  ifelse(ciudades$pais_c=="JAM",2015,2018)))
    
    ciudades$infl_factor <-ifelse(ciudades$year=="2019",1/(1+ (ciudades$infl_2019/100)),
                                  ifelse(ciudades$year=="2017",1+(ciudades$infl_2017/100),
                                         1))
      ## quick check / validation    
        g <- ciudades$pais_c
        z<- split(ciudades, g)
        z$BRA
        z$CHL
        z$ECU
        z$PRY
    
  ## multiply itpc and hhincome by inflation factor to get 2018 values
  
    ciudades$itpc_2018 <- ciudades$itpc*ciudades$infl_factor
    ciudades$ing_hogar_2018 <- ciudades$ing_hogar*ciudades$infl_factor
    
  ## USD conversion
    ciudades$itpc_usd_2018 <- ciudades$itpc_2018/ciudades$xr
    ciudades$ing_hogar_usd_2018 <- ciudades$ing_hogar_2018/ciudades$xr
min(ciudades$itpc_usd_2018)    

   

 ##Violin Plots
    ##simplify dataframe for graphics
    ciudades_simple <- ciudades[, c("pais_c","idh_ch", "ciudad1", "nmiembros_ch", "ing_hogar_usd_2018","itpc_usd_2018")]
    ciudades_simple$ciudad1 <- as.factor(ciudades_simple$ciudad1)
    
    library(lattice)
    options(scipen=5)
    
    itpcplot<- bwplot(ciudades_simple$itpc_usd_2018 ~ ciudades_simple$ciudad1, do.out = TRUE,
                   xlab='City', ylab='Income per capita in USD 2018 equivalent',
                   outer=TRUE, as.table=TRUE, horizontal=FALSE,
                   col='cyan4',
                   ylim=c(-20, 20000),
                   ##panel=panel.violin, ## this changes it from a boxplot to a violin plot.
                   par.settings = list(box.rectangle=list(col='black'),
                                       plot.symbol = list(pch='.', cex = 0.1)),
                   scales=list(x=list(rot=45, cex=0.5)))
    library(ggplot2)
    itpcplot2 <- ggplot(ciudades_simple, aes(x=ciudad1, y=itpc_usd_2018, color = pais_c)) + 
                   ylim(0, 2000) +
                   geom_violin(trim=FALSE, fill='lightgrey', color="cyan4")+
                   geom_boxplot(width=0.1)+
                   scale_x_discrete(guide = guide_axis(n.dodge = 2))+
                   labs(title = "Income per capita in featured cities", x = "Cities", y = "Income per capita, USD 2018 equivalent", color = "Country")
   
        ##the y axis on the above violin plot is limited to 2k to see detail.
    
    
    png("itpc_boxplot_plot.png", width = 1200, height = 600)
    itpcplot
    dev.off()
    png("itpc_violin_plot.png", width = 1200, height = 600)
    itpcplot2
    dev.off()




##This creates income quintiles using hutils, factor_ch are weights, uses household per capita income
install.packages("hutils")
library(hutils)
cities<-mutate_ntile(ciudades, "itpc", n=5, weights = "factor_ch", by = "ciudad1", keyby = NULL,
                          new.col = "quintilpc", character.only = TRUE, overwrite = TRUE,
                          check.na = FALSE)

### could not find a function to do weighted medians, so instead did two xtiles, then got max and min point of these to get cutoff point.
median<-mutate_ntile(ciudades, "itpc", n=2, weights = "factor_ch", by = "ciudad1", keyby = NULL,
                     new.col = "median", character.only = TRUE, overwrite = TRUE,
                     check.na = FALSE)

table(ciudades$ciudad)
## add normalization stuff here


library(dplyr)
summaryquintil<-cities %>%
  group_by(pais_c,ciudad1,quintilpc) %>%
  summarize(ave_income_pc=weighted.mean(itpc, factor_ch,na.rm=TRUE),
            ave_income_pcna=weighted.mean(itpc, factor_ch,na.rm=FALSE),
            medianincome=median(itpc,weights(factor_ch),na.rm=TRUE),
            homesize=weighted.mean(nmiembros_ch, factor_ch,na.rm=TRUE),
            access=weighted.mean(pipedprem, factor_ch,na.rm=TRUE), ###change to piped water to plot instead
            sewer=weighted.mean(sewer, factor_ch,na.rm=TRUE))

summarymedianl<-median %>%
  group_by(pais_c,ciudad1,median) %>%
  summarize(minimo=min(itpc, na.rm=TRUE),
            maximo=max(itpc,na.rm=TRUE))

summarymedianl$points<- ifelse(summarymedianl$median==1,summarymedianl$maximo,
summarymedianl$minimo)
mediana<- summarymedianl %>%
  group_by(pais_c,ciudad1) %>%
  summarize(mediana=mean(points))
citmerge<-merge(summaryquintil,mediana,by=c("pais_c","ciudad1"), all=TRUE)
citmerge$fam4<-citmerge$ave_income_pc*4
install.packages("dplyr")
library(dplyr)
library(tidyr)

write.csv(citmerge,'ciudadesaffordability8132021.csv')


                                                                                  
table1<-summaryquintil                                                                            
                                                                           