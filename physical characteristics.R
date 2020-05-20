################################
##Long-term seining abundances##
################################

#rm(list=ls())

setwd("C:/Users/Emu/Dropbox/BBP Historical analysis/Historical comparisons/")
#GET THE DATA IN
##library(RODBC)
##db<- "Z:/BBP Public/BBP Research Data/Barnegat Bay Partnership Research Database.accdb"
##con <- odbcConnectAccess2007(db)


library(plyr) 
library(dplyr)
library(tidyr)
#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
library("factoextra")
library(tibble)


############################################ 2010s data #############################################################

###Hey!!! Make sure this is updated with any edits or additions from Jim!!!
collections <- read.csv("collections.csv") #YSI info w/ site names
collections$Month <- format(as.Date(collections$CollectionDt,"%Y-%m-%d"), "%m")
collections$Year <- format(as.Date(collections$CollectionDt,"%Y-%m-%d"), "%Y")
#collections <- collections %>% unite(Year_Month, Year:Month, sep = "_", remove = FALSE, na.rm = FALSE)
lengths <- read.csv("lengths.csv") #lengths of specimens (no species)
species <- read.csv("species.csv") #species and total number matched 
species$Species<-revalue(species$Species, c("quinquecirrha"="chesapeakei")) #fixing chrysaora misID (dplyr)
species<-species %>% filter(!Genus %in% c("Chrysaora","Cyanea","Aurelia","Beroe","Mnemiopsis", "Pleurobrachia", "Bougainvillia","Callinectes", "Panopeus", "Scylla","Ovalipes" ,"Libinia","Carcinus","Cambarus", "Procambarus","Orconectes","Limulus","Busycotypus","Chelydra","Chrysemys","Malaclemys","Sternotherus","Kinosternon","Trachemys","Rhacostoma","Unidentified")) 
species<-species %>% filter(!Species %in% c("sp.")) 
tows <- read.csv("tows.csv") #time, lat/lon, wind
tows <- droplevels.data.frame(tows) #remove any extraneous levels from what didn't make it into "seining"


seining <- merge(tows, collections, by.x= "CollectionID", by.y="CollectionID", all.x=TRUE)
seining <- subset(seining, seining$`Project.Name` == "Long Term Seining") #only use the collections that are part of the seining project
seining <- droplevels.data.frame(seining) #remove any extraneous levels from what didn't make it into "seining"
seining$Miscellaneous <- as.character((seining$Miscellaneous))    
seining$Miscellaneous [is.na(seining$Miscellaneous)] = 0

merged <- merge (seining, species, by.x = "TowID", by.y = "TowID", all.x = TRUE, sort = TRUE)

merged$gensp <- paste(merged$Genus, merged$Species)
merged <- droplevels(merged) 
merged$'Total.Number'[is.na(merged$'Total.Number')] <- 0 ## isn't this supposed to replace the NAs with 0s? It replaced everything without NA to 0
merged<-merged%>% filter(Site.Name %in% c('Allen Road','Barnegat Public Beach')) #,'Lavallette','Ocean Gate','Shelter Cove')) #now it's only the sites that correspond to hsitorical data!



yolo<-select(merged, Site.Name, CollectionID, Temperature...C., Salinity..ppt., DO..mg.L. , pH )

yolo3<-yolo %>% 
  distinct(CollectionID, Temperature...C., Salinity..ppt., DO..mg.L. , pH, .keep_all = TRUE)

yolo3 %>% 
  group_by(Site.Name) %>%
  na.omit()%>%
  summarise(avg = mean(Temperature...C.))


Temp<-yolo3$Temperature...C.%>%
  na.omit()
Sal<-yolo3$Salinity..ppt.%>%
  na.omit()
DO<-yolo3$DO..mg.L.%>%
  na.omit()
pH<-yolo3$pH%>%
  na.omit()
Averages2010s<- data.frame("Temp" = mean(Temp), "Sal" = mean(Sal), "DO"= mean(DO), "pH" = mean(pH))

anova_one_way <- aov(Salinity..ppt.~Temperature...C., data = yolo3)
summary(anova_one_way)


############################# 1960s data ##################################################################################################


species60s <- read.csv("Sites&Species DataCSV.csv", na.strings="~")
species60s$Year <- format(as.Date(species60s$Sampling.Date,"%m/%d/%Y"), "%Y")
species60s$Month <- format(as.Date(species60s$Sampling.Date,"%m/%d/%Y"), "%m")
species60s$Day <- format(as.Date(species60s$Sampling.Date,"%m/%d/%Y"), "%d")
species60s <- species60s %>% select(Year, Month, Day, everything())
colnames(species60s) <- gsub(colnames(species60s), pattern= "[.]", replacement = " ")
names(species60s)[names(species60s) == "X2010s names"] <- "Site.Name"

Temp60s<-species60s$Temp%>%
  na.omit()
Sal60s<-species60s$Salinity%>%
  na.omit()
Averages1960s<- data.frame("Temp" = mean(Temp60s), "Sal" = mean(Sal60s))

species60s %>% 
  group_by(Site.Name) %>%
  na.omit()%>%
  summarise(avg = mean(Temp))


##############################################################################


compare<-rbind.fill(Averages1960s,Averages2010s)
row.names(compare)<-c("1960s","2010s")
