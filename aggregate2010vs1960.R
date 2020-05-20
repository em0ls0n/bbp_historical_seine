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

species60s <- read.csv("Z:/Emily O/Sites&Species DataCSV.csv", na.strings="~")
species60s$Year <- format(as.Date(species60s$Sampling.Date,"%m/%d/%Y"), "%Y")
species60s$Month <- format(as.Date(species60s$Sampling.Date,"%m/%d/%Y"), "%m")
species60s$Day <- format(as.Date(species60s$Sampling.Date,"%m/%d/%Y"), "%d")
species60s <- species60s %>% select(Year, Month, Day, everything())
colnames(species60s) <- gsub(colnames(species60s), pattern= "[.]", replacement = " ")
names(species60s)[names(species60s) == "X2010s names"] <- "Site.Name"

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


###########Total data set CPUE for each species###########################################
####We are getting a CPUE for every collection event in the data set
#Return the number of unique towIDs for each collection event (ex: 3 tows at Lavalette in aug 8 2012 (collection ID 9082)) -> x
num_tows_per_collectionALL<- merged %>%
  group_by_at(vars(CollectionID,Site.Name)) %>% #Group collectionID and sites that are in the same Year_Month
  summarise(a = n_distinct(TowID))


#For all _sites_ add the total number of _tows_ across the whole dataset
#ex: For all Berkley Island Park in 2015 add the levels of column a
num_tows_per_SITEall<- num_tows_per_collectionALL %>%
  group_by_at(vars(Site.Name)) %>%
  summarise(p = sum(a))



#combine the merged_sub$Total.Number for gensp that are the same
sum_sp_per_SITEall<- merged %>%
  group_by_at(vars(Site.Name,gensp)) %>%
  summarise(q = sum(Total.Number)) 

#Divide each q by p
#Make a new column that is called CPUE

merge_CPUE_ALL<-merge(sum_sp_per_SITEall, num_tows_per_SITEall, by=c("Site.Name")) #"out of 214 tows at allen road, only 3 Alosa aestivalis were caught, so the CPUE is..."


merge_CPUE_ALL[merge_CPUE_ALL==0] <- NA #clean up 0s and NAs...
merge_CPUE_ALL<-(na.omit(merge_CPUE_ALL)) #is it OK I did this...?

the2010s_CPUE<- merge_CPUE_ALL %>%
  mutate(total_cpue=q/p)



#### reframe data for PCA

just_2010s_CPUE<- the2010s_CPUE %>% 
  select(-p,-q)

newALL<-just_2010s_CPUE%>% #this is funky... it's like you have to run it line by line to avoid "Error in x:y : argument of length 0" ... fix this?
  spread(gensp,total_cpue)

newALL<-add_column(newALL, time="2010s") %>%
  unite(Collection, c(time,Site.Name), sep = "_", remove = TRUE, na.rm = FALSE)
  

#now i've kept the metadat in the file. I can take it out LATER.



###########################################################################################################################






############################# 1960s data ##################################################################################################


#### reframe data for PCA
##these are already individual sampling events with multiple tows aggregated into the CPUES listed for each species

CPUEs_60s_all<- species60s %>% 
  select(-"Sampling Date",-Year,-Month,-Site,-Day,-Temp,-Salinity)

total_CPUEs_60s<- CPUEs_60s_all %>% 
  group_by(Site.Name) %>%
  summarize_each(funs(sum)) 

total_CPUEs_60s<-add_column(total_CPUEs_60s, time="1960s") %>%
  unite(Collection, c(time,Site.Name), sep = "_", remove = TRUE, na.rm = FALSE)

################### Aggregated 1960s and 2010s data ###############################################


##### comparing on a monthly (per year) basis #####

versusALL_meta<-rbind.fill(newALL,total_CPUEs_60s)

versusALL<-versusALL_meta
row.names(versusALL)<-versusALL$Collection
versusALL<-select(versusALL,-Collection)
versusALL[is.na(versusALL)] <- 0
versusALL<-log(versusALL+1)

pca_versusALL<-prcomp(versusALL, center = TRUE, scale = FALSE)
biplot(pca_versusALL)


pca_versusALL$x #rotate so the collections are back at the columns and data is organized like versusYE_meta
pca_data <- data.frame(pca_versusALL$x, versusALL_meta$Collection)# add species information back into PCA data
ggplot(pca_data, aes(x=PC1, y=PC2, color=versusALL_meta.Collection)) + geom_point() +  stat_ellipse()


#
fviz_pca_biplot(pca_versusALL, label="var", habillage=versusALL_meta$Collection, addEllipses=TRUE, ellipse.level=0.90)
fviz_pca_ind(pca_versusALL, label = FALSE, habillage=versusALL_meta$Collection, addEllipses=TRUE, ellipse.level=0.90)
#

fviz_pca_ind(pca_versusALL)
fviz_pca_var(pca_versusALL)
fviz_pca_biplot(pca_versusALL,label="var",col.ind="cos2") + scale_color_gradient2(low="black", mid="blue",high="red", midpoint=0.95)
      #above = heat gradient of wht sites are similar vs dissimilar



#--rank by least common observation occurance (ex: rank names in gensp by least to most common and show number of instances) 
#--R translation: rank level by lowest number of elements assigned to that level within Factor of Sp and show # of elements
#--R translation: show least common level (by # of elements)

levels(merged$gensp)
merged$gensp<-as.factor(merged$gensp) #the species names were characters, now the column is a factor and the names are levels
nlevels(merged$gensp)
most_abund_2010s<-data.frame(sort(summary(merged$gensp),decreasing=TRUE))

###MOST ABUNDANT BY CPUE ALL
abund2010all<-data.frame(t(newALL[4:ncol(newALL)])) 
abund2010all[is.na(abund2010all)] <- 0
abund2010all <-abund2010all %>%
  rownames_to_column('sp') %>%
  mutate(CPUE=rowSums(.[2:ncol(abund2010all)]))%>% 
  select(sp,CPUE) %>%
  arrange(desc(CPUE))%>%
  column_to_rownames('sp')

###MOST ABUNDANT BY CPUE 1960s
abund1960s<-data.frame(t(species60s[9:ncol(species60s)])) 
abund1960s[is.na(abund1960s)] <- 0
abund1960s <-abund1960s %>%
  rownames_to_column('sp') %>%
  mutate(CPUE=rowSums(.[2:ncol(abund1960s)]))%>% 
  select(sp,CPUE) %>%
  arrange(desc(CPUE))%>%
  column_to_rownames('sp')






