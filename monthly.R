################################
#Monthly comparison between 1960s sites and 2010s sites


setwd("C:/Users/Emu/Dropbox/BBP Historical analysis/Historical comparisons/")

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
  collections$Day <- format(as.Date(collections$CollectionDt,"%Y-%m-%d"), "%d")
#collections <- collections %>% unite(Year_Month, Year:Month, sep = "_", remove = FALSE, na.rm = FALSE)
lengths <- read.csv("lengths.csv") #lengths of specimens (no species)
species <- read.csv("species.csv") #species and total number matched 
  species$Species<-revalue(species$Species, c("quinquecirrha"="chesapeakei")) #fixing chrysaora misID (dplyr)
  species<-species %>% filter(!Genus %in% c("Chrysaora","Cyanea","Aurelia","Beroe","Mnemiopsis", "Pleurobrachia", "Bougainvillia","Callinectes", "Panopeus", "Scylla","Ovalipes" ,"Libinia","Carcinus","Cambarus", "Procambarus","Orconectes","Limulus","Busycotypus","Chelydra","Chrysemys","Malaclemys","Sternotherus","Kinosternon","Trachemys","Rhacostoma","Unidentified")) 
  species<-species %>% filter(!Species %in% c("sp."))
tows <- read.csv("tows.csv") #time, lat/lon, wind
  tows <- droplevels.data.frame(tows) #remove any extraneous levels from what didn't make it into "seining"

species60s <- read.csv("Sites&Species DataCSV.csv", na.strings="~")
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
merged$'Total.Number'[is.na(merged$'Total.Number')] <- 0 
merged<-merged%>% filter(Site.Name %in% c('Allen Road','Barnegat Public Beach', 'Lavallette','Ocean Gate','Shelter Cove')) #now it's only the sites that correspond to hsitorical data!

####################################TO GET A Monthly CPUE (per year)###################################################
###HEY you're gonna have to divide by total number of net pulls -in the month-!!! You can't just add CPUEs!

#Return the number of unique towIDs for the timeframe (ex: 4 tows in May = 4) -> x
num_tows_per_collectionMO<- merged %>%
  group_by_at(vars(Year,Month,CollectionID,Site.Name)) %>% #Group collectionID and sites that are in the same Year_Month
  summarise(a = n_distinct(TowID))


#For all _site_ in Year add the total number of sampling events
#ex: For all Berkley Island Park in 2015 add the levels of column a
num_tows_per_SITEmo<- num_tows_per_collectionMO %>%
  group_by_at(vars(Year,Month,Site.Name)) %>%
  summarise(c = sum(a))



#combine the merged_sub$Total.Number for gensp that are the same
#JV - I would look at dplyr, probably using group_by_at and summarise

sum_sp_per_SITEmo<- merged %>%
  group_by_at(vars(Year,Month,Site.Name,gensp)) %>%
  summarise(d = sum(Total.Number)) 

#Divide each d by c
#Make a new column that is called CPUE

merge_CPUE_MO<-merge(sum_sp_per_SITEmo, num_tows_per_SITEmo, by=c("Year","Month","Site.Name"))

merge_CPUE_MO[merge_CPUE_MO==0] <- NA #clean up 0s and NAs...
merge_CPUE_MO<-(na.omit(merge_CPUE_MO)) #is it OK I did this...?

monthly_CPUE<- merge_CPUE_MO %>%
  mutate(monthly_cpue=d/c)


#### reframe data for PCA

just_monthly_CPUE<- monthly_CPUE %>% 
  unite(Collection, c(Year,Month,Site.Name), sep = "_", remove = FALSE, na.rm = FALSE) %>%
  select(-d,-c)

newMO<-just_monthly_CPUE%>% 
  group_by(Collection)%>%
  spread(gensp,monthly_cpue)
#now i've kept the metadat in the file. I can take it out LATER.


###########################################################################################################################


############################# 1960s data ##################################################################################################


#### reframe data for PCA
##break it into month and year chunks

################## Monthly averages #############################
##Lavalette, Ocean Gate, and Shelter Cove are already monthly!

CPUEs_60s_mo<- species60s %>% 
  select(-"Sampling Date",-Site,-Day,-Temp,-Salinity)

monthly_CPUEs_60s<- CPUEs_60s_mo %>% 
  unite(Collection, c(Year,Month,Site.Name), sep = "_", remove = FALSE, na.rm = FALSE)%>%
  group_by(Collection,Year,Month,Site.Name) %>%
  summarize_each(funs(sum))  #this is condensing everything by month


################### Aggregated 1960s and 2010s data ###############################################


##### comparing on a monthly (per year) basis #####

versusMO_meta<-rbind.fill(newMO,monthly_CPUEs_60s)
versusMO<-select(versusMO_meta,-Year,-Month,-Site.Name)

###now i gotta figure out how to remove the metadata but keep it so I can put it back at the end###
#                                                                                                 #
#                                                                                                 #
#                                                                                                 #
#                                                                                                 #
###################################################################################################

row.names(versusMO)<-versusMO$Collection
versusMO<-select(versusMO,-Collection)
versusMO[is.na(versusMO)] <- 0
versusMO<-log(versusMO+1)

pca_versusMO<-prcomp(versusMO, center = TRUE, scale = FALSE)
biplot(pca_versusMO)


pca_versusMO$x #rotate so the collections are back at the columns and data is organized like versusMO_meta
pca_data <- data.frame(pca_versusMO$x, versusMO_meta$Year,versusMO_meta$Month,versusMO_meta$Site.Name)# add species information back into PCA data
ggplot(pca_data, aes(x=PC1, y=PC2, color=versusMO_meta.Year)) + geom_point() +   stat_ellipse() + geom_text_repel()


#
fviz_pca_biplot(pca_versusMO, label="var", habillage=versusMO_meta$Year, addEllipses=TRUE, ellipse.level=0.90)
fviz_pca_ind(pca_versusMO, label = FALSE, habillage=versusMO_meta$Year, addEllipses=TRUE, ellipse.level=0.90)
#

fviz_pca_ind(pca_versusMO)
fviz_pca_var(pca_versusMO)
fviz_pca_biplot(pca_versusMO,label="var",col.ind="cos2") + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.6)



#--rank by least common observation occurance (ex: rank names in gensp by least to most common and show number of instances) 
#--R translation: rank level by lowest number of elements assigned to that level within Factor of Sp and show # of elements
#--R translation: show least common level (by # of elements)

 ###MOST ABUNDANT BY NUMBERS 2010s... this can't be compared though because 1960s are in CPUE
levels(merged$gensp)
merged$gensp<-as.factor(merged$gensp) #the species names were characters, now the column is a factor and the names are levels
nlevels(merged$gensp)
most_abund_2010s<-data.frame(sort(summary(merged$gensp),decreasing=TRUE))

###MOST ABUNDANT BY CPUE 2010s
abund2010mo<-data.frame(t(newMO[5:ncol(newMO)])) 
abund2010mo[is.na(abund2010mo)] <- 0
abund2010mo <-abund2010mo %>%
  rownames_to_column('sp') %>%
  mutate(CPUE=rowSums(.[2:ncol(abund2010mo)]))%>% 
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


