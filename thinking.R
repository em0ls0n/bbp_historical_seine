
#### reframe data for PCA
##break it into month and year chunks

####Monthly averages
##Lavalette, Ocean Gate, and Shelter Cove are already monthly!

CPUEs_60s<- species60s %>% 
  select(-Sampling.Date,-Site,-Temp,-Salinity)

monthly_CPUEs_60s<-aggregate(CPUEs_60s[,5:ncol(CPUEs_60s)],list(CPUEs_60s$Year,CPUEs_60s$Month,CPUEs_60s$X2010s.names), mean) ##Why can't I tell it to start at Ammodytes.americanus? It only works if I put the position of the row rather than its name
monthly_CPUEs_60s <- monthly_CPUEs_60s %>% unite(Collection, c(Group.1,Group.2,Group.3), sep = "_", remove = TRUE, na.rm = FALSE)

row.names(monthly_CPUEs_60s)<-monthly_CPUEs_60s$Collection
monthly_CPUEs_60s$Collection<-NULL
monthly_CPUEs_60s[is.na(monthly_CPUEs_60s)] <- 0



###Yearly averages
CPUEs_60s<- species60s %>% 
  select(-Sampling.Date,-Site,-Temp,-Salinity)

yearly_CPUEs_60s<-aggregate(CPUEs_60s[,5:ncol(CPUEs_60s)],list(CPUEs_60s$Year,CPUEs_60s$X2010s.names), mean) ##Why can't I tell it to start at Ammodytes.americanus? It only works if I put the position of the row rather than its name
yearly_CPUEs_60s <- yearly_CPUEs_60s %>% unite(Collection, c(Group.1,Group.2), sep = "_", remove = TRUE, na.rm = FALSE)

row.names(yearly_CPUEs_60s)<-yearly_CPUEs_60s$Collection
yearly_CPUEs_60s$Collection<-NULL
yearly_CPUEs_60s[is.na(yearly_CPUEs_60s)] <- 0



####WE DON'T ADD IT BACK TO TEMP/SAL/WHATEVER UNLESS/UNTIL WE MAKE AVERAGES FOR THE TIME PERIOD FOR THOSE VALUES
#collections$CollectionID<-as.factor(collections$CollectionID) #__$Year_Month
#monthly_CPUE$CollectionID<-as.factor(monthly_CPUE$CollectionID)
#collections_obvsMO<-left_join(monthly_CPUE,collections, by=c("Year_Month","Site.Name"), sort = TRUE)
#collections_obvsMO = subset(collections_obvsMO, select = -c(y,x,EntryDt,Project.Name,X,OldKey,Project.ID,Location.ID) ) #clean it...again


