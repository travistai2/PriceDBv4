
rm(list=ls())
library(plyr)
library(XLConnect)
library(reshape)
setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts")
## reported price data
report.dat<-read.csv("ReportedPrice_All_v3.csv",header=T,na.strings=c("","NA"),strip.white=T,
                     stringsAsFactors=F)
## taxonomic key for each species
taxa.dat<-read.csv("TaxonNom_v3.csv",header=T,na.strings=c("","NA"),strip.white=T,
                   stringsAsFactors=F)


## Species list for matching
setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts/Reported Price Data Aggregation/Extracted data")
species<-read.csv("SpeciesList.csv",header=T,na.strings=c("","NA"),strip.white=T,
                  stringsAsFactors=F)
head(species)








## EU data
setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts/Reported Price Data Aggregation/Extracted data/EU/Data")

#################################################
## reorganize data Function
#################################################
reorg<-function(x){
  Dat<-x
  Dat[Dat==":"]<-NA
  Species<-Dat[which(Dat[,1]=="SPECIES"),2]    ## Species Name
  Unit<-Dat[which(Dat[,1]=="UNIT"),2]         ## Unit of measure (LV or Landings)
  Landed.Country<-Dat[which(Dat[,1]=="GEO"),2]  ## Country it was landed in (diff from flagship of vessel)
  Index0<-which(Dat[,1]=="NATVESSR/TIME")
  Index1<-which(Dat[,1]=="Special value:")
  
  if(length(Index0)==1 & length(Index1)==1){
    ## Isolating actual values of data
    Values<-as.data.frame(as.matrix(Dat[(Index0+1):(Index1-2),]))
    colnames(Values)<-Dat[Index0,]
    
    Output<-na.omit(melt(Values,id=1))
    colnames(Output)[1]<-"FlagCountry"
    if(nrow(Output)>0){
      Output$Unit=Unit
      Output$Species<-Species
      Output$Landed.Country<-Landed.Country
    } 
    return(Output)
    
  } else {
    dummynames<-c("FlagCountry","variable","value","Unit","Species","Landed.Country")
    Indicator<-data.frame(matrix(NA,1,6))
    colnames(Indicator)<-dummynames
    return(Indicator)
  }
}







foldernames <- list.files(".", full.names=TRUE)
EU.Data<-data.frame()
pb<-txtProgressBar(min = 0, max = length(foldernames), initial = 0,style = 3) 
for(i in 1:length(foldernames)){
  filenames<-list.files(foldernames[i],full.names=TRUE)
  wb<-loadWorkbook(filenames)
  lst<-readWorksheet(wb,sheet=getSheets(wb))
  DataList<-lapply(lst,reorg)
  Out<-Reduce(rbind,DataList)
  EU.Data<-rbind(EU.Data,Out)
  
  ## Update progress bar
  setTxtProgressBar(pb, i)
}

str(EU.Data)
head(EU.Data)

write.csv(EU.Data,"",row.names = F)





## Formatting data

setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts/Reported Price Data Aggregation/Extracted data/EU/Data")
eu.species<-read.csv("../EU_SpeciesList.csv",header=T,na.strings=c("","NA"),strip.white=T,
                     stringsAsFactors=F)
str(eu.species)
colnames(eu.species)[1]<-"Name"

eu.out<-ddply(eu.species,names(eu.species),
              function(x){
                vec<-strsplit(x[,1],"-")
                if(length(vec[[1]])==1){
                  data.frame(CommonName = vec[[1]][1],
                             Species = vec[[1]][1])
                } else {
                  data.frame(CommonName= vec[[1]][1],
                             Species = vec[[1]][2])
                }
                
                
                
                
              })
head(eu.out)
str(eu.out)






########################################
#
# Matching data to taxon name with SpeciesList
# 27/08/2015
#
########################################

rm(list=ls())
library(plyr)
library(reshape)
setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts/Reported Price Data Aggregation/Extracted data/EU")

eu.dat<-read.csv("EU_Data_v2.csv",header=T,na.strings=c("","NA"),strip.white=T,
                 stringsAsFactors=F)
spe.dat<-read.csv("../SpeciesList.csv",header=T,na.strings=c("","NA"),strip.white=T,
                  stringsAsFactors=F)
str(eu.dat)
str(spe.dat)

eu.dat<-na.omit(eu.dat)
str(eu.dat)
spe.dat<-na.omit(spe.dat)


eu.out<-merge(eu.dat,spe.dat,by.x = "Species",by.y = "Species",all.x = T)
str(eu.out)

str(na.omit(eu.out))
str(eu.out[-which(is.na(eu.out$TaxonKey)),])

eu.test<-eu.out[-which(is.na(eu.out$TaxonKey)),]
str(eu.test)


eu.test1<-as.data.frame(cast(eu.test,Species+FlagCountry+variable+Landed.Country+TaxonKey~Unit))
str(eu.test1)
head(eu.test1)
names(eu.test1)<-c("Species","FishingEntityName","Year","Landed.Country","TaxonKey",
                   "LandedValue","Landings")
eu.test1<-na.omit(eu.test1)
str(eu.test1)
eu.test2<-eu.test1[-which(eu.test1$LandedValue==0 | eu.test1$Landings==0),]
str(eu.test2)

eu.test2$ObservedPrice<-eu.test2$LandedValue/eu.test2$Landings


write.csv(eu.test2,"",row.names = F)






########################################
## Editing FishingEntityNames in eu.data to ensure match
rm(list=ls())
library(plyr)
library(reshape)
setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts/Reported Price Data Aggregation/Extracted data/EU")

eu.dat<-read.csv("EU_Data_v3.csv",header=T,na.strings=c("","NA"),strip.white=T,
                 stringsAsFactors=F)



setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts")
## PPP tables
ppp<-read.csv("ExRate_PPP_1950-2010_v5.csv",header=T,na.strings=c("","NA"),strip.white=T,
              stringsAsFactors=F)
## FishingEntityIDS
FEN<-read.csv("FishingEntityMatch_v2.csv",header=T,na.strings=c("","NA"),strip.white=T,
              stringsAsFactors=F)

eu.names<-unique(eu.dat$FishingEntityName)
eu.names[-which(eu.names %in% FEN$FishingEntityName)]

eu.LC<-unique(eu.dat$Landed.Country)
eu.LC[-which(eu.LC %in% FEN$FishingEntityName)]

eu.dat$FishingEntityName[which(eu.dat$FishingEntityName=="Cyprus")]<-"North Cyprus"
eu.dat$FishingEntityName[grep("Germany*",eu.dat$FishingEntityName)]<-"Germany"
eu.dat$Landed.Country[which(eu.dat$Landed.Country=="Cyprus")]<-"North Cyprus"
eu.dat$Landed.Country[grep("Germany*",eu.dat$Landed.Country)]<-"Germany"


eu.names<-unique(eu.dat$FishingEntityName)
absent<-eu.names[-which(eu.names %in% FEN$FishingEntityName)]

out<-eu.dat[-which(eu.dat$FishingEntityName %in% absent),]
unique(out$FishingEntityName)
unique(out$Landed.Country)
head(out)
str(out)

out<-out[which(out$Year<=2010),]
unique(out$Year)
str(out)

## Adjust EURO to currency countries without euro
cnames<-unique(out$FishingEntityName)
cnames[order(cnames)]




EU<-c("Austria", "Belgium", "North Cyprus", "Finland", "France", "Germany", "Greece", "Ireland", "Italy",
"Luxembourg", "Malta", "Netherlands", "Portugal", "Slovakia", "Slovenia", "Spain")

change.names<-cnames[-which(cnames %in% EU)]


eu.out<-out[-which(out$FishingEntityName %in% change.names),]
world.out<-out[which(out$FishingEntityName %in% change.names),]
world.out$ref<-1:nrow(world.out)
str(world.out)




NewWorld.out<-ddply(world.out, names(world.out),.progress = "text",function(x){
  Ent<-x$FishingEntityName
  CLand<-x$Landed.Country
  Yr<-x$Year
  
  Ent.Ex<-ppp$XRAT[which(ppp$FishingEntityName==Ent & ppp$Year==Yr)]
  
  if((CLand %in% EU)==T){
    LC.Ex<-ppp$XRAT[which(ppp$FishingEntityName==CLand & ppp$Year==Yr)]
  } else {
    LC.Ex<-median(ppp$XRAT[which((ppp$FishingEntityName %in% EU) & ppp$Year==Yr)])
  }
    
  LVCor<-(x$LandedValue/LC.Ex)*Ent.Ex
  data.frame(LandedValue.Corrected = LVCor)
})

str(world.out)
str(NewWorld.out)
str(eu.out)

NewWorld.out$LandedValue<-NewWorld.out$LandedValue.Corrected
NewWorld.out$ObservedPrice<-NewWorld.out$LandedValue/NewWorld.out$Landings

eu.final<-rbind(eu.out,NewWorld.out[,1:8])

str(eu.final)



write.csv(eu.final,"EU_Data_v4.csv",row.names = F)












