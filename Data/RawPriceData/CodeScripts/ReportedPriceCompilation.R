

rm(list=ls())
library(plyr)
setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts")
## reported price data
report.dat<-read.csv("ReportedPrice_All_v3.csv",header=T,na.strings=c("","NA"),strip.white=T,
                     stringsAsFactors=F)
FEN<-read.csv("FishingEntityMatch_v2.csv",header=T,na.strings=c("","NA"),strip.white=T,
                    stringsAsFactors=F)

setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts/Reported Price Data Aggregation")

filelist<-list.files(".",full.names = T)
filelist<-filelist[grep("*.csv",filelist)]

dat<-data.frame()

for(i in 1:length(filelist)){
  A<-read.csv(filelist[i],header=T,na.strings=c("","NA"),strip.white=T,
                    stringsAsFactors=F)
  B<-subset(A,select = c(TaxonKey,Year,FishingEntityName,LandedValue,Landings,ObservedPrice))
  
  dat<-rbind(dat,B)
}

str(dat)
dat<-merge(dat,FEN[,3:4],by.x = "FishingEntityName",by.y = "FishingEntityName",all.x = T)
str(dat)




str(report.dat)
rep.sub<-subset(report.dat,select = c(TaxonKey,Year,FishingEntityName,
                                      LandedValue,Landings,ObservedPrice,FishingEntityID))
str(rep.sub)





report.out<-rbind(rep.sub,dat)

str(report.out)

write.csv(report.out,"",row.names = F)






















