
rm(list=ls())
library(plyr)
setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts")


## reported price data
report.dat<-read.csv("ReportedPrice_All_v3.csv",header=T,na.strings=c("","NA"),strip.white=T,
                     stringsAsFactors=F)
## taxonomic key for each species
taxa.dat<-read.csv("TaxonNom_v3.csv",header=T,na.strings=c("","NA"),strip.white=T,
                   stringsAsFactors=F)



## Guam data
setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts/Reported Price Data Aggregation/Extracted data/N Marianas Islands CNMI")


read.dat<-scan("2010.txt",what=list(character()))[[1]]
dat<-read.dat[-c(1:4)]
datnames<-read.dat[1:4]

OUT<-dat[1]
for(i in 2:length(dat)){
  value1<-substr(dat[i],1,1)
  if(value1 %in% c(as.character(0:9),"$")){
    OUT<-c(OUT,dat[i])
    
  } else {
    value2<-substr(dat[i-1],1,1)
    if(value2 %in% c(as.character(0:9),"$")){
      OUT<-c(OUT,dat[i])
    } else {
      OUT[length(OUT)]<-paste(OUT[length(OUT)],dat[i],sep=" ")
    }
  }
}
OUT
length(OUT)
out.dat<-as.data.frame(matrix(OUT,ncol=4,byrow=T))
colnames(out.dat)<-c("Species","Pounds","LandedValue","PriceLb")
out.dat

out.dat$Pounds<-as.numeric(gsub(",","",out.dat$Pounds))
out.dat$LandedValue<-(gsub(",","",out.dat$LandedValue))
out.dat$LandedValue<-as.numeric(gsub("\\$","",out.dat$LandedValue))
out.dat$PriceLb<-as.numeric(gsub("\\$","",out.dat$PriceLb))
out.dat$Landings<-out.dat$Pounds*0.45359237/1000
out.dat$ObservedPrice<-out.dat$LandedValue/out.dat$Landings
out.dat$FishingEntityName<-"North Marianas (USA)"
out.dat$Year<-2010
out.dat<-out.dat[,-which(colnames(out.dat) %in% c("Pounds","PriceLb"))]
out.dat

#dat.write<-out.dat[0,]

dat.write<-rbind(dat.write,out.dat)
str(dat.write)




write.csv(dat.write,"ReportedPrice_NMariana_2001-2010.csv",row.names=F)























dat1<-read.csv("",header=T,na.strings=c("","NA"),strip.white=T,
               stringsAsFactors=F)
dd1<-dat1

dat2<-read.csv("",header=T,na.strings=c("","NA"),strip.white=T,
               stringsAsFactors=F)
dd1<-rbind(dd1,dat2)
str(dd1)







write.csv(dd1,"",row.names = F)
write.csv(unique(dd1$Species),"",row.names = F)







## Species list for matching
setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts/Reported Price Data Aggregation/Extracted data")
species<-read.csv("SpeciesList.csv",header=T,na.strings=c("","NA"),strip.white=T,
                  stringsAsFactors=F)
species[duplicated(species),]

## Guam data
setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts/Reported Price Data Aggregation/Extracted data/N Marianas Islands CNMI")
nm.dat<-read.csv("ReportedPrice_NMariana_2001-2010.csv",header=T,na.strings=c("","NA"),strip.white=T,
                   stringsAsFactors=F)
str(nm.dat)
str(species)

out<-merge(nm.dat,species,by.x = "Species",by.y = "Species",all.x = T)
str(out)

write.csv(unique(out$Species[which(is.na(out$TaxonKey))]),"",row.names = F)







write.csv(out,"ReportedPrice_NMariana_2001-2010_v2.csv",row.names = F)






