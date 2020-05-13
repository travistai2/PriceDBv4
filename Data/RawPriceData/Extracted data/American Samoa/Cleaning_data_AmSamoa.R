
rm(list=ls())
library(plyr)
setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts")


## reported price data
report.dat<-read.csv("ReportedPrice_All_v3.csv",header=T,na.strings=c("","NA"),strip.white=T,
                     stringsAsFactors=F)
## taxonomic key for each species
taxa.dat<-read.csv("TaxonNom_v3.csv",header=T,na.strings=c("","NA"),strip.white=T,
                   stringsAsFactors=F)



## American Samoa data
setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts/Reported Price Data Aggregation/Extracted data/American Samoa")



read.dat<-scan("2003.txt",what=list(character()))[[1]]
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
out.dat$FishingEntityName<-"American Samoa"
out.dat$Year<-2003
out.dat<-out.dat[,-which(colnames(out.dat) %in% c("Pounds","PriceLb"))]
out.dat

write.csv(out.dat,"2003.csv",row.names=F)








dat1<-read.csv("2001.csv",header=T,na.strings=c("","NA"),strip.white=T,
               stringsAsFactors=F)
dd1<-dat1

dat2<-read.csv("2010.csv",header=T,na.strings=c("","NA"),strip.white=T,
               stringsAsFactors=F)
dd1<-rbind(dd1,dat2)
str(dd1)




index<-grep("\\$",dd1$Species)

dd1$Species[index]<-substring(dd1$Species[index],14)







write.csv(dd1,"ReportedPrice_AmericanSamoa_2001-2010.csv",row.names = F)


write.csv(unique(dd1$Species),"AmSam_SpeciesList.csv",row.names = F)





## taxonomic key for each species

rm(list=ls())
library(plyr)
setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts")
taxa.dat<-read.csv("TaxonNom_v3.csv",header=T,na.strings=c("","NA"),strip.white=T,
                   stringsAsFactors=F)

setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts/Reported Price Data Aggregation/Extracted data/American Samoa")
spelist<-read.csv("AmSam_SpeciesList.csv",header=T,na.strings=c("","NA"),strip.white=T,
                  stringsAsFactors=F)

speout<-merge(spelist,taxa.dat[,c(1,9)],by.x = "x", by.y = "CommonName",all.x = T)
speout

write.csv(speout,"",row.names = F)


