

##################
####
##
# START 
#
# Reported data formatting: USA 1950-2010
#
# 14/03/2016
##
####
##################

rm(list=ls())
setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts/Reported Price Data Aggregation/Extracted data")
species<-read.csv("SpeciesList.csv",header=T,stringsAsFactors = F,strip.white=T)

setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts")
tax.dat<-read.csv("TaxonNom_v3.csv",header=T,stringsAsFactors = F,strip.white=T)

setwd("C:/Users/t.tai/Documents/Price Database/International prices method & R scripts/Reported Price Data Aggregation/Extracted data/USA")
dat<-read.csv("ReportedPrice_USA_1950-2010_v1.csv",header=T,stringsAsFactors = F,strip.white=T)



str(species)
str(tax.dat)
str(dat)


dat2<-merge(dat,species,by.x = "Species", by.y = "Species", all.x=T)
str(dat2)


## extracting species not listed in SpeciesList for TaxonKeys
miss.spec<-unique(dat2$Species[which(is.na(dat2$TaxonKey))])
miss.spec[miss.spec %in% species$Species]




## write data 
dat3<-dat2[-which(is.na(dat2$TaxonKey)),]
write.csv(dat3,"ReportedPrice_USA_1950-2010_v2.csv",row.names=F)












