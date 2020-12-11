
####################
#
# PRICE DATABASE - k cross validation version
# Rewriting the algorithm to estimate international prices
#   Calculating estimated prices for current catch database
#   
#   VERSION 6
#
# By Travis Tai
#  2020/08/27
#
####################

rm(list=ls())
## set working directory
setwd("C:/Users/t.tai/Documents/GitHub/PriceDBv4")
setwd("Someworkingdirectory") ## set this to your WD


# Read libraries
source("./Scripts/ipak_func.R")  ## change this to where you keep R script functions
pckg <- c("tidyverse","tidyselect")
ipak(pckg)

# Read model functions
source("./Scripts/PriceDatabaseFunctions_kcross.R")
source("./Scripts/PriceDB_kcrossFunc.R")

####################
#
# Read in datasets
#
####################

## coding key for algorithm schematic
code.key<-read.csv("./Data/CodingKey.csv",header=T,na.strings=c("","NA"),strip.white=T,
                   stringsAsFactors=F)

## taxonomic key for each species
taxa.dat<-read.csv("./Data - syd/taxa.dat_updatedTT.csv",header=T,na.strings=c("","NA"),strip.white=T,
                   stringsAsFactors=F)

## PPP tables
ppp<-read.csv("./Data - syd/ppp.dat_updatedSA.csv",header=T,na.strings=c("","NA"),strip.white=T,
              stringsAsFactors=F) %>%
  select(Year,FishingEntityID,XRAT,PPP,PPP.XRAT)   ## filter out PPP columns



### US CPI table
cpi.dat<-read.csv("./Data - syd/cpi.dat.csv",header=T,na.strings=c("","NA"),strip.white=T,
                  stringsAsFactors=F) 


## reported price data
report.dat<-read.csv("./Data - syd/report.dat_filteredonepercentSA.csv",header=T,na.strings=c("","NA"),strip.white=T,
                     stringsAsFactors=F) %>%
  select(Year,FishingEntityID,TaxonKey,ObservedPrice,EndProduct) %>% ## Filter out columns
  filter(ObservedPrice>0) %>%   ## Remove observed prices of zero
  left_join(ppp, by=c("Year","FishingEntityID")) 
origreport.dat<-report.dat


## catch data for which prices need to be estimated
catch.dat<-read.csv("./Data - syd/catch.dat.csv",header=T,na.strings=c("","NA"),strip.white=T,
                    stringsAsFactors=F) %>%
  select(Year,FishingEntityID,TaxonKey)
#select(-DHC_Amount,-FMFO_Amount,-Other_Amount) ## Filter out unneeded columns


####################
#
# RUN MODEL
#
####################

##### PARAMETERS #####

yrs<-2011:2016  ## which year(s) to estimate prices for
refyear<-2010   ## price reference year for inflation; default=2010 dollars
mindat<-3   ## minimum number of datapoints for price estimation; default=3
p.alpha<-0.05  ## alpha value for model estimation; default=0.05
end_prod<-"DHC"  ## set end product value; only uses reported prices with this category

## filter prices based on end product (end_prod)
treport.dat<-origreport.dat %>% filter(EndProduct == end_prod)

## kcross parameters
## 
# how to sample data: 1) random by year 2) % of unique taxon data 3) sample % of unique cntry 4) sample % from each cntry
samp_type<-"cntry_half" ## 1) random, 2) taxa, 3) cntry_half, 4) cntry_all
test_prop<-0.5  ## testing proportion of data to use either 0.25 or 0.5; 0.5 ideal and more parsimonious 


##### END PARAMETERS #####



##### START SIMULATION #####
## run code 

test.list<-list()
train.list<-list()

kcross.out<-list()

set.seed(100) ## this sets the "random" point to start at the same place in random sampling

## setting up test and training data
out.list<-KCross.price(in.repdat = treport.dat, years = yrs, tprop = test_prop,
                       stype = samp_type)




timestart<-proc.time()    ## Starts timer

for(i in 1:(1/test_prop)){
  kdat1<-out.list[[i]]
  obs.dat<-kdat1 %>% filter(test.train == "train") %>% select(-test.train)
  report.dat<-kdat1 %>% filter(test.train == "test") %>% select(-test.train)
  catch.dat<-obs.dat %>% 
    select(Year,FishingEntityID,TaxonKey)
  
  ## model here
  tkcross<-PRICE.FUNC(years = yrs, RelYr = refyear, minData = mindat, alpha = p.alpha,
                      debugtest = T,kcrosstest = T)
  tcpi.dat<-cpi.dat %>%
    mutate(RelIndex = Index/cpi.dat$Index[which(cpi.dat$Year==refyear)]) %>%
    select(-Index)
  tname<-paste0("ObsPrice_",refyear,"USD")
  merged.dat<-obs.dat %>% 
    left_join(tcpi.dat,by = "Year") %>%
    mutate(ObsPrice_USD = ObservedPrice/XRAT * RelIndex)
  names(merged.dat)[10]<-tname
  merged.dat<-merged.dat %>% select(-RelIndex) %>%
    left_join(tkcross, by = c("Year","FishingEntityID","TaxonKey"))
  
  kcross.out[[i]]<-merged.dat
  
  
}

timeend<-proc.time()
timeend-timestart  ## time elapsed

##### END SIMULATION #####



##### PLOTTING CORRELATION OUTPUT #####

comb.dat<-do.call(rbind,kcross.out)

fit1<-lm(Price2010USD_Mean~ObsPrice_2010USD,data=comb.dat)
summary(fit1)

## obtain statistics 
r2<-round(summary(fit1)$r.squared,2)
cor1<-round(cor(x=log(comb.dat$ObsPrice_2010USD),
                y=log(comb.dat$Price2010USD_Mean),
                use="complete.obs"),2)



if(samp_type=="random"){
  ptitle<-"Sample: random"
} else {
  if(samp_type=="taxa"){
    ptitle<-"Sample: taxa"
  } else {
    if(samp_type=="cntry_half"){
      ptitle<-"Sample: % countries"
    } else {
      if(samp_type=="cntry_all"){
        ptitle<-"Sample: % from each country"
      } else {
        print("error")
      }
    }
  }
}

date<-Sys.Date()
plot.name<-paste0("./Plots/kcross_",samp_type,"_",date,".pdf")

pdf(plot.name)
p1<-ggplot(comb.dat) 
p1 + geom_point(aes(x=log(ObsPrice_2010USD),y=log(Price2010USD_Mean))) + 
  geom_abline(slope=1,intercept=0) +
  labs(x="log(Reported price)",y="log(Estimated price)",
       title = paste0("kcross ",samp_type)) +
  coord_cartesian(xlim=c(0,12),ylim=c(0,12)) + 
  annotate("text",label=paste0("r2 = ",r2,"; cor = ",cor1),
           x=9.5,y=1)
dev.off()




























