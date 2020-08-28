
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
ppp<-read.csv("./Data - syd/ppp.dat.csv",header=T,na.strings=c("","NA"),strip.white=T,
              stringsAsFactors=F) %>%
  select(Year,FishingEntityID,XRAT,PPP,PPP.XRAT)   ## filter out PPP columns

### US CPI table
cpi.dat<-read.csv("./Data - syd/cpi.dat.csv",header=T,na.strings=c("","NA"),strip.white=T,
                  stringsAsFactors=F) 


## reported price data
report.dat<-read.csv("./Data - syd/report.dat.csv",header=T,na.strings=c("","NA"),strip.white=T,
                     stringsAsFactors=F) %>%
  select(Year,FishingEntityID,TaxonKey,ObservedPrice,EndProduct) %>% ## Filter out columns
  filter(ObservedPrice>0) %>%   ## Remove observed prices of zero
  left_join(ppp, by=c("Year","FishingEntityID")) 
treport.dat<-report.dat


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

yrs<-2011  ## which year(s) to estimate prices for
refyear<-2010   ## price reference year for inflation; default=2010 dollars
mindat<-3   ## minimum number of datapoints for price estimation; default=3
p.alpha<-0.05  ## alpha value for model estimation; default=0.05
end_prod<-"DHC"  ## set end product value; only uses reported prices with this category

## filter prices based on end product (end_prod)
report.dat<-report.dat %>% filter(EndProduct == end_prod)

## kcross parameters
## 
test_prop<-0.5  ## testing proportion of data to use either 0.25 or 0.5
samp_type<-"cntry_half" ## random, taxa, cntry_half, cntry_all
# how to sample data: 1) random by year 2) % of unique taxon data 3) % unique cntry 4) % from each cntry
##

##### END PARAMETERS #####



##### START SIMULATION #####


kcross.out<-list()

set.seed(100) ## this sets the "random" point to start at the same place in random sampling

## setting up test and training data
out.list<-KCross.price(in.repdat = report.dat, years = yrs, tpro = test_prop,
                       stype = samp_type)


tdat1<-out.list[[1]]
tdat1.t1<-tdat1 %>% filter(test.train=="test")
tdat1.t2<-tdat1 %>% filter(test.train=="train")
unique(tdat1.t1$FishingEntityID)
unique(tdat1.t2$FishingEntityID)

tdat2<-out.list[[2]]

timestart<-proc.time()    ## Starts timer

for(i in 1:(1/test_prop)){
  kdat1<-out.list[[i]]
  report.dat<-kdat1 %>% filter(test.train == "test") %>% select(-test.train)
  catch.dat<-kdat1 %>% filter(test.train == "train") %>% 
    select(Year,FishingEntityID,TaxonKey)
  
  ## model here
  kcross.out[[i]]<-PRICE.FUNC(years = yrs, RelYr = refyear, minData = mindat, alpha = p.alpha,
                              debugtest = T,kcrosstest = T)
  
  
}








## model here
out.dat<-PRICE.FUNC(years = yrs, RelYr = refyear, minData = mindat, alpha = p.alpha,
                    debugtest = T)

timeend<-proc.time()
timeend-timestart  ## time elapsed

##### END SIMULATION #####


output.name<-paste0("./Output/PriceDatabaseOutputEstimates_",Sys.Date(),".csv")
write.csv(out.dat,output.name,row.names = F)


























