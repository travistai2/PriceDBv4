####################
#
# PRICE DATABASE
# Rewriting the algorithm to estimate international prices
#   Calculating estimated prices for current catch database
#   
#   VERSION 6
#
# By Travis Tai
#  2020/06/15
#
####################

rm(list=ls())
## set working directory
setwd("C:/Users/t.tai/Documents/Price Database/")

# Read libraries
source("./Scripts/ipak_func.R")  ## change this to where you keep R script functions
pckg <- c("tidyverse","tidyselect")
ipak(pckg)

# Read model functions
source("./Scripts/PriceDatabaseFunctions.R")


####################
#
# Read in datasets
#
####################

## coding key for algorithm schematic
code.key<-read.csv("./Data/CodingKey.csv",header=T,na.strings=c("","NA"),strip.white=T,
                   stringsAsFactors=F)

## taxonomic key for each species
taxa.dat<-read.csv("./Data/TaxonTable_12052016_v8TC.csv",header=T,na.strings=c("","NA"),strip.white=T,
                   stringsAsFactors=F)

## PPP tables
ppp<-read.csv("./Data/ExRate_PPP_1950-2010_v7ChileUpdate.csv",header=T,na.strings=c("","NA"),strip.white=T,
              stringsAsFactors=F) %>%
  select(Year,FishingEntityID,XRAT,PPP,PPP.XRAT)   ## filter out PPP columns

## reported price data
report.dat<-read.csv("./Data/ReportedPrice_All_v7Filtered.csv",header=T,na.strings=c("","NA"),strip.white=T,
                     stringsAsFactors=F) %>%
  filter(FishingEntityName!="Turkey") %>% ## REMOVE TURKEY DATA FROM REPORTED PRICE AS IT CREATES LARGE OUTLIERS
  filter(ObservedPrice>0) %>%   ## Remove observed prices of zero
  select(Year,FishingEntityID,TaxonKey,ObservedPrice) %>% ## Filter out columns
  left_join(ppp, by=c("Year","FishingEntityID"))
  #select(Year,FishingEntityID,TaxonKey,ObservedPrice,ppp,IPrice,USPrice,Real2010IPrice,Real2010USP) ## Filter out columns



### US CPI table
cpi.dat<-read.csv("./Data/US_CPI_2010.csv",header=T,na.strings=c("","NA"),strip.white=T,
                  stringsAsFactors=F) 

## catch data for which prices need to be estimated
catch.dat<-read.csv("./Data/CatchDB_Full_20161201TC.csv",header=T,na.strings=c("","NA"),strip.white=T,
                    stringsAsFactors=F) %>%
  select(-DHC_Amount,-FMFO_Amount,-Other_Amount) ## Filter out unneeded columns


####################
#
# RUN MODEL
#
####################

##### PARAMETERS #####

yrs<-2010  ## which year(s) to estimate prices for
refyear<-2010   ## price reference year for inflation; default=2010 dollars
mindat<-3   ## minimum number of datapoints for price estimation; default=3
p.alpha<-0.05  ## alpha value for model estimation; default=0.05

##### END PARAMETERS #####




##### START SIMULATION #####
## run code 

timestart<-proc.time()    ## Starts timer

## model here
out.dat<-PRICE.FUNC(years = yrs, RelYr = refyear, minData = mindat, alpha = p.alpha)

timeend<-proc.time()
timeend-timestart  ## time elapsed

##### END SIMULATION #####


output.name<-paste0("./Output/PriceDatabaseOutputEstimates_",Sys.Date(),".csv")
write.csv(out.dat,output.name,row.names = F)






