
####################
#
# PRICE DATABASE
# Plot price outputs for updated prices and landed values
#   for years 2011-2016
#   
#   VERSION 6
#
# By Travis Tai
#  2020/06/15
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

####################
#
# Read in datasets
#
####################

## estimated price data
DHCprice.dat<-read.csv("Output/PriceDatabaseOutputEstimates_2020-07-29.csv", ## update and change this based on date
                    header=T,stringsAsFactors = F, strip.white = T) %>%
  distinct()  ## keep only disctinct values

FMFOprice.dat<-read.csv("Output/SomeFMFOPriceFile.csv", ## update and change this based on date
                       header=T,stringsAsFactors = F, strip.white = T) %>%
  distinct()  ## keep only disctinct values

Otherprice.dat<-read.csv("Output/SomeOtherPriceFile.csv", ## update and change this based on date
                        header=T,stringsAsFactors = F, strip.white = T) %>%
  distinct()  ## keep only disctinct values

discprice.dat<-read.csv("Output/SomeDiscardPriceFile.csv", ## update and change this based on date
                        header=T,stringsAsFactors = F, strip.white = T) %>%
  distinct()  ## keep only disctinct values

## catch data
catch.dat<-read.csv("Data/CathcDB_2011-2016.csv",  
                    header=T,stringsAsFactors = F, strip.white = T) %>% 
  rename(FishingEntityID = fishing_entity_id,
         Year = year,
         TaxonKey = taxon_key)

####################
#
# Combining datasets
#
####################

## remove below three lines once we have separate datasets for end_use
FMFOprice.dat<-DHCprice.dat
Otherprice.dat<-DHCprice.dat
discprice.dat<-DHCprice.dat


## merge data sets to match prices to catch database

DHC.dat<-catch.dat %>% 
  filter(end_use == "Direct human consumption") %>%
  left_join(DHCprice.dat, by = c("Year","FishingEntityID","TaxonKey")) %>%
  mutate(landed_value = catch_sum*Price2010USD_Mean)

FMFO.dat<-catch.dat %>% 
  filter(end_use == "Fishmeal and fish oil") %>%
  left_join(FMFOprice.dat, by = c("Year","FishingEntityID","TaxonKey")) %>%
  mutate(landed_value = catch_sum*Price2010USD_Mean)

other.dat<-catch.dat %>% 
  filter(end_use == "Other") %>%
  left_join(Otherprice.dat, by = c("Year","FishingEntityID","TaxonKey")) %>%
  mutate(landed_value = catch_sum*Price2010USD_Mean)

discard.dat<-catch.dat %>% 
  filter(end_use == "Discards") %>%
  left_join(discprice.dat, by = c("Year","FishingEntityID","TaxonKey")) %>%
  mutate(landed_value = catch_sum*Price2010USD_Mean)

merge.dat<-DHC.dat %>%
  bind_rows(FMFO.dat,other.dat,discard.dat)




####################
#
# Plotting
#
####################

## weighted by catch mean for global prices over time
mean.price<-merge.dat %>%
  group_by(Year) %>%
  summarize(Price2010USD_wMean = weighted.mean(Price2010USD_Mean,catch_sum,na.rm=T))


## weighted by catch mean for global prices by sector over time
meansector.price<-merge.dat %>%
  group_by(Year,sector_type) %>%
  summarize(Price2010USD_wMean = weighted.mean(Price2010USD_Mean,catch_sum,na.rm=T))


## weighted by catch mean for global prices by end_use over time
meanenduse.price<-merge.dat %>%
  group_by(Year,end_use) %>%
  summarize(Price2010USD_wMean = weighted.mean(Price2010USD_Mean,catch_sum,na.rm=T))

## global landed value over time
landedval<-merge.dat %>%
  group_by(Year) %>%
  summarize(landed_value = sum(landed_value,na.rm=T))


## global landed value over time by sector
landedval.sec<-merge.dat %>%
  group_by(Year,sector_type) %>%
  summarize(landed_value = sum(landed_value,na.rm=T))


## global landed value over time by sector
landedval.end<-merge.dat %>%
  group_by(Year,end_use) %>%
  summarize(landed_value = sum(landed_value,na.rm=T))



















