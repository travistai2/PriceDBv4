# PriceDBv4
Updating ex-vessel price database - version 4

# Running model simulation 

File: PriceDB_RunSimulation.R

Steps to follow:
1. Ensure working directory is correct
2. Ensure required folders (i.e. 'Data', 'Scripts') and files are reachable by the model code
3. Data files to be read in
  a. code.key - this is the coding schematic
  b. taxa.dat - updated taxon table; one thing to make sure this is up to date and has all TaxonKey codes listed in the catch database. Model will still run, but will result in blank estimations
  c. ppp - purchasing power parity, and exchange rate table; need columns: Year, FishingEntityID, XRAT, PPP, PPP.XRAT
  d. report.dat - reported raw price data in domestic currency; need columns: Year, FishingEntityID, TaxonKey, ObservedPrice
  e. cpi.dat - USA consumer price index; get updated table; need columns: Year, Index
  f. catch.dat - SAU catch database; need columns: Year, FishingEntityID, TaxonKey
                don't need columns but good for calculated landed value later Total (catch in tonnes), DHC_Proportion, FMFO_Proportion, Other_Proportion             
4. Debugging - currently there is a line of code that subsamples the catch data to reduce the amount of data and time that I included to debug the model. You can run the model to test the code first. But remove these lines (or ## comment it out) when ready to run the model fully:
  PriceDatabaseFunction.R - Line26&27, and the %>% at the end of Line 25
5. Only need to run PriceDB_RunSimulation.R to run model. This script reads all the functions and data you need to run the price model.
