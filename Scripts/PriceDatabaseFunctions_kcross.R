
####################
#
# Price Database model Functions
# 
# Code written by Travis Tai
# Latest update: 2020/06/22
#
####################

require("tidyverse","tidyselect")

##########
## Full model function to match prices, estimate international prices 
##########
PRICE.FUNC<-function(years,RelYr = 2010,minData = 3,alpha=0.05,debugtest=F,kcrosstest = T){ ## estimate international prices
  
  tcpi.dat<-cpi.dat %>% 
    mutate(IndYr = Index/(Index[which(Year==RelYr)])*100) %>%
    select(Year,IndYr)
  
  ##### STEP 1 ##### 
  ## find matching price data for Country:Year:TaxonKey
  
  tdat<-catch.dat %>% filter(Year %in% years) 
  
  tdat1<-tdat %>% select(Year,FishingEntityID,TaxonKey) %>%
    left_join(report.dat,by = c("Year","FishingEntityID","TaxonKey")) %>% 
    na.omit() %>%
    group_by(Year,FishingEntityID,TaxonKey) %>%
    summarize(Price_Mean = mean(ObservedPrice),
              Price_CI = 1.96*sd(ObservedPrice,na.rm=T)/sqrt(length(ObservedPrice)),
              Price_N = length(ObservedPrice)) %>% 
    mutate(Price_CI = ifelse(is.na(Price_Mean)==F &
                               is.na(Price_CI)==T,0,Price_CI)) %>%
    mutate(MatchCode = "TaxonKeyCntry:Match")
  
  
  tcatch.dat<-tdat %>% 
    left_join(tdat1,
              by = c("Year","FishingEntityID","TaxonKey")) %>%
    left_join(ppp,by=c("Year","FishingEntityID"))

  ##### STEP 2 ##### 
  ## Use rule-based step-wise schematic to estimate international prices 
  
  ## Create storage for international prices
  ##    for each Year and Taxon combination of the catch database
  ID<-unique(with(tcatch.dat,paste(Year,TaxonKey,sep=":")))
  iprice.dat<-data.frame(Year = as.numeric(substr(ID,1,4)),
                         TaxonKey = as.numeric(substr(ID,6,11)),
                         ID = ID)
  
  ## print number of taxon that are not in the taxon table database
  no.match<-iprice.dat$TaxonKey[-which(iprice.dat$TaxonKey %in% taxa.dat$TaxonKey)]
  print(paste0("# taxon w/out match in taxa.dat: ",length(no.match)))
  print(no.match)
  
  ## create directory and output file for International prices
  date<-Sys.Date()
  dir.create("./Output",showWarnings = F)
  file.out<-paste0("./Output/IPrice_",end_prod,"_",date,".txt")
  if(kcrosstest == T){
    i_inditer<-i
    file.out<-paste0("./Output/IPrice_",end_prod,"_kcross",i_inditer,"_",date,".txt")
  }
  
  print(paste0("International Price output file: ",file.out))
  cat(paste("Year","TaxonKey","ID",
            "IPrice_Mean","IPrice_CI","IPrice_N","IPrice_pval",
            "MatchCode",sep="\t"),
      file=file.out,sep="\n")
  
  ## testing model: create output for extracted data
  debug.out<-data.frame()
  
  
  ## progress bar for iprice loop
  pb <- txtProgressBar(min = 0, max = nrow(iprice.dat), style = 3)
  
  for(i in 1:nrow(iprice.dat)){
    SCH<-code.key                   ## CodingKey
    ext.dat<-data.frame()           ## Create empty data for extracting reported prices
    
    xdat<-iprice.dat[i,]  
    tYear<-xdat$Year      ## year and taxonkey to match to price data
    tTaxon<-xdat$TaxonKey
    
    ##### STEP 2A #####
    ## WHILE loop to continue through CodingKey until minData is met
    while(nrow(ext.dat)<minData){  ## require minimum data points for estimate
      
      if(nrow(SCH)==0){break}   ## end the while loop if reach the end of the schematic code
      
      ext.dat<-data.frame()                   ## resets ext.dat to zero
      NAM<-colnames(SCH)[which(SCH[1,]==1)]   ## Select criteria from first row of CodingKey
      
      tTaxaDat<-xdat %>% left_join(taxa.dat, by = "TaxonKey") %>%
        select(all_of(NAM))
      
      if(nrow(na.omit(tTaxaDat))==0){ ## need this to prevent matching taxon by NA
        SCH<-SCH[-1,]             ## remove first row of schematic and continue with stepwise estimation
        next
      } 
      
      tSpp<-tTaxaDat %>% left_join(taxa.dat,by = NAM[which(NAM != "Year")]) %>% select(TaxonKey)  ## match and retrieve all TaxonKey that share higher level taxa classification
      ext.dat<-report.dat %>% filter(Year == tYear, TaxonKey %in% tSpp$TaxonKey) ## extract price data
      
      if(nrow(ext.dat)>=minData){break}  ## if >2 data is extracted for anything but taxonmatch, break the 'while' loop
      ext.dat<-data.frame()  ## reset extracted dataframe 
      SCH<-SCH[-1,]   ## remove first row of schematic and continue with stepwise estimation
    }
    
    ##### STEP 2B #####
    ## Generating model outputs for international price estimation
    if(nrow(ext.dat)>=minData){
      try(fit1<-nls(log(ObservedPrice)~log(a)+log(PPP),data=ext.dat,start=list(a=1)),silent=T)
      
      if(exists("fit1")==T){  ## if model is fitted but p-value is > alpha_value
        if(summary(fit1)$coefficients[,4]>=alpha){ ## model is not significant
          rm(fit1)
        }
      }
      
      
      while(exists("fit1")==F){  ## if fitted model doesn't exist, match by broader taxonclass and add more data
        
        if(nrow(SCH)==0){break}   ## end the while loop if reach the end of the schematic code
        
        SCH<-SCH[-1,]   ## remove first row of schematic and continue with stepwise estimation
        ext.dat<-data.frame()                   ## resets ext.dat to zero
        NAM<-colnames(SCH)[which(SCH[1,]==1)]   ## Select criteria from first row of CodingKey
        
        tTaxaDat<-xdat %>% left_join(taxa.dat, by = "TaxonKey") %>%
          select(all_of(NAM))
        
        if(nrow(na.omit(tTaxaDat))==0){ ## need this to prevent matching taxon by NA
          SCH<-SCH[-1,]             ## remove first row of schematic and continue with stepwise estimation
          next
        } 
        
        tSpp<-tTaxaDat %>% left_join(taxa.dat,by = NAM[which(NAM != "Year")]) %>% select(TaxonKey)  ## match and retrieve all TaxonKey that share higher level taxa classification
        ext.dat<-report.dat %>% filter(Year == tYear, TaxonKey %in% tSpp$TaxonKey) ## extract price data
        
        try(fit1<-nls(log(ObservedPrice)~log(a)+log(PPP),data=ext.dat,start=list(a=1)),silent=T)
        
        if(exists("fit1")==T){  ## if model is fitted but p-value is > alpha_value
          if(summary(fit1)$coefficients[,4]>=alpha){ ## model is not significant
            rm(fit1)
          }
        }
      }
      
      if(exists("fit1")==T){ ## if model is fitted and is significant
        if(summary(fit1)$coefficients[,4]<alpha){
          iP<-summary(fit1)$coefficients[,1]
          iP_CI<-summary(fit1)$coefficients[,2]
          NRow<-nrow(ext.dat)
          iP_pval<-summary(fit1)$coefficients[,4]
          Cd<-SCH[1,"CODE"]
          cat(paste(tYear,tTaxon,xdat$ID,
                    iP,iP_CI,NRow,iP_pval,
                    Cd,sep="\t"),file=file.out,sep="\n",append = T)
        } else {
          tOUT<-SecondMatch.FUNC(xdat,minData,RelYr,tcpi.dat)
          cat(tOUT,file=file.out,sep="\n",append = T)
          rm(tOUT)
        }
      } else {
        tOUT<-SecondMatch.FUNC(xdat,minData,RelYr,tcpi.dat)
        cat(tOUT,file=file.out,sep="\n",append = T)
        rm(tOUT)
      }
      rm(fit1)
      
    } else {
      tOUT<-SecondMatch.FUNC(xdat,minData,RelYr,tcpi.dat)
      cat(tOUT,file=file.out,sep="\n",append = T)
      rm(tOUT)
    }
    
    ##
    if(debugtest==T){
      if(nrow(ext.dat>0)){
        text.dat<-ext.dat %>%
          mutate(IDforPrice = xdat$ID,
                 MatchCode = SCH[1,"CODE"],
                 IPrice = ObservedPrice/PPP,
                 PriceUSD = ObservedPrice/XRAT)
        debug.out<-rbind(debug.out,text.dat)
      }
    }
    
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  ## testing output of ext.dat
  if(debugtest==T){
    debug.file<-paste0("./Output/DebugExtractReportedData_",end_prod,"_",date,".csv")
    write.csv(debug.out,debug.file,row.names=F)
  }
  rm(debug.out,debug.file)
  ##
  
  iprice.dat<-read.table(file.out,header=T,sep="\t",stringsAsFactors = F)  ## read in IPrice text file
  
  ##### STEP 3 #####
  ## Match international prices to catch data base
  
  tempdat<-tcatch.dat %>% filter(!is.na(Price_Mean))  %>% ## separate data with Price!=NA
    mutate(PriceUSD_Mean = Price_Mean/XRAT,  ## convert prices to USD
           PriceUSD_CI = Price_CI/XRAT) %>%
    select(-Price_Mean,-Price_CI)
  tempdat.na<-tcatch.dat %>% filter(is.na(Price_Mean)) %>% ## separate data with Price==NA
    select(-Price_Mean,-Price_CI)
  
  tbinddat<-tempdat.na %>%  ## Match NA prices to International price data and estimate local currencies
    select(Year,FishingEntityID,TaxonKey,PPP.XRAT) %>% 
    left_join(iprice.dat %>% select(-ID), by=c("Year","TaxonKey")) %>%
    mutate(MatchCode = paste0(MatchCode,ifelse(is.na(PPP.XRAT),":PPP_NA",""))) %>%
    replace_na(list(PPP.XRAT=1)) %>%
    mutate(PriceUSD_Mean = IPrice_Mean*PPP.XRAT/100,  ## convert Iprice to PriceUSD
           PriceUSD_CI = IPrice_CI*PPP.XRAT/100,
           Price_N = IPrice_N) %>%
    select(-IPrice_Mean,-IPrice_CI,-IPrice_N,-IPrice_pval,-PPP.XRAT)
  tcatch.dat2<-tbinddat %>%
    bind_rows(tempdat %>% select(-XRAT,-PPP,-PPP.XRAT))
  rm(tempdat,tempdat.na,tbinddat)
  
  
  ##### STEP 4 #####
  ## Match with US CPI and convert to USD and correct with inflation
  tempdat1<-tcatch.dat2 %>% 
    left_join(tcpi.dat,by="Year") %>%
    mutate(PriceYEARUSD_Mean = PriceUSD_Mean*(IndYr/100),
           PriceYEARUSD_CI = PriceUSD_CI*(IndYr/100)) %>%
    select(-PriceUSD_Mean,-PriceUSD_CI,-IndYr)
  names(tempdat1)[6:7]<-paste0("Price",RelYr,"USD_",c("Mean","CI"))
  
  tcatch.dat<-tempdat1
  rm(tempdat1)
  
  return(tcatch.dat) 
}


##########
## Subroutine function 
##########

SecondMatch.FUNC<-function(x,minData,tRelYr,tcpi.dat){  ## function to match prices if initial matching fails;
  ## 1st: matching by CPI-adjusted TaxonKey, 2nd: year-median price
  CPI.ratio<-(tcpi.dat$IndYr[which(tcpi.dat$Year==x$Year)]) / 
    (tcpi.dat$IndYr[which(tcpi.dat$Year==tRelYr)])
  
  ext.dat<-report.dat %>% filter(TaxonKey == x$TaxonKey) %>%
    left_join(tcpi.dat,by="Year") %>%
    mutate(IPrice = ObservedPrice/PPP,
           IPriceRelYrDoll = ObservedPrice/PPP*(IndYr/100)) %>%
    drop_na(IPrice)
  
  if(nrow(ext.dat)>minData){
    
    iP<-mean(ext.dat$IPriceRelYrDoll,na.rm=T)*CPI.ratio
    iP_CI<-sd(ext.dat$IPriceRelYrDoll,na.rm=T)/sqrt(nrow(ext.dat))*CPI.ratio
    NRow<-nrow(ext.dat)
    iP_pval<-NA
    Cd<-"TaxaAveTime"
    out.line<-paste(x$Year,x$TaxonKey,x$ID,
                    iP,iP_CI,NRow,iP_pval,
                    Cd,sep="\t")
    return(out.line)
  } else {
    
    ext.dat<-report.dat %>% filter(Year == x$Year)  %>%
      left_join(tcpi.dat,by="Year") %>%
      mutate(IPrice = ObservedPrice/PPP,
             IPriceRelYrDoll = ObservedPrice/PPP*(IndYr/100)) %>%
      drop_na(IPrice)
    MeanIPriceRelYrDoll<-median(ext.dat$IPriceRelYrDoll,na.rm=T)*CPI.ratio
    CIIPriceRelYrDoll<-sd(ext.dat$IPriceRelYrDoll,na.rm=T)/sqrt(nrow(ext.dat))*CPI.ratio
    
    iP<-MeanIPriceRelYrDoll
    iP_CI<-NA  ## using median so no CI; if using mean then us iP_CI<-CIIPriceRelYrDoll
    NRow<-nrow(ext.dat)
    iP_pval<-NA
    Cd<-"YearMedian"
    out.line<-paste(x$Year,x$TaxonKey,x$ID,
                    iP,iP_CI,NRow,iP_pval,
                    Cd,sep="\t")
    return(out.line)
  }
}

##########
## END
##########


