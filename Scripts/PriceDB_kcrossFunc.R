
####################
#
# Price Database - k cross validation separate testing and training data function
# 
# Code written by Travis Tai
# Latest update: 2020/06/22
#
####################

KCross.price<-function(in.repdat = report.dat, years, tprop = test_prop,
                       stype = c("random","taxa","cntry_half","cntry_all")){
  # how to sample data: 1) random by year 2) % of unique taxon data 3) % unique cntry 4) % from each cntry
  
  output.list<-list()
  
  for(j in 1:years){
    trep<-in.repdat %>% filter(Year==years[j])
    
    if(stype=="random"){
      frac<-round(nrow(trep)*tprop)
      n_ind<-sample(1:nrow(trep),nrow(trep),replace=F)
      for(i in 1:(1/tprop)){
        tn_ind<-na.omit(n_ind[((i-1)*frac+1):(frac*i)])
        ttest<-trep[tn_ind,] %>%
          mutate(test.train = "test")
        ttrain<-trep[-tn_ind,] %>%
          mutate(test.train = "train")
        
        if(j==1){
          output.list[[i]]<-data.frame()
        }
        output.list[[i]]<-rbind(output.list[[i]],ttest)
        output.list[[i]]<-rbind(output.list[[i]],ttrain)
      }
      rm(frac,n_ind,tn_ind,ttest,ttrain)
    } 
    
    if(stype=="taxa"){
      taxa.u<-unique(trep$TaxonKey)
      frac<-round(length(taxa.u)*tprop)
      n_ind<-sample(1:length(taxa.u),length(taxa.u),replace=F)
      for(i in 1:(1/tprop)){
        tn_ind<-na.omit(n_ind[((i-1)*frac+1):(frac*i)])
        tn_taxa.u<-taxa.u[tn_ind]
        ttest<-trep[which(trep$TaxonKey %in% tn_taxa.u),] %>%
          mutate(test.train = "test")
        ttrain<-trep[-which(trep$TaxonKey %in% tn_taxa.u),] %>%
          mutate(test.train = "train")
        
        if(j==1){
          output.list[[i]]<-data.frame()
        }
        output.list[[i]]<-rbind(output.list[[i]],ttest)
        output.list[[i]]<-rbind(output.list[[i]],ttrain)
      }
      rm(taxa.u,frac,n_ind,tn_ind,tn_taxa.u,ttest,ttrain)
    }
    
    if(stype=="cntry_half"){
      cntry.u<-unique(trep$FishingEntityID)
      frac<-round(length(cntry.u)*tprop)
      n_ind<-sample(1:length(cntry.u),length(cntry.u),replace=F)
      for(i in 1:(1/tprop)){
        tn_ind<-na.omit(n_ind[((i-1)*frac+1):(frac*i)])
        tn_cntry.u<-cntry.u[tn_ind]
        ttest<-trep[which(trep$FishingEntityID %in% tn_cntry.u),] %>%
          mutate(test.train = "test")
        ttrain<-trep[-which(trep$FishingEntityID %in% tn_cntry.u),] %>%
          mutate(test.train = "train")
        
        if(j==1){
          output.list[[i]]<-data.frame()
        }
        output.list[[i]]<-rbind(output.list[[i]],ttest)
        output.list[[i]]<-rbind(output.list[[i]],ttrain)
      }
      rm(cntry.u,frac,n_ind,tn_ind,tn_cntry.u,ttest,ttrain)
    }
    
    if(stype=="cntry_all"){
      cntry.u<-unique(trep$FishingEntityID)
      for(k in 1:length(cntry.u)){
        ttrep<-trep %>% filter(FishingEntityID == cntry.u[k])
        frac<-round(nrow(ttrep)*tprop)
        n_ind<-sample(1:nrow(ttrep),nrow(ttrep),replace=F)
        for(i in 1:(1/tprop)){
          tn_ind<-na.omit(n_ind[((i-1)*frac+1):(frac*i)])
          ttest<-trep[tn_ind,] %>%
            mutate(test.train = "test")
          ttrain<-trep[-tn_ind,] %>%
            mutate(test.train = "train")
          
          if(j==1){
            output.list[[i]]<-data.frame()
          }
          output.list[[i]]<-rbind(output.list[[i]],ttest)
          output.list[[i]]<-rbind(output.list[[i]],ttrain)
        }
      }
      
      rm(cntry.u,ttrep,frac,n_ind,tn_ind,ttest,ttrain)
    }
    
    rm(trep)
  }
  return(output.list)
}