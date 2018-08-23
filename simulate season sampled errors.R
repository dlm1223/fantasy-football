# load("Misc Data/2017 NFL Data.RData")

#ERROR ANALYSIS####

projections<-read.csv(paste0("Player Data/Projections_", scoring,".csv" ))
colnames(projections)[colnames(projections)=="PosFFA"]<-"Pos"
projections$Pos<-ifelse(grepl("LB", projections$Pos), "LB",
                        ifelse(grepl("DT|DL|DE|NT", projections$Pos)| projections$Pos%in% "T", "DL", 
                               ifelse(grepl("SS|FS|DB|CB", projections$Pos)| projections$Pos%in% "S", "DB", projections$Pos)))
projections$Pos[grepl("FB", projections$Pos)]<-"RB"
projections$Pos<-gsub("[/]", ";", projections$Pos)
projections$fantPts_bin<-cut(projections$fantPts_agg, breaks=c(-50, 25, 75, 125, 175, 225, 400))
projections$error<-projections$fantPts-projections$fantPts_agg
projections<-projections[which(projections$Season%in%2009:2017& projections$fantPts_agg>=25& !is.na(projections$fantPts_bin) & projections$Pos!=""),]

errors<-ddply(projections, .(fantPts_bin, Pos),summarize,  
              meanError=mean(error, na.rm=T),
              medianError=median(error, na.rm=T),
              # meanRelativeError=mean((error)/fantPts_agg, na.rm=T),
              sdError=sd(error, na.rm=T), 
              n=sum(!is.na(error)))
errors<-errors[errors$n>5,]
errors<-errors[order(errors$Pos, errors$fantPts_bin),]
errors<-errors[which(errors$Pos%in% c("K",  "DST", "QB", "RB", "WR", "TE")),]
errors

#center all error-bins around 0
projections<-merge(projections, errors[, c("Pos", "fantPts_bin", "meanError")], by=c("Pos", "fantPts_bin"))
projections$error_adj<-projections$error-projections$meanError 
projections$fantPts_bin<-as.character(projections$fantPts_bin)


###SIM-SEASON FUNCTIONS####


# plot(adp$HALF^.4~adp$HALF)
simSeason<-function(picks, numRB=2, numWR=2, numTE=1, numQB=1, numK=1, numDST=1, numFLEX=1, scoring="HALF", returnLineup=F){
  
  #numRB<-2; numWR<-2; numTE<-1; numQB<-1; numK<-1; numDST<-1; numFLEX<-1;scoring<-"HALF"
  picks$fantPts_bin<-as.character(cut(picks[, scoring]-picks$meanError, breaks=c(-50, 25, 75, 125, 175, 225, 400))) #basing error-bin on HALF projection
  picks$fantPts_bin[picks$Pos=="DST"& picks$fantPts_bin=="(125,175]"]<-"(75, 125]" 
  #sample bias-adjusted errors from 2010-2017, by player and position
  picks$error<-sapply(1:nrow(picks), function(x){
    sample(projections$error_adj[which(projections$fantPts_bin==picks$fantPts_bin[x]& projections$Pos==picks$Pos[x])], 1)
  })
  
  picks$Sim<-picks[, scoring]+picks$error
  picks$Sim[picks$Sim<0]<-0
  
  #undrafted players--sample errors
  undrafted<-adp[is.na(adp$ADP_half)& !adp$Pos==''& adp[, scoring]>=25,]
  undrafted$fantPts_bin<-as.character(cut(undrafted[, scoring]-undrafted$meanError, breaks=c(-50, 25, 75, 125, 175, 225, 400)))
  undrafted$error<-NA
  #for each "error-bin-type", get errors for undrafted players in that bin
  for(i in 1:nrow(errors)){
    bool<-undrafted$Pos==errors$Pos[i]& undrafted$fantPts_bin==errors$fantPts_bin[i]
    undrafted$error[bool]<-
      sample(projections$error_adj[which(projections$fantPts_bin==errors$fantPts_bin[i]& projections$Pos==errors$Pos[i])], sum(bool))
    
  }
  
  undrafted$Sim<-undrafted[, scoring]+undrafted$error
  undrafted$Sim[undrafted$Sim<0]<-0
  undrafted<-undrafted[order(undrafted$Sim, decreasing = T),]
  
  #assuming these are the players I can get through free agency 
  #should allow me to not have to draft a backup kicker
  #assuming i can get the 4-th best player at each position 
  freeAgent<-c(which(grepl("QB", undrafted$Pos))[4],
               which(grepl("RB", undrafted$Pos))[4],
               which(grepl("WR", undrafted$Pos))[4],
               which(grepl("TE", undrafted$Pos))[4],
               which(grepl("DST", undrafted$Pos))[4],
               which(grepl("K", undrafted$Pos))[4])
  
  picks<-rbind.fill(picks, undrafted[freeAgent,colnames(undrafted)%in% colnames(picks)])
  picks<-picks[!duplicated(picks$Player), ]
  topLineup<-getTopLineup(picks, numRB=numRB, numWR=numWR, numTE=numTE, numQB=numQB, numK=numK, numDST=numDST, numFLEX=numFLEX)#,optmode=optmode
  
  if(returnLineup==F){
    sum(topLineup$Sim)
  } else{
    topLineup
  }  
  
}

#get optimal starting lineup based on picks & sims
getTopLineup<-function(picks, numRB=2, numWR=2, numTE=1, numQB=1, numK=1, numDST=1, numFLEX=1,  optmode="lpsolve"){
  # numRB<-2;numRB<-2;numWR<-2;numWR<-2;numTE<-1;numQB<-1;numK<-1;numDST<-1;numFLEX<-1
  
  #get top lineup from Sims
  picks<-picks[order(picks$Sim, decreasing = T),]
  for(i in c("RB", "WR", "TE", "QB", "DST", "K")){
    picks[, i]<-ifelse(grepl(i, picks$Pos), cumsum(grepl(i, picks$Pos)), NA)
  }
  starters<-picks[which(picks$RB<=numRB| picks$WR<=numWR| picks$QB<=numQB|picks$DST<=numDST| picks$TE<=numTE| picks$K<=numK),] #starters
  flex<-picks[which(!picks$Player%in% starters$Player&grepl("RB|WR|TE", picks$Pos)),][1,] #flex
  
  result<-list(x=as.numeric(picks$Player%in% c(starters$Player, flex$Player)))
  
  picks[as.logical(result$x),!colnames(picks)%in% c("RB", "WR", "TE", "QB", "DST", "K")]
}