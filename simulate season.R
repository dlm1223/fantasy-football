# load("Misc Data/2017 NFL Data.RData")

#analyze errors~projections+position

projections<-read.csv(paste(c("Projections_HALF.csv" ), collapse=""))
colnames(projections)[colnames(projections)=="PosFFA"]<-"Pos"
projections$Pos<-ifelse(grepl("LB", projections$Pos), "LB",
                        ifelse(grepl("DT|DL|DE|NT", projections$Pos)| projections$Pos%in% "T", "DL", 
                               ifelse(grepl("SS|FS|DB|CB", projections$Pos)| projections$Pos%in% "S", "DB", projections$Pos)))
projections$Pos[grepl("FB", projections$Pos)]<-"RB"
projections$Pos<-gsub("[/]", ";", projections$Pos)
projections$fantPts_bin<-cut(projections$fantPts_agg, breaks=c(-50, 50, 100, 150, 200, 250, 400))

errors<-ddply(projections[which(projections$Season>=2012& !is.na(projections$fantPts_bin) & projections$Pos!=""),], .(fantPts_bin, Pos),summarize,  
              mean=mean(fantPts-fantPts_agg, na.rm=T),
              median=median(fantPts-fantPts_agg, na.rm=T),
              sd=sd(fantPts-fantPts_agg, na.rm=T), 
              n=sum(!is.na(fantPts-fantPts_agg)))
errors<-errors[errors$n>10,]
errors<-errors[order(errors$Pos),]
errors[which(errors$Pos=="DB"),]
errors[order(errors$Pos),]


# plot(adp$HALF^.4~adp$HALF)
simSeason<-function(picks, numRB=2, numWR=2, numTE=1, numQB=1, numK=1, numDST=1, numFLEX=1, scoring="HALF", optmode="lpsolve", returnLineup=F){
  #numRB<-2; numWR<-3; numTE<-1; numQB<-1; numK<-1; numDST<-1; numFLEX<-1
  picks$Score<-picks[, scoring]  
  picks$ScoreSD<-ifelse(picks$Pos%in% c("K"), 30,
                        ifelse(picks$Pos%in% c("DST"), 30,
                               ifelse(grepl("WR" ,picks$Pos), 32.50+.15*picks$Score, 
                                      ifelse(grepl("TE" ,picks$Pos),28+.15*picks$Score,
                                             ifelse(grepl("RB", picks$Pos), 26.6667+0.2667*picks$Score, 
                                                    ifelse(grepl("QB", picks$Pos), 70, 
                                                           ifelse(grepl("LB", picks$Pos),23+.125*picks$Score, 
                                                                  ifelse(grepl("DL",picks$Pos ), 17+.1*picks$Score, 
                                                                         ifelse(grepl("DB",picks$Pos ),23+.1*picks$Score, 
                                                                                NA
                                                                         )))))))))
  picks$Sim<-rnorm(nrow(picks), mean=picks$Score, sd=picks$ScoreSD)
  picks$Sim[picks$Sim<0]<-0
  
  undrafted<-adp[is.na(adp$ADP_est)& !adp$Pos=='',]
  undrafted$Score<-undrafted[, scoring]  
  undrafted$ScoreSD<-ifelse(undrafted$Pos%in% c("K"), 30,
                            ifelse(undrafted$Pos%in% c("DST"), 30,
                                   ifelse(grepl("WR" ,undrafted$Pos), 32.50+.15*undrafted$Score, 
                                          ifelse(grepl("TE" ,undrafted$Pos), 28+.15*undrafted$Score, 
                                                 ifelse(grepl("RB", undrafted$Pos), 26.6667+0.2667*undrafted$Score, 
                                                        ifelse(grepl("QB", undrafted$Pos), 70, 
                                                               ifelse(grepl("LB", undrafted$Pos),23+.125*undrafted$Score, 
                                                                      ifelse(grepl("DL",undrafted$Pos ),17+.1*undrafted$Score, 
                                                                             ifelse(grepl("DB",undrafted$Pos ),23+.1*undrafted$Score, 
                                                                                    NA
                                                                             )))))))))
  undrafted$Sim<-rnorm(nrow(undrafted), mean=undrafted$Score, sd=undrafted$ScoreSD)
  undrafted$Sim[undrafted$Sim<0]<-0
  undrafted<-undrafted[order(undrafted$Sim, decreasing = T),]
  
  #assuming these are the players I can get through free agency 
  #should allow me to not have to draft a backup kicker
  #assuming i can get the 3-rd best player at each position 
  freeAgent<-c(which(grepl("QB", undrafted$Pos))[3],
               which(grepl("RB", undrafted$Pos))[3],
               which(grepl("WR", undrafted$Pos))[3],
               which(grepl("TE", undrafted$Pos))[3],
               which(grepl("DST", undrafted$Pos))[3],
               which(grepl("K", undrafted$Pos))[3])
  
  picks<-rbind.fill(picks, undrafted[freeAgent,colnames(undrafted)%in% colnames(picks)])
  topLineup<-getTopLineup(picks,    numRB=numRB, numWR=numWR, numTE=numTE, numQB=numQB, numK=numK, numDST=numDST, numFLEX=numDST)#,optmode=optmode
  if(returnLineup==F){
    sum(topLineup$Sim)
  } else{
    topLineup
  }  
  # picks
}

#get top lineup--faster function---doesn't handle multiple eligible players
# getTopLineup2<-function(picks, numRB=2, numWR=2, numTE=1, numQB=1, numK=1, numDST=1, numFLEX=1, scoring="Sim"){
#   picks<-picks[order(picks[, scoring], decreasing = T),]
#   
#   for(i in c("RB", "WR", "TE", "QB", "DST", "K")){
#     picks[, i]<-ifelse(grepl(i, picks$Pos), cumsum(grepl(i, picks$Pos)), NA)
#   }
#   test<-picks[which(picks$RB<=numRB| picks$WR<=numWR| picks$QB<=numQB|picks$DST<=numDST| picks$TE<=numTE| picks$K<=numK),] #starters
#   flex<-picks[which(!picks$Player%in% test$Player&grepl("RB|WR|TE", picks$Pos)),][1,] #flex
#   test<-rbind(test, flex)
#   test[, !colnames(test)%in%  c("RB", "WR", "TE", "QB", "DST", "K", "RB|WR|TE"),]
# }


#get optimal starting lineup
getTopLineup<-function(picks, numRB=2, numWR=2, numTE=1, numQB=1, numK=1, numDST=1, numFLEX=1, scoring="Sim", optmode="lpsolve"){
  
  model <- list()
  A<-matrix(0, ncol=nrow(picks), nrow=20) #cols=decision variables, rows=constraints on variables
  model$obj<-picks[, scoring]  #goal to maximize sum of FPTS for chosen variables
  model$modelsense <- "max"
  numPicks<-numRB+numQB+numWR+numTE+numFLEX+numDST+numK
  
  #position constraints
  q<-1
  A[q, grep("RB", picks$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numRB;q<-q+1 #>=2 RBs
  A[q, grep("WR", picks$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numWR;q<-q+1 #>=3 WRs
  A[q, grep("TE", picks$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numTE;q<-q+1 
  A[q, grep("QB", picks$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numQB;q<-q+1
  A[q, grep("K", picks$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numK;q<-q+1 
  A[q, grep("DST", picks$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numDST;q<-q+1
  A[q, grep("WR|RB|TE", picks$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numRB+numWR+numTE+numFLEX;q<-q+1
  
  A[q, 1:nrow(picks)]<-1; model$sense[q]<-"<="; model$rhs[q]<-numPicks;q<-q+1 #constrain total numPicks
  
  model$vtype   <- 'B'
  params <- list(OutputFlag=0)
  model$A<-A[1:(q-1),]
  
  if(optmode=="lpsolve") {
    result<-lp ("max", objective.in=model$obj, const.mat=A[1:(q-1),],
                const.dir=model$sense, const.rhs=model$rhs, all.bin=TRUE )
    result$x<-result$solution
  }else if (optmode=="Rsymphony"){
    model$sense[model$sense=="="]<-"=="
    result<-Rsymphony_solve_LP(max=T, obj=model$obj, mat=A[1:(q-1),], types=rep("B",ncol(A)),
                               rhs=model$rhs, dir = model$sense, time_limit = 20, gap_limit = .02 )
    result$x<-result$solution
  }  else if(optmode=="RCplex"){
    model$sense[model$sense=="="]<-"E"
    model$sense[model$sense=="<="]<-"L"
    model$sense[model$sense==">="]<-"G"
    result<-Rcplex(cvec=model$obj,Amat=A[1:(q-1),],  bvec=model$rhs,sense = model$sense,
                   objsense = model$modelsense,vtype="B" ,control = list(trace=0) )
    result$x<- round(result$xopt, 1)
  } 
  picks[as.logical(result$x),]
}

#the amount of players you take at a position is a function of the amount you need to start at the position, and 
# the expected decrease in value of starting a replacement level player
# x<-""
# sd(simScores)

# sd(simScores)
# sapply(paste("Slot", 1:numTeams, sep=""), function(x) {
# })



