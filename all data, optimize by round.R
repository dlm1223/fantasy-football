library(png)
library(grid)
library(doSNOW)
library(plyr)
library(dplyr)
library(rvest)
library(ggplot2)
library(data.table)
library(XML)
library(lpSolve)
library(zoo)
library(MASS)
options(stringsAsFactors = F, scipen =999)
source('functions.R', encoding = 'UTF-8')


# year<-2017

#ORGANIZE ADPS AND PROJECTIONS####


load("Player Data/Draft Data.RData")

adp<-merge(ffcalc[ffcalc$Year==year, c("Player", "ADP_half", "ADPSD_half")], ffpros, by=c("Player"), all.x=T)

adp$Pos[adp$Player%in%c("Chris Thompson", "Jd Mckissic", "Byron Marshall")& adp$Pos=="WR;RB"]<-"RB"
adp$Pos[adp$Player%in%c("Ryan Hewitt")]<-"TE"
adp$Pos[adp$Player%in%c("Tavon Austin")]<-"WR"

adp<-adp[order(adp$ADP_half, decreasing = F),]

adp$ADP_Rank[!is.na(adp$ADP_half)]<-rank(adp$ADP_half[!is.na(adp$ADP_half)], ties.method = 'first')
adp$ADP_Rank[is.na(adp$ADP_Rank)]<-500 #undrafted

adp[, c("HALF", "PPR","STD")][is.na(adp[, c("HALF", "PPR","STD")])]<-0 #no projection

customProj<-function(type="STD"){
  proj<-read.csv(paste(c("Player Data/Projections_", type,".csv" ), collapse=""))
  colnames(proj)[colnames(proj)=="DepthTeam"]<-"Team"
  proj<-proj[proj$Season%in%year, c("Player","PosFFA", "Season", "Team", "fantPts_agg", "fantPts")]
  colnames(proj)[colnames(proj)=="fantPts_agg"]<-type
  colnames(proj)[colnames(proj)=="fantPts"]<-paste0(type, "_actual")
  colnames(proj)[colnames(proj)=="PosFFA"]<-"Pos"
  proj$Pos<-ifelse(grepl("LB", proj$Pos), "LB",
                   ifelse(grepl("DT|DL|DE|NT", proj$Pos)| proj$Pos%in% "T", "DL", 
                          ifelse(grepl("SS|FS|DB|CB", proj$Pos)| proj$Pos%in% "S", "DB", proj$Pos)))
  proj$Pos[grepl("FB", proj$Pos)]<-"RB"
  proj$Pos<-gsub("[/]", ";", proj$Pos)
  proj<-proj[, c("Player","Pos", "Team",type, paste0(type, "_actual"))]
  proj
}

projections<-Reduce(function(dtf1, dtf2)  merge(dtf1, dtf2, by =c("Player", "Team", "Pos"), all = TRUE), lapply(c("HALF", "CUSTOM", "STD", "PPR"), customProj))
projections<-projections[!projections$Pos%in% c("LB", "DL", "DB"),, ]
projections<-projections[order(projections$HALF,decreasing = T), ]
projections<-projections[!duplicated(projections$Player), ]

adp<-merge(adp, projections[, colnames(projections)%in% c("Player", "Pos", "HALF", scoring, paste0(scoring, "_actual"))], by=c("Player"), all=T)
adp$HALF<-ifelse(is.na(adp$HALF.y), adp$HALF.x, adp$HALF.y)
adp$Pos<-ifelse(is.na(adp$Pos.x), adp$Pos.y, adp$Pos.x)
adp<-adp[, !grepl("[.]", colnames(adp))]
adp<-adp[order(adp$ADP_half, decreasing = F),]
adp<-adp[!grepl(";", adp$Pos)& !adp$Pos=="",]
adp<-adp[!is.na(adp$Player),]
adp[duplicated(adp$Player), ] #check if duplicates--if so will need to change getTopLineup() in simseason.R
head(adp, 15)




####OPTIMIZATION FUNCTIONS#####

#getting pick for a given round, number of positions specifies how much of each position to plan out to take
getPick<-function(slot, pickNum, alreadyChosen,numRB=5, numWR=5, numTE=1, numQB=2,numK=1,numDST=1,  numFLEX=0, shift=0, 
                  out=c(), fix=c(), scoring="HALF", adpCol="ADP_Rank",  outPos=c(), onePos=c(), optmode="lpsolve", adp=adp, pickDF=pickDF){
  
  #shift=how conservative you want to be in planning future picks
  #outPos is a vector of positions you want to wait on ex outPos=c("QB", "QB", "QB") means draft 0 qb in first 3 rounds
  #onePos is vector of positions you want to draft at most 1 of so c("QB", "QB", "QB") means draft at most 1 in first 3 rounds
  #right now can only specify one vector for outPos and onePos
  
  # slot<-"Slot4";round<-1;numRB<-4;numWR<-5;numTE<-1;numQB<-2;numK<-1;numFLEX<-1;numDST<-2;shift<-0;out<-c();fix<-c();scoring<-"HALF"
  #1qb, 3wr, 2rb, 1te, 1def, 1k, 1flex, 7 bench
  #dataframe of snake draft
  
  numPicks<-numRB+numWR+numTE+numQB+numK+numDST+numFLEX
  model <- list()
  A<-matrix(0, ncol=nrow(adp), nrow=1000) #cols=decision variables, rows=constraints on variables
  model$obj<-adp[, scoring]  #goal to maximize sum of FPTS for chosen variables
  model$modelsense <- "max"
  
  #position constraints
  q<-1
  A[q, grep("RB", adp$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numRB;q<-q+1 #>=2 RBs
  A[q, grep("WR", adp$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numWR;q<-q+1 #>=3 WRs
  A[q, grep("TE", adp$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numTE;q<-q+1 
  A[q, grep("QB", adp$Pos)]<-1; model$sense[q]<-"="; model$rhs[q]<-numQB;q<-q+1
  A[q, grep("K", adp$Pos)]<-1; model$sense[q]<-"="; model$rhs[q]<-numK;q<-q+1 
  A[q, grep("DST", adp$Pos)]<-1; model$sense[q]<-"="; model$rhs[q]<-numDST;q<-q+1
  A[q, grep("WR|RB|TE", adp$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numRB+numWR+numTE+numFLEX;q<-q+1
  
  A[q, 1:nrow(adp)]<-1; model$sense[q]<-"="; model$rhs[q]<-numPicks;q<-q+1 #constrain total numPicks
  
  A[q, adp$Player%in% c(out, alreadyChosen)]<-1; model$sense[q]<-"<="; model$rhs[q]<-0;q<-q+1 #exclude players
  A[q, adp$Player%in% fix]<-1; model$sense[q]<-">="; model$rhs[q]<-sum( adp$Player%in% fix);q<-q+1 #fix players
  
  #account for  opponent drafting:
  
  #ex:current pick-4th. next-pick=21th, pick after=28th
  #assume can take at most 1-player from top 17 remaining
  # can take at most 2-players from top 28-4=24 remainnig
  #shift is the parameter that controls how conservative want to be
  
  picks<-pickDF[, slot][pickNum:nrow(pickDF)]
  pickDelta<-picks[-1]-picks[1] 
  topRemaining<-which(!adp$Player%in% c(alreadyChosen, fix))  
  
  if(length(shift)>0){
    
    #constraining ADP of future picks
    if(length(pickDelta)>=1){
      
      #constraining future players taken based on uncertainty sets and number of picks  
      for(i  in 1:length(pickDelta) ){
        A[q,topRemaining[1:(pickDelta[i]*(1+shift))]]<-1; model$sense[q]<-"<="; model$rhs[q]<-i;q<-q+1 
      }
    }
  }
  
  #constraining position (i.e. QB, WR) of future picks
  
  #optionally constraining players by position
  #outPos is a vector of times to fix certain positions as out c("RB", "RB", "QB", "QB") means to fade qbs and rbs for 2 rounds
  #outPos<-c(rep("QB", 10), rep("RB", 10))
  
  outPos<-lapply(unique(outPos), function(x) rep(x, sum(outPos==x)))
  
  for(x in outPos){
    #if i am still constraining at current pick
    if(length(x)>=pickNum){ 
      
      i<-length(x)-pickNum+1 #number of future picks to constrain
      out<-adp$Player[which(adp$Player%in%adp$Player[topRemaining[1:(pickDelta[i]*(1+shift))]] & grepl(x[length(x)], adp$Pos))] #players to constrain
      
      A[q,adp$Player%in% out]<-1; model$sense[q]<-"<="; model$rhs[q]<-0;q<-q+1 #constrain
      
    }
  }
  
  onePos<-lapply(unique(onePos),function(x) rep(x, sum(onePos==x)))
  
  for(x in onePos)  {
    
    if(length(x)>=pickNum){ 
      #onePos<-rep("QB", 8)
      
      i<-length(x)-pickNum+1 #number of future picks to constrain
      out<-adp$Player[which(adp$Player%in%adp$Player[1:(topRemaining[round(pickDelta[i]*(1+shift))])]&  grepl(x[length(x)], adp$Pos) )] 
      
      A[q,(adp$Player%in% out ) ]<-1 ;model$sense[q]<-"<="; model$rhs[q]<-1;q<-q+1 #constrain players
      
    }
  }  
  
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
  
  adp[as.logical(result$x),]  #rest of players i plan to take, based on uncertainty set parameter
  adp[as.logical(result$x)& !adp$Player%in% fix& !adp$Pos%in% unlist(lapply(outPos, function(x) x[pickNum])),][1,]  #only store the next I am taking 
}

#getting picks for the whole draft
#specify the number of picks to take of each position

#returns pickDF, allowing for customPicks inserted at given slot i.e. if want to test a draft pick trade
getPickDF<-function(slot="Slot4", numPicks=15, numTeams=12, customPicks=c()){
  pickDF<-data.frame(matrix(NA, ncol=numTeams, nrow=numPicks))
  colnames(pickDF)<-paste("Slot", 1:numTeams, sep="")
  last<-0
  for(i in 1:numPicks){
    if(i%%2==1){
      pickDF[i, ]<-(last+1):(last+numTeams)
    }else{
      pickDF[i, ]<-(last+numTeams):(last+1)
      
    }
    last<-last+numTeams
  }
  if(length(customPicks)>0){
    pickDF[, slot]<-customPicks
  }
  pickDF
}
simDraft<-function(slot="Slot4",numRB=5, numWR=5, numTE=1, numQB=2,numK=1,numDST=1,  numFLEX=0, shift=0, numTeams=12,scoring="HALF",
                   out=c(), outPos=c(),  onePos=c(), optmode="lpsolve", customPicks=c(), customADP=F){
  
  # slot<-"Slot4";numRB<-4;numWR<-5;numTE<-1;numQB<-2;numK<-1;numFLEX<-1;numDST<-1;out<-c();fix<-c();customPicks<-c();scoring<-"HALF";numTeams<-12;shift<-0;outPos<-c();onePos<-c();optmode<-"lpsolve"
  
  adp$ADP_sim[!is.na(adp$ADP_half)]<-rnorm(sum(!is.na(adp$ADP_half)), mean = adp$ADP_half[!is.na(adp$ADP_half)], sd=adp[!is.na(adp$ADP_half), "ADPSD_half"])
  if(customADP){
    adp$ADP_sim<-adp$customADP
  }
  adp$ADP_sim[is.na(adp$ADP_sim)]<-500
  # shift<-0;outPos<-c();onePos<-c();out<-c();optmode<-"lpsolve"
  
  numPicks<-numRB+numWR+numTE+numQB+numK+numDST+numFLEX
  pickDF<-getPickDF(slot=slot, numTeams=numTeams, numPicks=numPicks, customPicks = customPicks)
  
  storePick<-data.frame(matrix(NA, nrow=numPicks, ncol=ncol(adp)))
  colnames(storePick)<-colnames(adp)
  
  for(pickNum in 1:numPicks){
    
    if(pickNum==1){
      numPlayers<-pickDF[pickNum, slot]-1 #number of players to sim
      alreadyChosen<-c()
    } else if(customADP==F){
      numPlayers<-pickDF[pickNum, slot]-pickDF[pickNum-1, slot]-1 #number of players to sim
    } else{
      numPlayers<-pickDF[pickNum, slot]-pickDF[pickNum-1, slot] #number of players to sim
      
    }
    
    #get next-players chosen from adp-simulation
    players<-adp$Player[order(adp$ADP_sim, decreasing = F)]
    if(numPlayers>0){
      players<-players[!players%in% c(alreadyChosen, storePick$Player)][1:numPlayers]
    } else{
      players<-c()
    }
    alreadyChosen<-c(alreadyChosen, players) %>% unique()
    alreadyChosen<-alreadyChosen[!is.na(alreadyChosen)]
    fix<-storePick$Player[!is.na(storePick$Player)]
    
    # head(adp[!adp$Player%in% c(alreadyChosen, storePick$Player),])
    #optimize rest of picks based on projected adp & store top pick from it
    storePick[pickNum, ]<-getPick(slot=slot, pickNum=pickNum, alreadyChosen=alreadyChosen,scoring=scoring,
                                  numRB=numRB, numWR=numWR, numTE=numTE, numQB=numQB,numK=numK, numFLEX=numFLEX, numDST=numDST,
                                  shift=shift, out=out, fix=fix,  adpCol="ADP_Rank", outPos=outPos, onePos = onePos, optmode=optmode, 
                                  adp=adp, pickDF=pickDF)
    
  }
  storePick$Slot<-pickDF[, slot]
  storePick
}

source("simulate season sampled errors.R")

#simDraft() simulations draft w. specified strategy. 
#strategy parameters include: # of players per position, shift (how conservate to plan future picks, default=1), whether to wait on certain positions

adp$fantPts_bin<-as.character(cut(adp[, scoring], breaks=error.breaks))
adp<-merge(adp[, !grepl("meanError", colnames(adp))], errors[, c("meanError", "fantPts_bin", "Pos")], by=c("fantPts_bin", "Pos"))
adp[, scoring]<-adp[, scoring]+adp$meanError
adp<-adp[order(adp$ADP_half, decreasing = F),]
