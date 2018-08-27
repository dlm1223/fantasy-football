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

adp$ADP_Rank[!is.na(adp$ADP_half)]<-rank(adp$ADP_half[!is.na(adp$ADP_half)], ties.method ="first" )
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



###DRAFT PICK OPTIMIZATION#####
#getPicks(), default=slot4

getPicks<-function(slot="Slot4",numTeams=12, numRB=5, numWR=5, numTE=1, numQB=2,numK=1, numFLEX=0, numDST=1, shift=0,
                   out=c(), fix=c(), scoring="HALF", strategy=c(),adpCol="ADP_Rank", outPos=c(),onePos=c()){
  # slot<-"Slot4";numRB<-5;numWR<-5;numTE<-1;numQB<-1;numK<-1;numFLEX<-1;numDST<-1;shift<-0;out<-c();fix<-c();scoring<-"HALF";numTeams<-12
  #1qb, 3wr, 2rb, 1te, 1def, 1k, 1flex, 7 bench
  #dataframe of snake draft
  
  numPicks<-numRB+numQB+numWR+numTE+numFLEX+numDST+numK
  
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
  
  
  model <- list()
  optmode<-'lpsolve'
  A<-matrix(0, ncol=nrow(adp), nrow=1000) #cols=decision variables, rows=constraints on variables
  model$obj<-adp[, scoring]  #goal to maximize sum of FPTS for chosen variables
  model$modelsense <- "max"
  
  #position constraints
  q<-1
  A[q, grep("RB", adp$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numRB;q<-q+1 #>=2 RBs
  A[q, grep("WR", adp$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numWR;q<-q+1 #>=3 WRs
  A[q, grep("TE", adp$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numTE;q<-q+1 
  A[q, grep("QB", adp$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numQB;q<-q+1
  A[q, grep("K", adp$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numK;q<-q+1 
  A[q, grep("DST", adp$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numDST;q<-q+1
  A[q, grep("WR|RB|TE", adp$Pos)]<-1; model$sense[q]<-">="; model$rhs[q]<-numRB+numWR+numTE+numFLEX;q<-q+1
  
  A[q, 1:nrow(adp)]<-1; model$sense[q]<-"<="; model$rhs[q]<-numPicks;q<-q+1 #constrain total numPicks
  
  A[q, adp$Player%in% out]<-1; model$sense[q]<-"<="; model$rhs[q]<-0;q<-q+1 #constrain total numPicks
  A[q, adp$Player%in% fix]<-1; model$sense[q]<-">="; model$rhs[q]<-sum( adp$Player%in% fix);q<-q+1 #constrain total numPicks
  

  #1st pick needs 10 players with ADPs above its slot, 2nd pick needs 9 players, etc.
  tot<-numPicks
  for(i in 1:numPicks){
    storePick<-pickDF[i, slot]
    A[q, which(adp[,adpCol]*(1-shift)>=storePick)]<-1; model$sense[q]<-">="; model$rhs[q]<-tot;q<-q+1  #must include a player with adp greater than or equal to pick number
    tot<-tot-1 #first pick needs to have every pick be after it, each subsequent pick needs one less to be after it
  }
  
  if(length(onePos)>=1){
    storePick<-pickDF[length(onePos)+1, slot]
    # numPos<-get(paste0("num", onePos[1])) #need to take at least numPos-1 players at 
    A[q, which(adp[,adpCol]*(1-shift)<storePick& adp$Pos==onePos[1])]<-1; model$sense[q]<-"<="; model$rhs[q]<-1;q<-q+1  #must include a player with adp greater than or equal to pick number
  }
  
  if(length(outPos)>=1){
    storePick<-pickDF[length(outPos)+1, slot]
    # numPos<-get(paste0("num", outPos[1])) #need to take at least numPos-1 players at 
    A[q, which(adp[,adpCol]*(1-shift)<storePick& adp$Pos==outPos$Pos[1])]<-1; model$sense[q]<-"<="; model$rhs[q]<-0;q<-q+1  #must include a player with adp greater than or equal to pick number
  }
  
  
  
  model$vtype   <- 'B'
  params <- list(OutputFlag=0)
  model$A<-A[1:(q-1),]
  
  if(optmode=="lpsolve") {
    result<-lp ("max", objective.in=model$obj, const.mat=A[1:(q-1),],
                const.dir=model$sense, const.rhs=model$rhs, all.bin=TRUE )
    result$x<-result$solution
  }
  
  picks<-adp[as.logical(result$x),]
  picks$Slot<-pickDF[, slot]
  picks
}
#shift means shift everyone's ADP to be x% earlier i.e. if someone''s adp is 100, need to take them at 80



source("simulate season sampled errors.R")

#simDraft() simulations draft w. specified strategy. 
#strategy parameters include: # of players per position, shift (how conservate to plan future picks, default=1), whether to wait on certain positions

adp$fantPts_bin<-as.character(cut(adp[, scoring], breaks=error.breaks))
adp<-merge(adp[, !grepl("meanError", colnames(adp))], errors[, c("meanError", "fantPts_bin", "Pos")], by=c("fantPts_bin", "Pos"))
adp[, scoring]<-adp[, scoring]+adp$meanError
adp<-adp[order(adp$ADP_half, decreasing = F),]


