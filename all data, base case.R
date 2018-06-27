library(plyr)
library(dplyr)
library(rvest)
library(ggplot2)
library(data.table)
library(XML)
library(lpSolve)
library(zoo)
library(MASS)
options(stringsAsFactors = F)
source('functions.R', encoding = 'UTF-8')

###SCRAPE DRAFT DATA####

getProj<-function(pos, scoring="HALF"){
  print(pos)
  page<-read_html(paste(c("https://www.fantasypros.com/nfl/projections/",tolower(pos), ".php?scoring=", scoring,"&week=draft"), collapse=""))
  data<-page%>% html_table()
  data<-data[[1]]
  if(!"Player"%in%colnames(data) ){
    store<-as.character(data[2, ])
    colnames(data)<-store
  }
  names<-page%>% html_nodes("#data .player-name") %>% html_text()
  data<-data[!is.na(data$FPTS)& !grepl(paste(c(letters, LETTERS), collapse="|"), data$FPTS)& !data$FPTS=="", c("Player", "FPTS")]
  data$Player<-names
  data$Pos<-toupper(pos)
  data
}
projections<-ldply(lapply(c("QB", "WR", "RB", "TE", "DST", "K"), getProj), data.frame)
projections$FPTS<-as.numeric(projections$FPTS)
projections<-projections[!projections$Player%in% c("", " ", "Player") & !is.na(projections$FPTS)& !projections$FPTS==0,]
projections<-ddply(projections, .(Player), summarize, FPTS=sum(FPTS), Pos=paste(Pos, collapse=";"))
projections[grep(";", projections$Pos),]
colnames(projections)[colnames(projections)=="FPTS"]<-"HALF"


ppr<-ldply(lapply(c("QB", "WR", "RB", "TE", "DST", "K"), function(x) getProj(x, scoring="PPR")), data.frame)
ppr$FPTS<-as.numeric(ppr$FPTS)
ppr<-ppr[!ppr$Player%in% c("", " ", "Player") & !is.na(ppr$FPTS)& !ppr$FPTS==0,]
ppr<-ddply(ppr, .(Player), summarize, FPTS=sum(FPTS), Pos=paste(Pos, collapse=";"))
ppr[grep(";", ppr$Pos),]
colnames(ppr)[colnames(ppr)=="FPTS"]<-"PPR"

std<-ldply(lapply(c("QB", "WR", "RB", "TE", "DST", "K"), function(x) getProj(x, scoring="STD")), data.frame)
std$FPTS<-as.numeric(std$FPTS)
std<-std[!std$Player%in% c("", " ", "Player") & !is.na(std$FPTS)& !std$FPTS==0,]
std<-ddply(std, .(Player), summarize, FPTS=sum(FPTS), Pos=paste(Pos, collapse=";"))
std[grep(";", std$Pos),]
colnames(std)[colnames(std)=="FPTS"]<-"STD"

readffcalc<-function(year, scoring="standard"){
  url<-paste(c("https://fantasyfootballcalculator.com/adp?format=", scoring, "&year=", year,"&teams=12&view=graph&pos=all"), sep="", collapse="")
  page<-read_html(url)
  stats<-page%>%html_table(fill=T)
  stats<-stats[[1]]
  if(ncol(stats)==12){
    colnames(stats)<-c("Rk", "Round", "Player", "Pos", "Team",  "ADP", "ADPSD","High", "Low", "TimesDrafted", "Graph", "Graph2" )
    
  } else{
    colnames(stats)<-c("Rk", "Round", "Player", "Pos", "Team", "Bye", "ADP", "ADPSD","High", "Low", "TimesDrafted", "Graph", "Graph2" )
    
  }
  stats<-stats[, !grepl("High|Low|TimesDrafted|Graph|Rk|Round|Bye", colnames(stats))]
  stats$Year<-year
  stats
}
ffcalc_ppr<-ldply(lapply(2015:2018, function(x) readffcalc(x, scoring="ppr")), data.frame)
ffcalc<-ldply(lapply(2015:2018, readffcalc), data.frame)

ffcalc$Player<-coordName(ffcalc$Player)
ffcalc_ppr$Player<-coordName(ffcalc_ppr$Player)
ffcalc$Pos[ffcalc$Pos=="PK"]<-"K"
ffcalc_ppr$Pos[ffcalc_ppr$Pos=="PK"]<-"K"
ffcalc_ppr<-ffcalc_ppr[!duplicated(ffcalc_ppr[, c("Player", "Year")]), ]
ffcalc<-ffcalc[!duplicated(ffcalc[, c("Player", "Year")]), ]

colnames(ffcalc_ppr)[colnames(ffcalc_ppr)%in% c("ADP", "ADPSD")]<-c("ADP2", "ADPSD2")
ffcalc<-merge(ffcalc, ffcalc_ppr, by=c("Player", "Year", "Team", "Pos"), all=T)

ffcalc$ADP_est<-rowMeans(ffcalc[, c("ADP", "ADP2")], na.rm=T)
ffcalc$ADPSD_est<-rowMeans(ffcalc[, c("ADPSD", "ADPSD2")], na.rm=T)
plot(ffcalc$ADPSD_est~ffcalc$ADP_est)
ffcalc<-ffcalc[order(ffcalc$Year, ffcalc$ADP_est, decreasing = F), ]
ffcalc[ffcalc$Year==2018,][1:20,]


#load adp
# https://www.fantasypros.com/nfl/adp/ppr-overall.php

adp_alt<-read_html("https://www.fantasypros.com/nfl/adp/overall.php")
names<-adp_alt%>% html_nodes("#data .player-name")%>% html_text()
adp_alt<- html_table(adp_alt)[[1]]
adp_alt<-adp_alt[!is.na(adp_alt$Rank),]
adp_alt$Player<-names[1:nrow(adp_alt)]
adp_alt$Player<-gsub(" DST", "", adp_alt$Player)
adp_alt<-adp_alt[, c("Player", "AVG")]
colnames(adp_alt)<-c("Player","ADP")
adp_alt$ADP<-as.numeric(adp_alt$ADP)
adp_alt$Player<-coordName(adp_alt$Player)

ffcalc$Player<-coordName(ffcalc$Player)
projections$Player<-coordName(projections$Player)
ppr$Player<-coordName(ppr$Player)
std$Player<-coordName(std$Player)
setdiff(adp_alt$Player, projections$Player)
setdiff(ffcalc$Player[ffcalc$Year==2018], ppr$Player)

table(projections$Pos)
table(ppr$Pos)
table(std$Pos)

ffpros<-Reduce(function(x, y) merge(x, y, all=TRUE, by=c("Player", "Pos")), list(projections, ppr, std))

adp<-merge(ffcalc[ffcalc$Year==2018, c("Player", "ADP_est", "ADPSD_est")], ffpros, by=c("Player"), all=T)

adp$Pos[adp$Player%in%c("Chris Thompson", "Jd Mckissic", "Byron Marshall")& adp$Pos=="WR;RB"]<-"RB"
adp$Pos[adp$Player%in%c("Ryan Hewitt")]<-"TE"
adp$Pos[adp$Player%in%c("Tavon Austin")]<-"TE"


save(list=c("adp", "ffcalc", "ffpros"), file="Draft Data.RData")


####PREPARE DRAFT DATA#####

load("Draft Data.RData")

# adp$ADP_sim<-rnorm(nrow(adp), mean = adp$ADP_est, sd=adp$ADPSD_est)

adp<-adp[order(adp$ADP_est, decreasing = F),]

adp$ADP_Rank[!is.na(adp$ADP_est)]<-rank(adp$ADP_est[!is.na(adp$ADP_est)])
adp$ADP_Rank[is.na(adp$ADP_Rank)]<-500 #undrafted

adp[, c("HALF", "PPR","STD")][is.na(adp[, c("HALF", "PPR","STD")])]<-0 #no projection


customProj<-function(type="STD"){
  proj<-read.csv(paste(c("Projections_", type,".csv" ), collapse=""))
  proj<-proj[proj$Season==2018& proj$Player%in% adp$Player, c("Player","PosFFA", "Season", "Team", "fantPts_agg")]
  colnames(proj)[colnames(proj)=="fantPts_agg"]<-type
  colnames(proj)[colnames(proj)=="PosFFA"]<-"Pos"
  proj$Pos<-ifelse(grepl("LB", proj$Pos), "LB",
                   ifelse(grepl("DT|DL|DE|NT", proj$Pos)| proj$Pos%in% "T", "DL", 
                          ifelse(grepl("SS|FS|DB|CB", proj$Pos)| proj$Pos%in% "S", "DB", proj$Pos)))
  proj$Pos[grepl("FB", proj$Pos)]<-"RB"
  proj$Pos<-gsub("[/]", ";", proj$Pos)
  proj<-proj[, c("Player","Pos", "Team",type)]
  proj
}
projections<-Reduce(function(dtf1, dtf2)  merge(dtf1, dtf2, by =c("Player", "Team", "Pos"), all = TRUE), lapply(c("HALF", "STD", "PPR"), customProj))
projections<-projections[!projections$Pos%in% c("LB", "DL", "DB"),, ]
projections<-projections[order(projections$STD,decreasing = T), ]
projections<-projections[!duplicated(projections$Player), ]

adp<-merge(adp, projections[, c("Player", "HALF", "STD", "PPR")], by=c("Player"), all.x=T)
adp$HALF<-ifelse(is.na(adp$HALF.y), adp$HALF.x, adp$HALF.y)
adp$STD<-ifelse(is.na(adp$STD.y), adp$STD.x, adp$STD.y)
adp$PPR<-ifelse(is.na(adp$PPR.y), adp$PPR.x, adp$PPR.y)
adp<-adp[, !grepl("[.]", colnames(adp))]
adp<-adp[order(adp$ADP_est, decreasing = F),]

adp[duplicated(adp$Player), ] #check if duplicates--if so will need to change getTopLineup() in simseason.R
head(adp, 15)

###DRAFT PICK OPTIMIZATION#####

getPicks<-function(slot,numTeams=12, numRB=2, numWR=2, numTE=1, numQB=1,numK=1, numFLEX=1, numDST=1, shift=0,
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
  picks[,c( "Player", "ADP_est", adpCol, "Pos",scoring, "Slot")]
}
#shift means shift everyone's ADP to be x% earlier i.e. if someone''s adp is 100, need to take them at 80
adp$HALF2<-ifelse(adp$Pos=="RB"& adp$HALF>=100, adp$HALF-20,
                  ifelse(adp$Pos%in% c("TE", "QB")& adp$HALF>100, adp$HALF-10, 
                         ifelse(grepl("WR", adp$Pos)& adp$HALF>100, adp$HALF-15, 
                                ifelse(adp$Pos%in% "DST", adp$HALF+10, adp$HALF   )))) 

picks<- getPicks(slot="Slot4", numRB=5, numWR =5 , numFLEX = 0,numQB=2,shift=0,numDST=1, numK=1,numTE=1,
                  out=c(), fix=c(), scoring='HALF', onePos=rep("RB", 1))
picks

#expected points for each team:
numTeams<-12
sapply(paste("Slot", 1:numTeams, sep=""), function(x) 
  sum(getPicks(slot=x,numTeams = numTeams,    numRB=4, numWR = 6, numFLEX = 0,numQB=2,shift=0,numDST=1, scoring='HALF', out=c(), fix=c())$HALF))


source("simulate season.R")
picks<-getPicks(slot="Slot4", numRB=4, numWR =4,numFLEX=1,numQB=2,numTE=2,numDST=1,numK=1,
                strategy=c(),shift=0,
                scoring='HALF', fix=c(),
                out=c(#"Doug Baldwin"
                  # adp$Player[adp$Pos=="QB"& adp$ADP_Rank<69]
                  # ,adp$Player[adp$Pos=="WR"& adp$ADP_Rank<52]
                  # ,adp$Player[adp$Pos=="DST"& adp$ADP_Rank<141]
                ))
picks
nrow(picks)
getPicks(slot="Slot4", numRB=5, numWR = 3,numTE=2,numK=1,numQB=2,
         numDST=1,numFLEX = 1,shift=0,  out=adp$Player[adp$ADP_Rank<=51& adp$Pos=="RB"], fix=c(), scoring='HALF')

allSims<-replicate(2500,  simSeason(adp), simplify = F) #repeat simulation several times
simScores<-sapply(allSims,function(x) getTopLineup(x, picks))


