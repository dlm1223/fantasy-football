library(plyr)
library(rvest)
library(dplyr)
library(XML)
library(lpSolve)
library(ggplot2)
options(stringsAsFactors = F)
source('functions.R', encoding = 'UTF-8')

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

head(adp, 15)

source("simulate season.R")

#analyze parameters

picks1<-getPicks(slot="Slot4", numRB=5, numWR = 5,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring='HALF')
simScores1<-replicate(10000, simSeason(picks, scoring="HALF"))

#rb heavy?
picks1.5<-getPicks(slot="Slot4", numRB=6, numWR = 4,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring='HALF')
simScores1.5<-replicate(10000, simSeason(picks1.5, scoring="HALF"))

#take 2 TEs/QBs?
picks2<-getPicks(slot="Slot4", numRB=4, numWR = 4,numTE=2,numK=1,numQB=2, numDST=1,numFLEX = 1,shift=0,  out=c(), fix=c(), scoring='HALF')
simScores2<-replicate(10000, simSeason(picks2, scoring="HALF"))

#take 2 TEs, DST and QB?
picks3<-getPicks(slot="Slot4", numRB=4, numWR = 4,numTE=2,numK=1,numQB=2, numDST=2,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring='HALF')
simScores3<-replicate(10000, simSeason(picks3, scoring="HALF"))


#take 1QB and 2 DST/TE?
picks4<-getPicks(slot="Slot4", numRB=4, numWR = 4,numTE=2,numK=1,numQB=1, numDST=2,numFLEX = 1,shift=0,  out=c(), fix=c(), scoring='HALF')
simScores4<-replicate(10000, simSeason(picks4, scoring="HALF"))

#2 TE/QB + take Antonio Brown at pick 1?
picks5<-getPicks(slot="Slot4", numRB=4, numWR =4 ,numTE=2,numK=1,numQB=2, numDST=1,numFLEX = 1,shift=0,  out=c(), fix="Antonio Brown", scoring='HALF')
simScores5<-replicate(10000, simSeason(picks5, scoring="HALF"))

#Antonio Brown + RB heavy?
picks6<-getPicks(slot="Slot4", numRB=6, numWR = 4,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix="Antonio Brown", scoring='HALF')
simScores6<-replicate(10000, simSeason(picks6, scoring="HALF"))

#zero-RB in R1-4?
picks7<-getPicks(slot="Slot4", numRB=4, numWR = 4,numTE=2,numK=1,numQB=2, numDST=1,numFLEX = 1,shift=0,  out=adp$Player[adp$ADP_Rank<=51& adp$Pos=="RB"], fix=c(), scoring='HALF')
simScores7<-replicate(10000, simSeason(picks7, scoring="HALF"))

#TE in R2?
picks8<-getPicks(slot="Slot4", numRB=4, numWR = 4,numTE=2,numK=1,numQB=2, numDST=1,numFLEX = 1,shift=0,  out=, fix="Robert Gronkowski", scoring='HALF')
simScores8<-replicate(10000, simSeason(picks8, scoring="HALF"))
summary(simScores6)


Parameters<-c("1. RBx5,WRx5,QBx2,K/DST/TEx1", "2. RBx6,WRx4,QBx2,K/DST/TEx1",
              "3. RBx4, WRx4, QB/TEx2, FLEX/K/DSTx1",    "4. RBx4, WRx4, QB/TE/DSTx2, Kx1",
              "5. RBx4, WRx4, TE/DSTx2, QB/FLEX/Kx1", "case 3+Antonio Brown", "case 2+Antonio Brown", "case 3+Zero RB in R1-4",
              "case 3+Gronk in R2" )

Sims<-c(simScores1, simScores1.5, simScores2, simScores3, simScores4, simScores5, simScores6, simScores7, simScores8)
Sims<-data.frame(Sim=Sims, Parameter=rep(Parameters, each=10000))
Sims<-ddply(Sims, .(Parameter), summarize, 
            N    = length(Sim),
            mean = mean(Sim),
            sd   = sd(Sim),
            se   = sd / sqrt(N) )

library(ggplot2)
ggplot(Sims, aes(x=Parameter, y=mean, fill=Parameter)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  theme(axis.text.x=element_blank(), 
        axis.title.x = element_blank()
        )+
  coord_cartesian(ylim=c(1750, 1950))+
  geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), #add confidence interval (+/-1.96*SE)
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Parameter") +
  ylab("Mean-Simulated Starting Lineup (10,000 sims)") +
  ggtitle("Simulation Results for Different Draft Parameters")

