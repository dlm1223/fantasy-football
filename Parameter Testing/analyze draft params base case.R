#need to have getPicks() defined

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

source("simulate season sampled errors.R")

adp$fantPts_bin<-as.character(cut(adp$HALF, breaks=c(-50, 25, 75, 125, 175, 225, 400)))
adp<-merge(adp[, !grepl("meanError", colnames(adp))], errors[, c("meanError", "fantPts_bin", "Pos")], by=c("fantPts_bin", "Pos"))
adp$HALF2<-adp$HALF+adp$meanError
adp<-adp[order(adp$ADP_est, decreasing = F),]

#analyze parameters

scoring<-"HALF2"
fixPlayer<-"Alvin Kamara"

picks1<-getPicks(slot="Slot4", numRB=5, numWR = 5,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring=scoring)
simScores1<-replicate(8000, simSeason(picks1, scoring=scoring))

plot(cummean(simScores1), ylim=c(1800, 1900))

#rb heavy?
picks1A<-getPicks(slot="Slot4", numRB=6, numWR = 4,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring=scoring)
simScores1A<-replicate(8000, simSeason(picks1A , scoring=scoring))

lines(cummean(simScores1A),col="red")

picks1B<-getPicks(slot="Slot4", numRB=4, numWR = 6,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring=scoring)
simScores1B<-replicate(8000, simSeason(picks1B , scoring=scoring))

lines(cummean(simScores1B),col="blue")

#take 2 TEs/QBs?
picks2<-getPicks(slot="Slot4", numRB=4, numWR = 5,numTE=2,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring=scoring)
simScores2<-replicate(8000, simSeason(picks2 , scoring=scoring))

lines(cummean(simScores2),col="yellow")

#take 2 TEs, DST and QB?
picks3<-getPicks(slot="Slot4", numRB=4, numWR = 4,numTE=2,numK=1,numQB=2, numDST=2,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring=scoring)
simScores3<-replicate(8000, simSeason(picks3 , scoring=scoring))

lines(cummean(simScores3),col="green")

#take 1QB and 2 DST/TE?
picks4<-getPicks(slot="Slot4", numRB=4, numWR = 5,numTE=2,numK=1,numQB=1, numDST=2,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring=scoring)
simScores4<-replicate(8000, simSeason(picks4 , scoring=scoring))

lines(cummean(simScores4),col="purple")

#2 TE/QB + take Antonio Brown at pick 1?
picks5<-getPicks(slot="Slot4", numRB=5, numWR =5 ,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=fixPlayer, scoring=scoring)
simScores5<-replicate(8000, simSeason(picks5 , scoring=scoring))


lines(cummean(simScores5),col="orange")

# #Antonio Brown + RB heavy?
# picks6<-getPicks(slot="Slot4", numRB=6, numWR = 4,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix="Antonio Brown", scoring=scoring)
# simScores6<-replicate(8000, simSeason(picks6, scoring=scoring, scoring=scoring))


#zero-RB in R1-4?
picks7<-getPicks(slot="Slot4", numRB=5, numWR = 5,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=adp$Player[adp$ADP_Rank<=51& adp$Pos=="RB"], fix=c(), scoring=scoring)
simScores7<-replicate(8000, simSeason(picks7 , scoring=scoring))

lines(cummean(simScores7),col="dark green")

#TE in R2?
picks8<-getPicks(slot="Slot4", numRB=5, numWR = 5,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=, fix="Robert Gronkowski", scoring=scoring)
simScores8<-replicate(8000, simSeason(picks8 , scoring=scoring))
summary(simScores8)

lines(cummean(simScores8),col="brown", lty=2)

picks9<-getPicks(slot="Slot4", onePos=rep("QB", 10), numRB=5, numWR = 5,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring=scoring)
simScores9<-replicate(8000, simSeason(picks9 , scoring=scoring))

lines(cummean(simScores9),col="dark red", lty=2)


Parameters<-c("1. RBx5,WRx5,QBx2,K/DST/TEx1", "2. RBx6,WRx4,QBx2,K/DST/TEx1","3. RBx4,WRx6,QBx2,K/DST/TEx1",
              "4. RBx4, WRx5, QB/TEx2, K/DSTx1",    "5 RBx4, WRx4, QB/TE/DSTx2, Kx1",  "6. RBx4, WRx5, TE/DSTx2, QB/Kx1", 
              paste0("case 1+ ",fixPlayer ," in R1"), "case 1 + Zero RB in R1-4",
              "case 1+Gronk in R2", "case 1 + \u2264 1QB in R1-10" )

Sims<-c(simScores1, simScores1A,simScores1B,simScores2, simScores3, simScores4, simScores5, simScores7, simScores8, simScores9)
Sims<-data.frame(Sim=Sims, Parameter=rep(Parameters, each=8000))
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
  coord_cartesian(ylim=c(1750, 1850))+
  geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), #add confidence interval (+/-1.96*SE)
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Parameter") +
  ylab("Mean-Simulated Starting Lineup (8,000 sims)") +
  ggtitle("Simulation Results for Different Draft Parameters")

ggsave(paste0(scoring, " scoring-base case parameters.jpeg"),width = 7, height=4 , units = "in")
