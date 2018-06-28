source("all data, optimize by round.R")


numDrafts<-400
numSims<-100

#default parameters
drafts<-lapply(1:numDrafts,function(x)simDraft())  #get picks going round by round,players taken probabilistically based on adp
drafts[[5]]
table(unlist(lapply(drafts, function(x) x$Player[6])))
hist(sapply(drafts, function(x) sum(x$HALF)))

simScores<-lapply(drafts, function(x)replicate(numSims, simSeason(x))) 
save(simScores, file="simScores by round 1.RData")

#plot stabilization~numdrafts and numSims..sims stabilize very fast--large number of sims not needed--drafts stabilize slower,keep large numDrafts
bydraft<-cummean(unlist(simScores))
bysim<-cummean(unlist(lapply(1:numSims, function(x) lapply(simScores, `[[`, x))))
plot(bydraft ,type="l", ylim=c(1840, 1880)) #cummean~draft
lines(bysim, col="red", lty=2) #cummean~sim
length(bysim)

bysim[20000]

quantile(unlist(simScores[sapply(drafts, function(x) sum(x$Player[1:4]=="Demaryius Thomas")==1)]))
median(sapply(drafts[sapply(drafts, function(x) "QB"%in% x$Pos[1:6])], function(x) sum(x$HALF)))
table(sapply(drafts2, function(x) sum(x$Pos[1:8]=="QB")))
hist(unlist(simScores))
quantile(unlist(simScores))

#alpha=1
drafts<-lapply(1:numDrafts,function(x)simDraft(alpha=1)) 
simScores<-lapply(drafts, function(x)replicate(numSims,simSeason(x)))


#alpha=1.25
drafts2<-lapply(1:numDrafts,function(x)simDraft(alpha=1.25)) 
simScores2<-lapply(drafts2, function(x)replicate(numSims,simSeason(x)))

#alpha=.85
drafts3<-lapply(1:numDrafts,function(x)simDraft(alpha=.85)) 
simScores3<-lapply(drafts3, function(x)replicate(numSims,simSeason(x)))

#alpha=.85, WR in R1
drafts4<-lapply(1:numDrafts,function(x)simDraft(alpha=.85,outPos = c(rep("RB", 1)))) #draft 1 qb in midrounds
simScores4<-lapply(drafts4, function(x)replicate(numSims,simSeason(x)))

#alpha=.85, zeroRB in R1-4
drafts5<-lapply(1:numDrafts,function(x)simDraft(alpha=.85,outPos = c(rep("RB", 4))))  #draft 1 qb in midrounds
simScores5<-lapply(drafts5, function(x)replicate(numSims,simSeason(x)))

#alpha=85, <=1 QB in R1-11
drafts6<-lapply(1:numDrafts,function(x)simDraft(alpha=.85, onePos = rep("QB", 11))) #waiting on QB and DEF
simScores6<-lapply(drafts6, function(x)replicate(numSims,simSeason(x)))
quantile(unlist(simScores6))

#alpha=.85, heavyWR
drafts7<-lapply(1:numDrafts,function(x)simDraft(alpha=.85, numRB=4, numWR=6, numTE=1, numQB=2,numK=1, numFLEX=0, numDST=1))
simScores7<-lapply(drafts7, function(x)replicate(numSims,simSeason(x)))
quantile(unlist(simScores7))

#alpha=.85, heavyRB, <=1 QB in r1-11
drafts8<-lapply(1:numDrafts,function(x)simDraft(alpha=.85, numRB=6, numWR=4, numTE=1, numQB=2,numK=1, numFLEX=0, numDST=1)) 
simScores8<-lapply(drafts8, function(x)replicate(numSims,simSeason(x)))
quantile(unlist(simScores8))

#alpha=.85, no QB backup
drafts9<-lapply(1:numDrafts,function(x)simDraft(alpha=.85, numRB=5, numWR=5, numTE=2, numQB=1,numK=1, numFLEX=0, numDST=1))
simScores9<-lapply(drafts9, function(x)replicate(numSims,simSeason(x)))

#alpha=.85, QB+TE backup, <=1 QB in R1-11
drafts10<-lapply(1:numDrafts,function(x)simDraft(alpha=.85,numRB=4, numWR=5, numTE=2, numQB=2, numK=1, numDST=1 )) #onePos = rep("QB", 11)
simScores10<-lapply(drafts10, function(x)replicate(numSims,simSeason(x)))

drafts11<-lapply(1:numDrafts,function(x)simDraft(alpha=.85,numRB=4, numWR=4, numTE=2, numQB=2, numK=1, numDST=2 )) #onePos = rep("QB", 11)
simScores11<-lapply(drafts11, function(x)replicate(numSims,simSeason(x)))



quantile(unlist(simScores11))
freqs<-as.data.frame.matrix(table(unlist(lapply(drafts8, function(x) x$Player)), unlist(lapply(drafts8, function(x) 1:nrow(x)))))
freqs[ order(freqs[,1], freqs[, 2], freqs[,3], freqs[, 4], freqs[, 5], freqs[, 5], freqs[, 5], freqs[, 6], freqs[, 7], freqs[, 8], decreasing = T),]



hist(unlist(simScores))
table(unlist(lapply(drafts10, function(x) x$Player[3])))

#loop through every draft slot--may take a while
drafts_allSlots<-lapply(paste("Slot", 1:12, sep=""), function(x)
  lapply(1:numDrafts,function(y) simDraft(alpha=.85,numRB=4, numWR=5, numTE=2, numQB=2, numK=1, numDST=1 , slot = x))
)
simScores_allSlots<-lapply(drafts_allSlots, function(x){lapply(x, function(y)replicate(numSims,simSeason(y)))})
#results
lapply(simScores_allSlots, function(x)quantile(unlist(x)))

save(list=ls()[grepl("simScores|drafts",ls())], file="analyze draft params by round.RData")

pos<-4
freqs<-as.data.frame.matrix(table(unlist(lapply(drafts_allSlots[[pos]], function(x) x$Player)), unlist(lapply(drafts_allSlots[[pos]], function(x) 1:nrow(x)))))
freqs[ order(freqs[,1], freqs[, 2], freqs[,3], freqs[, 4], freqs[, 5], freqs[, 5], freqs[, 5], freqs[, 6], freqs[, 7], freqs[, 8], decreasing = T),]
quantile(unlist(allSlotSims[[pos]][sapply(allSlots[[pos]], function(x) sum(x$Pos[1:4]%in% c( "WR"))==0)]))



#####PLOT#####

load("analyze draft params by round.RData")

Parameters<-c("1. RBx5,WRx5,QBx2,K/DST/TEx1 (default)", "2. default, alpha=1.25","3. default, alpha=.85",
              "4. zero RB in R1, alpha=.85", "5. zero RB in R1-4, alpha=.85",  "6. \u2264 1QB in R1-11, alpha=.85", 
              "7. RBx4, WRx6, alpha=.85", "8. RBx6, WRx4, \u2264 1QB in R1-11, alpha=.85",
              "9. RBx5,WRx5,TEx2,QB/DST/Kx1, alpha=.85", "10. RBx4,WRx5,QB/TEx2,DST/Kx1, alpha=.85", "11. RBx4, WRx4, QB/TE/DSTx2,Kx1, alpha=.85" )

Sims<-list(simScores,simScores2, simScores3, simScores4, simScores5,
           simScores6, simScores7, simScores8, simScores9, simScores10, simScores11) %>% unlist(recursive = T)


Sims<-data.frame(Sim=Sims, Parameter=rep(Parameters, each=40000))
Sims$Parameter<-factor(Sims$Parameter, levels=unique(Sims$Parameter))
Sims<-ddply(Sims, .(Parameter), summarize, 
            N    = length(Sim),
            mean = mean(Sim),
            sd   = sd(Sim),
            se   = sd / sqrt(N) )
library(ggplot2)
head(Sims)

ggplot(Sims, aes(x=Parameter, y=mean, fill=Parameter)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  theme(axis.text.x=element_blank(), 
        axis.title.x = element_blank()
  )+
  coord_cartesian(ylim=c(1800, 1900))+
  geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), #add confidence interval (+/-1.96*SE)
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Parameter") +
  ylab("Mean-Simulated Starting Lineup (40,000 sims)") +
  ggtitle("Simulation Results for Different Draft Parameters")

scoring<-"HALF"
ggsave(paste0(scoring, " scoring-by round parameters.jpeg"),width = 7, height=4 , units = "in")




freqs<-as.data.frame.matrix(table(unlist(lapply(drafts10, function(x) x$Player)), unlist(lapply(drafts10, function(x) 1:nrow(x)))))
freqs$Player<-row.names(freqs)
freqs$Pos<-adp$Pos[match(freqs$Player, adp$Player)]
row.names(freqs)<-NULL
mostCommon<-lapply(1:15, function(x) freqs[order(freqs[, x], decreasing = T),c(x, "Player", "Pos") ][1:3,])


