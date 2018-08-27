
year<-2018
scoring<-"CUSTOM"
error.breaks<-c(-50, 25, 75, 125, 200, 400)

source("all data, base case.R")


#analyze parameters


picks1<-getPicks(slot="Slot4", numRB=5, numWR = 5,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring=scoring)
picks1
simScores1<-replicate(4000, simSeason(picks1, scoring=scoring))

plot(cummean(simScores1))

#rb heavy?
picks1A<-getPicks(slot="Slot4", numRB=6, numWR = 4,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring=scoring)
picks1A
simScores1A<-replicate(4000, simSeason(picks1A , scoring=scoring))

lines(cummean(simScores1A),col="red")

picks1B<-getPicks(slot="Slot4", numRB=4, numWR = 6,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring=scoring)
picks1B
simScores1B<-replicate(4000, simSeason(picks1B , scoring=scoring))

lines(cummean(simScores1B),col="blue")

#take 2 TEs/QBs?
picks2<-getPicks(slot="Slot4", numRB=4, numWR = 5,numTE=2,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring=scoring)
picks2
simScores2<-replicate(4000, simSeason(picks2 , scoring=scoring))

lines(cummean(simScores2),col="yellow")

#take 2 TEs, DST and QB?
picks3<-getPicks(slot="Slot4", numRB=4, numWR = 4,numTE=2,numK=1,numQB=2, numDST=2,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring=scoring)
picks3
simScores3<-replicate(4000, simSeason(picks3 , scoring=scoring))

lines(cummean(simScores3),col="green")

#take 1QB and 2 DST/TE?
picks4<-getPicks(slot="Slot4", numRB=4, numWR = 5,numTE=2,numK=1,numQB=1, numDST=2,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring=scoring)
picks4
simScores4<-replicate(4000, simSeason(picks4 , scoring=scoring))

lines(cummean(simScores4),col="purple")

#2 TE/QB + take Antonio Brown at pick 1?
fixPlayer<-"Antonio Brown"
picks5<-getPicks(slot="Slot4", numRB=5, numWR =5 ,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=fixPlayer, scoring=scoring)
picks5
simScores5<-replicate(4000, simSeason(picks5 , scoring=scoring))


lines(cummean(simScores5),col="orange")

# #Antonio Brown + RB heavy?
# picks6<-getPicks(slot="Slot4", numRB=6, numWR = 4,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix="Antonio Brown", scoring=scoring)
# simScores6<-replicate(4000, simSeason(picks6, scoring=scoring, scoring=scoring))


#zero-RB in R1-4?
picks7<-getPicks(slot="Slot4", numRB=5, numWR = 5,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=adp$Player[adp$ADP_Rank<=51& adp$Pos=="RB"], fix=c(), scoring=scoring)
picks7
simScores7<-replicate(4000, simSeason(picks7 , scoring=scoring))

lines(cummean(simScores7),col="dark green")

#TE in R2?
picks8<-getPicks(slot="Slot4", numRB=5, numWR = 5,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=, fix="Robert Gronkowski", scoring=scoring)
picks8
simScores8<-replicate(4000, simSeason(picks8 , scoring=scoring))
summary(simScores8)

lines(cummean(simScores8),col="brown", lty=2)

picks9<-getPicks(slot="Slot4", onePos=rep("QB", 10), numRB=5, numWR = 5,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring=scoring)
picks9
simScores9<-replicate(4000, simSeason(picks9 , scoring=scoring))

lines(cummean(simScores9),col="dark red", lty=2)


Parameters<-c("1. RBx5,WRx5,QBx2,K/DST/TEx1", "2. RBx6,WRx4,QBx2,K/DST/TEx1","3. RBx4,WRx6,QBx2,K/DST/TEx1",
              "4. RBx4, WRx5, QB/TEx2, K/DSTx1",    "5 RBx4, WRx4, QB/TE/DSTx2, Kx1",  "6. RBx4, WRx5, TE/DSTx2, QB/Kx1", 
              paste0("case 1+ ",fixPlayer ," in R1"), "case 1 + Zero RB in R1-4",
              "case 1+Gronk in R2", "case 1 + \u2264 1QB in R1-10" )

Sims<-c(simScores1, simScores1A,simScores1B,simScores2, simScores3, simScores4, simScores5, simScores7, simScores8, simScores9)
Sims<-data.frame(Sim=Sims, Parameter=rep(Parameters, each=4000))
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

ggsave(paste0("Parameter Testing/" ,scoring, " scoring-base case parameters.jpeg"),width = 7, height=4 , units = "in")
