source("all data, optimize by round.R")

adp$fantPts_bin<-as.character(cut(adp$HALF, breaks=c(-50, 25, 75, 125, 175, 225, 400)))
adp<-merge(adp[, !grepl("meanError", colnames(adp))], errors[, c("meanError", "fantPts_bin", "Pos")], by=c("fantPts_bin", "Pos"))
adp$HALF2<-adp$HALF+adp$meanError
adp<-adp[order(adp$ADP_est, decreasing = F),]

numDrafts<-250
numSims<-100
scoring<-"HALF"

#default parameters
# drafts<-lapply(1:numDrafts,function(x)simDraft(scoring=scoring))  #get picks going round by round,players taken probabilistically based on adp
# drafts[[2]]
# table(unlist(lapply(drafts, function(x) x$Player[6])))
# hist(sapply(drafts, function(x) sum(x$HALF)))
# 
# simScores<-lapply(drafts, function(x)replicate(numSims, simSeason(x, scoring=scoring))) 
# save(simScores, file="simScores by round 1.RData")

#plot stabilization~numdrafts and numSims..sims stabilize very fast--large number of sims not needed--drafts stabilize slower,keep large numDrafts
# bydraft<-cummean(unlist(simScores))
# bysim<-cummean(unlist(lapply(1:numSims, function(x) lapply(simScores, `[[`, x))))
# plot(bydraft ,type="l", ylim=c(2030, 2080)) #cummean~draft
# lines(bysim, col="red", lty=2) #cummean~
# length(bysim)

# bysim[20000]
# 
# quantile(unlist(simScores[sapply(drafts, function(x) sum(x$Player[1:4]=="Demaryius Thomas")==1)]))
# median(sapply(drafts[sapply(drafts, function(x) "QB"%in% x$Pos[1:6])], function(x) sum(x$HALF)))
# table(sapply(drafts2, function(x) sum(x$Pos[1:8]=="QB")))
# hist(unlist(simScores))
# quantile(unlist(simScores))

cl<-makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

#shift=0
drafts<-lapply(1:numDrafts,function(x)simDraft(scoring=scoring,shift=0)) 
simScores<-foreach(x=drafts, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring)) }

#shift=.25
drafts2<-lapply(1:numDrafts,function(x)simDraft(scoring=scoring,shift=.25)) 
simScores2<-foreach(x=drafts2, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring)) }

#shift=0
drafts3<-lapply(1:numDrafts,function(x)simDraft(scoring=scoring,shift=-.15)) 
simScores3<-foreach(x=drafts3, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring)) }

quantile(unlist(simScores))

#shift=0, WR in R1
drafts4<-lapply(1:numDrafts,function(x)simDraft(scoring=scoring,shift=0,outPos = c(rep("RB", 1)))) #draft 1 qb in midrounds
simScores4<-foreach(x=drafts4, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring)) }

#shift=0, zeroRB in R1-4sim
drafts5<-lapply(1:numDrafts,function(x)simDraft(scoring=scoring,shift=0,outPos = c(rep("WR", 1))))  #draft 1 qb in midrounds
simScores5<-foreach(x=drafts5, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring)) }

#alpha=85, <=1 QB in R1-11
drafts6<-lapply(1:numDrafts,function(x)simDraft(scoring=scoring,shift=0, onePos = rep("QB", 11))) #waiting on QB and DEF
simScores6<-foreach(x=drafts6, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring)) }
quantile(unlist(simScores6))

#shift=0, heavyWR
drafts7<-lapply(1:numDrafts,function(x)simDraft(scoring=scoring,shift=0, numRB=4, numWR=6, numTE=1, numQB=2,numK=1, numFLEX=0, numDST=1))
simScores7<-foreach(x=drafts7, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring)) }
quantile(unlist(simScores7))

#shift=0, heavyRB, <=1 QB in r1-11
drafts8<-lapply(1:numDrafts,function(x)simDraft(scoring=scoring,shift=0, numRB=6, numWR=4, numTE=1, numQB=2,numK=1, numFLEX=0, numDST=1)) 
simScores8<-foreach(x=drafts8, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring)) }
quantile(unlist(simScores8))

#shift=0, no QB backup
drafts9<-lapply(1:numDrafts,function(x)simDraft(scoring=scoring,shift=0, numRB=5, numWR=5, numTE=2, numQB=1,numK=1, numFLEX=0, numDST=1))
simScores9<-foreach(x=drafts9, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring)) }
quantile(unlist(simScores9))

#shift=0, QB+TE backup, <=1 QB in R1-11
drafts10<-lapply(1:numDrafts,function(x)simDraft(scoring=scoring,shift=0,numRB=4, numWR=5, numTE=2, numQB=2, numK=1, numDST=1 )) #onePos = rep("QB", 11)
simScores10<-foreach(x=drafts10, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring)) }
quantile(unlist(simScores10))

#zero WR
drafts11<-lapply(1:numDrafts,function(x)simDraft(scoring=scoring,shift=0,numRB=6, numWR=4, numTE=1, numQB=2, numK=1, numDST=1,
                                                 onePos = rep("QB", 11), outPos=rep("RB", 4) )) #onePos = rep("QB", 11)
simScores11<-foreach(x=drafts11, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring)) }

quantile(unlist(simScores))
freqs<-as.data.frame.matrix(table(unlist(lapply(drafts, function(x) x$Player)), unlist(lapply(drafts, function(x) 1:nrow(x)))))
freqs[ order(freqs[,1], freqs[, 2], freqs[,3], freqs[, 4], freqs[, 5], freqs[, 5], freqs[, 5], freqs[, 6], freqs[, 7], freqs[, 8], decreasing = T),]


hist(unlist(simScores))
table(unlist(lapply(drafts10, function(x) x$Player[3])))


#loop through every draft slot--may take a while
drafts_allSlots<-lapply(paste("Slot", 1:12, sep=""), function(x)
  lapply(1:numDrafts,function(y) simDraft(scoring=scoring,shift=0,numRB=5, numWR=5, numTE=1, numQB=2, numK=1, numDST=1 , slot = x))
)
simScores_allSlots<-foreach(x=drafts_allSlots,.packages = c("data.table", "dplyr" ,"plyr"))%dopar%{
  lapply(x, function(y)replicate(numSims,simSeason(y, scoring=scoring)))}
#results
lapply(simScores_allSlots, function(x)quantile(unlist(x)))


#loop through every draft slot--may take a while
drafts_allSlots_zeroRB<-lapply(paste("Slot", 1:12, sep=""), function(x)
  lapply(1:numDrafts,function(y) simDraft(scoring=scoring,shift=0,numRB=5, numWR=5, numTE=1, numQB=2, numK=1, numDST=1 , slot = x, outPos=rep("RB", 1)))
)
simScores_allSlots_zeroRB<-foreach(x=drafts_allSlots_zeroRB,.packages = c("data.table", "dplyr" ,"plyr"))%dopar%{
  lapply(x, function(y)replicate(numSims,simSeason(y, scoring=scoring)))}


#loop through every draft slot--may take a while
drafts_allSlots_zeroWR<-lapply(paste("Slot", 1:12, sep=""), function(x)
  lapply(1:numDrafts,function(y) simDraft(scoring=scoring,shift=0,numRB=5, numWR=5, numTE=1, numQB=2, numK=1, numDST=1 , slot = x, outPos=rep("WR", 1)))
)
simScores_allSlots_zeroWR<-foreach(x=drafts_allSlots_zeroWR,.packages = c("data.table", "dplyr" ,"plyr"))%dopar%{
  lapply(x, function(y)replicate(numSims,simSeason(y, scoring=scoring)))}

save(list=ls()[grepl("simScores|drafts",ls())], file=paste0("Parameter Testing/" ,scoring," analyze draft params by round.RData"))

pos<-1
draft<-drafts_allSlots_zeroRB[[pos]]
freqs<-as.data.frame.matrix(table(unlist(lapply(draft, function(x) x$Player)), unlist(lapply(draft, function(x) 1:nrow(x)))))
freqs[ order(freqs[,1], freqs[, 2], freqs[,3], freqs[, 4], freqs[, 5], freqs[, 5], freqs[, 5], freqs[, 6], freqs[, 7], freqs[, 8], decreasing = T),][1:30,]

freqs<-as.data.frame.matrix(table(unlist(lapply(draft, function(x) x$Player)), unlist(lapply(draft, function(x) 1:nrow(x)))))
freqs$Player<-row.names(freqs)
freqs$Pos<-adp$Pos[match(freqs$Player, adp$Player)]
row.names(freqs)<-NULL
mostCommon<-lapply(1:15, function(x) {ret<-freqs[order(freqs[, x], decreasing = T),c(x, "Player", "Pos") ][1:4,];colnames(ret)[1]<-"Times";ret$Round<-x;ret})
mostCommon<-ldply(mostCommon, data.frame)
mostCommon[,c("Player", "Pos", "Round", "Times")]


#####PLOT#####

load(paste0("Parameter Testing/" ,scoring," analyze draft params by round.RData"))

Parameters<-c("1. RBx5,WRx5,QBx2,K/DST/TEx1 (default)", "2. default, shift=.25","3. default, shift=-.15",
              "4. zero RB in R1, shift=0", "5. zero WR in R1, shift=0",  "6. \u2264 1QB in R1-11, shift=0", 
              "7. RBx4, WRx6, shift=0", "8. RBx6, WRx4, \u2264 1QB in R1-11, shift=0",
              "9. RBx5,WRx5,TEx2,QB/DST/Kx1, shift=0", "10. RBx4,WRx5,QB/TEx2,DST/Kx1, shift=0", 
              "11. Zero RB in R1-4, \u2264 1QB in R1-11,  shift=0" )
makeParamPlot(Parameters=Parameters, Title="Simulation Results for Different Draft Parameters")

ggsave(paste0("Parameter Testing/" ,scoring, " scoring-by round parameters.jpeg"),width = 7, height=3.4 , units = "in")


freqs<-as.data.frame.matrix(table(unlist(lapply(drafts10, function(x) x$Player)), unlist(lapply(drafts10, function(x) 1:nrow(x)))))
freqs$Player<-row.names(freqs)
freqs$Pos<-adp$Pos[match(freqs$Player, adp$Player)]
row.names(freqs)<-NULL
mostCommon<-lapply(1:15, function(x) freqs[order(freqs[, x], decreasing = T),c(x, "Player", "Pos") ][1:3,])


#####PLOT#####
load(paste0("Parameter Testing/" ,scoring," analyze draft params by round.RData"))

p<-makeSlotPlot()
p
ggsave(paste0("Parameter Testing/" ,scoring, " scoring-by round slots.jpeg"),width = 7, height=3.4 , units = "in")

