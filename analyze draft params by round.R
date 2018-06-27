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
drafts6<-lapply(1:numDrafts,function(x)simDraft(alpha=.85, outPos=rep("QB", 3), onePos = rep("QB", 11))) #waiting on QB and DEF
simScores6<-lapply(drafts6, function(x)replicate(numSims,simSeason(x)))
quantile(unlist(simScores6))

#alpha=.85, heavyWR
drafts7<-lapply(1:numDrafts,function(x)simDraft(alpha=.85, numRB=4, numWR=6, numTE=1, numQB=2,numK=1, numFLEX=0, numDST=1)) #waiting on QB and DEF
simScores7<-lapply(drafts7, function(x)replicate(numSims,simSeason(x)))
quantile(unlist(simScores7))

#alpha=.85, heavyRB
drafts8<-lapply(1:numDrafts,function(x)simDraft(alpha=.85, numRB=6, numWR=4, numTE=1, numQB=2,numK=1, numFLEX=0, numDST=1,
                                           outPos=rep("QB", 4), onePos = rep("QB", 11))) #waiting on QB and DEF
simScores8<-lapply(drafts8, function(x)replicate(numSims,simSeason(x)))
quantile(unlist(simScores8))

#alpha=.85, no QB backup
drafts9<-lapply(1:numDrafts,function(x)simDraft(alpha=.85, numRB=5, numWR=5, numTE=2, numQB=1,numK=1, numFLEX=0, numDST=1)) #waiting on QB and DEF
simScores9<-lapply(drafts9, function(x)replicate(numSims,simSeason(x)))

#alpha=.85, QB+TE backup, <=1 QB in R1-11
drafts10<-lapply(1:numDrafts,function(x)simDraft(alpha=.85,numRB=4, unmWR=5, numTE=2, numQB=2, numK=1, numDST=1, onePos = rep("QB", 11))) #draft 1 qb in midrounds
simScores10<-lapply(drafts10, function(x)replicate(numSims,simSeason(x)))


quantile(unlist(simScores11))
#r1-4: 2-4RBs, 0-3WR, 1TE, 0QB
#R1-7: 3-4RBs, 1-2WRs, 1TE, 1QB

freqs<-as.data.frame.matrix(table(unlist(lapply(drafts8, function(x) x$Player)), unlist(lapply(drafts8, function(x) 1:nrow(x)))))
freqs[ order(freqs[,1], freqs[, 2], freqs[,3], freqs[, 4], freqs[, 5], freqs[, 5], freqs[, 5], freqs[, 6], freqs[, 7], freqs[, 8], decreasing = T),]

hist(unlist(simScores))

table(unlist(lapply(drafts10, function(x) x$Player[3])))

#loop through every draft slot--may take a while
allSlots<-lapply(paste("Slot", 1:12, sep=""), function(x)
  lapply(1:numDrafts,function(y) simDraft(alpha=.85,outPos = rep("QB",3) ,onePos = rep("QB", 11), slot = x))
)
allSlotSims<-lapply(allSlots, function(x){lapply(x, function(y)replicate(numSims,simSeason(y)))})


pos<-4

freqs<-as.data.frame.matrix(table(unlist(lapply(allSlots[[pos]], function(x) x$Player)), unlist(lapply(allSlots[[pos]], function(x) 1:nrow(x)))))
freqs[ order(freqs[,1], freqs[, 2], freqs[,3], freqs[, 4], freqs[, 5], freqs[, 5], freqs[, 5], freqs[, 6], freqs[, 7], freqs[, 8], decreasing = T),]

quantile(unlist(allSlotSims[[pos]][sapply(allSlots[[pos]], function(x) sum(x$Pos[1:4]%in% c( "WR"))==0)]))


#results
lapply(allSlotSims, function(x)quantile(unlist(x)))

