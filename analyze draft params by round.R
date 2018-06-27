source("all data, optimize by round.R")


#default parameters

drafts<-lapply(1:500,function(x)simDraft())  #get picks going round by round,players taken probabilistically based on adp
drafts[[5]]
table(unlist(lapply(drafts, function(x) x$Player[6])))
hist(sapply(drafts, function(x) sum(x$HALF)))


simScores<-lapply(drafts, function(x)replicate(100, simSeason(x))) 

quantile(unlist(simScores[sapply(drafts, function(x) sum(x$Player[1:4]=="Demaryius Thomas")==1)]))
median(sapply(drafts[sapply(drafts, function(x) "QB"%in% x$Pos[1:6])], function(x) sum(x$HALF)))
table(sapply(drafts2, function(x) sum(x$Pos[1:8]=="QB")))
hist(unlist(simScores))
quantile(unlist(simScores))

#testing out different algorithm parameters

#no qb constraints, can take 2 early
drafts5<-lapply(1:1000,function(x)simDraft(alpha=.9)) 
simScores5<-lapply(drafts5, function(x)replicate(100, simSeason(x)))
quantile(unlist(simScores5))

#alpha=1, default params
drafts6<-lapply(1:1000,function(x)simDraft(alpha=1, outPos=rep("QB", 3), onePos = rep("QB", 11))) #waiting on QB and DEF
simScores6<-lapply(drafts6, function(x)replicate(100, simSeason(x)))
quantile(unlist(simScores6))

#
drafts7<-lapply(1:1000,function(x)simDraft(alpha=.8, numRB=4, numWR=4, numTE=1, numQB=2,numK=1, numFLEX=2, numDST=1,
                                           outPos=rep("QB", 4), onePos = rep("QB", 11))) #waiting on QB and DEF
simScores7<-lapply(drafts7, function(x)replicate(100, simSeason(x)))
quantile(unlist(simScores7))

#dst instead of te
drafts8<-lapply(1:1000,function(x)simDraft(alpha=.85, numRB=5, numWR=5, numTE=1, numQB=2,numK=1, numFLEX=0, numDST=1,
                                           outPos=rep("QB", 4), onePos = rep("QB", 11))) #waiting on QB and DEF
simScores8<-lapply(drafts8, function(x)replicate(100, simSeason(x)))
quantile(unlist(simScores8))

drafts9<-lapply(1:1000,function(x)simDraft(alpha=.85,outPos=rep("DST", 11), onePos = rep("QB", 11))) #waiting on QB and DEF
simScores9<-lapply(drafts9, function(x)replicate(100, simSeason(x)))

quantile(unlist(simScores9))

drafts10<-lapply(1:1000,function(x)simDraft(alpha=.85,outPos = c(rep("QB", 3)) ,onePos = rep("QB", 11))) #draft 1 qb in midrounds
simScores10<-lapply(drafts10, function(x)replicate(100, simSeason(x)))

# freqs<-as.data.frame.matrix(table(unlist(lapply(drafts10, function(x) x$Player)), unlist(lapply(drafts10, function(x) 1:nrow(x)))))
# freqs[ order(freqs[,1], freqs[, 2], freqs[,3], freqs[, 4], freqs[, 5], freqs[, 5], freqs[, 5], freqs[, 6], freqs[, 7], freqs[, 8], decreasing = T),]

quantile(unlist(simScores10))

quantile(unlist(simScores10[sapply(drafts10, function(x) sum(x$Pos[1:4]=="RB")==3)]))

drafts11<-lapply(1:1000,function(x)simDraft(alpha=.75,outPos = c(rep("QB", 4)) ,onePos = rep("QB", 11))) #draft 1 qb in midrounds
simScores11<-lapply(drafts11, function(x)replicate(100, simSeason(x)))

quantile(unlist(simScores11))
#r1-4: 2-4RBs, 0-3WR, 1TE, 0QB
#R1-7: 3-4RBs, 1-2WRs, 1TE, 1QB

freqs<-as.data.frame.matrix(table(unlist(lapply(drafts8, function(x) x$Player)), unlist(lapply(drafts8, function(x) 1:nrow(x)))))
freqs[ order(freqs[,1], freqs[, 2], freqs[,3], freqs[, 4], freqs[, 5], freqs[, 5], freqs[, 5], freqs[, 6], freqs[, 7], freqs[, 8], decreasing = T),]

hist(unlist(simScores))

table(unlist(lapply(drafts10, function(x) x$Player[3])))

#loop through every draft slot--may take a while
allSlots<-lapply(paste("Slot", 1:12, sep=""), function(x)
  lapply(1:1000,function(y) simDraft(alpha=.85,outPos = rep("QB",3) ,onePos = rep("QB", 11), slot = x))
)
allSlotSims<-lapply(allSlots, function(x){lapply(x, function(y)replicate(100, simSeason(y)))})


pos<-4

freqs<-as.data.frame.matrix(table(unlist(lapply(allSlots[[pos]], function(x) x$Player)), unlist(lapply(allSlots[[pos]], function(x) 1:nrow(x)))))
freqs[ order(freqs[,1], freqs[, 2], freqs[,3], freqs[, 4], freqs[, 5], freqs[, 5], freqs[, 5], freqs[, 6], freqs[, 7], freqs[, 8], decreasing = T),]

quantile(unlist(allSlotSims[[pos]][sapply(allSlots[[pos]], function(x) sum(x$Pos[1:4]%in% c( "WR"))==0)]))


#results
lapply(allSlotSims, function(x)quantile(unlist(x)))

