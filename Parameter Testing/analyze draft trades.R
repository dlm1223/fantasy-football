source("all data, optimize by round.R")

adp$fantPts_bin<-as.character(cut(adp$HALF, breaks=c(-50, 25, 75, 125, 175, 225, 400)))
adp<-merge(adp[, !grepl("meanError", colnames(adp))], errors[, c("meanError", "fantPts_bin", "Pos")], by=c("fantPts_bin", "Pos"))
adp$HALF2<-adp$HALF+adp$meanError
adp<-adp[order(adp$ADP_est, decreasing = F),]

numDrafts<-250
numSims<-100
scoring<-"HALF"
load( file=paste0(scoring," analyze draft params by round.RData"))


drafts<-lapply(1:numDrafts,function(x)
  simDraft(scoring=scoring,alpha=1,numWR=6, numRB=5, numTE=1, numQB=2, outPos=rep("RB", 1),onePos=c(rep("QB", 10))))
simScores<-foreach(x=drafts, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring, numWR = 3)) }


drafts1<-lapply(1:numDrafts,function(x)
  simDraft(scoring=scoring,alpha=1,numWR=6, numRB=5, numTE=1, numQB=2,onePos=c(rep("QB", 10))))
cl<-makeCluster(2, type = "SOCK")
registerDoSNOW(cl)
simScores1<-foreach(x=drafts1, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring, numWR = 3)) }


drafts2<-lapply(1:numDrafts,function(x)
  simDraft(scoring=scoring,alpha=1,numWR=6, numRB=5, numTE=1, numQB=2, outPos=rep("WR", 1),onePos=c(rep("QB", 10))))
simScores2<-foreach(x=drafts2, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring, numWR = 3)) }

drafts3<-lapply(1:numDrafts,function(x)
  simDraft(scoring=scoring,alpha=1,numWR=6, numRB=4, numTE=2, numQB=2,onePos=c(rep("QB", 10))))
simScores3<-foreach(x=drafts3, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring, numWR = 3)) }


drafts4<-lapply(1:numDrafts,function(x)
  simDraft(scoring=scoring,alpha=1,numWR=7, numRB=4, numTE=1, numQB=2,onePos=c(rep("QB", 10))))
simScores4<-foreach(x=drafts4, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring, numWR = 3)) }



quantile(unlist(simScores))
quantile(unlist(simScores1))
quantile(unlist(simScores2))
quantile(unlist(simScores3))
quantile(unlist(simScores4))

cl<-makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

customPicks<-c(4, 21,55, 66, pickDF$Slot4[4:15] )
customPicks<-customPicks[order(customPicks)]
customdrafts1<-lapply(1:numDrafts,function(x)
  simDraft(scoring=scoring,customPicks=customPicks, alpha=1,numWR=6, numRB=5, numTE=1, numQB=2,onePos=c(rep("QB", 10))))
customScores1<-foreach(x=customdrafts1, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring, numWR = 3)) }

customPicks<-c( 15, 39, 21,28, pickDF$Slot4[4:15] )
customPicks<-customPicks[order(customPicks)]
customdrafts2<-lapply(1:numDrafts,function(x)
  simDraft(scoring=scoring,customPicks=customPicks,alpha=1,numWR=6, numRB=5, numTE=1, numQB=2,onePos=c(rep("QB", 10))))
customScores2<-foreach(x=customdrafts2, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring, numWR = 3)) }


customPicks<-c( 4,41, 56, 28, pickDF$Slot4[4:15] )
customPicks<-customPicks[order(customPicks)]
customdrafts3<-lapply(1:numDrafts,function(x)
  simDraft(scoring=scoring,customPicks=customPicks,alpha=1,numWR=6, numRB=5, numTE=1, numQB=2,onePos=c(rep("QB", 10))))
customScores3<-foreach(x=customdrafts3, .packages = c("data.table", "dplyr", "plyr"))%dopar%{
  replicate(numSims, simSeason(x, scoring=scoring, numWR = 3)) }


freqs<-as.data.frame.matrix(table(unlist(lapply(drafts4, function(x) x$Player)), unlist(lapply(drafts4, function(x) 1:nrow(x)))))
freqs[ order(freqs[,1], freqs[, 2], freqs[,3], freqs[, 4], freqs[, 5], freqs[, 5], freqs[, 5], freqs[, 6], freqs[, 7], freqs[, 8], decreasing = T),]

quantile(unlist(customScores1))
quantile(unlist(customScores2))
quantile(unlist(customScores3))


