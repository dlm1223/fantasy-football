library(rvest)
library(httr)
library(plyr)
library(dplyr)
library(RJSONIO)
library(XML)
library(jsonlite)
library(RCurl)
library(curl)
library(data.table)
library(devtools)
library(nflscrapR)
library(MASS)
library(zoo)
library(RSelenium)
library(caret)
library(StatRank)
options(stringsAsFactors=FALSE)
source('functions.R', encoding = 'UTF-8')
# devtools::install_github(repo = "maksimhorowitz/nflscrapR")
# devtools::install_github("ropensci/RSelenium")



###SCRAPE DATA####

##ESPN.COM DATA###
getStats<-function(team, year){
  page<-read_html(paste(c("http://www.espn.com/nfl/team/stats/_/name/",team ,"/year/", year), sep="", collapse=""))
  stats<-page%>% html_table(fill=T, header=T)
  cleanStats<-function(df){
    names<-as.character(df[which(df[, 1]=="NAME"),])
    colnames(df)<-names
    df[-(1:which(df[, 1]=="NAME")),!is.na(colnames(df))& !colnames(df)%in%"NA"]
  }
  stats<-lapply(stats, cleanStats)
  ids<-page%>% html_nodes("td a")%>% html_attr("href")
  players<-page%>% html_nodes("td a")%>% html_text()
  
  ids<-data.frame(ID=ids, NAME=players)
  ids<-unique(ids)
  
  colnames(stats[[1]])[-1]<-paste0("pass", colnames(stats[[1]])[-1])
  colnames(stats[[2]])[-1]<-paste0("rush", colnames(stats[[2]])[-1])
  colnames(stats[[3]])[-1]<-paste0("rec", colnames(stats[[3]])[-1])
  colnames(stats[[4]])[-1]<-paste0("def", colnames(stats[[4]])[-1])
  colnames(stats[[6]])<-c("NAME", "kick.retATT", "kick.retYDS", "kick.retAVG", "kick.retLONG", "kick.retTD", "punt.retATT", "punt.retYDS", 
                          "punt.retAVG", "punt.retLONG", "punt.retTD", "punt.retFC")
  colnames(stats[[7]])[-1]<-paste0("kick", colnames(stats[[7]])[-1])
  stats[[9]]<-ids
  if(year==2008& team=="dal"){
    stats[[4]]$NAME[stats[[4]]$NAME=="Roy Williams"& stats[[4]]$defSOLO==4]<-"Roy Williams 2"
    stats[[9]]$NAME[stats[[9]]$ID=="http://www.espn.com/nfl/player/_/id/3536/roy-williams"]<-"Roy Williams 2"
  }
  
  stats<-Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by =c("NAME"), all = TRUE),
                stats[-c(5, 8)])
  stats$NAME<-gsub(" 2", "", stats$NAME)
  stats$Year<-year
  stats$Team<-team
  # stats[!stats$NAME%in% c("Totals", "Opponents"), ]
  stats
}

ids<-read_html("http://www.espn.com/nfl/standings")%>% html_nodes(".hide-mobile a")%>% html_attr("href")
teams<-sapply(strsplit(ids, "/"), `[[`, 6)
abbrev$Abbrev4<-teams[order(teams)][c(1:16,18:27, 17, 28:32) ]
abbrev$Abbrev4[abbrev$Abbrev=="Sea"]<-"sea"
abbrev$Abbrev4[abbrev$Abbrev=="Sfo"]<-"sf"

importGrid<-expand.grid(team=teams,Year=2000:2017 )
importGrid<-importGrid[!(importGrid$team=="hou" & importGrid$Year<2002),]

season.stats<-ldply(lapply(1:nrow(importGrid), function(x) 
{print(x); getStats(team = as.character(importGrid$team[x]), year=importGrid$Year[x])}), data.frame)

season.stats$Team<-coordName(abbrev$Abbrev[match( season.stats$Team, abbrev$Abbrev4)])
season.stats$NAME<-coordName(season.stats$NAME)
colnames(season.stats)[colnames(season.stats)=="ID"]<-"ESPNID"


# team.stats<-season.stats[season.stats$NAME%in% c("Totals", "Opponents"), ]
season.stats<-season.stats[!season.stats$NAME%in% c("Totals", "Opponents"),]
# table(team.stats$Team)

###ESPN GAME LOGS####
ids<-read_html("http://www.espn.com/nfl/standings/_/season/2017")%>% html_nodes(".hide-mobile a")%>% html_attr("href")
teams<-sapply(strsplit(ids, "/"), `[[`, 6)
abbrev$Abbrev4<-teams[order(teams)][c(1:16,18:27, 17, 28:32) ]
abbrev$Abbrev4[abbrev$Abbrev=="Sea"]<-"sea"
abbrev$Abbrev4[abbrev$Abbrev=="Sfo"]<-"sf"

importGrid<-expand.grid(team=teams,Year=2005:2018, stringsAsFactors = F )

library(lubridate)
getGameLinks<-function(team, year){
  print(c(team, year))
  page<-read_html(paste0("http://www.espn.com/nfl/team/schedule/_/name/", team,"/year/", year))
  sched<-page%>% html_table() 
  sched<-sched[[1]]
  sched$Type<-""
  if(year!=2018){
    sched$Type[1:grep("Regular Season", sched$X1)]<-"Postseason"
    sched$Type[grep("Regular Season", sched$X1):grep("Preseason", sched$X1)]<-"Regular Season"
    sched$Type[grep("Preseason", sched$X1):nrow(sched)]<-"Preseason"
  } else{
    sched$Type[1:grep("Regular Season", sched$X1)]<-"Preseason"
    sched$Type[grep("Regular Season", sched$X1):nrow(sched)]<-"Regular Season"
  }
  sched<-sched[!grepl("Regular|Preseason|Postseason|DATE", sched$X1)& !grepl("BYE|DATE", sched$X2),]
  sched$DATE<-as.Date(sched$X2, format="%a, %b %d")
  year(sched$DATE)<-ifelse(month(sched$DATE)%in% 1:2, year+1, year)
  
  #games w missing links
  sched<-sched[!(sched$X5%in% c( "POSTPONED", "CANCELED")),] 
  if(team%in% c("kc", "gb")& year==2003){
    sched<-sched[!(sched$X5%in% ""),] 
  }
  
  sched<-sched[, c(1,8, 9)]
  colnames(sched)[1]<-"Week"
  
  games<-page%>% html_nodes("td:nth-child(4) a")%>% html_attr("href") %>% gsub("recap", "boxscore", .)
  if(length(games)==0){games<-NA}
  sched$GameLink<-games
  sched$GameLink[sched$DATE>=Sys.Date()]<-NA
  
  sched$Team<-team
  sched$Year<-year
  sched
}
gameLinks<-list();length(gameLinks)<-nrow(importGrid)

#inspect: den2005(x=80), ari05 (x=95)
for(x in 1:length(gameLinks)){
  gameLinks[[x]]<- getGameLinks(team = importGrid$team[x], year=importGrid$Year[x])
}
gameLinks<-ldply(gameLinks, data.frame)
table(gameLinks$Year)

importGrid<-unique(gameLinks$GameLink[gameLinks$Year>=2005& !(gameLinks$Type=="Preseason"& gameLinks$Year<=2013)])

importGame<-function(link){
  # link<-importGrid[2]
  page<-tryCatch(read_html(paste0("http:", link)), error=function(e){})
  stats<-tryCatch(page%>%html_table(fill=T), error=function(e) {})
  if(length(stats)>3){
    stats<-stats[-c(1,20, 21, 22, 23)]
    
    ids<-page%>% html_nodes("td a")%>% html_attr("href")
    players<-page%>% html_nodes("td a")%>% html_text()
    
    ids<-data.frame(ID=ids, NAME=players)
    ids<-unique(ids)
    
    colnames(stats[[1]])[-1]<-paste0("pass", colnames(stats[[1]])[-1])
    colnames(stats[[2]])[-1]<-paste0("pass", colnames(stats[[2]])[-1])
    
    colnames(stats[[3]])[-1]<-paste0("rush", colnames(stats[[3]])[-1])
    colnames(stats[[4]])[-1]<-paste0("rush", colnames(stats[[4]])[-1])
    
    colnames(stats[[5]])[-1]<-paste0("rec", colnames(stats[[5]])[-1])
    colnames(stats[[6]])[-1]<-paste0("rec", colnames(stats[[6]])[-1])
    
    colnames(stats[[7]])[-1]<-paste0("fum", colnames(stats[[7]])[-1])
    colnames(stats[[8]])[-1]<-paste0("fum", colnames(stats[[8]])[-1])
    
    colnames(stats[[9]])[-1]<-c('defTOT', "defSOLO", "defSACKS", "defTFL", "defPD", "defQBHits", "defRecTD", "defMISC")
    colnames(stats[[10]])[-1]<-c('defTOT', "defSOLO", "defSACKS", "defTFL", "defPD", "defQBHits", "defRecTD", "defMISC")
    
    colnames(stats[[11]])[-1]<-paste0("def", colnames(stats[[11]])[-1])
    colnames(stats[[12]])[-1]<-paste0("def", colnames(stats[[12]])[-1])
    
    colnames(stats[[13]])[-1]<-paste0("kick.ret", colnames(stats[[13]])[-1])
    colnames(stats[[14]])[-1]<-paste0("kick.ret", colnames(stats[[14]])[-1])
    
    colnames(stats[[15]])[-1]<-paste0("punt.ret", colnames(stats[[15]])[-1])
    colnames(stats[[16]])[-1]<-paste0("punt.ret", colnames(stats[[16]])[-1])
    
    colnames(stats[[17]])[-1]<-paste0("kick", colnames(stats[[17]])[-1])
    colnames(stats[[18]])[-1]<-paste0("kick", colnames(stats[[18]])[-1])
    
    stats<-list(rbindlist(stats[1:2]), rbindlist(stats[3:4]), rbindlist(stats[5:6]),
                rbindlist(stats[7:8]), rbindlist(stats[9:10]), rbindlist(stats[11:12]), rbindlist(stats[13:14]), rbindlist(stats[15:16]), rbindlist(stats[17:18]))
    
    cleanStats<-function(x){
      colnames(x)[1]<-"NAME"
      x<-data.frame(x)
      x[!x$NAME%in%c("TEAM", ""),]
    }
    stats<-lapply(stats, cleanStats)
    
    stats[[10]]<-ids[grepl("player", ids$ID), ]
    # if(year==2008& team=="dal"){
    #   stats[[4]]$NAME[stats[[4]]$NAME=="Roy Williams"& stats[[4]]$defSOLO==4]<-"Roy Williams 2"
    #   stats[[9]]$NAME[stats[[9]]$ID=="http://www.espn.com/nfl/player/_/id/3536/roy-williams"]<-"Roy Williams 2"
    # }
    # 
    stats<-Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by =c("NAME"), all = TRUE),  stats)
    
    # string<-paste0(paste0(LETTERS, sep="", collapse = "[.]|"), "[.]")
    # stats$NAME<-sapply(strsplit(stats$NAME, string), `[[`, 1)
    # stats$NAME<-gsub(" 2", "", stats$NAME)
    stats$NAME<-NULL
    stats$GameLink<-link
  } else{
    stats<-data.frame()
  }
  stats
  
}
importGame(importGrid[973])

if(!exists("gameLogs")){
  gameLogs<-list();length(gameLogs)<-length(importGrid)
  for(x in 1:length(gameLogs)){
    print(x)
    gameLogs[[x]]<-importGame(importGrid[x])
  }
  gameLogs<-ldply(gameLogs, data.frame)
  
} else{
  importGrid<-importGrid[!importGrid%in% teamLogs$GameLink]
  gameLogs2<-list();length(gameLogs2)<-length(importGrid)
  for(x in 1:length(gameLogs2)){
    print(x)
    gameLogs2[[x]]<-importGame(importGrid[x])
  }
  #rbind
  gameLogs2<-ldply(gameLogs2, data.frame)
  gameLogs<-rbind(gameLogs[!gameLogs$GameLink%in%gameLogs2$GameLink, ], gameLogs2)
}



####TEAM-GAME-LOGS#######

importGrid<-unique(gameLinks$GameLink[gameLinks$Year>=2002])

importGame<-function(link){
  # link<-importGrid[1251]
  page<-tryCatch(read_html(paste0("http:", link)), error=function(e){})
  stats<-tryCatch(page%>%html_table(fill=T), error=function(e) {})
  if(length(stats)>=1){
    stats<-stats[1][[1]]
  }
  colnames(stats)[1]<-"Team"
  stats$GameLink<-link
  stats$TeamLOC<-c("AWAY", "HOME")
  stats
}
if(!exists("teamLogs")){
  teamLogs<-list();length(teamLogs)<-length(importGrid)
  for(x in 1:length(teamLogs)){
    print(x)
    teamLogs[[x]]<-importGame(importGrid[x])
  }
  teamLogs<-ldply(teamLogs, data.frame)
  
} else{
  importGrid<-importGrid[!importGrid%in% teamLogs$GameLink]
  teamLogs2<-list();length(teamLogs2)<-length(importGrid)
  for(x in 1:length(teamLogs2)){
    print(x)
    teamLogs2[[x]]<-importGame(importGrid[x])
  }
  #rbindfill
  teamLogs2<-ldply(teamLogs2, data.frame)
  teamLogs2<-teamLogs2[!teamLogs2$GameLink=="//www.espn.com/nfl/boxscore/_/gameId/231019015", ]
  teamLogs<-rbind(teamLogs[!teamLogs$GameLink%in%teamLogs2$GameLink, ], teamLogs2[, colnames(teamLogs2)%in% colnames(teamLogs)])
}


###DST STATS#####

getYear<-function(year){
  page<-read_html(paste(c("https://fantasydata.com/nfl-stats/nfl-fantasy-football-stats.aspx?fs=4&stype=0&sn=",2017-year ,"&scope=0&w=0&ew=0&s=&t=0&p=11&st=FantasyPointsYahoo&d=1&ls=&live=false&pid=false&minsnaps=4"), sep="", collapse=""))
  stats<-page%>% html_table()
  stats<-stats[[1]]
  stats$Season<-year
  stats
}
defense.stats<-ldply(lapply(2002:2017, getYear), data.frame)
defense.stats<-defense.stats[, !colnames(defense.stats)%in% c("Return.TD", "Rk", "PPG", "Gms")]
defense.stats[, c("Player", "Team")]<-sapply(defense.stats[, c("Player", "Team")], coordName)
defense.stats[, !colnames(defense.stats)%in% c("Player", "Pos","Team")]<-sapply(defense.stats[, !colnames(defense.stats)%in% c("Player", "Pos","Team")], as.numeric)
colnames(defense.stats)[colnames(defense.stats)=="Fantasy.Points"]<-"fantPts"

tail(defense.stats)

#defense.passing

getYear<-function(year){
  print(year)
  page<-read_html(paste0("http://www.espn.com/nfl/statistics/team/_/stat/total/position/defense/year/", year))
  stats<-page%>% html_table(header = T)
  stats<-stats[[1]]
  stats<-stats[, c(2, 4, 6, 8, 10)]
  colnames(stats)<-c("Team", "YDS.Allowed", "PassYDS.Allowed", "RushYDS.Allowed", "Pts.Allowed")
  stats$Year<-year
  stats
  
}
defense.passing<-ldply(lapply(2002:2018, getYear), data.frame)
defense.passing$Team<-coordName(defense.passing$Team)
setdiff(defense.passing$Team, season.stats$Team)
defense.passing[, 2:6]<-sapply(defense.passing[, 2:6], as.numeric)
defense.passing[defense.passing$Year==2018, 2:5]<-NA #current year values are NA


#espn ids###
# getPlayerInfo<-function(ESPNID){
#   page<-read_html(ESPNID)
#   htwt<-page%>% html_nodes("#content > div.mod-container.mod-no-header-footer.mod-page-header > div.mod-content > div.player-bio > ul.general-info > li:nth-child(2)")%>% html_text()
#   pos<-page%>% html_nodes("#content > div.mod-container.mod-no-header-footer.mod-page-header > div.mod-content > div.player-bio > ul.general-info > li.first")%>% html_text()
#   birth<-page%>% html_nodes("#content > div.mod-container.mod-no-header-footer.mod-page-header > div.mod-content > div.player-bio > ul.player-metadata.floatleft > li:nth-child(1)")%>% html_text()
#   if (length(htwt)==0){
#     htwt<-NA
#   } 
#   if(length(pos)==0){
#     pos<-NA
#   }
#   if(length(birth==0)){
#     birth<-NA
#   }
#   
#   data.frame(ESPNID=ESPNID, HTWT=htwt, Pos=pos, Birth=birth)
# }
# ids<-unique(season.stats$ESPNID)
# id_list<-list();length(id_list)<-length(ids)
# for(i in 1:length(ids)){
#   id_list[[i]]<-getPlayerInfo(ids[i])
#   if(i%%100==0){
#     print(i)
#   }
# }

# htwt<-trimws(unlist(strsplit(htwt, "\"," )))
# pos<-trimws(gsub(paste(c(0:9, "#"), collapse="|"), "", pos))
# birth<-gsub("Born", "", birth)

###COMBINE DATA####
pos<-"RB"
year<-2018

getStats<-function(pos, year){
  print(c(pos, year))
  page<-read_html(paste0("https://www.pro-football-reference.com/play-index/nfl-combine-results.cgi?request=1&year_min=",year ,"&year_max=",year ,"&pos%5B%5D=",
                         pos ,"&show=all&order_by=year_id"))
  stats<-page%>% html_table()
  stats<-stats[[1]]
  stats$ID<-NULL
  ids<-page%>%html_nodes("td:nth-child(3) a")%>% html_attr("href")
  names<-page%>%html_nodes("td:nth-child(3) a")%>% html_text()
  names<-trimws(names)
  if(length(ids)>0){
    ids<-data.frame(ID=ids, Player=names)
    stats<-merge(stats, ids, by="Player", all.x=T)
  } else{
    stats$ID<-NA
  }
  stats<-stats[!stats$Player=="Player",]
  stats
}
importGrid<-expand.grid(pos=c("RB", "WR", "TE", "QB","FB"), year=2000:2018, stringsAsFactors = F)

combine.stats<-ldply(lapply(1:nrow(importGrid),function(x) getStats(pos=importGrid$pos[x], year=importGrid$year[x])), data.frame)
combine.stats$Player<-coordName(combine.stats$Player)
combine.stats[, c("X40YD","Vertical", "BenchReps", "Broad.Jump","X3Cone", "Shuttle")]<-
  sapply(combine.stats[, c("X40YD","Vertical", "BenchReps", "Broad.Jump","X3Cone", "Shuttle")], as.numeric)


#roster data#####
library(RSelenium)
remDr<-rsDriver(browser = "chrome")
importGrid<-expand.grid(Team=abbrev$Abbrev3, Season=2000:2018, stringsAsFactors = F)
importGrid<-importGrid[!(importGrid$Team=="htx"& importGrid$Season<2002),]

importRoster<-function(team, season){
  
  remDr$client$navigate(paste0(c("http://www.pro-football-reference.com/teams/", team,"/", season,"_roster.htm"), collapse = ""))
  Sys.sleep(2)
  players<-readHTMLTable(remDr$client$getPageSource(url)[[1]])  
  players<-players$games_played_team
  players$Team<-team
  players$Season<-season
  ids<-read_html(remDr$client$getPageSource(url)[[1]])%>%html_nodes("#games_played_team .left:nth-child(2) a")%>% html_attr("href")
  players$ID<-NA
  players$ID[1:length(ids)]<-ids
  players
}
rostersList<-list();length(rostersList)<-nrow(importGrid)
for(x in 12:nrow(importGrid)){
  print(x)
  rostersList[[x]]<-importRoster(as.character(importGrid$Team[x]), importGrid$Season[x])
}
rosters<-rbindlist(rostersList[sapply(rostersList, is.data.frame)], fill=T)
rosters<-data.frame(rosters)
rosters$Player[is.na(rosters$Player)]<-rosters$Player?.[is.na(rosters$Player)]
rosters$Player?.<-NULL
rosters$Ht[rosters$Ht%in% c("0-0", "")]<-NA
rosters$Ht[!is.na(rosters$Ht)]<-as.numeric(sapply(strsplit(rosters$Ht[!is.na(rosters$Ht)], "-"), `[[`, 1))+
  as.numeric(gsub("\"", "",sapply(strsplit(rosters$Ht[!is.na(rosters$Ht)], "-"), `[[`, 2)))/12
rosters[, c("Wt", "Ht", "Age")]<-sapply(rosters[, c("Wt", "Ht", "Age")], as.numeric)
rosters$Yrs[rosters$Yrs%in% c("Rook","-1", "-2", "-3")]<-0
rosters$Yrs<-as.numeric(rosters$Yrs)
rosters[, c("Player", "Team")]<-sapply(rosters[, c("Player", "Team")], coordName)
rosters$Pick[grepl("pick", rosters$Drafted..tm.rnd.yr.)]<-sapply(strsplit(rosters$Drafted..tm.rnd.yr.[grepl("pick", rosters$Drafted..tm.rnd.yr.)], "/" ),`[[`,  3)
rosters$Pick<-stringr::str_extract_all(rosters$Pick, "[0-9]+") %>% unlist()%>% as.numeric()
rosters$Pick[rosters$Pick>=250| is.na(rosters$Pick)]<-250
rosters$Team<-coordTeam(rosters$Team)

#player ids
id_df<-ldply(lapply(paste("Player Data/", list.files("Player Data/", pattern = "rosters"), sep=""), read.csv), data.frame)
id_df[, c("Player","Team")]<-sapply(id_df[, c("Player","Team")], coordName)

setdiff(coordName(id_df$Player), rosters$Player)
setdiff(coordName(id_df$Player), season.stats$NAME)
setdiff(id_df$Team, season.stats$Team)

#guys with stats not matching up to rosters data..this should return 0 players
setdiff(season.stats$NAME, rosters$Player)

colnames(id_df)[colnames(id_df)=="Pos"]<-"PosNFL"
colnames(rosters)[colnames(rosters)=="Pos"]<-"PosREF"
rosters$PosREF<-toupper(rosters$PosREF)
id_df<-merge(rosters, id_df, by=c("Player","Season", "Team"), all.x=T )


id_df<-merge(id_df, season.stats[, c("NAME", "Year","Team", "ESPNID")], by.x=c("Player","Season", "Team"),by.y=c("NAME","Year", "Team"), all.x=T )
id_df<-id_df[!duplicated(id_df[,c("Player", "Season", "Team")])& !duplicated(id_df[, c("Player","Season", "Team")], fromLast = T), ]
id_df$RookieYear<-id_df$Season-id_df$Yrs

#clean IDS
id_df$ID[id_df$Season==2018& is.na(id_df$ID)]<-1:sum(id_df$Season==2018& is.na(id_df$ID))

test<-unique(id_df[!is.na(id_df$ESPNID)& !is.na(id_df$ID), c("ID", "Player", "ESPNID")])
test<-test[!duplicated(test$ID), ]

id_df$ESPNID[is.na(id_df$ESPNID)]<-test$ESPNID[match(id_df$ID[is.na(id_df$ESPNID)], test$ID)] #some players missing 2018 ids for some reason
id_df$ESPNID[id_df$Season==2018& is.na(id_df$ESPNID)]<-1:sum(id_df$Season==2018& is.na(id_df$ESPNID)) #still missing ESPNIDs for rookies
id_df<-id_df[order(id_df$Season, decreasing = F), ]

id_df[id_df$Player=="Kelvin Benjamin",]
# id_df<-unique(id_df[!is.na(id_df$GSIS_ID), c("name", "GSIS_ID", "Player","RookieYear","Ht", "Wt", "BirthDate")])
# id_df[duplicated(id_df$GSIS_ID)| duplicated(id_df$GSIS_ID, fromLast = T),]
# id_df<-id_df[!duplicated(id_df$GSIS_ID), ]

combine.stats[duplicated(combine.stats$ID)& !is.na(combine.stats$ID),]

id_df<-merge(id_df, combine.stats[!is.na(combine.stats$ID), c("ID", "X40YD", "Vertical", "BenchReps", "Broad.Jump", "X3Cone", "Shuttle")], 
             by="ID", all.x=T )


###RED ZONE DATA#####


importRZ<-function(season){
  
  page<- read_html(paste0("https://www.pro-football-reference.com/years/", season, "/redzone-receiving.htm"))
  stats<-page%>%html_table(fill=T)
  stats<-stats[[1]]
  colnames(stats)<-c("Player", "Team", "recTAR20", "recREC20", "recCATCH.20", "recYDS20", "recTD20", "recTAR.20", 
                     "recTAR10", "recREC10", "recCATCH.10", "recYDS10", "recTD10", "recTAR.10", "Info")
  stats<-stats[!stats$Player=="Player",!colnames(stats)%in% "Info" ]
  stats$ID<-page%>% html_nodes("th a")%>% html_attr("href")
  stats$Season<-season
  stats
}
season_receiving_rz<-ldply(lapply(2002:2017, importRZ), data.frame)


importRZ<-function(season){
  
  page<- read_html(paste0("https://www.pro-football-reference.com/years/", season, "/redzone-rushing.htm"))
  stats<-page%>%html_table(fill=T)
  stats<-stats[[1]]
  colnames(stats)<-c("Player", "Team", "rushATT20", "rushYDS20", "rushTD20", "rushATT.20",  
                     "rushATT10", "rushYDS10", "rushTD10", "rushATT.10",
                     "rushATT5", "rushYDS5", "rushTD5", "rushATT.5",
                     "Info")
  stats<-stats[!stats$Player=="Player",!colnames(stats)%in% "Info" ]
  stats$ID<-page%>% html_nodes("th a")%>% html_attr("href")
  stats$Season<-season
  stats
}
season_rushing_rz<-ldply(lapply(2002:2017, importRZ), data.frame)


importRZ<-function(season){
  
  page<- read_html(paste0("https://www.pro-football-reference.com/years/", season, "/redzone-passing.htm"))
  stats<-page%>%html_table(fill=T)
  stats<-stats[[1]]
  colnames(stats)<-c("Player", "Team", "passCMP20", "passATT20", "passCMP.20", "passYDS20","passTD20", "passINT20",
                     "passCMP10", "passATT10", "passCMP.10", "passYDS10","passTD10", "passINT10",                     "Info")
  stats<-stats[!stats$Player=="Player",!colnames(stats)%in% "Info" ]
  stats$ID<-page%>% html_nodes("th a")%>% html_attr("href")
  stats$Season<-season
  stats
}
season_passing_rz<-ldply(lapply(2002:2017, importRZ), data.frame)


season_rz_stats<-Reduce(function(x, y) merge(x, y, all=TRUE, by=c("Player", "Team", "ID", "Season")), list(season_passing_rz,season_rushing_rz, season_receiving_rz))
season_rz_stats[season_rz_stats$Player=="Jay Ajayi",] #teams already merged
season_rz_stats$Team<-NULL
season_rz_stats[, !colnames(season_rz_stats)%in% c("ID", "Season", "Player")]<-
  sapply(season_rz_stats[, !colnames(season_rz_stats)%in% c("ID", "Season", "Player")], function(x) as.numeric(gsub("%", "", x)))
season_rz_stats<-season_rz_stats[!season_rz_stats$Player=="", ]
season_rz_stats[, !grepl("recCATCH[.]|passCMP[.]", colnames(season_rz_stats))][is.na(season_rz_stats[, !grepl("recCATCH[.]|passCMP[.]", colnames(season_rz_stats))])]<-0

season_rz_stats[1:2,]
hist(season_rz_stats$rushATT.10)



##historical depth charts####

#use rselenium to get link formats of archived depth charts

remDr<-rsDriver(browser = "chrome")
links<-data.frame(matrix(NA, ncol=2, nrow=0))
colnames(links)<-c("DATE", "URL")

original<-"http://www.ourlads.com/nfldepthcharts/archive/42/phi"
remDr$client$navigate(original)
remDr$client$findElement(using = "css select", value = "#page-content-wrapper .pull-left")$clickElement()
text<-remDr$client$findElements(using="css select", value=".text")

dates<-unlist(sapply(text, function(x) x$getElementText()))
dates<-as.Date(dates, format="%m/%d/%Y")
for(i in 1:length(dates)){
  text[[i]]$clickElement()
  links[nrow(links)+1,]<-list(DATE=as.Date(dates[i]), URL=unlist(remDr$client$getCurrentUrl()))
  remDr$client$navigate(original)
  Sys.sleep(1)
  remDr$client$findElement(using = "css select", value = "#page-content-wrapper .pull-left")$clickElement()
  text<-remDr$client$findElements(using="css select", value=".text")
}
links<-links[!duplicated(links),]
links$DATE<-as.Date(links$DATE)
write.csv(links, file= "Player Data/Depth Chart Links.csv", row.names = F)

#once i have link formats, scrape depth charts from specified links

links<-read.csv("Player Data/Depth Chart Links.csv")
links<-rbind(links, data.frame(DATE=c("2018-09-01"), URL="http://www.ourlads.com/nfldepthcharts/depthchart/phi"))
links$DATE<-as.Date(links$DATE)
teamLinks<-read_html("http://www.ourlads.com/nfldepthcharts/")%>% html_nodes(".nfl-dc-mm-team-links a:nth-child(1)")%>% html_attr("href")
teamLinks<-gsub("depthchart/", "" ,teamLinks)

team<-teamLinks[1]
date<-as.Date("2009-09-06")

getStats<-function(date, team){
  url<-gsub("phi", team,links$URL[links$DATE==date])
  depth<-readHTMLTable(url)[[1]]
  depth<-depth[!is.na(depth[, "Player 1"])& !depth[, "Player 1"]=="?", !grepl("No", colnames(depth))]
  depth$DATE<-as.Date(date)
  depth$Team<-team
  depth
}
importGrid<-expand.grid(date=links$DATE[month(links$DATE)%in% 8:9][!duplicated(year(links$DATE[month(links$DATE)%in% 8:9]))],
                        team=teamLinks)
if(!exists("depthCharts")){
  depthCharts<-ldply(lapply(1:nrow(importGrid), function(x) getStats(date=importGrid$date[x],team=importGrid$team[x] )))
} else{
  depthCharts<-depthCharts[!depthCharts$DATE>=Sys.Date(),]
  depthCharts2<-ldply(lapply(which(!importGrid$date%in% depthCharts$DATE), function(x) getStats(date=importGrid$date[x],team=importGrid$team[x] )))
  depthCharts<-rbind(depthCharts, depthCharts2)
}

#PBP derived season stats
season_passing_df<-data.frame(fread("Player Data/season_passing_df.csv" ))
season_rushing_df<-data.frame(fread("Player Data/season_rushing_df.csv" ))
season_receiving_df<-data.frame(fread("Player Data/season_receiving_df.csv" ))
head(season.stats[1,])
head(id_df)


#vegas data#####

importOdds<-function(year){
  odds<-readHTMLTable(paste0("http://www.sportsoddshistory.com/nfl-win/?y=",year ,"&sa=nfl&t=win&o=t"))
  odds<-odds[[2]]
  odds$Year<-year
  odds
}
team.futures<-ldply(lapply(2001:2017, importOdds), data.frame)
team.futures[, c("Win.Total", "Over.Odds", "Under.Odds")]<-sapply(team.futures[, c("Win.Total", "Over.Odds", "Under.Odds")], as.numeric)
team.futures$Team<-coordName(team.futures$Team)

#live odds
page<-read_html("https://www.oddsshark.com/nfl/nfl-over-under-win-totals")
stats<-page%>% html_table()
stats<-stats[[1]]
stats<-stats[, c(1,3, 4, 5)]
colnames(stats)<-c("Team", "Win.Total", "Over.Odds", "Under.Odds")
stats[stats=='EVEN']<-"-100"
stats[, c("Win.Total", "Over.Odds", "Under.Odds")]<-sapply(stats[, c("Win.Total", "Over.Odds", "Under.Odds")], as.numeric)
stats$Team<-coordName(stats$Team)
stats$Year<-2018

team.futures<-rbind.fill(team.futures[team.futures$Year!=2018,], stats)
table( team.futures$Team, team.futures$Year)


####suspensions data####
page<-read_html("https://en.wikipedia.org/wiki/List_of_suspensions_in_the_National_Football_League")
stats<-page%>% html_table(fill=T)
stats<-stats[[2]]
stats$`Date suspended`<-as.Date(stats$`Date suspended`,format="%B %d, %Y")
stats$`Team at the time of suspension`<-coordName(stats$`Team at the time of suspension`)
stats$`Suspension length`[grepl("later reduced to", stats$`Suspension length`)]<-
  sapply( strsplit(stats$`Suspension length`[grepl("later reduced to", stats$`Suspension length`)], "later reduced to|\\["),`[[`, 2)
stats$`Suspension length`[grepl("later changed to", stats$`Suspension length`)]<-
  sapply( strsplit(stats$`Suspension length`[grepl("later changed to", stats$`Suspension length`)], "later changed to|\\["),`[[`, 2)
stats$Suspension<-as.numeric(gsub(" games|game|\\(|\\)", "", stats$`Suspension length`))
stats$Suspension[grepl("Entire|entire", stats$`Suspension length`)& grepl("season|Season", stats$`Suspension length`)]<-16

#filter
stats<-stats[which(stats$`Date suspended`>=as.Date("2007-02-10")), ]
stats<-stats[!grepl("overturned", stats$`Suspension length`), ]
stats<-stats[month(stats$`Date suspended`)%in% 2:9 , ]
stats<-stats[!(month(stats$`Date suspended`)%in% 9 & lubridate::day(stats$`Date suspended`)>8),] #suspended after week 1
stats<-stats[!grepl("Indefinite", stats$`Suspension length`), ]
stats$Name<-coordName(stats$Name)
stats$Season<-lubridate::year(stats$`Date suspended`)
stats<-stats[, c("Name", "Team at the time of suspension", "Suspension", "Season")]
colnames(stats)[2]<-c("Team")
suspensions<-stats

###game-injury data#####
importGrid<-expand.grid(Team=abbrev$Abbrev3, Season=2009:2017, stringsAsFactors = F)
importGrid<-importGrid[!(importGrid$Team=="htx"& importGrid$Season<2002),]

getInjured<-function(team,season){
  print(c(team, season))
  page<-read_html(paste0("https://www.pro-football-reference.com/teams/", team,"/",season ,"_injuries.htm"))
  stats<-html_table(page)[[1]]
  stats<-stats[stats$Player!="",]
  stats$ID<-page%>% html_nodes(".left a")%>% html_attr("href")
  stats<-melt(stats, id.vars =c("Player", "ID"), variable.name = "GameInfo", value.name = "Status")
  stats$Team<-team
  stats$Season<-season
  stats
}
game_injuries<-ldply(lapply(1:nrow(importGrid), function(x)getInjured(team=importGrid$Team[x], season=importGrid$Season[x]) ),data.frame)
game_injuries$GameInfo<-as.character(game_injuries$GameInfo)
game_injuries$DATE<-sapply(strsplit(game_injuries$GameInfo, "vs"), `[[`, 1)
game_injuries$DATE<-as.Date(paste(game_injuries$DATE, game_injuries$Season, sep="/"), format="%m/%d/%Y")
year(game_injuries$DATE)[month(game_injuries$DATE)%in% 1:2]<-game_injuries$Season[month(game_injuries$DATE)%in% 1:2]+1


###fref game links###

importGrid<-expand.grid(Team=abbrev$Abbrev3, Season=2003:2017, stringsAsFactors = F)
getLinks<-function(team,season){
  print(c(team, season))
  page<-read_html(paste0("https://www.pro-football-reference.com/teams/", team,"/",season ,".htm"))
  stats<-html_table(page)[[2]]
  names<-as.character(stats[1,])
  colnames(stats)<-names
  stats<-stats[!stats$Day%in% c("", "Day"),]
  ids<-page%>% html_nodes(".center a")%>% html_attr("href")
  stats$GameID<-ids[1:nrow(stats)]
  stats$Team<-team
  stats$Season<-season
  stats
}
gameLinks_FREF<-ldply(lapply(1:nrow(importGrid), function(x)getLinks(team=importGrid$Team[x], season=importGrid$Season[x]) ),data.frame)
head(gameLinks_FREF)


###game snaps#####

importGrid<-unique(gameLinks_FREF$GameID[gameLinks_FREF$Season%in% 2012:2017])
remDr<-rsDriver(browser = "chrome")
# remDr$client$setImplicitWaitTimeout(milliseconds = 5000)
#setTimeout(type = "implicit", milliseconds = 5000)

importGame<-function(gameid){
  
  #navigate to page & scrape html data
  url<-paste0("https://www.pro-football-reference.com",gameid)
  
  # remDr$client$setTimeout(type = "implicit", milliseconds = 5000)
  remDr$client$navigate(url)
  Sys.sleep(1)
  remDr$client$setTimeout(type = "implicit", milliseconds = 5000)
  
  # while(class(try(remDr$client$getPageSource(header = TRUE), silent = TRUE))=="try-error"){try(remDr$client$refresh(), silent = TRUE)}
  
  
  page<-remDr$client$getPageSource()[[1]]
  page2<-read_html(page)
  stats<-readHTMLTable(page)
  
  #organize snaps data and gameinfo data
  home_snaps<-stats$home_snap_counts
  colnames(home_snaps)<-c("Player", "Pos","OffSnaps", "OffPct", "DefSnaps", "DefPct","STSnaps", "STPct")
  home_snaps$ID<-page2%>% html_nodes("#home_snap_counts a")%>%html_attr("href")
  home_snaps$GameID<-gameid
  
  vis_snaps<-stats$vis_snap_counts
  colnames(vis_snaps)<-c("Player","Pos" ,"OffSnaps", "OffPct", "DefSnaps", "DefPct","STSnaps", "STPct")
  vis_snaps$ID<-page2%>% html_nodes("#vis_snap_counts a")%>%html_attr("href")
  vis_snaps$GameID<-gameid
  
  game_info<-stats$game_info
  colnames(game_info)<-c("Stat", "Value")
  game_info$GameID<-gameid
  
  list(home_snaps=home_snaps, vis_snaps=vis_snaps,game_info=game_info)
}
#taking too long to load oh well..

snapLogs2<-list();length(snapLogs2)<-length(importGrid)
for(x in 4:length(snapLogs2)){
  print(x)
  snapLogs2[[x]]<-importGame(gameid = importGrid[x])
}

##snap counts-alternative scrape
##
importGrid<-unique(id_df[id_df$Season%in% 2012:2017, c("ID",  "Season")])
year<-importGrid$Season[68];id<-importGrid$ID[68]


importSnaps<-function(id, year){
  page<-read_html(paste0("https://www.pro-football-reference.com/",gsub("[.]htm",  "", id) ,"/fantasy/", year))
  stats<-html_table(page)[[1]]
  stats<-stats[, stats[2, ]%in% c("Rk", "G#", "Date", "Tm", "Opp", "Result", "Pos","Num", "Pct")]
  colnames(stats)<-c("Rk", "G#","DATE", "Tm", "OPP", "Result", "Pos", "SnapsOff",
                     "SnapsOffPct", "SnapsDef","SnapsDefPct", "SnapsST", "SnapsSTPct")
  stats<-stats[!stats$DATE%in% c("", "Date", "Total"), ]
  if(nrow(stats)>0){
    stats$ID<-id
    stats$Season<-year
  }
  stats
}
snapLogs<-list();length(snapLogs)<-nrow(importGrid)
for(x in 1:length(snapLogs)){
  print(x)
  snapLogs[[x]]<-importSnaps(id = importGrid$ID[x] ,year=importGrid$Season[x])
}



importGrid<-expand.grid(Team=abbrev$Abbrev3, Season=2012:2017, stringsAsFactors = F)
importGrid<-importGrid[!(importGrid$Team=="htx"& importGrid$Season<2002),]

getSeason<-function(team,season){
  print(c(team, season))
  page<-read_html(paste0("https://www.pro-football-reference.com/teams/", team,"/",season ,"-snap-counts.htm"))
  stats<-html_table(page)[[1]]
  colnames(stats)<-c("Player", "Pos", "SnapsOff", "SnapsOffPct", "SnapsDef","SnapsDefPct", "SnapsST", "SnapsSTPct", "Description")
  stats<-stats[!stats$Player=='', !colnames(stats)%in% c("Description")]
  stats$ID<-page%>% html_nodes(".left a")%>% html_attr("href")
  stats$Team<-team
  stats$Season<-season
  stats
}
seasonSnaps<-ldply(lapply(1:nrow(importGrid), function(x) getSeason(team=importGrid$Team[x], season=importGrid$Season[x])), data.frame)


####game inactives#######
importGrid<-expand.grid(season=2011:2017, week=1:21)
importGrid<-importGrid[!(importGrid$season==2015& importGrid$week>=18),]

nicknames<-sapply(strsplit(abbrev$FullName, " "), function(x)tail(x, 1))
nicknames<-c(nicknames, 'Bucs')
nicknames2<-sapply(strsplit(abbrev$FullName, " "), function(x)paste(x[-length(x)],collapse=" "))
nicknames<-c(nicknames, nicknames2)

getInactive<-function(season, week){
  #season<-2011;week<-5
  #season<-2017;week<-9
  print(c(season, week))
  url<-paste0("https://fftoday.com/nfl/", substring(season, 3, 4),"_inactives_wk",week,".html")
  page<-read_html(url)
  stats<-page%>% html_nodes(".bodycontent li")%>% html_text()
  # headlines<-page%>% html_nodes(".headline, .largeheadline")%>% html_text()%>% paste(collapse = "|")
  # headlines<-headlines[headlines!=""]
  stats<-unlist(strsplit(stats, "\n\n"))
  stats<-gsub("\t|\n", " ", stats)
  
  
  # if(length(headline)>0){
  # stats<-gsub(headlines, " ",stats)
  # }
  stats<-gsub(paste(c(nicknames, abbrev$Abbrev2, " at "), collapse="|")," ", stats)
  # 
  # if(season==2012& week==2){
  #   stats[1:2]<-c("Denver: QB Caleb Hanie, OL Philip Blake, WR Andre Caldwell, CB Chris Harris, RB Ronnie Hillman, G Chris Kuper, DT Sealver Siliga",
  #                 "Atlanta: QB Dominique Davis, OG Joe Hawley, OT Lamar Holmes, CB Terrence Johnson, DE Jonathan Massaquoi, DE Cliff Matthews, S Charles Mitchell")
  # }
  stats<-stats[grepl(": | - ", stats) &!grepl(" AT | at ", stats)& sapply(stats, nchar)>=10]
  stats<-strsplit(stats, ", |: | - | and ")
  stats<-unlist(stats)
  stats<-stringr::str_replace(gsub("\\s+", " ", stringr::str_trim(stats)), "B", "b")
  stats<-stats[!stats==""& !grepl(" AM | PM ", stats)]
  stats<-gsub("linebacker |and ", "", stats)
  # stats  
  store<-" ol | c | wr | lb | ot | rb | cb | de | t | fb | qb | olb | lb | te | dt | g | dl | s "
  
  error<-stats[grepl(store, tolower(stats))]
  while(length(error)>0){
    error<-lapply(error, function(x){
      split<-gregexpr(store, tolower(x))[[1]][1];
      c(substring(x, 1, split-1), substring(x, split+1, nchar(x)))
    }) %>%
      unlist()%>% unname()
    stats<-c(stats[!grepl(store, tolower(stats))], error)
    
    #recalculate if errors
    error<-stats[grepl(store, tolower(stats))]
    
  }
  # toDF<-function(x){
  #   x<-data.frame(Team=x[1], Player=x[-1])
  #   x
  # }
  # stats<-ldply(lapply(stats, toDF), data.frame)
  stats<-data.frame(Player=stats)
  stats$Pos<-sapply(strsplit(stats$Player, " "), `[[`, 1)
  stats$Player<-sapply(strsplit(stats$Player, " "), function(x) paste(x[-1], collapse=" "))
  stats$Week<-week
  stats$Season<-season
  stats
}
game_inactives<-ldply(lapply(1:nrow(importGrid), function(x) getInactive(season=importGrid$season[x], week=importGrid$week[x])), data.frame)

game_inactives$Player<-coordName(game_inactives$Player)
setdiff(game_inactives$Player, rosters$Player)

#####vegas data#####




importGrid<-expand.grid(Team=abbrev$Abbrev3, Season=2003:2017, stringsAsFactors = F)
importGrid<-importGrid[!(importGrid$Team=="htx"& importGrid$Season<2002),]

getOdds<-function(team,season){
  print(c(team, season))
  page<-read_html(paste0("https://www.pro-football-reference.com/teams/", team,"/",season ,"_lines.htm"))
  stats<-html_table(page)[[1]]
  stats$GameID<-page%>% html_nodes("td:nth-child(5) a")%>% html_attr("href")
  stats$Team<-team
  stats$Season<-season
  stats
}
game_odds<-ldply(lapply(1:nrow(importGrid), function(x) getOdds(team=importGrid$Team[x], season=importGrid$Season[x])), data.frame)


###game locations####

importGrid<-expand.grid(Team=abbrev$Abbrev3, Season=2003:2017, stringsAsFactors = F)
importGrid<-importGrid[!(importGrid$Team=="htx"& importGrid$Season<2002),]

getLocs<-function(team, season){
  print(c(team, season))
  page<-read_html(paste0("https://www.pro-football-reference.com/teams/", team, "/", season, "_travel.htm"))
  stats<-html_table(page)[[1]]
  stats$Dist<-NULL
  stats$GameID<-page%>% html_nodes("th a")%>%html_attr("href")
  stats$Season<-season
  stats$Team<-team
  stats
  
}
game_locs<-ldply(lapply(1:nrow(importGrid), function(x) getLocs(team=importGrid$Team[x], season=importGrid$Season[x])), data.frame)
game_locs$Stadium[game_locs$Stadium=="Mercedes-Benz Stadium"]<-"Mercedes-Benz Superdome"
game_locs$Stadium[game_locs$Stadium=="Azteca Stadium"]<-"Estadio Azteca"

stadium_df<-read.csv("Misc Data/nfl_stadiums.csv")
setdiff(game_locs$Stadium, stadium_df$stadium_name)
game_locs<-merge(game_locs, stadium_df[, c("stadium_type", "stadium_name", "stadium_location", "stadium_weather_type", "LATITUDE", "LONGITUDE", "ELEVATION")], 
                 by.x="Stadium",by.y="stadium_name", all.x=T )
game_locs$Team<-coordName(game_locs$Team)
game_locs$Team_Full<-abbrev$Abbrev2[match(game_locs$Team,  abbrev$Abbrev )]
game_locs$Team_Full[game_locs$Team_Full%in% c("N.Y. Jets", "N.Y. Giants")]<-"East Rutherford"
game_locs$Team_Full[game_locs$Team_Full%in% c("Minnesota")]<-"Minneapolis"
game_locs$Team_Full[game_locs$Team_Full%in% c("Tampa Bay")]<-"Tampa"
game_locs$Team_Full[game_locs$Team_Full%in% c("Arizona")]<-"Glendale"
game_locs$Team_Full[game_locs$Team_Full%in% c("Carolina")]<-"Charlotte"
game_locs$Team_Full[game_locs$Team_Full%in% c("New England")]<-"Foxborough"
game_locs$Team_Full[game_locs$Team_Full%in% c("Tennessee")]<-"Nashville"
game_locs$ELEVATION[is.na(game_locs$ELEVATION)]<-0
us_cities<-read.csv("Misc Data/us_cities.csv")
us_cities<-us_cities[which((us_cities$population>=90000& us_cities$city%in% game_locs$Team_Full)| us_cities$city%in% c("East Rutherford", "Foxborough")), ]
us_cities$zips<-NULL
us_cities<-us_cities[!us_cities$id%in% c(1840008535,1840020483, 1840014653),]

colnames(game_locs)[colnames(game_locs)%in% c("LATITUDE", "LONGITUDE", "ELEVATION")]<-c("GameLat", "GameLong", "GameElevation")
colnames(us_cities)[colnames(us_cities)%in%c("lat", "lng")]<-c("TeamLat", "TeamLong")
game_locs<-merge(game_locs, us_cities[, c("city", "TeamLat", "TeamLong")], by.x="Team_Full", by.y="city")
game_locs$Dist<-geosphere::distHaversine(game_locs[, c( "GameLong", "GameLat")], game_locs[, c( "TeamLong", "TeamLat")])/1000

###madden data####
files<-list.files("Madden Data/")
readFile<-function(file){
  # file<-files[6]
  print(file)
  stats<-tryCatch(read.csv(paste0("Madden Data/", file),
                           quote = "",
                           row.names = NULL, 
                           stringsAsFactors = FALSE),
                  error=function(e){
                    read.csv(paste0("Madden Data/", file),
                             # quote = "",
                             row.names = NULL, 
                             stringsAsFactors = FALSE)
                    
                  }
  )
  # print(colnames(stats)[ tolower(colnames(stats))%in% c("position", "pos")])
  stats<-stats[, !colnames(stats)%in% c("Height")]
  stats$Pos<-stats[, tolower(colnames(stats))%in% c( "position", "pos")]
  
  if("name"%in% tolower(colnames(stats))){
    stats$Player<-stats$Name
  }else{
    stats$Player<-paste(stats[, grepl("first", tolower(colnames(stats)))], stats[, grepl("last", tolower(colnames(stats)))], sep=" ")
  }
  #extract team from file
  team<-sapply(strsplit(file, "_"), function(x) paste(x[1], x[2], collapse=" "))
  #some problems w. teams that have same city locs
  if(grepl("giants", file)){ team<-"Giants"} else if (grepl("jets", file)){team<-"Jets"} else if(grepl("rams", file)){team<-"Rams"} else if (grepl("chargers", file)){team<-"Chargers"}
  stats$Team<-team
  
  #extract stats from file
  stats$Rating<-stats[,grepl("ovr|overall", tolower(colnames(stats)))]
  file<-gsub("49", "", file)
  year<-as.numeric(stringr::str_extract_all(file, "[0-9]+")[[1]])
  if(year==25){year<-2014} else if(year<20){year<-year+2000}
  stats$Madden.Year<-year
  stats$Season<-year-1
  stats<-stats[!stats$Player%in% c("","Name"),]
  stats[, c("Player", "Pos", "Team", "Rating", "Madden.Year", "Season")]
}
madden.data<-ldply(lapply(files, readFile), data.frame)
madden.data$Team<-coordName(madden.data$Team)
madden.data$Rating<-as.numeric(madden.data$Rating)
table(madden.data$Team, madden.data$Season)
hist(madden.data$Rating)


###espn power rankings####
ids<-read_html("http://www.espn.com/nfl/standings/_/season/2017")%>% html_nodes(".hide-mobile a")%>% html_attr("href")
teams<-sapply(strsplit(ids, "/"), `[[`, 6)

importGrid<-expand.grid(season=2002:2015, team=teams, stringsAsFactors = F)


readESPN<-function(season, team){
  #season<-2015;team<-"ari"
  print(c(season, team))
  url<-paste0("http://www.espn.com/nfl/team/rankings/_/name/",team, "/year/",season)
  stats<-readHTMLTable(url, header = T, skip.rows = 1)[[1]]
  stats$COMMENT<-NULL
  stats$Team<-team
  stats$Season<-season
  stats
  
}
espn_pwr<-ldply(lapply(1:nrow(importGrid), function(x) readESPN(season=importGrid$season[x], team=importGrid$team[x])), data.frame)
table(espn_pwr$WEEK)
espn_pwr$WEEK[espn_pwr$WEEK=='Preseason']<-0
espn_pwr[, c("RANK", "WEEK")]<-sapply(espn_pwr[, c("RANK", "WEEK")], function(x) as.numeric(gsub("Week ", "", x)))

#2016 and 2017 annoyingly are sepearte web pages

link<-"/nfl/story/_/id/18395280/nfl-2016-final-regular-season-power-rankings-new-england-patriots-dallas-cowboys-pittsburgh-steelers"
links16<-read_html(paste0("http://www.espn.com",link))%>% html_nodes("em a")%>% html_attr("href")
importGrid<-data.frame(week=18:0, link=c(link, links16), season=2016)

link<-"/nfl/story/_/page/NFLpowerrankingsx180102/2017-nfl-power-rankings-final-regular-season-edition-new-england-patriots-pittsburgh-steelers-minnesota-vikings-finish-top"
links17<-read_html(paste0("http://www.espn.com",link))%>% html_nodes("em a")%>% html_attr("href")
importGrid2<-data.frame(week=c(18:2, 0), link=c(link, links17), season=2017)
importGrid<-rbind(importGrid, importGrid2)

readLink<-function(link){
  # link<-importGrid$link[30]
  print(link)
  page<-read_html(paste0("http://www.espn.com",link))
  text<-page%>% html_nodes("h2")%>% html_text()
  text<-text[startsWith(text, "1")| startsWith(text, "2")| startsWith(text, "3")| startsWith(text, "4")| startsWith(text, "5")|
               startsWith(text, "6")| startsWith(text, "7")| startsWith(text, "8")| startsWith(text, "9")]
  text<-text[!grepl(":", text)& !grepl("NFL", text)& !grepl("[,]", text)]
  stats<-data.frame(Text=text,Season=importGrid$season[importGrid$link==link], WEEK=importGrid$week[importGrid$link==link] )
  stats$RANK<-sapply(strsplit(stats$Text, "[.] "), `[[`, 1)
  stats$Team<-sapply(strsplit(stats$Text, "[.] "), `[[`, 2)
  stats$Text<-NULL
  stats
}
espn_pwr2<-ldply(lapply(importGrid$link, readLink),data.frame)
espn_pwr<-rbind.fill(espn_pwr[,!colnames(espn_pwr)=="RECORD"], espn_pwr2)
espn_pwr$Team<-coordName(espn_pwr$Team)
espn_pwr$RANK<-as.numeric(espn_pwr$RANK)
colnames(espn_pwr)<-sapply(colnames(espn_pwr),simpleCap)%>% unname()
table(espn_pwr$Team)



elo_rank<-read.csv("C:/Users/David/Documents/NFL/nfl-elo-game-master/Elo Predictions.csv", header=T)
elo_rank<-elo_rank[elo_rank$season>=2002,]
elo_rank$date<-as.Date(elo_rank$date)
elo_rank[, c("team1", "team2")]<-sapply(elo_rank[, c("team1","team2")],coordName)

yahoo_links<-read.csv("Misc Data/yahoo pickem.csv")
readLink<-function(link){
  print(link)
  # link<-yahoo_links$Link[2]
  page<-read_html(link)
  stats<-page%>% html_table(fill=T)
  stats<-stats[[which(sapply(stats, function(x) length(grep("Pick", colnames(x)))>0))]][, 2:3]
  colnames(stats)<-c("Team", "Yahoo.Pickem")
  stats$Season<-yahoo_links$Season[yahoo_links$Link==link]
  stats$Week<-yahoo_links$Week[yahoo_links$Link==link]
  stats
}
yahoo.pickem<-ldply(lapply(yahoo_links$Link, readLink), data.frame)
yahoo.pickem$Team<-gsub("[@]", "", yahoo.pickem$Team)
yahoo.pickem$Team<-coordName(yahoo.pickem$Team)
yahoo.pickem$Yahoo.Pickem<-as.numeric(gsub("%", "", yahoo.pickem$Yahoo.Pickem))


###detailed vegas data
importGrid<-expand.grid(week=1:21, season=2009:2017)

getStats<-function(week, season){
  #week<-15;season<-2012
  
  print(c(week, season))
  #superbowl is listed as 22
  if(week==21){
    week<-22
  }
  
  page<-read_html(paste0("http://www.vegasinsider.com/nfl/matchups/matchups.cfm/week/", week,"/season/", season))
  stats<- page%>%html_nodes("td")%>% html_text()
  links<-page%>% html_nodes("td a")%>% html_attr("href")
  links<-links[sapply(links, function(x) grepl("line-movement", x))]
  
  
  if("Teams"%in% stats){
    
    stats<-stats[which(stats=="Teams")[1]:length(stats)]
    stats<-gsub("[\r\n\t]", "", stats)
    stats<-stats[!grepl("Tracker|ClosingSpreadMoney", stats)]
    
    if("ATS"%in% stats[1:10]){
      stats<-stats[unlist(lapply(which(stats=="Teams"), function(x) seq(x, x+26, 1)))]
      stats<-data.frame(matrix(stats, ncol=9, byrow=T))
      names<-as.character(stats[1,])
    } else{
      #for games in future, use:
      stats<-stats[unlist(lapply(which(stats=="Teams"), function(x) seq(x, x+23, 1)))]
      stats<-data.frame(matrix(stats, ncol=8, byrow=T))
      names<-as.character(stats[1,])
    }
    #superbowl is listed as 22
    if(week==22){
      week<-21
    }
    
    colnames(stats)<-names
    colnames(stats)[colnames(stats)=="Current"]<-"Closing"
    
    stats<-stats[stats$Teams!="Teams", ]
    stats$Link<-rep(links, each=2)
    stats$Week<-week
    stats$Season<-season
    
  } else{
    stats<-data.frame()
  }
  stats
}
if(!exists("game_odds_detailed")){
  game_odds_detailed<-ldply(lapply(1:nrow(importGrid), function(x) getStats(week = importGrid$week[x], season=importGrid$season[x])), data.frame)
} else{
  test<-game_odds_detailed[!duplicated(game_odds_detailed[, c("Week", "Season")]), ]
  importGrid<-merge(importGrid, test, all=T, by.x=c("week", "season"), by.y=c("Week", "Season"))
  importGrid<-importGrid[is.na(importGrid$Teams), c("week", "season")]
  
  game_odds_detailed2<-ldply(lapply(1:nrow(importGrid), function(x) getStats(week = importGrid$week[x], season=importGrid$season[x])), data.frame)
  game_odds_detailed<-rbind.fill(game_odds_detailed, game_odds_detailed2)
  game_odds_detailed<-game_odds_detailed
}


links<-unique(game_odds_detailed$Link)


readLink<-function(link, offshore=T){
  print(link)
  #link<-tail(links, 1)
  link2<-sapply(strsplit(link, "/time"),`[[`, 1)
  link2<-paste("http://www.vegasinsider.com",link2, sep="")
  if(offshore==T){
    link2<-gsub("las-vegas", "offshore", link2)
  }
  page<-read_html(link2)
  
  #clean data
  stats<-page%>% html_nodes("td")%>% html_text()
  stats<-gsub("[\r\n\t]", "", stats)
  stats<-stats[!grepl("Tracker|ClosingSpreadMoney|Sunday Night|FavDog|Vegas|News", stats)]
  stats<-stats[!stats%in% c("Money Line", "Spread", "Total", "1st Half", "2nd Half")]
  if(length(stats)>=100){
    #save vegas info, remove vegas info
    gameInfo<-stats[1:(grep("LINE MOVEMENT", stats)[1]-1)]
    stats<-stats[-c(1:(grep("LINE MOVEMENT", stats)[1]-1))]
    
    #save vegas books, remove vegas books
    books<-stats[grep("LINE MOVEMENTS", stats)]
    stats<-stats[-c(grep("LINE MOVEMENTS", stats), grep("LINE MOVEMENTS", stats)+1)]
    
    #convert to dataframe
    stats<-stats[1:(length(stats)-length(stats)%%12)] #end of stats has a couple errors
    stats<-data.frame(matrix(stats, ncol=12, byrow=T))
    stats<-stats[grepl("/|Date", stats[,1]), ]
    
    inds<-c(which(stats[, 1]=="Date"), nrow(stats)+1)
    stats$Book<-mapply(rep, books, diff(inds)) %>% unlist()
    stats<-stats[!grepl("Date", stats[, 1]), ]
    colnames(stats)<-c("DATE", "Time","Fav.Odds", "Dog.Odds","Fav.Spread", "Dog.Spread", "Over.Total", "Under.Total",'Fav.Half1', "Dog.Half1",
                       "Fav.Half2", "Dog.Half2" , "Book")
    
    stats$Link<-link
    stats
  } else{
    data.frame()
  }
}
if(!exists("offshore_line_movement")){
  #scrape
  offshore_line_movement<-list();length(offshore_line_movement)<-length(links)
  for(i in 1:length(links)){
    offshore_line_movement[[i]]<-readLink(links[i])
  }
  
} else{
  #get missing links
  test<- rbindlist(offshore_line_movement)
  links<-setdiff(links,test$Link)
  
  #scrape and append
  offshore_line_movement2<-list();length(offshore_line_movement2)<-length(links)
  for(i in 1:length(links)){
    offshore_line_movement2[[i]]<-readLink(links[i])
  }
  offshore_line_movement<-append(offshore_line_movement, offshore_line_movement2) 
  offshore_line_movement<- offshore_line_movement[!duplicated(offshore_line_movement)]
}



if(!exists("vegas_line_movement")){
  
  #scrape each link
  vegas_line_movement<-list();length(vegas_line_movement)<-length(links)
  for(i in 1811:length(links)){
    vegas_line_movement[[i]]<-readLink(links[i], offshore = F)
  }
  
} else{
  #get missing links
  test<- rbindlist(vegas_line_movement)
  links<-setdiff(links,test$Link)
  
  #scrape and append
  vegas_line_movement2<-list();length(vegas_line_movement2)<-length(links)
  for(i in 1:length(links)){
    vegas_line_movement2[[i]]<-readLink(links[i], offshore = F)
  }
  vegas_line_movement<-append(vegas_line_movement, vegas_line_movement2) 
  vegas_line_movement<- vegas_line_movement[!duplicated(vegas_line_movement)]
}



####save data#####

#seasonal data
save(list=c("season_passing_df", "season_rushing_df", "season_receiving_df", "season_rz_stats", "season.stats", "id_df", "rosters","combine.stats", "madden.data" ,#player stats
            "depthCharts","defense.stats", "defense.passing", "team.futures", "seasonSnaps", "suspensions", #season stats
            "gameLinks","gameLinks_FREF", "gameLogs","teamLogs","snapLogs"
            
),file="Player Data/NFL Data.RData")


#game data data
save(list=c(  "game_injuries", "game_inactives", "game_odds","game_odds_detailed","offshore_line_movement","vegas_line_movement",
              "espn_pwr", "elo_rank" , "game_locs", "yahoo.pickem"#game stats
), file="Player Data/NFL Game Data.RData")



#load("Player Data/NFL Data.RData")
#load("Player Data/NFL Game Data.RData")

########FFANALYTICS SEASONAL PROJECTIONS######

FFANALYTICS<-ldply(lapply(paste("FFAnalytics/", list.files("FFAnalytics/", pattern="-0"), sep=""),
                          function(x) {
                            df<-read.csv(x);
                            df$Season<-as.numeric(substring(x, 31,34))
                            df
                          }), data.frame)
FFANALYTICS[, c("player", "team")]<-sapply(FFANALYTICS[, c("player", "team")], coordName)
FFANALYTICS[, !colnames(FFANALYTICS)%in% c("player", "team", "position")]<-sapply(FFANALYTICS[, !colnames(FFANALYTICS)%in% c("player", "team","position")], as.numeric)

########FANTASYDATA.com SEASONAL PROJECTIONS######

#useful for seasonal projections and depth charts
remDr<-rsDriver(browser = "chrome")


getTeam<-function( Season, Team, Pos,Site=5,  postseason=F){
  
  #5=yahoo
  # https://fantasydata.com/nfl-stats/fantasy-football-weekly-projections?position=4&team=17&season=2018&seasontype=1&scope=1
  
  url<-paste0(c("https://fantasydata.com/nfl-stats/fantasy-football-weekly-projections?position=",Pos, "&team=",Team,"&season=",Season,
                "&seasontype=", 1+2*postseason,"&scope=1&scoringsystem=",Site), collapse=""   )
  remDr$client$navigate(url)
  Sys.sleep(3)
  page<-remDr$client$getPageSource()[[1]]
  page<-read_html(page)
  
  cols<-page%>% html_nodes(".k-link")%>% html_text()
  test<-page%>% html_table(fill=T)
  test<-do.call( "cbind", test[3:4])
  test<-test[, -1]
  if(Pos==2){
    colnames(test)<-c("Player", "Team", "Pos", "Gms", "PassComp", "PassAtt", "PassPct", "PassYds","PassYds.Att", "PassTD", "PassInt","PassRating",
                      "RushAtt", "RushYds","RushAvg", "RushTD", "PPG", "FantPts" )
    
  }else if(Pos==3){
    colnames(test)<-c("Player", "Team", "Pos", "Gms", "RushAtt", "RushYds", "RushAvg", "RushTD","Targets","Rec",  "RecYds","RecTD",
                      "Fums","FumsLost","PPG", "FantPts"  )
    
  }else if(Pos%in% 4:5){
    colnames(test)<-c("Player", "Team", "Pos", "Gms", "Targets", "Rec", "RecRec.Tgt", "RecYds","RecTD", "RecLong", "RecYds.Tgt", "RecYds.Rec",
                      "RushAtt", "RushYds", "RushAvg", "RushTD",
                      "Fums","FumsLost","PPG", "FantPts"  )
  } else if (Pos==6){
    colnames(test)<-c("Player","Team", "Pos", "Gms", "FGM", "FGA", "FGPCT", "FGLong", "XPM", "XPA", "PPG", "FantPts")
  } else if(Pos==7){
    colnames(test)<-c("Player","Team", "Pos", "Gms", "TFL", "Sacks", 
                      "QB.Hits", "Def.Int", "Fum.Rec", "Safeties", "Def.TD", "Return.TD","Pts.Allowed" , "PPG", "FantPts")
  }
  test$ID<-page%>%html_nodes("td:nth-child(2) a")%>% html_attr("href")
  test$Season<-Season
  print(c(Team, Pos))
  test
}
#links to import
testGrid<-expand.grid(Pos=c(2, 3, 4, 5, 6, 7), Team=0:31, Season=2014:2018)
if(exists("FDATA_SEASONAL")){
  testGrid<-testGrid[testGrid$Season==2018,]
}
#import links
testList<-list();length(testList)<-nrow(testGrid)
for(x in 1:nrow(testGrid)){
  testList[[x]]<- getTeam(Season=testGrid$Season[x], Team=testGrid$Team[x], Pos=testGrid$Pos[x])
}
#combine and clean
if(!exists("FDATA_SEASONAL")){
  FDATA_SEASONAL<-data.frame(rbindlist(testList,fill=T ))
} else{
  FDATA_SEASONAL2<-data.frame(rbindlist(testList,fill=T ))
  FDATA_SEASONAL<-rbind.fill(FDATA_SEASONAL2[, colnames(FDATA_SEASONAL2)%in% colnames(FDATA_SEASONAL)], 
                             FDATA_SEASONAL[!(FDATA_SEASONAL$Season==2018),])
}
FDATA_SEASONAL<-FDATA_SEASONAL[, !colnames(FDATA_SEASONAL)%in% c("PassPct", "PassYds.Att", "PassRating", "RushAvg", "RecLong", "RushLong",
                                                                 "RecYds.Tgt", "RecYds.Rec")]
FDATA_SEASONAL[, !colnames(FDATA_SEASONAL)%in% c("Player", "Pos", "Team", "ID")]<-
  sapply(FDATA_SEASONAL[, !colnames(FDATA_SEASONAL)%in% c("Player", "Pos", "Team", "ID")], function(x) as.numeric(gsub("[,]", "", x)))
head(FDATA_SEASONAL)

##FFTODAY###
importGrid<-expand.grid(Season=2008:2018, PosID=c(seq(10, 70, 10), 99), Page=0:2)

readFFTODAY<-function(Season, PosID, Page){
  print(c(Season, PosID, Page))
  
  if(PosID%in%c(50, 60, 70)){
    leagueID<-193033 #idp scoring
  }else{
    leagueID<-17#yahoo scoring
  }
  page<-read_html(paste0("http://fftoday.com/rankings/playerproj.php?Season=",Season ,"&PosID=",PosID ,"&LeagueID=", leagueID, "&order_by=FFPts&sort_order=DESC&cur_page=", Page)) 
  stats<-page%>% html_table(  fill=T)
  stats<-stats[[length(stats)-1]]
  stats<-stats[-c(1:2),-1 ]
  if(nrow(stats)>1){
    if(PosID==10){
      colnames(stats)<-c("Player", "Team", "Bye", "passATT", "passCMP", "passYDS", "passTD", "passINT", "rushATT", "rushYDS", "rushTD", "fantPts")
      stats$Pos<-"QB"
    }else if(PosID==20){
      colnames(stats)<-c("Player", "Team", "Bye","rushATT", "rushYDS", "rushTD", "recREC", 'recYDS',"recTD", "fantPts" )
      stats$Pos<-"RB"
    } else if(PosID==30){
      colnames(stats)<-c("Player", "Team", "Bye", "recREC", 'recYDS',"recTD", "rushATT", "rushYDS", "rushTD","fantPts" )
      stats$Pos<-"WR"
    }else if(PosID==40){
      colnames(stats)<-c("Player", "Team", "Bye", "recREC", 'recYDS',"recTD", "fantPts" )
      stats$Pos<-"TE"
    }else if(PosID==50){
      colnames(stats)<-c("Player", "Team", "Bye", "defSOLO", 'defAST',"defSACK", "defPD" , "defINT", "defFF", "defFR", "fantPts")
      stats$Pos<-"DL"
    }else if(PosID==60){
      colnames(stats)<-c("Player", "Team", "Bye", "defSOLO", 'defAST',"defSACK", "defPD" , "defINT", "defFF", "defFR", "fantPts")
      stats$Pos<-"LB"
    }else if(PosID==70){
      colnames(stats)<-c("Player", "Team", "Bye", "defSOLO", 'defAST',"defSACK", "defPD" , "defINT", "defFF", "defFR", "fantPts")
      stats$Pos<-"DB"
    }else if(PosID==99){
      stats<-stats[, c(1, ncol(stats))]
      colnames(stats)<-c("Player", "fantPts" )
      stats$Team<-stats$Player
      stats$Pos<-"DST"
    }
    stats$Season<-Season
  } else{
    stats<-data.frame()
  }
  stats
}
FFTODAY<-ldply(lapply(1:nrow(importGrid), function(x) readFFTODAY(Season = importGrid$Season[x], PosID = importGrid$PosID[x], Page=importGrid$Page[x])), data.frame)
FFTODAY[, !colnames(FFTODAY)%in% c("Player", "Team", "Pos")]<-sapply(FFTODAY[, !colnames(FFTODAY)%in% c("Player", "Team", "Pos")], function(x) as.numeric(gsub("[,]","", x)))
FFTODAY$Bye<-NULL
FFTODAY[is.na(FFTODAY)]<-0
hist(FFTODAY$fantPts)
table(FFTODAY$Pos)



library(readxl)

readIDP<-function(year){
  if(year==2015){
    file<-"IDP Guru/2015 IDP Redraft Projections_9.8.15.xlsx"
  } else if (year==2016){
    file<-"IDP Guru/2016 IDP Redraft Projections_9.7.16.xlsx"
  } else if (year==2017){
    file<-"IDP Guru/2017 IDP Redraft Projections_9.6.17.xlsx"
  } else if (year==2018){
    file<-"IDP Guru/2018 IDP Redraft Projections_8.21.18.xlsx"
  }
  getPos<-function(pos){
    stats<-read_excel(file, sheet = pos) %>% data.frame()
    stats$Pos<-pos
    stats
  }
  stats<-ldply(lapply(c("DL", "DB", "LB"), getPos), data.frame)
  stats$Year<-year
  stats
  
}
IDPGURU<-ldply(lapply(2015:2018, readIDP), data.frame)
IDPGURU$Player<-paste(IDPGURU$First.Name, IDPGURU$Last.Name)
IDPGURU<-IDPGURU[, !colnames(IDPGURU)%in% c("Total.Tackles", "Bye", "Player.Name", "First.Name", "Last.Name", "Projections.Rank", "Position", "Age", "Games")]
IDPGURU$Team<-sapply(strsplit(IDPGURU$Team, "/"), `[[`,1)
table(IDPGURU$Year)


getIDP<-function(year){
  if(year==2014){
    link<-"http://web.archive.org/web/20140825162912/https://www.fantasypros.com/nfl/rankings/idp-cheatsheets.php"
    stats<-link%>%read_html()%>% html_table(fill=T)
    stats<-stats[[length(stats)]]
  }else if(year==2015){
    link<-"http://web.archive.org/web/20150826073507/https://www.fantasypros.com/nfl/rankings/idp-cheatsheets.php"
    stats<-link%>%read_html()%>% html_table(fill=T)
    stats<-stats[[length(stats)]]
  }else if (year==2017){
    link<-"http://web.archive.org/web/20170830083409/https://www.fantasypros.com/nfl/rankings/idp-cheatsheets.php"
    stats<-link%>%read_html()%>% html_table(fill=T)
    stats<-stats[[1]]
  }else if(year==2018){
    link<-"https://www.fantasypros.com/nfl/rankings/idp-cheatsheets.php"
    stats<-link%>%read_html()%>% html_table(fill=T)
    stats<-stats[[1]]
    names<-link%>% read_html()%>% html_nodes(".full-name")%>% html_text()
    
  }
  stats<-stats[, !colnames(stats)%in%'WSID'&  !is.na(colnames(stats))& !grepl("NA[.]", colnames(stats))]
  colnames(stats)[2]<-"Player"
  stats<-stats[!grepl("google", stats$Player)& !stats$Player==''& !grepl("nbsp", stats$Player),]
  stats$Year<-year
  if(year==2018){
    stats$Player<-names
  }
  stats<-stats[, c("Rank", "Player", "Pos", "Year")]
  stats
}
IDPFPRO<-ldply(lapply(c(2014:2015, 2017:2018), getIDP), data.frame)
IDPFPRO$Player<-sapply(strsplit(IDPFPRO$Player, " "), function(x) paste(x[1], x[2], sep=" "))
IDPFPRO$Player<-coordName(IDPFPRO$Player)
IDPFPRO$Pos<-NULL
IDPFPRO$Rank<-as.numeric(IDPFPRO$Rank)


save(list=c("FDATA_SEASONAL", "FFANALYTICS", "FFTODAY", "IDPGURU", "IDPFPRO"), file="Player Data/Projection Data.RData")

# load("Player Data/Projection Data.RData")



###SCRAPE DRAFT DATA####

##misc data##
year<-2018

readROTO<-function(year){
  
  if(year==2018){
    
    page<-read.csv(paste0("Rotoworld/",year ,".txt"), header = F, blank.lines.skip = T, sep = "\n")
    page$Player<-sapply(strsplit(page[, 1], "\\("), `[[`, 1)
    page$Player<-gsub("[^a-z A-Z]", "", page$Player)
    page$Player<-coordName(page$Player)
    page$Rank<-1:nrow(page)
    
  }else if(year!=2018){
    
    page<-read.csv(paste0("Rotoworld/",year ,"_PPR.txt"), header = F, blank.lines.skip = T, sep = "\n")
    page$Player<-sapply(strsplit(page[, 1], "\\(|--"), `[[`, 1)
    page$Player<-gsub("[^a-z A-Z]", "", page$Player)
    page$Player<-coordName(page$Player)
    page$Rank_PPR<-1:nrow(page)
    
    page2<-read.csv(paste0("Rotoworld/",year ,"_STD.txt"), header = F, blank.lines.skip = T, sep = "\n")
    page2$Player<-sapply(strsplit(page2[, 1], "\\(|--"), `[[`, 1)
    page2$Player<-gsub("[^a-z A-Z]", "", page2$Player)
    page2$Player<-coordName(page2$Player)
    page2$Rank_STD<-1:nrow(page2)
    page<-merge(page, page2, by="Player", all=T)
    page$Rank<-rowMeans(page[, c("Rank_PPR", "Rank_STD")], na.rm=T)
  }
  
  page$Year<-year
  page[,c("Player","Rank", "Year")]
  
}
rotoworld<-ldply(lapply(c(2014, 2016:2018), readROTO), data.frame)



##fantasypros data###
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
  if(year==2018){
    
    url<-paste(c("https://fantasyfootballcalculator.com/adp?format=half-ppr", "&year=", year,"&teams=12&view=graph&pos=all"), sep="", collapse="")
    
  }
  page<-read_html(url)
  stats<-page%>%html_table(fill=T)
  stats<-stats[[1]]
  if(ncol(stats)==11){
    colnames(stats)<-c("Rk", "Round", "Player", "Pos", "Team",  "ADP", "ADPSD","High", "Low", "TimesDrafted", "Graph" )
    
  } else{
    colnames(stats)<-c("Rk", "Round", "Player", "Pos", "Team", "Bye", "ADP", "ADPSD","High", "Low", "TimesDrafted", "Graph")
    
  }
  stats<-stats[, !grepl("High|Low|TimesDrafted|Graph|Rk|Round|Bye", colnames(stats))]
  stats$Year<-year
  colnames(stats)[colnames(stats)%in% c("ADP","ADPSD")]<-paste(c("ADP", "ADPSD"), gsub("andar", "", scoring), sep="_")
  stats
}
ffcalc_ppr<-ldply(lapply(2010:2018, function(x) readffcalc(x, scoring="ppr")), data.frame)
ffcalc<-ldply(lapply(2007:2018, readffcalc), data.frame)

ffcalc<-merge(ffcalc, ffcalc_ppr, by=c("Player", "Pos","Team", "Year"), all=T)
ffcalc$Pos[ffcalc$Pos=="PK"]<-"K"
ffcalc$Pos[ffcalc$Pos=="DEF"]<-"DST"
ffcalc$Player<-coordName(ffcalc$Player)

ffcalc$ADP_half<-rowMeans(ffcalc[, c("ADP_std", "ADP_ppr")], na.rm=T)
ffcalc$ADPSD_half<-rowMeans(ffcalc[, c("ADPSD_std", "ADPSD_ppr")], na.rm=T)
ffcalc[ffcalc$Year==2007, grepl("SD", colnames(ffcalc))]<-NA
plot(ffcalc$ADPSD_half~ffcalc$ADP_half)
ffcalc$ADPSD_half[ffcalc$ADPSD_half>30]<-30
ffcalc<-ffcalc[order(ffcalc$Year, ffcalc$ADP_half, decreasing = F), ]
ffcalc[ffcalc$Year==2007,][1:30,]

ffcalc[, c("Team", "Player")]<-sapply(ffcalc[, c("Team", "Player")], coordName)

#load adp
# https://www.fantasypros.com/nfl/adp/ppr-overall.php


projections$Player<-coordName(projections$Player)
ppr$Player<-coordName(ppr$Player)
std$Player<-coordName(std$Player)
setdiff(adp_alt$Player, projections$Player)
setdiff(ffcalc$Player[ffcalc$Year==2018], ppr$Player)

table(projections$Pos)
table(ppr$Pos)
table(std$Pos)

ffpros<-Reduce(function(x, y) merge(x, y, all=TRUE, by=c("Player", "Pos")), list(projections, ppr, std))



save(list=c("ffcalc", "ffpros", "rotoworld"), file="Player Data/Draft Data.RData")


#load("Player Data/Draft Data.RData")