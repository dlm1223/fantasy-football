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
ids<-read_html("http://www.espn.com/nfl/standings")%>% html_nodes(".hide-mobile a")%>% html_attr("href")
teams<-sapply(strsplit(ids, "/"), `[[`, 6)
abbrev$Abbrev4<-teams[order(teams)][c(1:16,18:27, 17, 28:32) ]
abbrev$Abbrev4[abbrev$Abbrev=="Sea"]<-"sea"
abbrev$Abbrev4[abbrev$Abbrev=="Sfo"]<-"sf"

importGrid<-expand.grid(team=teams,Year=2005:2017, stringsAsFactors = F )

library(lubridate)
getGameLinks<-function(team, year){
  print(c(team, year))
  page<-read_html(paste0("http://www.espn.com/nfl/team/schedule/_/name/", team,"/year/", year))
  sched<-page%>% html_table() 
  sched<-sched[[1]]
  sched$Type<-""
  sched$Type[1:grep("Regular Season", sched$X1)]<-"Postseason"
  sched$Type[grep("Regular Season", sched$X1):grep("Preseason", sched$X1)]<-"Regular Season"
  sched$Type[grep("Preseason", sched$X1):nrow(sched)]<-"Preseason"
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
  sched$GameLink<-page%>% html_nodes("td:nth-child(4) a")%>% html_attr("href") %>% gsub("recap", "boxscore", .)
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

gameLogs<-list();length(gameLogs)<-length(importGrid)
for(x in 1:length(gameLogs)){
  print(x)
  gameLogs[[x]]<-importGame(importGrid[x])
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

remDr<-rsDriver(browser = "chrome")
importGrid<-expand.grid(Team=abbrev$Abbrev3, Season=2000:2018)
importGrid<-importGrid[!(importGrid$Team=="htx"& importGrid$Season<2002),]

importRoster<-function(team, season){
  
  remDr$client$navigate(paste0(c("http://www.pro-football-reference.com/teams/", team,"/", season,"_roster.htm"), collapse = ""))
  players<-readHTMLTable(remDr$client$getPageSource(url)[[1]])  
  players<-players$games_played_team
  players$Team<-abbrev$Abbrev[abbrev$Abbrev3==team]
  players$Season<-season
  ids<-read_html(remDr$client$getPageSource(url)[[1]])%>%html_nodes("#games_played_team .left:nth-child(2) a")%>% html_attr("href")
  players$ID<-NA
  players$ID[1:length(ids)]<-ids
  players
}
rostersList<-list();length(rostersList)<-nrow(importGrid)
for(x in 1:nrow(importGrid)){
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
rosters$Wt<-as.numeric(rosters$Wt)
rosters$Age<-as.numeric(rosters$Age)
rosters$Ht<-as.numeric(rosters$Ht)
rosters$Yrs[rosters$Yrs%in% c("Rook","-1", "-2", "-3")]<-0
rosters$Yrs<-as.numeric(rosters$Yrs)
rosters[, c("Player", "Team")]<-sapply(rosters[, c("Player", "Team")], coordName)
rosters$Pick[grepl("pick", rosters$Drafted..tm.rnd.yr.)]<-sapply(strsplit(rosters$Drafted..tm.rnd.yr.[grepl("pick", rosters$Drafted..tm.rnd.yr.)], "/" ),`[[`,  3)
rosters$Pick<-stringr::str_extract_all(rosters$Pick, "[0-9]+") %>% unlist()%>% as.numeric()
rosters$Pick[rosters$Pick>=250| is.na(rosters$Pick)]<-250


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

head(id_df)
# id_df<-unique(id_df[!is.na(id_df$GSIS_ID), c("name", "GSIS_ID", "Player","RookieYear","Ht", "Wt", "BirthDate")])
# id_df[duplicated(id_df$GSIS_ID)| duplicated(id_df$GSIS_ID, fromLast = T),]
# id_df<-id_df[!duplicated(id_df$GSIS_ID), ]

combine.stats[duplicated(combine.stats$ID)& !is.na(combine.stats$ID),]

id_df<-merge(id_df, combine.stats[!is.na(combine.stats$ID), c("ID", "X40YD", "Vertical", "BenchReps", "Broad.Jump", "X3Cone", "Shuttle")], 
             by="ID", all.x=T )

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

####save data#####

save(list=c("season_passing_df", "season_rushing_df", "season_receiving_df", 
            "season.stats", "id_df", "rosters","combine.stats", "depthCharts","defense.stats", 
            "gameLinks", "gameLogs", "team.futures"
),
file="Player Data/NFL Data.RData")


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
  Sys.sleep(2.5)
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
  } else if (Pos==10){
    colnames(test)<-c("Player","Team", "Pos", "Gms", "FGM", "FGA", "FGPCT", "FGLong", "XPM", "XPA", "PPG", "FantPts")
  } else if(Pos==11){
    colnames(test)<-c("Player","Team", "Pos", "Gms", "TFL", "Sacks", 
                      "QB.Hits", "Def.Int", "Fum.Rec", "Safeties", "Def.TD", "Return.TD","Pts.Allowed" , "PPG", "FantPts")
  }
  test$ID<-page%>%html_nodes("td:nth-child(2) a")%>% html_attr("href")
  test$Season<-Season
  print(c(Team, Pos))
  test
}
#links to import
testGrid<-expand.grid(Pos=c(2, 3, 4, 5, 10, 11), Team=0:31, Season=2014:2018)
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
                             FDATA_SEASONAL[FDATA_SEASONAL$Season!=2018,])
}
FDATA_SEASONAL<-FDATA_SEASONAL[, !colnames(FDATA_SEASONAL)%in% c("PassPct", "PassYds.Att", "PassRating", "RushAvg", "RecLong", "RushLong",
                                                                 "RecYds.Tgt", "RecYds.Rec")]
FDATA_SEASONAL[, !colnames(FDATA_SEASONAL)%in% c("Player", "Pos", "Team", "ID")]<-
  sapply(FDATA_SEASONAL[, !colnames(FDATA_SEASONAL)%in% c("Player", "Pos", "Team", "ID")], function(x) as.numeric(gsub("[,]", "", x)))
head(FDATA_SEASONAL)



save(list=c("FDATA_SEASONAL", "FFANALYTICS"), file="Player Data/Projection Data.RData")





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
  if(ncol(stats)==11){
    colnames(stats)<-c("Rk", "Round", "Player", "Pos", "Team",  "ADP", "ADPSD","High", "Low", "TimesDrafted", "Graph" )
    
  } else{
    colnames(stats)<-c("Rk", "Round", "Player", "Pos", "Team", "Bye", "ADP", "ADPSD","High", "Low", "TimesDrafted", "Graph")
    
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


save(list=c("ffcalc", "ffpros"), file="Player Data/Draft Data.RData")
