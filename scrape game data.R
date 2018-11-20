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
load("Player Data/NFL Game Data.RData")
# devtools::install_github(repo = "maksimhorowitz/nflscrapR")
# devtools::install_github("ropensci/RSelenium")


current.week<-12;current.season<-2018;current.type<-"Regular Season"


###game-injury data#####
importGrid<-expand.grid(Team=abbrev$Abbrev3, Season=2009:2018, stringsAsFactors = F)
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
if(exists("game_injuries")){
  game_injuries2<-game_injuries[game_injuries$Season!=2018,]
  importGrid<-importGrid[importGrid$Season==2018,]
}
game_injuries<-ldply(lapply(1:nrow(importGrid), function(x)getInjured(team=importGrid$Team[x], season=importGrid$Season[x]) ),data.frame)
game_injuries$GameInfo<-as.character(game_injuries$GameInfo)
game_injuries$DATE<-sapply(strsplit(game_injuries$GameInfo, "vs"), `[[`, 1)
game_injuries$DATE<-as.Date(paste(game_injuries$DATE, game_injuries$Season, sep="/"), format="%m/%d/%Y")
year(game_injuries$DATE)[month(game_injuries$DATE)%in% 1:2]<-game_injuries$Season[month(game_injuries$DATE)%in% 1:2]+1

if(exists("game_injuries")){
  game_injuries<-rbind(game_injuries, game_injuries2)
} 
table(game_injuries$Season, game_injuries$Team)

###fref game links###

getLinks<-function(team,season){
  print(c(team, season))
  page<-read_html(paste0("https://www.pro-football-reference.com/teams/", team,"/",season ,".htm"))
  stats<-html_table(page)
  if(length(stats)==3){
    stats<-stats[[2]]
  } else{
    stats<-stats[[1]]
  }
  
  names<-as.character(stats[1,])
  colnames(stats)<-names
  stats<-stats[!stats$Opp%in% c("", "Bye Week", "Opp"),]
  ids<-page%>% html_nodes(".center a")%>% html_attr("href")
  stats$GameID<-ids[1:nrow(stats)]
  stats$Team<-team
  stats$Season<-season
  stats
}
importGrid<-expand.grid(Team=abbrev$Abbrev3, Season=2003:2018, stringsAsFactors = F)
if(!exists("gameLinks_FREF")){
  gameLinks_FREF<-ldply(lapply(1:nrow(importGrid), function(x)getLinks(team=importGrid$Team[x], season=importGrid$Season[x]) ),data.frame)
}else{
  importGrid<-importGrid[importGrid$Season==2018, ]
  gameLinks_FREF2<-ldply(lapply(1:nrow(importGrid), function(x)getLinks(team=importGrid$Team[x], season=importGrid$Season[x]) ),data.frame)
  gameLinks_FREF<-rbind(gameLinks_FREF2[, colnames(gameLinks_FREF2)%in% colnames(gameLinks_FREF)], gameLinks_FREF[gameLinks_FREF$Season!=2018,])
}
table(gameLinks_FREF$Team, gameLinks_FREF$Season)

###game locations####

importGrid<-expand.grid(Team=abbrev$Abbrev3, Season=2003:2018, stringsAsFactors = F)
importGrid<-importGrid[!(importGrid$Team=="htx"& importGrid$Season<2002),]

getLocs<-function(team, season){
  print(c(team, season))
  page<-read_html(paste0("https://www.pro-football-reference.com/teams/", team, "/", season, "_travel.htm"))
  stats<-html_table(page)[[1]]
  stats$Dist<-NULL
  stats<-stats[!grepl("there [&] back", stats$Stadium),]
  stats$GameID<-page%>% html_nodes("th a")%>%html_attr("href")
  stats$Season<-season
  stats$Team<-team
  stats
  
}
if(exists("game_locs")){
  game_locs2<-game_locs[game_locs$Season!=current.season,]
  importGrid<-importGrid[importGrid$Season==current.season,]
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

if(exists("game_locs2")){
  game_locs<-rbind(game_locs,game_locs2)
}



###game snaps#####
# remDr<-rsDriver(browser = "chrome", port=as.integer(4444))


# remDr$client$setImplicitWaitTimeout(milliseconds = 5000)
#setTimeout(type = "implicit", milliseconds = 5000)
importGame<-function(gameid){
  #x<-x+1;gameID<-importGrid[x]
  #navigate to page & scrape html data
  url<-paste0("https://www.pro-football-reference.com",gameid)
  
  print("a")
  
  remDr$client$navigate(url)
  Sys.sleep(1)
  
  print("b")
  
  #handinlg page refresh
  remDr$client$setImplicitWaitTimeout(milliseconds = 25000)
  remDr$client$setTimeout(type = "page load", milliseconds = 25000)
  while(class(try(remDr$client$getPageSource(header = TRUE), silent = TRUE))=="try-error"){
    try(remDr$client$refresh(), silent = TRUE);print("refresh")
  }
  print("c")
  page<-remDr$client$getPageSource()[[1]]
  print("d")
  
  page2<-read_html(page)
  stats<-readHTMLTable(page)
  
  #organize snaps data and gameinfo data
  home_snaps<-stats$home_snap_counts
  colnames(home_snaps)<-c("Player", "Pos","OffSnaps", "OffPct", "DefSnaps", "DefPct","STSnaps", "STPct")
  home_snaps<-home_snaps[!home_snaps$Player=="",]
  home_snaps$ID<-page2%>% html_nodes("#home_snap_counts a")%>%html_attr("href")
  home_snaps$GameID<-gameid
  home_snaps$Team<-page2%>% html_nodes("#all_home_snap_counts h2") %>% html_text()
  home_snaps$Team<-gsub(" Snap Counts", "", home_snaps$Team)
  
  vis_snaps<-stats$vis_snap_counts
  colnames(vis_snaps)<-c("Player","Pos" ,"OffSnaps", "OffPct", "DefSnaps", "DefPct","STSnaps", "STPct")
  vis_snaps<-vis_snaps[!vis_snaps$Player=="",]
  vis_snaps$ID<-page2%>% html_nodes("#vis_snap_counts a")%>%html_attr("href")
  vis_snaps$GameID<-gameid
  vis_snaps$Team<-page2%>% html_nodes("#all_vis_snap_counts h2") %>% html_text()
  vis_snaps$Team<-gsub(" Snap Counts", "", vis_snaps$Team)
  
  game_info<-stats$game_info
  colnames(game_info)<-c("Stat", "Value")
  game_info$GameID<-gameid
  Sys.sleep(1)
  list(home_snaps=home_snaps, vis_snaps=vis_snaps,game_info=game_info)
}

#games to import
importGrid<-unique(gameLinks_FREF$GameID[gameLinks_FREF$Season>= 2012& gameLinks_FREF$Offense!=''])
if(exists("snapLogs")){
  importGrid<-importGrid[!importGrid%in% snapLogs$GameID]
} 
snapLogs2<-list();length(snapLogs2)<-length(importGrid)


#import
remDr<-rsDriver(browser = "chrome", port=as.integer(4444))
for(x in 1:length(snapLogs2)){
  print(x)
  snapLogs2[[x]]<-tryCatch(importGame(gameid = importGrid[x]), error=function(e){list()})
  
  if(x%%100==0){
    save(snapLogs2, file="temp.Rda")
  }
}
snapLogs2<-snapLogs2[sapply(snapLogs2, length)>0]
home.snaps<-rbindlist(lapply(snapLogs2,`[[`, 1 ))
vis.snaps<-rbindlist(lapply(snapLogs2,`[[`, 2 ))

#combine snaps and gaminfo
if(exists("snapLogs")){
  snapLogs<-list(home.snaps,vis.snaps, snapLogs)%>% rbindlist()%>% data.frame()
  game.info<-data.frame(rbind(game.info, rbindlist(lapply(snapLogs2,`[[`, 3 ))))
} else{
  snapLogs<-list(home.snaps,vis.snaps)%>% rbindlist()%>% data.frame()
  game.info<-rbindlist(lapply(snapLogs2,`[[`, 3 ))
}
snapLogs<-snapLogs[!duplicated(snapLogs),]
game.info<-game.info[!duplicated(game.info),]

importGrid[!importGrid%in% snapLogs$GameID] #<-- check missing games,  have to re-scrape if there 


###game starters#####


# remDr$client$setImplicitWaitTimeout(milliseconds = 5000)
#setTimeout(type = "implicit", milliseconds = 5000)
importGame<-function(gameid){
  #x<-x+1;gameid<-importGrid[x]
  #navigate to page & scrape html data
  url<-paste0("https://www.pro-football-reference.com",gameid)
  
  print("a")
  
  remDr$client$navigate(url)
  Sys.sleep(1)
  
  print("b")
  
  #handinlg page refresh
  remDr$client$setImplicitWaitTimeout(milliseconds = 25000)
  remDr$client$setTimeout(type = "page load", milliseconds = 25000)
  while(class(try(remDr$client$getPageSource(header = TRUE), silent = TRUE))=="try-error"){
    try(remDr$client$refresh(), silent = TRUE);print("refresh")
  }
  print("c")
  page<-remDr$client$getPageSource()[[1]]
  print("d")
  
  page2<-read_html(page)
  stats<-readHTMLTable(page)
  
  #organize starters data and gameinfo data
  home_starters<-stats$home_starters
  colnames(home_starters)<-c("Player", "Pos")
  home_starters<-home_starters[!home_starters$Player=="",]
  home_starters$ID<-page2%>% html_nodes("#home_starters a")%>%html_attr("href")
  home_starters$GameID<-gameid
  home_starters$Team<-page2%>% html_nodes("#all_home_starters h2") %>% html_text()
  home_starters$Team<-gsub(" Starters", "", home_starters$Team)
  
  vis_starters<-stats$vis_starters
  colnames(vis_starters)<-c("Player","Pos")
  vis_starters<-vis_starters[!vis_starters$Player=="",]
  vis_starters$ID<-page2%>% html_nodes("#vis_starters a")%>%html_attr("href")
  vis_starters$GameID<-gameid
  vis_starters$Team<-page2%>% html_nodes("#all_vis_starters h2") %>% html_text()
  vis_starters$Team<-gsub(" Starters", "", vis_starters$Team)
  
  list(home_starters=home_starters, vis_starters=vis_starters)
}

#games to import
importGrid<-unique(gameLinks_FREF$GameID[gameLinks_FREF$Season>= 2012& gameLinks_FREF$Offense!=''])
if(exists("starterLogs")){
  importGrid<-importGrid[!importGrid%in% starterLogs$GameID]
} 
starterLogs2<-list();length(starterLogs2)<-length(importGrid)


#import
# remDr<-rsDriver(browser = "chrome", port=as.integer(4444))
for(x in 1:length(starterLogs2)){
  print(x)
  starterLogs2[[x]]<-tryCatch(importGame(gameid = importGrid[x]), error=function(e){list()})
  
  if(x%%100==0){
    save(starterLogs2, file="temp.Rda")
  }
}
starterLogs2<-starterLogs2[sapply(starterLogs2, length)>0]
home.starters<-rbindlist(lapply(starterLogs2,`[[`, 1 ))
vis.starters<-rbindlist(lapply(starterLogs2,`[[`, 2 ))

#combine starters and gaminfo
if(exists("starterLogs")){
  starterLogs<-list(home.starters,vis.starters, starterLogs)%>% rbindlist()%>% data.frame()
} else{
  starterLogs<-list(home.starters,vis.starters)%>% rbindlist()%>% data.frame()
}
starterLogs<-starterLogs[!duplicated(starterLogs),]

importGrid[!importGrid%in% starterLogs$GameID] #<-- check missing games, may have to re-scrape some



###ESPN GAME LOGS####
ids<-read_html("http://www.espn.com/nfl/standings/_/season/2017")%>% html_nodes(".hide-mobile a")%>% html_attr("href")
teams<-sapply(strsplit(ids, "/"), `[[`, 6)
abbrev$Abbrev4<-teams[order(teams)][c(1:16,18:27, 17, 28:32) ]
abbrev$Abbrev4[abbrev$Abbrev=="Sea"]<-"sea"
abbrev$Abbrev4[abbrev$Abbrev=="Sfo"]<-"sf"

library(lubridate)
getGameLinks<-function(team, year){
  print(c(team, year))
  page<-read_html(paste0("http://www.espn.com/nfl/team/schedule/_/name/", team,"/year/", year))
  sched<-page%>% html_table(fill=T) 
  sched<-sched[[3]]
  sched$Type<-""
  # if(year!=2018){
  sched$Type[1:grep("Regular Season", sched$X1)]<-"Postseason"
  sched$Type[grep("Regular Season", sched$X1):grep("Preseason", sched$X1)]<-"Regular Season"
  sched$Type[grep("Preseason", sched$X1):nrow(sched)]<-"Preseason"
  # } else{
  #   sched$Type[1:grep("Regular Season", sched$X1)]<-"Preseason"
  #   sched$Type[grep("Regular Season", sched$X1):nrow(sched)]<-"Regular Season"
  # }
  sched<-sched[!grepl("Regular|Preseason|Postseason|DATE", sched$X1)& !grepl("BYE|DATE|Date", sched$X2),]
  sched$DATE<-as.Date(sched$X2, format="%a, %b %d")
  year(sched$DATE)<-ifelse(month(sched$DATE)%in% 1:2, year+1, year)
  
  sched$TeamLOC<-ifelse(grepl("@", sched[, 3]), "AWAY", "HOME")
  
  #get opponent--weird error with san diego
  opps<-page%>% html_nodes("td:nth-child(3) a")%>% html_attr("href")
  opps<-opps[c(TRUE, FALSE)]
  sched$OPP<-NA
  sched$OPP[!grepl("San Diego", sched[, 3])]<-sapply(strsplit(opps, "/"),function(x) x[length(x)-1]) 
  sched$OPP[grepl("San Diego", sched[, 3])]<-"sdg"
  
  #games w missing links
  sched<-sched[!(sched$X5%in% c( "POSTPONED", "CANCELED")),] 
  if(team%in% c("kc", "gb")& year==2003){
    sched<-sched[!(sched$X5%in% ""),] 
  }
  
  colnames(sched)[1]<-"Week"
  sched<-sched[, c("Week", "Type", "DATE", "TeamLOC", "OPP")]
  games<-page%>% html_nodes("td:nth-child(4) a")%>% html_attr("href") %>% gsub("recap", "boxscore", .)
  games<-games[!grepl("preview", games)]
  if(length(games)==0){games<-NA}
  sched$GameLink<-games
  sched$GameLink[sched$DATE>=Sys.Date()]<-NA
  
  sched$Team<-team
  sched$Year<-year
  sched
}


#inspect: den2005(x=80), ari05 (x=95)

importGrid<-expand.grid(team=teams,Year=2003:2018, stringsAsFactors = F )
if(exists("gameLinks")){
  importGrid<-importGrid[importGrid$Year==2018,]
}


gameLinks2<-list();length(gameLinks2)<-nrow(importGrid)
for(x in 1:length(gameLinks2)){
  gameLinks2[[x]]<- getGameLinks(team = importGrid$team[x], year=importGrid$Year[x])
}
gameLinks2<-ldply(gameLinks2, data.frame)
gameLinks2$GameLink<-gsub("http[:][/][/]www.espn.com[/]nfl[/]game[?]gameId[=]", "//www.espn.com/nfl/boxscore/_/gameId/", gameLinks2$GameLink)
gameLinks2$GameLink<-gsub("http[:][/][/]www.espn.com[/]nfl[/]game[/][_][/]gameId[/]", "//www.espn.com/nfl/boxscore/_/gameId/", gameLinks2$GameLink)

if(!exists("gameLinks")){
  gameLinks<-gameLinks2
} else{
  gameLinks<-rbind(gameLinks[!gameLinks$GameLink%in% gameLinks2$GameLink& !gameLinks$Year==2018,], gameLinks2)
}
table(gameLinks$Year[!is.na(gameLinks$GameLink)])


importGame<-function(link){
  # link<-importGrid[1]
  page<-tryCatch(read_html(paste0("http:", link)), error=function(e){})
  stats<-tryCatch(page%>%html_table(fill=T), error=function(e) {})
  if(length(stats)>3){
    teams<-stats[[1]][,1]
    
    stats<-stats[-c(1,20, 21, 22, 23)]
    
    #need to store ID, NAME, Team key to match IDs to boxscore
    away.ids<-page%>% html_nodes(".gamepackage-away-wrap a")%>% html_attr('href')
    away.players<-page%>% html_nodes(".gamepackage-away-wrap a")%>% html_text()
    home.ids<-page%>% html_nodes(".gamepackage-home-wrap a")%>% html_attr('href')
    home.players<-page%>% html_nodes(".gamepackage-home-wrap a")%>% html_text()
    
    ids<-rbind(data.frame(ID=away.ids,NAME=away.players, Team=teams[1] ), data.frame(ID=home.ids,NAME=home.players, Team=teams[2] ))
    ids<-unique(ids)
    ids<-ids[grepl("player", ids$ID), ]
    
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
    
    #add teams
    stats[seq(1,17, 2 )]<-lapply(stats[seq(1,17, 2 )], function(x) {x$Team<-teams[1];x})
    stats[seq(2,18, 2 )]<-lapply(stats[seq(2,18, 2 )], function(x) {x$Team<-teams[2];x})
    
    stats<-list(rbindlist(stats[1:2]), rbindlist(stats[3:4]), rbindlist(stats[5:6]),
                rbindlist(stats[7:8]), rbindlist(stats[9:10]), rbindlist(stats[11:12]), rbindlist(stats[13:14]), rbindlist(stats[15:16]), rbindlist(stats[17:18]))
    
    cleanStats<-function(x){
      colnames(x)[1]<-"NAME"
      x<-data.frame(x)
      x[!x$NAME%in%c("TEAM", ""),]
    }
    stats<-lapply(stats, cleanStats)
    stats<-append(stats, list(ids))
    # if(year==2008& team=="dal"){
    #   stats[[4]]$NAME[stats[[4]]$NAME=="Roy Williams"& stats[[4]]$defSOLO==4]<-"Roy Williams 2"
    #   stats[[9]]$NAME[stats[[9]]$ID=="http://www.espn.com/nfl/player/_/id/3536/roy-williams"]<-"Roy Williams 2"
    # }
    # 
    stats<-Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by =c("NAME","Team"), all = TRUE),  stats)
    # string<-paste0(paste0(LETTERS, sep="", collapse = "[.]|"), "[.]")
    # stats$NAME<-sapply(strsplit(stats$NAME, string), `[[`, 1)
    # stats$NAME<-gsub(" 2", "", stats$NAME)
    stats<-stats[!is.na(stats$ID),!colnames(stats)%in% "NAME"]
    stats$GameLink<-link
  } else{
    stats<-data.frame()
  }
  stats
  
}

if(!exists("gameLogs")){
  importGrid<-unique(gameLinks$GameLink[gameLinks$Year>=2005& !(gameLinks$Type=="Preseason"& gameLinks$Year<=2013)])
  importGame(importGrid[10])
  
  gameLogs<-list();length(gameLogs)<-length(importGrid)
  for(x in 1:length(gameLogs)){
    print(x)
    gameLogs[[x]]<-importGame(importGrid[x])
  }
  gameLogs<-ldply(gameLogs, data.frame)
  
} else{
  importGrid<-unique(gameLinks$GameLink[gameLinks$Year>=2005& !(gameLinks$Type=="Preseason"& gameLinks$Year<=2013)& !is.na(gameLinks$GameLink)&
                                          !gameLinks$GameLink%in% unique(gameLogs$GameLink)])
  gameLogs2<-list();length(gameLogs2)<-length(importGrid)
  for(x in 1:length(gameLogs2)){
    print(x)
    gameLogs2[[x]]<-importGame(importGrid[x])
  }
  #rbind
  gameLogs2<-ldply(gameLogs2, data.frame)
  gameLogs<-rbind.fill(gameLogs[!gameLogs$GameLink%in% gameLogs2$GameLink,], gameLogs2)
}

#inspect results
table(gameLinks$Year[!is.na(gameLinks$GameLink)], gameLinks$Type[!is.na(gameLinks$GameLink)])
table(gameLinks$Year[!is.na(gameLinks$GameLink)& gameLinks$GameLink%in% gameLogs$GameLink],
      gameLinks$Type[!is.na(gameLinks$GameLink)& gameLinks$GameLink%in% gameLogs$GameLink]) #preseason stats only available 2014-present


####TEAM-GAME-LOGS#######


importGame<-function(link){
  # link<-importGrid[2]
  page<-tryCatch(read_html(paste0("http:", gsub("http:", "", link))), error=function(e){})
  stats<-tryCatch(page%>%html_table(fill=T), error=function(e) {})
  if(length(stats)>=1){
    stats<-stats[1][[1]]
  }
  colnames(stats)[1]<-"Team"
  stats$GameLink<-link
  stats$TeamLOC<-c("AWAY", "HOME")
  stats
}
importGrid<-unique(gameLinks$GameLink[gameLinks$Year>=2002& !is.na(gameLinks$GameLink)])

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
  teamLogs<-rbind.fill(teamLogs[!teamLogs$GameLink%in%teamLogs2$GameLink, ], 
                       teamLogs2[, colnames(teamLogs2)%in% colnames(teamLogs)])
}
teamLogs$GameLink<-gsub("http[:][/][/]www.espn.com[/]nfl[/]game[?]gameId[=]", "//www.espn.com/nfl/boxscore/_/gameId/", teamLogs$GameLink)
teamLogs[duplicated(teamLogs[, c("GameLink", "Team")]),]


###detailed vegas data


getStats<-function(week, season){
  #week<-1;season<-2018
  
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

importGrid<-expand.grid(week=1:21, season=2009:2018)
importGrid<-importGrid[!(importGrid$season==current.season& importGrid$week>current.week), ]

if(!exists("game_odds_detailed")){
  game_odds_detailed<-ldply(lapply(1:nrow(importGrid), function(x) getStats(week = importGrid$week[x], season=importGrid$season[x])), data.frame)
} else{
  #following code returns updated importgrid that only contains current.week or missing dates
  game_odds_detailed<-game_odds_detailed[!(game_odds_detailed$Season==current.season), ]
  importGrid<-importGrid[importGrid$season==current.season, c("week", "season")]
  
  game_odds_detailed2<-ldply(lapply(1:nrow(importGrid), function(x) getStats(week = importGrid$week[x], season=importGrid$season[x])), data.frame)
  game_odds_detailed<-rbind.fill(game_odds_detailed, game_odds_detailed2)
  table(game_odds_detailed$Season, game_odds_detailed$Week)
}
game_odds_detailed[game_odds_detailed$Week==current.week& game_odds_detailed$Season==current.season,]

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
  links<-unique(game_odds_detailed$Link)
  #scrape
  offshore_line_movement<-list();length(offshore_line_movement)<-length(links)
  for(i in 1:length(links)){
    offshore_line_movement[[i]]<-readLink(links[i])
  }
  
} else{
  links<-unique(game_odds_detailed$Link[game_odds_detailed$Season==current.season& game_odds_detailed$Week>=current.week-1])
  
  offshore_line_movement<-offshore_line_movement[!sapply(offshore_line_movement,function(x) x$Link[1]) %in% links]
  
  #scrape and append
  offshore_line_movement2<-list();length(offshore_line_movement2)<-length(links)
  for(i in 1:length(links)){
    offshore_line_movement2[[i]]<-readLink(links[i])
  }
  offshore_line_movement<-append(offshore_line_movement, offshore_line_movement2) 
  offshore_line_movement<- offshore_line_movement[!duplicated(offshore_line_movement)]
  offshore_line_movement<-offshore_line_movement[!sapply(offshore_line_movement, is.null)]
}


if(!exists("vegas_line_movement")){
  links<-unique(game_odds_detailed$Link)
  
  #scrape each link
  vegas_line_movement<-list();length(vegas_line_movement)<-length(links)
  for(i in 1:length(links)){
    vegas_line_movement[[i]]<-readLink(links[i], offshore = F)
  }
  
} else{
  links<-unique(game_odds_detailed$Link[game_odds_detailed$Season==current.season& game_odds_detailed$Week>=current.week-1])
  
  vegas_line_movement<-vegas_line_movement[!sapply(vegas_line_movement,function(x) x$Link[1]) %in% links]
  
  #scrape and append
  vegas_line_movement2<-list();length(vegas_line_movement2)<-length(links)
  for(i in 1:length(links)){
    vegas_line_movement2[[i]]<-readLink(links[i], offshore = F)
  }
  vegas_line_movement<-append(vegas_line_movement, vegas_line_movement2) 
  vegas_line_movement<- vegas_line_movement[!duplicated(vegas_line_movement)]
  vegas_line_movement<-vegas_line_movement[!sapply(vegas_line_movement, is.null)]
}

####detailed vegas bet movement#####

readLink<-function(link){
  print(link)
  #link<-links[12]
  link2<-sapply(strsplit(link, "/time"),`[[`, 1)
  link2<-paste("http://www.vegasinsider.com",link2, sep="")
  link2<-gsub("las-vegas/line-movement", "betting-trends", link2)
  link2<-paste0(link2, "/linechanges/y")
  page<-read_html(link2)
  
  #clean data
  stats<-page%>% html_nodes("td")%>% html_text()
  stats<-gsub("[\r\n\t]", "", stats)
  stats<-stats[!grepl("Tracker|ClosingSpreadMoney|Sunday Night|FavDog|Vegas|News", stats)]
  stats<-stats[!stats%in% c("Money Line", "Spread", "Total", "1st Half", "2nd Half")]
  if(sum(grepl("BETTING TRENDS MOVEMENT CHANGE", stats))>0){
    #clean data
    stats<-stats[!grepl("BETTING TRENDS MOVEMENT CHANGE", stats)]
    gameInfo<-stats[1:(grep("Home", stats)[1]-1)]
    stats<-stats[-c(1:(grep("Home", stats)[1]-1))]
    
    #Clean more data
    stats<-stats[-c(1:tail(which(stats=="M"), 1))]
    stats<-stats[1:(length(stats)-length(stats)%%20)] #if end of stats has errors
    
    
    #convert to dataframe
    stats<-data.frame(matrix(stats, ncol=20, byrow=T))
    stats<-stats[grepl("/", stats[,1]), ]
    stats<-stats[, c(1, 2, 9, 10, 12, 13)]
    colnames(stats)<-c("DATE", "Time","Home.Spread","Home.Bets", "Away.Spread",  "Away.Bets" )
    
    stats$Link<-link
    stats
  } else{
    data.frame()
  }
}


if(!exists("bet_movement")){
  links<-unique(game_odds_detailed$Link)
  #scrape
  bet_movement<-list();length(bet_movement)<-length(links)
  for(i in 1:length(links)){
    bet_movement[[i]]<-readLink(links[i])
  }
  
} else{
  links<-unique(game_odds_detailed$Link[game_odds_detailed$Season==current.season& game_odds_detailed$Week>=current.week-1])
  
  bet_movement<-bet_movement[!sapply(bet_movement,function(x) x$Link[1]) %in% links]
  
  #scrape and append
  bet_movement2<-list();length(bet_movement2)<-length(links)
  for(i in 1:length(links)){
    bet_movement2[[i]]<-readLink(links[i])
  }
  bet_movement<-append(bet_movement, bet_movement2) 
}
bet_movement<- bet_movement[!duplicated(bet_movement)]
bet_movement<-bet_movement[!sapply(bet_movement, is.null)]




###opening odds####


if(!exists("odds_open")){
  dates<-unique(gameLinks$DATE[gameLinks$Year>=2012& !is.na(gameLinks$DATE)])
} else{
  dates<-as.Date(setdiff(unique(gameLinks$DATE[gameLinks$Year>=2012]), odds_open$DATE))
  
}
dates<-c(dates, seq(Sys.Date(), Sys.Date()+7, 1)) %>% unique()
# 

importOdds<-function(date) {
  #   date<-dates[18]#Sys.Date()
  url<-paste0(c("http://classic.sportsbookreview.com/betting-odds/nfl-football/?date=",gsub("-", "", date )), collapse="")
  page<-read_html(url)
  team<-page %>%
    html_nodes(".eventLine-value") %>%
    html_text()
  
  opener<-page %>%
    html_nodes(".eventLine-opener") %>%
    html_text()
  
  if(length(opener)>=1){
    opener[opener!=""]<-sapply(strsplit(opener[opener!=""],'\\s'), `[[`, 1)
    opener[grepl("PK",opener)]<-"0"
     opener<-gsub("[?]|[=]|½", ".5", opener)
    opener<-as.numeric(opener)
    odds<-data.frame(Team=team)
    odds$Odds<-NA
    odds[seq(1, nrow(odds), 2),"Odds"]<-opener
    odds[seq(2, nrow(odds), 2),"Odds"]<-opener*-1
    
    if(nrow(odds)>1){
      odds$DATE<-as.Date(date)
    }
  } else{
    odds<-data.frame()
  }
  print(date)
  odds
}


oddsList<-list();length(oddsList)<-length(dates)
for(i  in 1:length(dates)){
  oddsList[[i]]<-importOdds(dates[i])
}
odds<-ldply(oddsList, data.frame)

odds<-odds[!is.na(odds$DATE),]
colnames(odds)[colnames(odds)=='Odds']<-"Spread.Open"
odds$Team<-gsub("\\(|\\)|0|1|2|3|4|5|6|7|8|9", "", odds$Team)
odds$Team<-trimws(odds$Team)
if(!exists("odds_open")){
  odds_open<-odds
} else{
  odds_open<-rbind(odds, odds_open)
}
odds_open<-odds_open[!is.na(odds_open$Spread.Open)& !abs(odds_open$Spread.Open)>70, ]
odds_open$Team<-coordName(odds_open$Team)
odds_open<-odds_open[!duplicated(odds_open[,c("Team", "DATE")]), ]
setdiff(coordName(teamLogs$Team),odds_open$Team)

head(odds_open)
hist(odds_open$Spread.Open)




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
# espn_pwr<-ldply(lapply(1:nrow(importGrid), function(x) readESPN(season=importGrid$season[x], team=importGrid$team[x])), data.frame)
# table(espn_pwr$WEEK)
# espn_pwr$WEEK[espn_pwr$WEEK=='Preseason']<-0
# espn_pwr[, c("RANK", "WEEK")]<-sapply(espn_pwr[, c("RANK", "WEEK")], function(x) as.numeric(gsub("Week ", "", x)))

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
# espn_pwr2<-ldply(lapply(importGrid$link, readLink),data.frame)
# espn_pwr<-rbind.fill(espn_pwr[,!colnames(espn_pwr)=="RECORD"], espn_pwr2)
# espn_pwr$Team<-coordName(espn_pwr$Team)
# espn_pwr$RANK<-as.numeric(espn_pwr$RANK)
# colnames(espn_pwr)<-sapply(colnames(espn_pwr),simpleCap)%>% unname()
# table(espn_pwr$Team)



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
# yahoo.pickem<-ldply(lapply(yahoo_links$Link, readLink), data.frame)
# yahoo.pickem$Team<-gsub("[@]", "", yahoo.pickem$Team)
# yahoo.pickem$Team<-coordName(yahoo.pickem$Team)
# yahoo.pickem$Yahoo.Pickem<-as.numeric(gsub("%", "", yahoo.pickem$Yahoo.Pickem))


table(gameLinks$Team[!is.na(gameLinks$GameLink)], gameLinks$Year[!is.na(gameLinks$GameLink)])
setdiff(gameLinks$GameLink, teamLogs$GameLink)
setdiff(gameLinks_FREF$GameID[gameLinks_FREF$Season>=2012&gameLinks_FREF$Var.5!="preview"], snapLogs$GameID) 




###HISTORICAL DEPTH CHARTS#######


##historical depth charts####

#use rselenium to get link formats of archived depth charts


# remDr<-rsDriver(browser = "chrome", port=as.integer(4444))
links<-read.csv("Player Data/Depth Chart Links.csv")
links$DATE<-as.Date(links$DATE)

original<-"http://www.ourlads.com/nfldepthcharts/archive/42/phi"
remDr$client$navigate(original)
remDr$client$findElement(using = "css select", value = "#page-content-wrapper .pull-left")$clickElement()
text<-remDr$client$findElements(using="css select", value=".text")

dates<-unlist(sapply(text, function(x) x$getElementText()))
dates<-as.Date(dates, format="%m/%d/%Y")
scrape<-dates[which(!dates%in% as.Date(links$DATE)& !is.na(dates))]
if(length(scrape)>=1){
  for(i in scrape){
    text[[which(dates==i)]]$clickElement()
    links[nrow(links)+1,]<-list(DATE=as.Date(i), URL=unlist(remDr$client$getCurrentUrl()))
    remDr$client$navigate(original)
    Sys.sleep(1)
    remDr$client$findElement(using = "css select", value = "#page-content-wrapper .pull-left")$clickElement()
    text<-remDr$client$findElements(using="css select", value=".text")
  }
  links<-links[!duplicated(links),]
  links$DATE<-as.Date(links$DATE)
  write.csv(links, file= "Player Data/Depth Chart Links.csv", row.names = F)
}
#once i have link formats, scrape depth charts from specified links

links<-read.csv("Player Data/Depth Chart Links.csv")
links<-rbind(links, data.frame(DATE=as.character(Sys.Date()), URL="http://www.ourlads.com/nfldepthcharts/depthchart/phi"))
links$DATE<-as.Date(links$DATE)
teamLinks<-read_html("http://www.ourlads.com/nfldepthcharts/")%>% html_nodes(".nfl-dc-mm-team-links a:nth-child(1)")%>% html_attr("href")
teamLinks<-gsub("depthchart/", "" ,teamLinks)


getStats<-function(date, team){
  # team<-teamLinks[1]
  # date<-as.Date("2009-09-06")
  print(c(as.character(date), team))
  url<-gsub("phi", team,links$URL[links$DATE==date])
  
  depth<-read_html(url)
  depth<-depth%>%html_table()
  depth<-depth[[1]]
  depth<-depth[!is.na(depth[, "Player 1"])& !depth[, "Player 1"]=="?"& 
                 !depth$Pos%in% c("Practice Squad", "Special Teams", "Reserves", "Defense", "Offense", "OFF", "DEF", "ST", "PS", "RES"), 
               !grepl("No", colnames(depth))]
  depth$DATE<-as.Date(date)
  depth$Team<-team
  depth
}
importGrid<-expand.grid(date=links$DATE,    team=teamLinks, stringsAsFactors = F)


if(!exists("game_depthCharts")){ #if !exists, import all dates
  game_depthCharts<-ldply(lapply(1:nrow(importGrid), function(x) getStats(date=importGrid$date[x],team=importGrid$team[x] )))
} else{
  #import current depth.chart and any archived depth charts that haven't been imported yet
  
  game_depthCharts<-game_depthCharts[game_depthCharts$DATE%in% links$DATE,] #remove current.depth
  importGrid<-importGrid[!importGrid$date%in%game_depthCharts$DATE,]
  
  #if exists, just import dates beyond current date
  game_depthCharts2<-ldply(lapply(1:nrow(importGrid), function(x) getStats(date=importGrid$date[x],team=importGrid$team[x] )))
  game_depthCharts<-rbind(game_depthCharts, game_depthCharts2)
}
table(game_depthCharts$DATE)


#game data data
save(list=c(  "game_injuries", "game_inactives", "game_odds","game_odds_detailed","offshore_line_movement","vegas_line_movement","game_depthCharts","bet_movement",
              "odds_open",
              "gameLinks","gameLinks_FREF", "gameLogs","teamLogs","snapLogs","game.info","starterLogs",
              "espn_pwr", "elo_rank" , "game_locs", "yahoo.pickem",#game stats
              "current.week", "current.season", "current.type"
), file="Player Data/NFL Game Data.RData")

#load("Player Data/NFL Game Data.RData")

