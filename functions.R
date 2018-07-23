library(png)
library(grid)
library(doSNOW)
library(plyr)
library(dplyr)
library(rvest)
library(ggplot2)
library(data.table)
library(XML)
library(lpSolve)
library(zoo)
library(MASS)
options(stringsAsFactors = F, scipen =999)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

####ABBREVIATIONS###
abbrev<-read.csv("Misc Data/Abbrev.txt", sep="\t", header=TRUE, row.names = NULL)
abbrev$Abbrev2<-unlist(lapply(strsplit(abbrev$FullName, " "), function(x) paste(x[-length(x)], collapse=" ")))
abbrev$Abbrev2[abbrev$Abbrev=="NYJ"]<-"N.Y. Jets"
abbrev$Abbrev2[abbrev$Abbrev=="NYG"]<-"N.Y. Giants"
abbrev$Abbrev3<-tolower(abbrev$Abbrev)
abbrev$Abbrev3[abbrev$Abbrev3=="ari"]<-"crd"
abbrev$Abbrev3[abbrev$Abbrev3=="bal"]<-"rav"
abbrev$Abbrev3[abbrev$Abbrev3=="hou"]<-"htx"
abbrev$Abbrev3[abbrev$Abbrev3=="ind"]<-"clt"
abbrev$Abbrev3[abbrev$Abbrev3=="oak"]<-"rai"
abbrev$Abbrev3[abbrev$Abbrev3=="ten"]<-"oti"
abbrev$Abbrev3[abbrev$Abbrev3=="stl"|abbrev$Abbrev3=="lar"]<-"ram"
abbrev$Abbrev<-sapply(abbrev$Abbrev, simpleCap)
abbrev$Abbrev[abbrev$Abbrev%in% c("Gb", "Kc", "La", "Ne", "No", "Sd","Sf", "Tb")]<-
  c("Gnb", "Kan", "Lar", "Nwe", "Nor", "Sdg", "Sfo", "Tam")

#https://stackoverflow.com/questions/20495598/replace-accented-characters-in-r-with-non-accented-counterpart-utf-8-encoding

unwanted_array <- list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'é'='e')

coordName<-function(x) {
  x<-chartr(paste(names(unwanted_array), collapse=''),
            paste(unwanted_array, collapse=''),
            x)
  x<-gsub("^\\s+|\\s+$", "", x)
  x<-gsub(" Jr.| Sr.","",x)
  x<-gsub("*|+|`", "",x)
  x<-gsub("[*]|[`]", "",x)
  x<-gsub("[+]", "",x)
  x<-gsub(", Jr.", " Jr.", x)
  x<-gsub("[.]|[']|[,]", "", x)
  x<-gsub(" Jr", "", x)
  x<-gsub(" III", "", x)
  x<-gsub(" IV", "", x)
  x<-gsub(" II", "", x)
  x<-gsub("-", " ", x)
  x<-gsub("  ", " ", x)
  
  x<-gsub("JuJu", "Juju", x)
  x<-gsub("Zac |Zack |Zachary |Zackary ", "Zach ", x)
  x<-gsub("ArDarius", "Ardarius", x)
  x<-gsub("Johnathan |Jon |John ", "Jonathan ", x)
  x<-gsub("De'Andre", "DeAndre", x)
  x<-gsub("Le'Ron", "LeRon", x)
  x<-gsub("Christopher ", "Chris ", x)
  x<-gsub("Billy ", "Bill ", x)
  x<-gsub("Deshone", "DeShone", x)
  x<-gsub("Khalif", "Kalif", x)
  x<-gsub("Gerell", "Gerrell", x)
  x<-gsub("Mitch ", "Mitchell ", x)
  x<-gsub("William ", "Will ", x)
  x<-gsub("Pat ", "Patrick ", x)
  x<-gsub("Alex ", "Alexander ", x)
  x<-gsub("Jimmy ", "Jim ", x)
  x<-gsub("Thad ", "Thaddeus ", x)
  x<-gsub("Tim |Timothhy |Timmy ", "Timothy ", x)
  x<-gsub("Matt ", "Matthew ", x)
  x<-gsub("Nathan |Nathaniel ", "Nate ", x)
  x<-gsub("Joe ", "Joseph ", x)
  x<-gsub("Shaquelle |Shaquille |Shaquil ", "Shaq ", x)
  x<-gsub("Johnnie Lee ", "Johnnielee ", x)
  x<-gsub("Josh ", "Joshua ", x)
  x<-gsub("Rich ", "Richard ", x)
  x<-gsub("Mike ", "Michael ", x)
  x<-gsub("Rob ", "Robert ", x)
  x<-gsub("Samuel ", "Sam ", x)
  x<-gsub("Nicholas |Nicolas |Nic ", "Nick ", x)
  x<-gsub("Daniel |Danny ", "Dan ", x)
  x<-gsub("Stevie ", "Steve ", x)
  x<-gsub("Brad ", "Bradley ", x)
  x<-gsub("Walt ", "Walter ", x)
  x<-gsub("Trenton ", "Trent ", x)
  x<-gsub("Edward ", "Ed ", x)
  x<-gsub("Ronald ", "Ron ", x)
  x<-gsub("Isreal ", "Israel ", x)
  x<-gsub("Vincent ", "Vince ", x)
  x<-gsub("Dave ", "David ", x)
  x<-gsub("Rodrick |Roderick ", "Rod ", x)
  x<-gsub("Ben |Benny ", "Benjamin ", x)
  x<-gsub("Brenden ", "Brendan ", x)
  x[ grepl("Odell", x) & grepl("Beckham", x)]<-"Odell Beckham"  
  x[x==  "Evan Dietrich Smith"]<-"Evan Smith"
  x[x==  "Chris Kirksey"]<-"Christian Kirksey"
  x[x==  "Antwon Blake"]<-"Valentino Blake"
  x[x==  "Joseph Lefeged"]<-"Joseph Young"
  x[x==  "Lac Edwards"]<-"Lachlan Edwards"
  x[x==  "Cb Bryant"]<-"Christian Bryant"
  x[x==  "Leterrius Walton"]<-"Lt Walton"
  x[x==  "Nordly Capi"]<-"Cap Capi"
  x[x==  "Jacob Schum"]<-"Jake Schum"
  x[x==  "Owamagbe Odighizuwa"]<-"Owa Odighizuwa"
  x[x==  "Ricky Wagner"]<-"Rick Wagner"
  x[x==  "Gator Hoskins"]<-"Harold Hoskins"
  x[x==  "Vladimir Ducasse"]<-"Vlad Ducasse"
  x[x==  "Dejonathan Gomes"]<-"Dejon Gomes"
  x[x==  "Dajonathan Harris"|x=="Dajon Harris"|x=="DaJohnathan Harris"|x=="DaJohn Harris"]<-"Dajohn Harris"
  x[x==  "Jeffrey Linkenbach"]<-"Jeff Linkenbach"
  x[x==  "Manuel Ramirez"]<-"Manny Ramirez"
  x[x==  "Herbert Taylor"]<-"Herb Taylor"
  x[x==  "Ike Ndukwe"]<-"Ikechuku Ndukwe"
  x[x==  "Vic Worsley"]<-"Victor Worsley"
  x[x==  "Baba Oshinowo"]<-"Babatunde Oshinowo"
  x[x==  "Bam Childress"]<-"Brandon Childress"
  x[x==  "Steve Cheek"]<-"Stephen Cheek"
  x[x==  "Steve Neal"]<-"Stephen Neal"
  x[x==  "Renaud Williams"]<-"Renauld Williams"
  x[x==  "Woody Dantzler"]<-"Woodrow Dantzler"
  x[x==  "Raymond Perryman"]<-"Ray Perryman"
  x[x==  "Jj Huggins"]<-"Johnny Huggins"
  x[x==  "Abdul Karim Al Jabbar"]<-"Karim Abdul Jabbar"
  x[x==  "Maugaula Tuitele"]<-"Ula Tuitele"
  x[x==  "Rocket Ismail"]<-"Raghib Ismail"
  x[x==  "Michael A Jones"]<-"Michael Jones"
  x[x==  "T Marcus Spriggs"]<-"Marcus Spriggs"
  x[x==  "Tyrone M Williams"]<-"Tyrone Williams"
  
  
  
  x[x==  "Greg K Jones"]<-"Greg Jones"
  x[x==  "Todd F Collins"]<-"Todd Collins"
  x[x==  "Kevin R Williams"]<-"Kevin Williams"
  x[x==  "Charles L Johnson"]<-"Charles Johnson"
  x[x==  "Michael L Lewis"]<-"Michael Lewis"
  x[x==  "Andre Hal"]<-"Andre Hall"
  x[x==  "R Jay Soward"]<-"Rjay Soward"
  x[x==  "Dede Dorsey"|x=="DeDe Dorsey"]<-"De Dorsey"
  x[x==  "Scott Vines"]<-"Scottie Vines"
  x[x==  "N D Kalu"]<-"ND Kalu"
  x[x==  "Will Peterson"]<-"Will James"
  x[x==  "AD Denham"]<-"Anthony Denham"
  x[x==  "Joseph Davenport"]<-"Joseph Dean Davenport"
  x[x==  "Derek M Smith"]<-"Derek Smith"
  x[x==  "Stephen Hauschka"]<-"Steven Hauschka"
  x[x==  "Roosevelt Williams"]<-"Roe Williams"
  x[x==  "Oshiomogho Atogwe"]<-"Oj Atogwe"
  x[x==  "Jolonn Dunbar"]<-"Jo Lonn Dunbar"
  x[x==  "Standford Keglar"]<-"Stanford Keglar"
  x[x==  "Chinedum Ndukwe"]<-"Nedu Ndukwe"
  x[x==  "Dwight Bentley"]<-"Bill Bentley"
  x[x==  "Ray Ventrone"]<-"Raymond Ventrone"
  x[x==  "Jay Elliott"]<-"Jayrone Elliott"
  x[x==  "Sammie Hill"]<-"Sammie Lee Hill"
  x[x==  "Cameron Cleeland"]<-"Cam Cleeland"
  x[x==  "Tony Hargrove"]<-"Anthony Hargrove"
  x[x==  "Ed Hartwell"]<-"Edgerton Hartwell"
  x[x==  "Junior Siav"]<-"Junior Siavii"
  x[x==  "Jerry Attaochu"]<-"Jeremiah Attaochu"
  x[x==  "Jalen Tabor"]<-"Teez Tabor"
  x[x==  "Albert Fincher"]<-"Alfred Fincher"
  x[x==  "Pep Levingston"]<-"Lazarius Levingston"
  x[x==  "Hebron Fangupo"]<-"Loni Fangupo"
  x[x==  "Jay Ratliff"]<-"Jeremiah Ratliff"
  x[x==  "Macho Harris"]<-"Victor Harris"
  x[x==  "Juqua Thomas"]<-"Juqua Parker"
  x[x==  "Christion Jones"]<-"Christian Jones"
  x[x==  "Mo Alexander"]<-"Maurice Alexander"
  x[x==  "Malcolm Floyd"]<-"Malcom Floyd"
  x[x==  "Joseph Unga"]<-"Jj Unga"
  x[x==  "Jean Phillipe Darche"]<-"Jp Darche"
  x[x==  "Christian Mohr"]<-"Chris Mohr"
  x[grepl( "Dom", x)& grepl("Rodgers", x)& grepl("romartie", x)]<-"Dominique Rodgers Cromartie"
  x[x==  "M White"]<-"Myles White"
  x[x==  "Nickell Robey"]<-"Nickell Robey Coleman"
  x[x==  "Deji Olatoye"]<-"Ayodeji Olatoye"
  x[x==  "Saverio Rocca"]<-"Sav Rocca"
  x[x==  "Travis Carrie"]<-"Tj Carrie"
  x[x==  "Jordan Dizon"]<-"Jordon Dizon"
  x[x==  "Donald Drer"]<-"Donald Driver"
  x[x==  "Ziggy Hood"]<-"Evander Hood"
  x[x==  "Ziggy Ansah"]<-"Ezekiel Ansah"
  x[x==  "Jacque Cesaire"]<-"Jacques Cesaire"
  x[x==  "Boobie Dixon"]<-"Anthony Dixon"
  x[x==  "Chad Ochocinco"]<-"Chad Johnson"
  x[x==  "Philip Rers"]<-"Philip Rivers"
  x[x==  "Tani Tupou"]<-"Taniela Tupou"
  x[x==  "Bobo Wilson"]<-"Jesus Wilson"
  x[x==  "John Paul Foschi"]<-"Jp Foschi"
  x[x==  "Danny Ware"]<-"Dj Ware"
  x[x==  "Jonathan Baldwin"]<-"Jon Baldwin"
  x[x==  "Chartric Darby"]<-"Chuck Darby"
  x[x==  "Trevor Graham"]<-"Tj Graham"
  x[x==  "J Talley"]<-"Julian Talley"
  x[x==  "Charles D Johnson"]<-"Charles Johnson"
  x[x==  "Seantavious Jones"]<-"Seantavius Jones"
  x[x==   "Will Fuller V"]<- "Will Fuller"
  x[x==   "Pacman Jones"]<- "Adam Jones"
  x[x==   "Broderick Bunkley"]<- "Brodrick Bunkley"
  x[x== "Evan Dietrich-Smith"  ]<-"Evan Smith" 
  x[x==  "Chris Simms" ]<- "Matt Simms"  
  x[x=="Philly Brown"  ]<-"Corey Brown"  
  x[x=="Zachdiles"  ]<-"Zach Diles"  
  
  x<-sapply(x,simpleCap)
  
  x[x==  "Samajae Perine"]<-"Samaje Perine"
  x[x==  "Phillip Rivers"]<-"Philip Rivers"
  x[x==  "Tedd Ginn"]<-"Ted Ginn"
  x[x==  "Alexander Ogletree"]<-"Alec Ogletree"
  x[x==  "Tony Jeffersom"]<-"Tony Jefferson"
  x[x==  "Vinateri"|x=="vinateri"|x=="Adam Vinateri"]<-"Adam Vinatieri"
  x[x==  "Khali Mack"]<-"Khalil Mack"
  x[x==  "Wendall Smallwood"]<-"Wendell Smallwood"
  x[x==  "Alshon Jeffrey"]<-"Alshon Jeffery"
  x[x==  "Will Lutz"]<-"Wil Lutz"
  x[x==  "Risshard Matthews"]<-"Rishard Matthews"
  x[x==  "Joey Brosa"]<-"Joey Bosa"
  x[x==  "Giovanni Bernard"]<-"Giovani Bernard"
  x[x==  "Jim Grahm"]<-"Jim Graham"
  x[x==  "Hassan Reddick"]<-"Haason Reddick"
  x[x==  "Dein Jones"]<-"Deion Jones"
  x[x==  "James Connor"]<-"James Conner"
  x[x==  "Robbie Anderson"]<-"Robby Anderson"
  x[x==  "Terrence West"]<-"Terrance West"
  x[x==  "Jay Ajayii"]<-"Jay Ajayi"
  x[x==  "Isiah Crowell"]<-"Isaiah Crowell"
  x[x==  "Bernard Mckinney"]<-"Benardrick Mckinney"
  x[x==  "Paul Puz"|x=="Paul Posluzsny"]<-"Paul Posluszny"
  x[x==  "Lagarette Blount"]<-"Legarrette Blount"
  x[x==  "Erik Decker"]<-"Eric Decker"
  x[x==  "Bilall Powell"]<-"Bilal Powell"
  x[x==  "Jedeveon Clowdney"|grepl("eon Clowney", x)]<-"Jadeveon Clowney"
  x[x==  "Mathew Stafford"]<-"Matthew Stafford"
  x[x==  "Chandler Carazano"]<-"Chandler Catanzaro"
  x[x==  "Navarro Bowman"]<-"Navorro Bowman"
  x[x==  "Emanuel Sanders"]<-"Emmanuel Sanders"
  x[x==  "Darren Mccfadden"]<-"Darren Mcfadden"
  x[x==  "James Laurinatis"]<-"James Laurinaitis"
  x[x==  "Gore"]<-"Frank Gore"
  x[x==  "Everson Griffin"]<-"Everson Griffen"
  x[x==  "Buck Allen"]<-"Javorius Allen"
  x[x==  "Reuben Randle"]<-"Rueben Randle"
  x[x==  "Derrick Jonhson"]<-"Derrick Johnson"
  x[x==  "Mo Wilkerson"]<-"Muhammad Wilkerson"
  x[x==  "Mcmanus"]<-"Brandon Mcmanus"
  x[x==  "Ryan Shaziwer"]<-"Ryan Shazier"
  x[x==  "Larry Donnel"]<-"Larry Donnell"
  x[x==  "Martellus Bennet"]<-"Martellus Bennett"
  x[x==  "Daniel Herron"]<-"Dan Herron"
  
  
  
  x[x=="Ne"]<-"NWE"
  x<-gsub(" Defense", "", x)
  x[x=="Ne"| grepl("Patriots", x)]<-"NWE"
  x[x=="Kc"| grepl("Chiefs", x)]<-"KAN"
  x[x=="Gb" | grepl("Packers", x)]<-"GNB"
  x[x=="Sd"| x=="Lac"|x=="LAC"| grepl("Chargers", x)]<-"SDG"
  x[x=="No" |grepl("Saints",x)]<-"NOR"
  x[x=="Tb" | grepl("Buccaneers", x)]<-"TAM"
  x[x=="Sf"| grepl("49ers", x)]<-"SFO"
  x[x=="La"|grepl("Rams|Stl|St. Louis|St Louis", x)]<-"LAR"
  x[x=="Jac"]<-"JAX"
  x[grepl("Cardinals", x)]<-"ARI"
  x[grepl("Falcons", x)]<-"ATL"
  x[grepl("Ravens", x)]<-"BAL"
  x[grepl("Bills", x)]<-"BUF"
  x[grepl("Carolina", x)]<-"CAR"
  x[grepl("Bears", x)]<-"CHI"
  x[grepl("Bengals", x)]<-"CIN"
  x[grepl("Browns", x)]<-"CLE"
  x[grepl("Cowboys", x)]<-"DAL"
  x[grepl("Broncos", x)]<-"DEN"
  x[grepl("Lions", x)]<-"DET"
  x[grepl("Packers", x)]<-"GNB"
  x[grepl("Texans", x)]<-"HOU"
  x[grepl("Colts", x)]<-"IND"
  x[grepl("Jaguars", x)| x=="JAC"]<-"JAX"
  x[grepl("Dolphins", x)]<-"MIA"
  x[grepl("Vikings", x)]<-"MIN"
  x[grepl("Giants", x)]<-"NYG"
  x[grepl("Jets", x)]<-"NYJ"
  x[grepl("Raiders", x)]<-"OAK"
  x[grepl("Panthers", x)]<-"CAR"
  x[grepl("Eagles", x)]<-"PHI"
  x[grepl("Steelers", x)]<-"PIT"
  x[grepl("Seahawks", x)]<-"SEA"
  x[grepl("Titans", x)]<-"TEN"
  x[grepl("Redskins", x)]<-"WAS"
  x[x%in% c("Arz", "ARZ")]<-"Ari"
  x[x%in% abbrev$Abbrev2]<-abbrev$Abbrev[match(x[x%in% abbrev$Abbrev2], abbrev$Abbrev2)]
  x[x%in% abbrev$Abbrev3]<-abbrev$Abbrev[match(x[x%in% abbrev$Abbrev3], abbrev$Abbrev3)]
  x[x%in% abbrev$FullName]<-abbrev$Abbrev[match(x[x%in% abbrev$Abbrev3], abbrev$FullName)]
  
  x<-sapply(x,simpleCap)
  x
}


moving<-function(x, length, operation="mean", include.current=F) { 
  if(include.current){
    x<-c(NA, x) 
  }
  
  #x<-c(2, 4, 1,2,4,1,2,4 ,4,4, 10, 1, NA, 1, 1, 1, 2 , NA,3, NA, 2);length<-10
  #x<-c(NA, NA)
  #if(operation%in% c())
  
  x2<-x
  if(operation%in% c("harmonic", "geometric")) {
    x[x<=0]<-NA
  }
  
  if(operation=="mean" & length(x)>=1) {
    y<-rep(NA, length(x))
    
    if(is.na(x[1])) {
      y[1]<-sum(x[!is.na(x)][1:min(length, length(x[!is.na(x)]))])/
        length(x[!is.na(x)][1:min(length, length(x[!is.na(x)]))])
    }
    
    
    if(length(x[!is.na(x)])>length) {
      
      #rolling mean of previous values
      y[!is.na(x)]<-append(rollmean(x[!is.na(x)], length,na.pad=TRUE, align="left" )[-1], NA) 
      
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)]<-rev(cumsum(rev(x[is.na(y) & !is.na(x)]))-rev(x[is.na(y) & !is.na(x)]))/
        rev(seq_along(x[is.na(y) & !is.na(x)])-1) 
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    } else{
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)]<-rev(cumsum(rev(x[is.na(y) & !is.na(x)]))-rev(x[is.na(y) & !is.na(x)]))/
        rev(seq_along(x[is.na(y) & !is.na(x)])-1) 
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
    }
  } else if (operation=="median"  & length(x)>=1) {
    y<-rep(NA, length(x))
    
    if(is.na(x[1])) {
      y[1]<-median(x[!is.na(x)][1:min(length, length(x[!is.na(x)]))])
    }
    if(length(x[!is.na(x)])>length) {
      
      #rolling median of previous values
      y[!is.na(x)]<-append(rollmedian(x[!is.na(x)], length,na.pad=TRUE, align="left" )[-1], NA) 
      
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)]<-rev(cumsum(rev(x[is.na(y) & !is.na(x)]))-rev(x[is.na(y) & !is.na(x)]))/
        rev(seq_along(x[is.na(y) & !is.na(x)])-1) 
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    } else{
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)]<-rev(cumsum(rev(x[is.na(y) & !is.na(x)]))-rev(x[is.na(y) & !is.na(x)]))/
        rev(seq_along(x[is.na(y) & !is.na(x)])-1) 
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    }
  } else if (operation=="max"  & length(x)>=1) {
    #x<-c(2, 4, 1,2,4,1,2,4 ,4,4, 10, 1, NA, 1, 1, 1, 2 , NA,3, NA, 2);length<-4
    y<-rep(NA, length(x))
    if(is.na(x[1])) {
      y[1]<-max(x[!is.na(x)][1:min(length, length(x[!is.na(x)]))])
    }
    if(length(x[!is.na(x)])>length) {
      
      #rolling median of previous values
      y[!is.na(x)]<-append(rollmax(x[!is.na(x)], length,na.pad=TRUE, align="left" )[-1], NA) 
      
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)][-length( y[is.na(y) & !is.na(x)])]<-   
        rev(cummax(rev(x[is.na(y) & !is.na(x)]))[-length(cummax(rev(x[is.na(y) & !is.na(x)])))])
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    } else{
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)][-length( y[is.na(y) & !is.na(x)])]<-   
        rev(cummax(rev(x[is.na(y) & !is.na(x)]))[-length(cummax(rev(x[is.na(y) & !is.na(x)])))])
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    }
  } else if (operation=="min"  & length(x)>=1) {
    x<-(-x)
    y<-rep(NA, length(x))
    
    if(is.na(x[1])) {
      y[1]<-max(x[!is.na(x)][1:min(length, length(x[!is.na(x)]))])
    }
    if(length(x[!is.na(x)])>length) {
      
      #rolling median of previous values
      y[!is.na(x)]<-append(rollmax(x[!is.na(x)], length,na.pad=TRUE, align="left" )[-1], NA) 
      
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)][-length( y[is.na(y) & !is.na(x)])]<-   
        rev(cummax(rev(x[is.na(y) & !is.na(x)]))[-length(cummax(rev(x[is.na(y) & !is.na(x)])))])
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    } else{
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)][-length( y[is.na(y) & !is.na(x)])]<-   
        rev(cummax(rev(x[is.na(y) & !is.na(x)]))[-length(cummax(rev(x[is.na(y) & !is.na(x)])))])
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    }
    y<-(-y)
  } else if (operation =="sum"  & length(x)>=1)   { 
    y<-rep(NA, length(x))
    
    if(is.na(x[1])) {
      y[1]<-sum(x[!is.na(x)][1:min(length, length(x[!is.na(x)]))])
    }
    
    if(length(x[!is.na(x)])>length) {
      
      #rolling sum of previous values
      y[!is.na(x)]<-append(rollsum(x[!is.na(x)], length,na.pad=TRUE, align="left" )[-1], NA) 
      
      #fill in first few games with cumulative moving sum
      y[is.na(y) & !is.na(x)]<-rev(cumsum(rev(x[is.na(y) & !is.na(x)]))-rev(x[is.na(y) & !is.na(x)]))
      
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      #fill in first game with next game if NA
      y<-na.locf(y, fromLast=TRUE,na.rm=FALSE)
      
      
    } else {
      
      #rolling mean of previous values
      #fill in first few games with cumulative moving sum
      y[is.na(y) & !is.na(x)]<-rev(cumsum(rev(x[is.na(y) & !is.na(x)]))-rev(x[is.na(y) & !is.na(x)]))
      
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      #fill in first game with next game 
      y<-na.locf(y, fromLast=TRUE,na.rm=FALSE)
      
      
    } 
  }else if (operation =="lag"  & length(x)>=1)   { 
    y<-rep(NA, length(x))
    
    
    if(length(x[!is.na(x)])>length) {
      
      #rolling sum of previous values
      y[!is.na(x)]<-lead(x[!is.na(x)], length)
      
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      toNA<-which(!is.na(x))[length(which(!is.na(x)))-length+1]
      y[toNA:length(y)]<-NA
      
    } 
  } else if( length(x)>=1) {
    
    y<-rep(NA, length(x))
    operationFunc<-get(operation)
    
    if(is.na(x[1])) {
      y[1]<-operationFunc(x[!is.na(x)][1:min(length, length(x[!is.na(x)]))])
    }
    if(length(x[!is.na(x)])>length) {
      
      #rolling median of previous values
      y[!is.na(x)]<-c(rollapply(x[!is.na(x)], width=length,FUN=operationFunc, fill=NA, align="left" )[-1], NA) 
      
      #fill in first few games with cumulative moving ave, rollapplyr is slower than cumsum, cummean, etc
      y[is.na(y) & !is.na(x)][-length( y[is.na(y) & !is.na(x)])]<- 
        rollapplyr(x[is.na(y) & !is.na(x)], rev(seq_along(x[is.na(y) & !is.na(x)])), operationFunc, align="left")[-1]
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    } else if(!all(is.na(x))){
      #fill in first few games with cumulative moving ave
      y[is.na(y) & !is.na(x)][-length( y[is.na(y) & !is.na(x)])]<- 
        rollapplyr(x[is.na(y) & !is.na(x)], rev(seq_along(x[is.na(y) & !is.na(x)])), operationFunc, align="left")[-1]
      
      #fill in missing games with previous game
      y<-na.locf(y, fromLast=FALSE,na.rm=FALSE) 
      
      
    }
    
    y[is.na(y) & !is.na(x2)]<-0
    
  }
  if(length(x)<1) {
    y<-NA
  }
  
  #adjust beginning games, sometimes errors if multiple NAs to start off
  NA_vec<-which(is.na(x2));
  if(operation%in% c("nonZero", "geometric", "harmonic")){
    NA_vec<-which(is.na(x2)| x2==0);
  }
  still_NA<-TRUE;
  y[length(x2)]<-NA
  adjust<-length(x2)
  
  #first 2 should be NA for var-variables
  if(operation %in% c("var", "sd", "kurtosis", "skewness")){
    y[length(x2)-1]<-NA
    adjust<-length(x2)-1
  }
  while(still_NA) {
    y[adjust]<-NA
    if((adjust) %in% NA_vec) {adjust<-adjust-1} else{ still_NA<-FALSE}
    
  }
  
  if(include.current){
    y<-y[-length(y)] 
    y[length(y)]<-x[length(x)]
    if(is.na(x[1])){
      y[1]<-NA
    }
    
  }
  
  y
  
}
harmonic<-function(x) {
  harm<-1/mean(1/x[x>0], na.rm=TRUE)
  if(is.nan(harm)){harm<-NA}
  harm
}
geometric<- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
trimmed<-function(x) {
  mean(x[-c(which.min(x),which.max(x))], na.rm=T)
}
trimmed2<-function(x) {
  mean(x[-c(which.min(x))], na.rm=T)
}
quartile75<-function(x) {
  unname(quantile(x, .75, na.rm=TRUE))
}
quartile25<-function(x) {
  unname(quantile(x, .25, na.rm=TRUE))
}
nonZero<-function(x) {
  mean(x[x!=0], na.rm=TRUE)
}
Evaluation.NDCG = function(EstimatedRank, RelevanceLevel) {
  Ordered.RelevanceLevel <- RelevanceLevel[order(-EstimatedRank)]
  Actual.RelevanceLevel <- RelevanceLevel[order(-RelevanceLevel)]
  dcg <- 0
  normalization.constant <- 0
  for(i in 1:length(Ordered.RelevanceLevel)) dcg <- dcg + (2^Ordered.RelevanceLevel[i] - 1) / log2(i+1)
  for(i in 1:length(Actual.RelevanceLevel)) normalization.constant <- normalization.constant + (2^Actual.RelevanceLevel[i] - 1) / log2(i+1)
  dcg / normalization.constant
}

makeSlotPlot<-function(Title="Simulation Results for All Draft Slots",
                       Parameters=c("Case 1", "Case 1 + ZeroRB in R1", "Case1 + ZeroWR in R1")){
  
  Slot<-rep( paste0("Slot", 1:12), 3)
  Sims<-unlist(c(simScores_allSlots, simScores_allSlots_zeroRB, simScores_allSlots_zeroWR))
  
  
  Sims<-data.frame(Sim=Sims, Slot=rep(Slot, each=25000), Parameter=rep(Parameters, each=length(Sims)/3))
  Sims$Slot<-factor(Sims$Slot, levels=unique(Sims$Slot))
  Sims<-ddply(Sims, .(Slot, Parameter), summarize, 
              N    = length(Sim),
              mean = mean(Sim),
              sd   = sd(Sim),
              se   = sd / sqrt(N) )
  library(ggplot2)
  head(Sims)
  
  p<-ggplot(Sims, aes(x=Slot, y=mean, fill=Parameter)) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", # Use black outlines,
             size=.3) +      # Thinner lines
    # theme(axis.text.x=element_blank(), 
    # axis.title.x = element_blank()
    # )+
    coord_cartesian(ylim=c(min(Sims$mean)-10, max(Sims$mean+20)))+
    geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), #add confidence interval (+/-1.96*SE)
                  size=.3,    # Thinner lines
                  width=.2,
                  position=position_dodge(.9)) +
    xlab("Slot") +
    ylab("Mean-Simulated Starting Lineup (25,000 sims)") +
    ggtitle(Title)
  p
}
makeParamPlot<-function(Parameters=c("1. RBx5,WRx5,QBx2,K/DST/TEx1 (default)", "2. default, shift=.25","3. default, shift=-.15",
                                     "4. zero RB in R1, shift=0", "5. zero WR in R1, shift=0",  "6. \u2264 1QB in R1-11, shift=0", 
                                     "7. RBx4, WRx6, shift=0", "8. RBx6, WRx4, \u2264 1QB in R1-11, shift=0",
                                     "9. RBx5,WRx5,TEx2,QB/DST/Kx1, shift=0", "10. RBx4,WRx5,QB/TEx2,DST/Kx1, shift=0", 
                                     "11. Zero RB in R1-4, \u2264 1QB in R1-11,  shift=0" ),
                        Title="Simulation Results for Different Draft Parameters"){
  Sims<-list(simScores,simScores2, simScores3, simScores4, simScores5,
             simScores6, simScores7, simScores8, simScores9, simScores10, simScores11) %>% unlist(recursive = T)
  
  
  Sims<-data.frame(Sim=Sims, Parameter=rep(as.character(Parameters), each=25000))
  Sims$Parameter<-factor(Sims$Parameter, levels=unique(Sims$Parameter))
  Sims<-ddply(Sims, .(Parameter), summarize, 
              N    = length(Sim),
              mean = mean(Sim),
              sd   = sd(Sim),
              se   = sd / sqrt(N) )
  library(ggplot2)
  head(Sims)
  
  p<-ggplot(Sims, aes(x=Parameter, y=mean, fill=Parameter)) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", # Use black outlines,
             size=.3) +      # Thinner lines
    theme(axis.text.x=element_blank(), 
          axis.title.x = element_blank()
    )+
    coord_cartesian(ylim=c(min(Sims$mean)-10, max(Sims$mean+20)))+
    geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), #add confidence interval (+/-1.96*SE)
                  size=.3,    # Thinner lines
                  width=.2,
                  position=position_dodge(.9)) +
    xlab("Parameter") +
    ylab("Mean-Simulated Starting Lineup (25,000 sims)") +
    ggtitle(Title)
  p
}
