#File clean.R
cleanGames<-function(games){
  #Change one error
  index<-which(games$Year=="2020")
  games[index,"Year"]="2009"
  
  #group the firsts years 1980-1994
  fy<-seq(1980,1994)
  fyears<-which(games$Year %in% fy)
  games$Year<-as.character(games$Year)
  games[fyears,"Year"]<-"1980-1994"
  
  
  #Delete the 3 games from 2017
  y2017=which(games$Year=="2017")
  games<-games[-y2017,]
  
  games$Year<-as.factor(games$Year)
  
  games
}