#function which prepares the data
#return: a data set without missing values

  getGames<-function(){
    
    games <- read.csv("input/games.csv")
    
    initialRows<-nrow(games)
    initialCols<-ncol(games)
    
    #Remove unsued columns
    games$NA_Sales<-NULL
    games$EU_Sales<-NULL
    games$JP_Sales<-NULL
    games$Other_Sales<-NULL
    
    games$User_Score<-NULL
    games$User_Count<-NULL
    
    games$Rating<-NULL
    
    games$Developer<-NULL
    
    games$Name<-NULL
    games$Publisher<-NULL
    games$Critic_Score<-NULL
    games$Critic_Count<-NULL
    
    #Delete rows without a proper Genre
    index<-which(games$Genre=="")
    games<-games[-index,]
    games$Genre<-factor(games$Genre)
    
    #Delete rows without the release year
    index<-which(games$Year_of_Release=="N/A")
    games<-games[-index,]
    
    #Set year of release as a date.
    #dates$Year<-as.Date(games$Year_of_Realese,"%Y")
    
    #Set proper names
    names(games)[2]<-"Year"
    names(games)[4]<-"Sales"
    
    games<-games[complete.cases(games), ]
    
    message(initialRows- nrow(games)," rows and "
            ,initialCols - ncol(games)," cols deleted" )
    
    games
  }

