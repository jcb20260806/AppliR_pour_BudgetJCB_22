# Get Index from StatBel
GetIndex <- function(){

  
  Index_de_Reference <- 2013
  SourceIndex<-"https://statbel.fgov.be/sites/default/files/files/opendata/Consumptieprijsindex%20en%20gezondheidsindex/CPI%20All%20base%20years.xlsx"
  destfile <- "./index.xlsx"
 download.file(SourceIndex, destfile)
  IndexDF<-read_excel(destfile,guess_max = 1048576) # Index DataFrame, Guess max sert à correctement identifier le datatype des colonnes
  # Clean IndexDF
  # select base Year = 2013 et année>2017
  IndexDF<-IndexDF[IndexDF$NM_BASE_YR==Index_de_Reference & IndexDF$NM_YR>2017,]
  # rename Fields
  names(IndexDF)[names(IndexDF) == 'NM_YR'] <- 'An'
  names(IndexDF)[names(IndexDF) == 'NM_MTH'] <- 'Mois'
  names(IndexDF)[names(IndexDF) == 'MS_HLTH_IDX'] <- 'Index'
  # select colonnes utiles
  IndexDF <- IndexDF[,c("An","Mois","Index")]
  IndexDF$Date <- as.Date(paste("1",IndexDF$Mois,IndexDF$An,sep="-"),format = "%d-%m-%Y")
  IndexDF$DayFE <- as.integer(as.Date(IndexDF$Date,format=	"%Y-%m-%d"))
  IndexDF <- IndexDF %>% mutate(An2 = year(ymd(Date)))
  IndexDF$Date <- as.POSIXct(IndexDF$Date, format = "%Y-%m-%d", tz = "UTC")
  return( IndexDF)
}
#Test <- GetIndex()

