# Cette fonction fait un data frame pour une combinaison Lot Locataire
# cette fonction utilise AllDates, AllLoyers et IndexDF
# 
MakeLotLoc <- function(parCurrentLot,parCurrentLoc,AllLoyers,AllDates){ 
  library(zoo)
  #browser()
  parDateIndex  <- AllLoyers$DateIndex[AllLoyers$Lot == parCurrentLot & AllLoyers$Locataire == parCurrentLoc] 
  StartDate <- as.POSIXct(parDateIndex, format = "%Y-%m-%d %Z", tz = "UTC")
  EndDate <- as.POSIXct(floor_date(Sys.Date(), unit = "month"), tz = "UTC")
  DatesLotLoc <- seq(StartDate,EndDate,"month") %>% as.data.frame()  
  names(DatesLotLoc)[1] <- "Dates"

  parCurrentLotLoc <- paste(parCurrentLot,parCurrentLoc,sep="_")
  DatesLotLoc <- DatesLotLoc %>% mutate(Day= as.numeric(as.POSIXct(Dates)))
  DatesLotLoc <- DatesLotLoc %>% mutate(Lot= parCurrentLot)
  DatesLotLoc <- DatesLotLoc %>% mutate(Loc= parCurrentLoc)
  DatesLotLoc <- DatesLotLoc %>% mutate(LotLoc= parCurrentLotLoc)
  
  
  DatesLotLoc <- left_join(DatesLotLoc,AllDates, by=c("Dates"="Date") )
  DatesLotLoc$LoyerdeBase <- AllLoyers$Loyer[AllLoyers$LotLoc == parCurrentLotLoc]
  DatesLotLoc$IndexduBail <- AllLoyers$Index[AllLoyers$LotLoc == parCurrentLotLoc]
  DatesLotLoc <- DatesLotLoc %>% mutate(LoyerDTD = LoyerdeBase*Index/IndexduBail )
  DatesLotLoc$MoisIndex <- AllLoyers$MoisIndex[AllLoyers$LotLoc==parCurrentLotLoc]
  DatesLotLoc$AnIndex <- AllLoyers$AnIndex[AllLoyers$LotLoc==parCurrentLotLoc]
  DatesLotLoc <- DatesLotLoc %>% mutate(Anniv = ifelse(Mois == as.integer(format(AllLoyers$Date4CalculNouveauLoyer, "%m")),TRUE,FALSE))
  #DatesLotLoc <- DatesLotLoc %>% mutate(LoyerLegal = ifelse(Anniv,LoyerDTD,NA))
  
  #DatesLotLoc$LoyerLegal <- na.locf(DatesLotLoc$LoyerLegal)
  
  return(DatesLotLoc)
}

