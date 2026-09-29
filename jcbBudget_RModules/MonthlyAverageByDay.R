MonthlyAveByDay<-function(InDF,Serie,ListVal,Filter,Divide){
  # Calcul de la rentrée mensuelle moyen sur 366 jours
  # delta de running solde sur 366jours divisé par 12
  # cette fonction crée un dataframe avec une série de records ( ListCol ) et jour par jour
  
  InDF<-filter(InDF,PBM %in% ListVal )
  InDF<-aggregate(InDF$MontantE, by=list(DayFE=InDF$DayFE), FUN=sum) # aggregate by DAy"
  
  names(InDF)[names(InDF) == 'x'] <- 'MontantE'
  InDF$MontantE<-round(InDF$MontantE,digit=2)
  write.csv(InDF, "./csv/InDFA.csv", row.names=TRUE)
  MvtByDay <- sqldf("SELECT * FROM Serie LEFT JOIN InDF on Serie.DayFE=InDF.DayFE")
  MvtByDay <- MvtByDay[ -c(3) ]
  MvtByDay$Date<-as.character(as.Date(as.POSIXct(MvtByDay$DayFE*86400L, origin="1970-01-01"),format="%Y-%m-%d"))
  write.csv(MvtByDay, "./csv/InDFB.csv", row.names=TRUE)
  
  
  
  MvtByDay$YearM1<-as.numeric(substr(MvtByDay$Date, 0,4))-1
  MvtByDay$DateM1<-paste(MvtByDay$YearM1,substr(MvtByDay$Date, 5,str_length(MvtByDay$Date)),sep='')
  MvtByDay$DayFEM1<-as.integer(as.Date(MvtByDay$DateM1,format=	"%Y-%m-%d"))
  write.csv(MvtByDay, "./csv/InDFC.csv", row.names=TRUE)
  
  #glimpse(MvtByDay)
  #MvtByDay <- subset( MvtByDay, select = -c(YearM1,	DateM1))
  MvtByDay <- MvtByDay[ -c(4,5) ]
  MvtByDay$MontantE[is.na(MvtByDay$MontantE)]<-0
  MvtByDay[,"RunSolde"] <- cumsum(MvtByDay$MontantE)
  write.csv(MvtByDay, "./csv/MvtByDayA.csv", row.names=TRUE)
  MvtByDay2<-MvtByDay
  MDB <- sqldf("SELECT MvtByDay.DayFE,MvtByDay.Date,MvtByDay.DayFEM1,MvtByDay.RunSolde as RSolde,
                       MvtByDay2.RunSolde as RSoldeM1
                       FROM MvtByDay Left JOIN MvtByDay2 on MvtByDay.DayFEM1=MvtByDay2.DayFE")
  #write.csv(MDB, "./csv/MDB1.csv", row.names=TRUE)
  #glimpse(MDB)
  #MDB<-MDB %>% fill(RSoldeM1)
  
 
  
  write.csv(MDB, "./csv/MDBA.csv", row.names=TRUE)
  
  #MDB<-MDB %>% fill(YearM1)%>% fill(RunSolde1)
  MDB$DeltaSolde<-round((MDB$RSolde-MDB$RSoldeM1)/Divide,2)
  MDB$MoisAbs<-unlist(lapply(MDB$Date,CalcMoisAbs))
  mod=loess(MDB$DeltaSolde~MDB$DayFE)
  MDB$DeltaSoldeSmooth<-round(predict(mod, newdata=MDB$DayFE),2)
  write.csv(MDB, "./csv/MDBB.csv", row.names=TRUE)
  MDB <- MDB[ -c(3,4,5) ]
  
  write.csv(MDB, "./csv/MDBC.csv", row.names=TRUE)
 
  return (MDB)
  
}