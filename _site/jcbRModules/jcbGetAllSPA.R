jcbGetSPAAll<-function(parSourceDir){ #(parDF,parLag){
  
  csv_SPA_2018<-gsub(" ","",paste(parSourceDir,"/SPA_2018.csv"))
  csv_SPA_2019<-gsub(" ","",paste(parSourceDir,"/SPA_2019.csv"))
  csv_SPA_2020<-gsub(" ","",paste(parSourceDir,"/SPA_2020.csv"))
  #
  SPA_2020 <- read.csv(csv_SPA_2020,header=TRUE,sep='*')
  SPA_2018 <- read.csv(csv_SPA_2018,header=TRUE,sep='*')
  SPA_2019 <- read.csv(csv_SPA_2019,header=TRUE,sep='*')
  # correction d' une erreur d' affectation signalée et corrigée par Limmo mais non reflètée dans les fichiers source
    
  SPA_All <- rbind(SPA_2020, SPA_2019)
  SPA_All <- rbind(SPA_All,SPA_2018)
  #
  SPA_All$Proprio <-gsub(" ", "",SPA_All$Proprio, fixed = TRUE)
  # chercher les entrées d' AD
  SPA_All$Four <- grepl("^[A][D]", SPA_All$Proprio) 
  # et les remplacer par ADREA
  #SPA_All$Proprio <- ifelse(SPA_All$Four, as.character("ADREA"), as.character(SPA_All$Proprio))
  SPA_All$Pro<-substr(SPA_All$Proprio,1,3)
  SPA_All$Date <-  as.Date(SPA_All$Date,'%d/%m/%Y')
  SPA_All$AnReel<-as.numeric(format(SPA_All$Date,'%Y'))
  #SPA_All$MontantE<-SPA_All$Montant/100
  xxx<-SPA_All
  a<-ls(pattern="SPA")
  #rm(list=a)
  
return(xxx)
}
