jcbGetDecAll<-function(parSourceDir){ #(parDF,parLag){
  
  csv_Dec_2018<-gsub(" ","",paste(parSourceDir,"/Decompte_2018.csv"))
  csv_Dec_2019<-gsub(" ","",paste(parSourceDir,"/Decompte_2019.csv"))
  csv_Dec_2020<-gsub(" ","",paste(parSourceDir,"/Decompte_2020.csv"))
  #
  Dec_2020 <- read.csv(csv_Dec_2020,header=TRUE,sep='*')
  Dec_2018 <- read.csv(csv_Dec_2018,header=TRUE,sep='*')
  Dec_2019 <- read.csv(csv_Dec_2019,header=TRUE,sep='*')
  # correction d' une erreur d' affectation signalée et corrigée par Limmo mais non reflètée dans les fichiers source
  Dec_2019$Proprio<-ifelse(Dec_2019$Proprio=="DE RUDDER Alexandra" & Dec_2019$Montant==7348,"BAR - BORN",paste(Dec_2019$Proprio))
  
  Dec_All <- rbind(Dec_2020, Dec_2019)
  Dec_All <- rbind(Dec_All,Dec_2018)
  #
  Dec_All$Proprio <-gsub(" ", "",Dec_All$Proprio, fixed = TRUE)
  # chercher les entrées d' AD
  Dec_All$Four <- grepl("^[A][D]", Dec_All$Proprio) 
  # et les remplacer par ADREA
  Dec_All$Proprio <- ifelse(Dec_All$Four, as.character("ADREA"), as.character(Dec_All$Proprio))
  Dec_All$Pro<-substr(Dec_All$Proprio,1,3)
  Dec_All$Date <-  as.Date(Dec_All$Date,'%d/%m/%Y')
  Dec_All$AnReel<-as.numeric(format(Dec_All$Date,'%Y'))
  #Dec_All$MontantE<-Dec_All$Montant/100
  xxx<-Dec_All
  a<-ls(pattern="Dec")
  #rm(list=a)
  
return(xxx)
}
