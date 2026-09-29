jcbGetHCCAll<-function(parSourceDir){ #(parDF,parLag){
  
  csv_HCC_2018<-gsub(" ","",paste(parSourceDir,"/Decompte_2018.csv"))
  csv_HCC_2019<-gsub(" ","",paste(parSourceDir,"/Decompte_2019.csv"))
  csv_HCC_2020<-gsub(" ","",paste(parSourceDir,"/HCC_2020.csv"))
  #
  HCC_2020 <- read.csv(csv_HCC_2020,header=TRUE,sep=';')
  HCC_2018 <- read.csv(csv_HCC_2018,header=TRUE,sep='*')
  HCC_2019 <- read.csv(csv_HCC_2019,header=TRUE,sep='*')
  # correction d' une erreur d' affectation signalée et corrigée par Limmo mais non reflètée dans les fichiers source
  HCC_2019$Proprio<-ifelse(HCC_2019$Proprio=="DE RUDDER Alexandra" & HCC_2019$Montant==7348,"BAR - BORN",paste(HCC_2019$Proprio))
  
  HCC_All <- rbind(HCC_2020, HCC_2019)
  HCC_All <- rbind(HCC_All,HCC_2018)
  #
  HCC_All$Proprio <-gsub(" ", "",HCC_All$Proprio, fixed = TRUE)
  # chercher les entrées d' AD
  HCC_All$Four <- grepl("^[A][D]", HCC_All$Proprio) 
  # et les remplacer par ADREA
  HCC_All$Proprio <- ifelse(HCC_All$Four, as.character("ADREA"), as.character(HCC_All$Proprio))
  HCC_All$Pro<-substr(HCC_All$Proprio,1,3)
  HCC_All$Date <-  as.Date(HCC_All$Date,'%d/%m/%Y')
  HCC_All$AnReel<-as.numeric(format(HCC_All$Date,'%Y'))
  #HCC_All$MontantE<-HCC_All$Montant/100
  xxx<-HCC_All
  a<-ls(pattern="HCC")
  #rm(list=a)
  
return(xxx)
}
