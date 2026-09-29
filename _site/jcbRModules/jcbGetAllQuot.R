jcbGetQuotAll<-function(parSourceDir){ #(parDF,parLag){
  
  csv_Quot_2018<-gsub(" ","",paste(parSourceDir,"/Quot_2018.csv"))
  csv_Quot_2019<-gsub(" ","",paste(parSourceDir,"/Quot_2019.csv"))
  csv_Quot_2020<-gsub(" ","",paste(parSourceDir,"/Quot_2020.csv"))
  #
  Quot_2020 <- read.csv(csv_Quot_2020,header=TRUE,sep='*')
  Quot_2018 <- read.csv(csv_Quot_2018,header=TRUE,sep='*')
  Quot_2019 <- read.csv(csv_Quot_2019,header=TRUE,sep='*')
  
  Quot_All <- rbind(Quot_2020, Quot_2019)
  Quot_All <- rbind(Quot_All,Quot_2018)
  #
  Quot_All$Proprio <-gsub(" ", "",Quot_All$Proprio, fixed = TRUE)
  # chercher les entrées d' AD
  Quot_All$Four <- grepl("^[A][D]", Quot_All$Proprio) 
  # et les remplacer par ADREA
  Quot_All$Proprio <- ifelse(Quot_All$Four, as.character("ADREA"), as.character(Quot_All$Proprio))
  Quot_All$Pro<-substr(Quot_All$Proprio,1,3)
  #Quot_All$AnReel<-as.numeric(format(Quot_All$Date,'%Y'))
  #Quot_All$MontantE<-Quot_All$Montant/100
  xxx<-Quot_All
  a<-ls(pattern="Quot")
  #rm(list=a)
  
return(xxx)
}
