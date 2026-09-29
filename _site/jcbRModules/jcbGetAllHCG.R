jcbGetHCGAll<-function(parSourceDir){ #(parDF,parLag){
  
  
  csv_HCG_2018=gsub(" ","",paste(parSourceDir,"/HCG_2018.csv"))
  csv_HCG_2019=gsub(" ","",paste(parSourceDir,"/HCG_2019.csv"))
  csv_HCG_2020=gsub(" ","",paste(parSourceDir,"/HCG_2020.csv"))
  #
  HCG_2020 <- read.csv(csv_HCG_2020,header=TRUE,sep=';')
  HCG_2018 <- read.csv(csv_HCG_2018,header=TRUE,sep=';')
  HCG_2019 <- read.csv(csv_HCG_2019,header=TRUE,sep=';')
  # Corriger Nettoyage 2019
  #HCG_2019$Code<-ifelse(HCG_2019$Tiers!="NETTOYAG",as.character(HCG_2019$Code),"A01")
  #HCG_2019$Code.Tiers<-ifelse(HCG_2019$Tiers!="NETTOYAG",HCG_2019$Code.tiers,HCG_2019$Code.Lib)
  
  
  
  
  HCG_All <- rbind(HCG_2020, HCG_2019)
  HCG_All <- rbind(HCG_All,HCG_2018)
  #
  HCG_All$Date <-  as.Date(HCG_All$Date,'%d/%m/%Y')
  # nnn HCG_All$AnReel<-as.numeric(format(HCG_All$Date,'%Y'))
  HCG_All$MontantE<-HCG_All$Montant/100
  # Add Codes Comptables
  HCG_All$Tiers<-as.character(HCG_All$Tiers)
  
  xxx<-HCG_All
  a<-ls(pattern="HCG")
  #rm(list=a)
  
return(xxx)
}
