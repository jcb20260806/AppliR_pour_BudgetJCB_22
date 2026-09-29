jcbGetHCCAllBar<-function(parSourceDir){
csv_HCC_All_Bar=gsub(" ","",paste(parSourceDir,"/HCC_BAR_2020.csv"))
HCC_All_Bar <- read.csv(csv_HCC_All_Bar,header=TRUE,sep=';')
  #
  HCC_All_Bar$Proprio <-gsub(" ", "",HCC_All_Bar$Proprio, fixed = TRUE)
  HCC_All_Bar$Pro<-substr(HCC_All_Bar$Proprio,1,3)
  HCC_All_Bar$Date <-  as.Date(HCC_All_Bar$Date,'%d/%m/%Y')
  HCC_All_Bar$AnReel<-as.numeric(format(HCC_All_Bar$Date,'%Y'))
  HCC_All_Bar$MontantE<-HCC_All_Bar$Montant/100
  
  
  
return(HCC_All_Bar)
}
