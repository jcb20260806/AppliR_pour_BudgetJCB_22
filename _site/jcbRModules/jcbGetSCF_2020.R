jcbGetSCF2020<-function(parSourceDir){ #(parDF,parLag){
  
  
  
  csv_SCF_2020=gsub(" ","",paste(parSourceDir,"/SCF_2020.csv"))
  #
  SCF_2020 <- read.csv(csv_SCF_2020,header=TRUE,sep=';')
  
 
  
  # Add Codes Comptables
  
  
  
return(SCF_2020)
}
