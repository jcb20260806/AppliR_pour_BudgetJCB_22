# Get CSV Suivi Loyer, echo=FALSE,eval=TRUE,message=FALSE,warning=FALSE}
#FichierLoyer <- "./Suivi_Loyers.xlsx"
library(readxl)
GetLoyer <- function(FichierLoyer){

AllLoyer<-read_excel(FichierLoyer) %>% mutate(LotLoc = paste(Lot,Locataire,sep='_'))
AllLoyer <- AllLoyer %>% mutate(Index = as.numeric(Index))
AllLoyer$MoisDébut <- month(ymd(AllLoyer$DateDébut))
AllLoyer$AnDébut <- year(ymd(AllLoyer$DateDébut))
AllLoyer$Date4CalculNouveauLoyer <- floor_date(AllLoyer$DateDébut, "month") - months(1)
AllLoyer$Mois4CalculNouveauLoyer <- month(ymd(AllLoyer$Date4CalculNouveauLoyer))
AllLoyer$An4CalculNouveauLoyer <- year(ymd(AllLoyer$Date4CalculNouveauLoyer))
return ( AllLoyer)
}
#g <- getcwd()
#Test <- GetLoyer("./Suivi_Loyers.xlsx")
                         
