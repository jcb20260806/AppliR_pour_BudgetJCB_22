jcbGetHCCSPA<-function(){ #(parDF,parLag){
csv_HCC_SoldesPA_2020="/mnt/NewINPG/2021-04-xx-Comptes_CoPro/Comptes_2021-Exercice_2020/py-HCC_All_2020/HCC_All_SoldesPA_2020.csv"
HCC_SPA_2020 <- read.csv(csv_HCC_SoldesPA_2020,header=TRUE,sep=';')

HCC_SPA_2020$Four <- grepl("^[A][D]", HCC_SPA_2020$Proprio) # chercher les entrées d' AD
# et les remplacer par ADREA
HCC_SPA_2020$Proprio <- ifelse(HCC_SPA_2020$Four, as.character("ADREA"), as.character(HCC_SPA_2020$Proprio))

HCC_SPA_2020$Date<-   as.Date(HCC_SPA_2020$Date)
HCC_SPA_2020$MontantE<-HCC_SPA_2020$Montant/100


return(HCC_SPA_2020)
}
