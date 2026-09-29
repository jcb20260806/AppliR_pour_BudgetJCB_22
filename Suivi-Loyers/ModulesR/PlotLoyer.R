# cette fonction dessine le plot Loyer
# Parwhat est le dataframe qui a été préparé
PlotLoyer<-function(parWhat){
  
  
  parUnit<-"Euros/mois"
  parTitle<-paste("Loyer ",head(parWhat,1)$Lot,seq="")
  # on crée le plot x = Day!
  P2<-ggplot(parWhat, aes(x=as.Date(Date), label = Date))
  P2<-P2+ggtitle("Evolution Loyer",subtitle = parTitle)
  P2 <- P2 + xlab("Dates")
  P2<-P2+scale_y_continuous(name=parUnit)
  # Add tous les Loyers
  P2<-P2+geom_line(aes(y = LoyerDTD,color="LoyerDTD"))
  P2<-P2+geom_line(aes(y = LoyerdeBase,color="LoyerdeBase"))
  P2<-P2+geom_line(aes(y = LoyerLegal,color="LoyerLegal"))
  P2 <- P2 + scale_x_date(
    date_breaks = "6 months",        # Une graduation par mois
    date_labels = "%b %Y"           # Format : "Jan 2023", "Fév 2023", etc.
  )
  P2 <- P2 + theme( axis.text.x = element_text(angle = 45, hjust = 1))
  # On ajoute les labels
  ListOfLocXs <- parWhat %>% group_by(Loc) %>% summarise(PosX=as.Date(first(Date)))
  ListOfLocYs <- parWhat %>% group_by(Loc) %>% summarise(PosY=first(LoyerdeBase))
  # print(nrow(ListOfLocXs))
  for ( i in 1:nrow(ListOfLocXs)){
    P2 <- P2 + annotate("text", x =ListOfLocXs$PosX[i]+10, y = ListOfLocYs$PosY[i], label = ListOfLocXs$Loc[i], size = 5, color = "red")
  }
 
  return (P2)}  
