# Cette fonction ajoute un data frame LotLoc au Dataframe qui va servir à dessiner le plot
Merge2LotLoc <- function ( Target, Source ){
  Target <- left_join(Target,Source,by =c("Day"="Day"))
  Target <-Target %>% mutate(LoyerDTD = ifelse(is.na(LoyerDTD.y),LoyerDTD.x,LoyerDTD.y))
  Target <-Target %>% mutate(LoyerdeBase = ifelse(is.na(LoyerdeBase.y),LoyerdeBase.x,LoyerdeBase.y))
  Target <-Target %>% mutate(LoyerLegal = ifelse(is.na(LoyerLegal.y),LoyerLegal.x,LoyerLegal.y))
  Target <-Target %>% mutate(Anniv = ifelse(is.na(Anniv.y),Anniv.x,Anniv.y))
  Target <-Target %>% mutate(Loc = ifelse(is.na(Loc.y),Loc.x,Loc.y))
  Target <-Target %>% mutate(Lot = ifelse(is.na(Lot.y),Lot.x,Lot.y))
  Target <-Target %>% select(Day,LoyerDTD,LoyerdeBase,Anniv,LoyerLegal,Loc,Lot)
  return(Target)
}