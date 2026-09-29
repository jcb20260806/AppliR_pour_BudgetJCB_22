#Routines pour Gérer MoisAbs
CalcMoisAbs<-function(parC){
  # Calcul MoisAbs à partir d' une date en format yyyy*mm*dd
  #print(parC)
  # on se base sur  la date valeur
  b1<-as.numeric(substr(parC,0,4))-1980
  b2<-as.numeric(substr(parC,6,7))
  return (b1*12+b2)
}
FromMoisAbsToSDate<-function(MoisAbs){
  # Routine pour convertir MoisAbs en date sous la for mm-yy 
  MoisAbs<-MoisAbs-1
  Y<-MoisAbs%/%12+1980
  M<-MoisAbs%%12+1
  M1<-paste("0",as.character(M),sep="")
  MC<-str_sub(M1,-2,-1)
  SDate<-paste(as.character(Y),MC,sep="-")
  return(SDate)
}