# "Source" est le lien vers la db budget
# Get MvtReelAll and clean
GetandCleanMvtReelAll<-function(Source){
  
  
  conn <- dbConnect(drv = SQLite(), dbname= Source)
  Query2='SELECT MvtReel.*, Types.PBM AS PBM, Types.TYPE AS TType, Types.LIBELLE AS TLib, Types.Prd as Prd FROM MvtReel LEFT OUTER JOIN Types ON Types.TYPE = MvtReel.TYPE ORDER BY NMvt ASC'
  MvtReelAll<-dbGetQuery(conn,Query2)
  # drop colonne en double
  #MvtReelAll <- MvtReelAll[ -c(18) ] # Type
  " Add Montant en Euro"
  MvtReelAll$MontantE<-MvtReelAll$MONTANT/100
  # drop colonnes inutiles
  #jcbMvtReelAll<-select(MvtReelAll,-c(MONTANT,Libelle,NextNew,MonBEF,Contrepartie,IBAN,Libelle2,  Libelle3,LL2,LL3,TType,LIBELLE,TLib)  )       
  # j' utlise DateVal
  # unformiser les Noms de Colonne"
  MvtReelAll$Date<-as.Date(MvtReelAll$DateVal,"%d/%m/%Y")
  #MvtReelAll$TLib<-paste(MvtReelAll$TType,"-",MvtReelAll$TLib,sep="")
  # Add Day From Epoch
  MvtReelAll$DayFE<- as.integer(as.Date(MvtReelAll$Date,format=	"%Y-%m-%d")) # make day from epoch"
  # rename
  names(MvtReelAll)[names(MvtReelAll)=="TYPE"]<-"Type"
   
  
  dbDisconnect(conn)
  return(MvtReelAll)
}