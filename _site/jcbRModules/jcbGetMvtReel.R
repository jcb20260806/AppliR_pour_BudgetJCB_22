library(DBI)
library(RSQLite)
jcbGetMvtReel<-function(){ #(parDF,parLag){
  
myDB<-"/mnt/INPG0721/0-P3-Budget/Budget-DataBase-Paral/BudgetJCB/db/BudgetJCBDB.sql"
# Get Mouvements de l' année
conn <- dbConnect(drv = SQLite(), dbname= myDB)
#LT<-dbListTables(conn)
#dbListFields(conn,"MvtReel")
#dbGetQuery(conn,"SELECT count(*) FROM MvtReel")
Query2='SELECT MvtReel.*, Types.*, Types.TYPE AS TType, Types.LIBELLE AS TLib FROM MvtReel LEFT OUTER JOIN Types ON Types.TYPE = MvtReel.TYPE ORDER BY NMvt ASC'
MvtReelAll<-dbGetQuery(conn,Query2)
# Historique des Comptes Clients
# unformiser les Noms de Colonne"
MvtReelAll$DateMvt<-   as.Date(MvtReelAll$DateMvt,"%d/%m/%Y")
MvtReelAll$DateVal<-   as.Date(MvtReelAll$DateVal,"%d/%m/%Y")

names (MvtReelAll)[4] <- "Date"
names (MvtReelAll)[2] <- "Montant"
MvtReelAll$MontantE<-MvtReelAll$Montant/100

dbDisconnect(conn)
return (MvtReelAll)
}
