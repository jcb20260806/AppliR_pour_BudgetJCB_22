jcbGetDictOfTiers<-function(parFile){ #(parDF,parLag){
library("rjson")
# Give the input file name to the function.
result <- fromJSON(file = parFile)
df <- data.frame(matrix(unlist(result), ncol = max(lengths(result)), byrow = TRUE))
json_data_frame <- as.data.frame(result)
names(df)[1]<-"Code"
names(df)[2]<-"CodeLib"
names(df)[3]<-"Tiers"
names(df)[4]<-"Code-Tiers"
# Print the result.
#print(df)
}
