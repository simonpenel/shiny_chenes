# Masting data
options(encoding="latin1")

masting <- read.csv("merged.csv",sep=";")
print(masting)

mean_var <- function(x, var) {
    mean(masting[masting$Arbre == x,][[var]])
    }

get_summary <- function(m){
  sum_mast  <- data.frame(m$Arbre,m$Latitude,m$Longitude)
  sum_mast <- sum_mast[!duplicated(sum_mast), ]
  print(sum_mast)
  arbres <- sum_mast$m.Arbre
  print(arbres)

  test = lapply(arbres,mean_var, var="tauxfructif")
  print(test)
#test2 = lapply(arbres,mean_var, var="Total_Fruits_per_m2")
# print(test2)
  sum_mast$meantauxfructif = test
  sum_mast

}

toto <- get_summary(masting)
print(toto)
