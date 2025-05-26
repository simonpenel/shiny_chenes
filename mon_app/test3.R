# Masting data
options(encoding="latin1")

masting <- read.csv("merged.csv",sep=";")



get_summary_2 <- function(m){
  sum_site  <- data.frame(Site=m$Site,Year=m$Year,Taux_moyen_arbres=0)
  sum_site <- unique(sum_site)
  print(sum_site)
  sites <- sum_site$Site
  test <- sapply(sites,extract_site,data=m,sum_site=sum_site)
  #print(test)
  #sum_site$Taux_moyen_arbres = test
  #print(sum_site)
}
mean_over_trees <- function(year,data,sum){
taux = data[data$Year == year ,]
site = unique(data$Site)
print("Site")
lol <-sum[sum$Site == site,]
print("LOL")
print("LOL")
print(lol)
print(site)
#print(taux)
taux = data[data$Year == year ,]$tauxfructif
print(taux)
mean_taux <- mean(taux)
lol$mean_taux_over_trees = mean_taux
#sum[sum$Site == site,]$mean_taux_over_trees = mean_taux
#sum[sum$Year == year,]$mean_taux_over_trees = mean_taux
print("LOLOLO")
print(lol)
mean_taux

}
extract_site <- function(site,data,sum_site) {
#print("test")
#print(x)
test <- data[data$Site == site ,]
years <- unique(test$Year)
#print(years)
mean_year <- sapply(years,mean_over_trees,data=test,sum=sum_site)
#print(mean_year)
#sum_site[sum_site$Site == site,]$Taux_moyen_arbres = mean_year
#sum_site[sum_site$Site == x,]$Taux_moyen_arbres = mean_year
#sum_site
mean_year
}
toto <- get_summary_2(masting)
#print(toto)
