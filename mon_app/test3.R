# Masting data
options(encoding="latin1")

masting <- read.csv("merged.csv",sep=";")



get_summary_2 <- function(m){
  sum_site  <- data.frame(Site=m$Site,Year=m$Year,Taux_moyen_arbres=0)
  sum_site <- unique(sum_site)
  #print(sum_site)
  sites <- unique(sum_site$Site)
  test <- lapply(sites,extract_site,data=m,sum_site=sum_site)
  print(test)
  #sum_site$Taux_moyen_arbres = test
  #print(sum_site)
  test
}
mean_over_trees <- function(year,data,sum){
taux = data[data$Year == year ,]
site = unique(data$Site)
#print(paste(c("Sites : ",data$Site)))
#print(paste(c("Site : ",site)))
lol <-sum[sum$Site == site,]
#print(lol)
print(paste(c("Site = ",site)))
print(paste(c("Year = ",year)))
#print(taux)
taux = data[data$Year == year ,]$tauxfructif
print(taux)
mean_taux <- mean(taux)
print("MEAN TAUX")
print(mean_taux)
#lol$mean_taux_over_trees = mean_taux
#print(lol[lol$Year == year ,])
#lol[lol$Year == year ,]$mean_taux_over_trees = mean_taux
#sum[sum$Site == site,]$mean_taux_over_trees = mean_taux
#sum[sum$Year == year,]$mean_taux_over_trees = mean_taux
print("LOLOLO")
#print(lol)
mean_taux

}
extract_site <- function(site,data,sum_site) {
print("")
print("")
print("")
print(paste(c("LOOP : SITE ",site)))
#print(x)
test <- data[data$Site == site ,]
years <- unique(test$Year)
#print(years)
mean_year <- sapply(years,mean_over_trees,data=test,sum=sum_site)
print(years)
print(mean_year)
sum_site[sum_site$Site == site,]$Taux_moyen_arbres = mean_year
print(sum_site[sum_site$Site == site,])
sum_site_val  = sum_site[sum_site$Site == site,]
#print(sum_site)
#sum_site[sum_site$Site == x,]$Taux_moyen_arbres = mean_year
#sum_site
sum_site_val
}
toto <- get_summary_2(masting)
print(toto)
