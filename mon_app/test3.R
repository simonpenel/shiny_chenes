# Masting data
options(encoding="latin1")

masting <- read.csv("merged.csv",sep=";")



get_summary_2 <- function(m){
  sum_site  <- data.frame(Site=m$Site,Year=m$Year,Taux_moyen_arbres=0)
  sum_site <- unique(sum_site)
  #print(sum_site)
  sites <- unique(sum_site$Site)
  test <- lapply(sites,extract_site,data=m,sum_site=sum_site)
  #print(test)
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
#print(paste(c("Site = ",site)))
#print(paste(c("Year = ",year)))
#print(taux)
taux = data[data$Year == year ,]$Total_Fruits_per_m2
#print(taux)
mean_taux <- mean(taux)
print("MEAN TAUX")
#print(mean_taux)
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
#print(years)
#print(mean_year)
sum_site[sum_site$Site == site,]$Taux_moyen_arbres = mean_year
#print(sum_site[sum_site$Site == site,])
sum_site_val  = sum_site[sum_site$Site == site,]
#print(sum_site)
#sum_site[sum_site$Site == x,]$Taux_moyen_arbres = mean_year
#sum_site
sum_site_val
}

plot_site_years_var <- function(df,var, main, ylab, type = "b", pch = 19,
xlab = "Year", leg = TRUE, posleg = "topleft", ...){
  nt   <- length(df)
  col <- hcl.colors(nt, "Dark 2")
  type <- "b"
  pch <-19

  #Recherche des maxs
  site_data <-df[[1]]
  ymax_all <- max(site_data[[var]])
  xmax_all <- max(site_data$Year)
  xmin_all  <- min(site_data$Year)
  for(j in 2:nt) {
    site_data <-df[[j]]
    ymax <- max(site_data[[var]])
    xmax <- max(site_data$Year)
    xmin <- min(site_data$Year)
    ymax_all <- max(c(ymax,ymax_all))
    xmax_all <- max(c(xmax,xmax_all))
    xmin_all  <- min(c(xmin,xmin_all))
  }


  site_data <-df[[1]]
  site_data <-site_data[order(site_data$Year),,drop=FALSE]
  ylim <- c(0,ymax_all)
  xlim <- c(xmin_all - 3, xmax_all + 1)
  plot(site_data$Year, site_data[[var]], type = type, pch = pch, col = col[1],
   xlim=xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = main,
   xaxp=c(xmin_all,xmax_all,xmax_all - xmin_all))
  print("SITE = ")
  print(unique(site_data$Site))
  liste_sites <- c(unique(site_data$Site))

  for(j in 2:nt) {
    site_data <-df[[j]]
    site_data <-site_data[order(site_data$Year),,drop=FALSE]
    points(site_data$Year, site_data[[var]], type = type, pch = pch, col = col[j])
    liste_sites <- append(liste_sites, unique(site_data$Site))
  }
  print(unique(df[[1]]$Site))
  legend("topleft", legend=liste_sites,box.lty=0,
       col=col, lty=1, cex=0.8)
}


toto <- get_summary_2(masting)
plot_site_years_var(toto,"Taux_moyen_arbres", "Nb de fruit par m2 (moyenne)", "Nb de fruit par m2")

#print(toto[1])
#print(toto[2])

#plot_site_years()

#site_data <-toto[[1]]
#site_data <-site_data[order(site_data$Year),,drop=FALSE]

#print(site_data)
#nt   <- length(toto)
#col <- hcl.colors(nt, "Dark 2")
#type <- "b"
#pch <-19
#ylim <- c(0, max(site_data$Taux_moyen_arbres))
#xlim <- c(min(site_data$Year)-1, max(site_data$Year)+1)
#plot(site_data$Year, site_data$Taux_moyen_arbres, type = type, pch = pch, col = col[1], xlim=xlim, ylim = ylim, xlab = "lol", ylab = "lol", main = "lol")
#plot(site_data$Year, site_data$Taux_moyen_arbres, type = type, pch = pch, col = col[1])

#for(j in 2:nt) {
#site_data <-toto[[j]]
#site_data <-site_data[order(site_data$Year),,drop=FALSE]
#points(site_data$Year, site_data$Taux_moyen_arbres, type = type, pch = pch, col = col[j])
#}
