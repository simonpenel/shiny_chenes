# Masting data
options(encoding="latin1")

masting <- read.csv("merged.csv",sep=";")


# Fonction qui renvoie une df avec la valeur moyenne sur les arbres par site/an
# ------------------------------------------------------------------------------
get_summary_site <- function(m){
  sum_site  <- data.frame(Site=m$Site,Year=m$Year,Moyenne_sur_les_arbres=0)
  sum_site <- unique(sum_site)
  sites <- unique(sum_site$Site)
  test <- lapply(sites,extract_site,data=m,sum_site=sum_site)
  test
}

# Fonction qui calcule la valeur moyenne pour une annee donnee
# ------------------------------------------------------------
mean_over_trees <- function(year,data,var){
  taux = data[data$Year == year ,][[var]]
  mean_taux <- mean(taux)
  mean_taux
}

# Fonction qui recupere les donnes pour un site, et renvoie une df
# avec les valeurs moyenne d'une variable sur les arbres par annee
# -----------------------------------------------------------------
extract_site <- function(site,data,sum_site) {
  test <- data[data$Site == site ,]
  years <- unique(test$Year)
  mean_year <- sapply(years,mean_over_trees,data=test,var="Total_Fruits_per_m2")
  sum_site[sum_site$Site == site,]$Moyenne_sur_les_arbres = mean_year
  sum_site_val  = sum_site[sum_site$Site == site,]
  sum_site_val
}

# Fonction d'affichage
# --------------------
plot_site_years_var <- function(df,var, main, ylab, type = "b", pch = 19,
xlab = "Year", leg = TRUE, posleg = "topleft", ...){
  nt   <- length(df)
  col <- hcl.colors(nt, "Dark 2")
  type <- "b"
  pch <-19

  # Recherche des maxs
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

  liste_sites <- c(unique(site_data$Site))

  for(j in 2:nt) {
    site_data <-df[[j]]
    site_data <-site_data[order(site_data$Year),,drop=FALSE]
    points(site_data$Year, site_data[[var]], type = type, pch = pch, col = col[j])
    liste_sites <- append(liste_sites, unique(site_data$Site))
  }
  legend("topleft", legend=liste_sites,box.lty=0,
       col=col, lty=1, cex=0.8)
}

toto <- get_summary_site(masting)
plot_site_years_var(toto,"Moyenne_sur_les_arbres", "Nb de fruit par m2 (moyenne)", "Nb de fruit par m2")
