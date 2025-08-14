library(shiny)
library(leaflet)

library(RColorBrewer)
library(shinydashboard)

library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(jsonlite)

# Masting data
options(encoding="utf8")

masting <- read.csv("merged.csv",sep=";")

sites <-unique(masting$Site)
print(sites)

#country_boundaries <- read_json("reserves-naturelles-regionales-rnr.geojson")
#country_boundaries <- read_json("Parcs_naturels_regionaux_de_France.geojson")
#country_boundaries <- read_json("onf_forets-publiques.json")

# UI
ui <- bootstrapPage(
  useShinyjs(),

  tags$style(type = "text/css",
    "html, body {width:100%;height:100%}
    #plotyear { background-color: #ddd; opacity: 0.80;}
    #plotsiteyear { background-color: #ddd; opacity: 0.80;}
    #plotsiteyearBAI { background-color: #ddd; opacity: 0.80;}
    #barplot1 {opacity: 0.80;}
    #barplot2 {opacity: 0.80;}"

  ),

  leafletOutput("map", width = "100%", height = "100%"),

  absolutePanel(bottom = 10, left = 10, width = 400,  height = 150,
    class = "panel panel-default", draggable = TRUE, 
    #downloadButton("download"), # supprime pour l'instant
    sliderInput("range", "Année", min(masting$Year), max(masting$Year),
      value = range(masting$Year), step = 1, sep ="", width=600
    ),
  ),

    absolutePanel( class = "panel panel-default",
      draggable = TRUE, bottom = 10, left = 420,
      width = 120, height =  150,
    sliderInput("circle_size", "Taille des cercles", 1, 20,
      value = 5, step = 1, sep ="", width=600
    ),
    ),

    absolutePanel( class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 180, left = 10, bottom = "auto",
      width = 120, height = "auto",

      prettyCheckboxGroup("select_sites", "Sites",
        choices = sites, selected = sites, status = "primary"
      ),

      actionButton("unselect_all", "Supprime tout",
        style = "opacity: .80; color:black;
         background-color: white; border-color: white"
      ),

      actionButton("select_all", "Sélectionne tout",
        style = "opacity: .80; color: black;
        background-color: white; border-color: white"
      ),

    ),


    fixedPanel(bottom = 10, right = 500,
      actionButton("plotBtn1", "Graphes par site",
        style = "opacity: .80; color: #fff; background-color: #a662e3;
        border-color: #a153e5"
      )
    ),

    fixedPanel(bottom = 10, right = 350,
      actionButton("plotBtn2", "Graphe par arbre",
        style = "opacity: .80; color: #fff; background-color: #a662e3;
        border-color: #a153e5"
      )
    ),

    fixedPanel(bottom = 10, right = 180,
      actionButton("barplotBtn1", "Moyenne par arbre",
        style = "opacity: .80; color: #fff; background-color: #a662e3;
        border-color: #a153e5"
      )
    ),

    fixedPanel(bottom = 10, right = 10,
      actionButton("barplotBtn2", "Maximum par arbre",
        style = "opacity: .80; color: #fff; background-color: #a662e3;
        border-color: #a153e5"
      )
    ),



    absolutePanel(id = "plotyear", class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 10, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
      plotOutput("plotPerYear", height = 250),
    ),

    absolutePanel(id = "barplot1", class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 260, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
      plotOutput("plotHisto", height = 250),
    ),


    absolutePanel(id = "barplot2", class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 510, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
      plotOutput("plotHistoMax", height = 250),
    ),

    absolutePanel(id = "plotsiteyear", class = "panel panel-default",
      fixed = TRUE, draggable = TRUE, top = 10, left = "auto",
      right = 350, bottom = "auto", width = 330, height = "auto",
      plotOutput("plotSitePerYear", height = 250),
    ),

    absolutePanel(id = "plotsiteyearBAI", class = "panel panel-default",
      fixed = TRUE, draggable = TRUE, top = 260, left = "auto", right = 350,
      bottom = "auto", width = 330, height = "auto",
      plotOutput("plotSitePerYearBAI", height = 250),
    ),
)


# Fonctions

# Fonction moyenne
# Calcule la moyenne de la variable var pour l'arbre x dans les donnees data 
# ----------------
mean_var <- function(x, var, data) {
  mean(data[data$Arbre == x, ][[var]])
}

# Fonction max
# Calcule le max de la variable var pour l'arbre x dans les donnees data 
# ------------
max_var <- function(x, var, data) {
  max(data[data$Arbre == x, ][[var]])
}

# Crée un df  des valeurs moyennes et maximum sur une  periode donnée:
# (toutes ces valeurs ne seront pas forcement afcichees, a discuter)
# -------------------------------------------------------------------
get_summary <- function(m) {
  sum_mast  <- data.frame(
    Arbre = m$Arbre,
    Latitude = m$Latitude,
    Longitude = m$Longitude
  )
  sum_mast <- unique(sum_mast)  # Vire la redondance des années
  arbres <- sum_mast$Arbre      # Liste des arbres
  # moyenne de tauxfructif sur la periode pour chaque arbre
  test <- sapply(arbres, mean_var, var = "tauxfructif", data = m)
  sum_mast$meantauxfructif <- c(test)
  # moyenne de Total_Flowers_per_m2 sur la periode pour chaque arbre
  test <- sapply(arbres, mean_var, var = "Total_Flowers_per_m2", data = m)
  sum_mast$meanTotal_Flowers_per_m2 <- test
  # moyenne de Total_Fruits_per_m2 sur la periode pour chaque arbre
  test <- sapply(arbres, mean_var, var = "Total_Fruits_per_m2", data = m)
  sum_mast$meanTotal_Fruits_per_m2 <- c(test)
  # max de tauxfructif sur la periode pour chaque arbre
  test <- sapply(arbres, max_var, var = "tauxfructif", data = m)
  sum_mast$maxtauxfructif <- c(test)
  # max de Total_Fruits_per_m2 sur la periode pour chaque arbre
  test <- sapply(arbres, max_var, var = "Total_Fruits_per_m2", data = m)
  sum_mast$maxTotal_Fruits_per_m2 <- c(test)
  # moyenne de BAI sur la periode pour chaque arbre
  test <- sapply(arbres, mean_var, var = "BAI", data = m)
  sum_mast$meanBAI <- c(test)
  sum_mast

}



# Selection les données sur la base de la surface affichee, l'année et le site
select_in_map <- function(input) {
  bounds <- input$map_bounds
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)
  selected_data <- subset(masting,
    Latitude >= latRng[1] & Latitude <= latRng[2] &
    Longitude >= lngRng[1] & Longitude <= lngRng[2] &
    Year >= input$range[1] &
    Year <= input$range[2] &
    Site %in% input$select_sites
  )
  selected_data
}

#mean_years_select_in_map <- function(input) {
#    select_in_map(input)
#}

# Fonction qui renvoie une df avec la valeur moyenne de var sur les arbres par site/an
# ------------------------------------------------------------------------------------
get_summary_site <- function(m, var) {
  sum_site  <- data.frame(
    Site = m$Site,
    Year = m$Year,
    Moyenne_sur_les_arbres = 0
  )
  sum_site <- unique(sum_site)
  sites <- unique(sum_site$Site)
  test <- lapply(sites, extract_site, data = m, sum_site = sum_site, var = var)
  test
}

# Fonction qui renvoie une df avec la position moyenne  par site
# ---------------------------------------------------------------
get_mean_positions_site <- function(m){
  sum_pos_site  <- data.frame(
    Site = m$Site,
    Longitude_moyenne = 0,
    Latitude_moyenne = 0
  )
  sum_pos_site <- unique(sum_pos_site)
  sites <- unique(sum_pos_site$Site)
  test <- lapply(
    sites,
    extract_mean_position,
    data = m,
    mean_positions = sum_pos_site
  )
  test
}

# Fonction qui calcule la valeur moyenne pour une annee donnee
# ------------------------------------------------------------
mean_over_trees <- function(year, data, var) {
  taux <- data[data$Year == year, ][[var]]
  mean_taux <- mean(taux)
  mean_taux
}

# Fonction qui recupere les donnes pour un site, et renvoie une df
# avec les valeurs moyenne d'une variable sur les arbres par annee
# -----------------------------------------------------------------
extract_site <- function(site, data, sum_site, var) {
  test <- data[data$Site == site, ]
  years <- unique(test$Year)
  mean_year <- sapply(years, mean_over_trees, data = test, var = var)
  sum_site[sum_site$Site == site, ]$Moyenne_sur_les_arbres <- mean_year
  sum_site_val  <- sum_site[sum_site$Site == site, ]
  sum_site_val
}


# Fonction qui recupere les donnes pour un site, et renvoie une df
# avec les valeurs moyenne des longitude et latitudes
# -----------------------------------------------------------------
extract_mean_position <- function(site, data, mean_positions) {
  test <- data[data$Site == site, ]
  mean_longitude <- mean(unique(test$Longitude))
  mean_latitude <- mean(unique(test$Latitude))
  mean_positions[mean_positions$Site == site, ]$Longitude_moyenne <- mean_longitude
  mean_positions[mean_positions$Site == site, ]$Latitude_moyenne <- mean_latitude
  mean_positions_val  <- mean_positions[mean_positions$Site == site, ]
  mean_positions_val
}

# Fonction d'affichage
# --------------------

# Graphe des valeurs moyenne par site pour chaque annee
# ------------------------------------------------------
plot_site_years_var <- function(df, var, main,
  ylab, type = "b", pch = 19, xlab = "Year", leg = TRUE, posleg = "topleft", ...
) {
  nt   <- length(df)
  col <- hcl.colors(nt, "Dark 2")
  type <- "b"
  pch <- 19

  # Recherche des maxs
  site_data <- df[[1]]
  ymax_all <- max(site_data[[var]])
  xmax_all <- max(site_data$Year)
  xmin_all <- min(site_data$Year)

  if (nt > 1) {
    for (j in 2:nt) {
      site_data <- df[[j]]
      ymax <- max(site_data[[var]])
      xmax <- max(site_data$Year)
      xmin <- min(site_data$Year)
      ymax_all <- max(c(ymax, ymax_all))
      xmax_all <- max(c(xmax, xmax_all))
      xmin_all  <- min(c(xmin, xmin_all))
    }
  }

  site_data <- df[[1]]
  site_data <- site_data[order(site_data$Year), , drop = FALSE]

  ylim <- c(0, ymax_all)
  xlim <- c(xmin_all - 3, xmax_all + 1)

  plot(site_data$Year, site_data[[var]], type = type, pch = pch, col = col[1],
    xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = main,
    xaxp = c(xmin_all, xmax_all, xmax_all - xmin_all)
  )

  liste_sites <- c(unique(site_data$Site))

  if (nt > 1) {
    for (j in 2:nt) {
      site_data <- df[[j]]
      site_data <- site_data[order(site_data$Year), , drop = FALSE]
      points(site_data$Year, site_data[[var]], type = type, pch = pch, col = col[j])
      liste_sites <- append(liste_sites, unique(site_data$Site))
    }
  }
  legend("topleft", legend = liste_sites, box.lty = 0,
    col = col, lty = 1, cex = 0.8
  )
}


# Graphe des valeurs  par arbre pour chaque annee
# ------------------------------------------------------
plot_years_var <- function(df, var, main, ylab, type = "b", pch = 19,
  xlab = "Year", leg = TRUE, posleg = "topleft", ...
) {
  arbres <- unique(df$Arbre)
  #arbres
  nt   <- length(arbres)
  col <- hcl.colors(nt, "Dark 2")
  arbre <- df[df$Arbre == arbres[1], ]
  ylim <- c(0, max(df[[var]]))
  xlim <- c(min(df$Year) - 1, max(df$Year) + 1)
  plot(arbre$Year, arbre[[var]], type = type, pch = pch, col = col[1], xlim=xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = main, ...)
  for (j in 2:nt) {
    arbre <- df[df$Arbre==arbres[j], ]
    points(arbre$Year, arbre[[var]], type = type, pch = pch, col = col[j])
  }
}

# Histograme
# ----------
# ( pas utilise)
plot_barplot <- function(df, ...){
  par(mar = c(8,4,1,1))
  v_mean <- df$meanTotal_Fruits_per_m2
  v_max <- df$maxTotal_Fruits_per_m2
  meanmax <- matrix(c(v_mean,v_max), nc = 2, byrow = F)

  barplot(df$meanTotal_Fruits_per_m2,names.arg=df$Arbre,las=2 )
}

# Histograme de la variable var pour chaque arbre
# ----------------------------------------------
plot_barplot_var <- function(df, var, main, ...) {
  par(mar = c(8, 4, 1, 1))
  barplot(df[[var]], names.arg = df$Arbre, las = 2, main = main )
}


# SERVER
# ------

server <- function(input, output, session) {


  # boutons pour afficher/cacher les graphes

  observeEvent(input$plotBtn2, {
    toggle("plotyear")
  })

  observeEvent(input$plotBtn1, {
    toggle("plotsiteyear")
    toggle("plotsiteyearBAI")
  })

  observeEvent(input$barplotBtn1, {
    toggle("barplot1")
  })

  observeEvent(input$barplotBtn2, {
    toggle("barplot2")
  })

  # selection des sites

  observeEvent(input$unselect_all, {
    updateCheckboxGroupInput(session, "select_sites", "Sites:", choices = sites)
  })

  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "select_sites", "Sites:", choices = sites, selected = sites)
  })

  # action sur la map

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(filteredData()) %>%
      addTiles(options = tileOptions(minZoom = 0, maxZoom = 25)) %>%
#    addGeoJSON(
#        country_boundaries,
#        opacity = 1,
#        dashArray = "9",
#        fillOpacity = 0.5,
#        color = "black",
#        weight = 1
#      )  %>%
      addScaleBar(position = 'topleft') %>%
      addMeasure(position = "topleft",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters",
        secondaryAreaUnit = "hectares",
        localization = "fr",
        activeColor = "red",
        completedColor = "red"
      ) %>%
      addLayersControl(
        overlayGroups = c("Fruits_m2", "Croissance"),
        options = layersControlOptions(collapsed = FALSE)
      )  %>%
  # hideGroup("Croissance")%>%
      fitBounds(~min(Longitude) - 0.001 , ~min(Latitude) - 0.001, ~max(Longitude) + 0.001 , ~max(Latitude) + 0.001)
  })


  # This will be used for the map.
  # Donnees reduite a la periode et aux sites selectiones
  filteredData <- reactive({
    masting[masting$Year >= input$range[1]
    & masting$Year <= input$range[2]
    & masting$Site %in% input$select_sites, ]
  })

  # This will be used for the map.
  # Statistiques que les donnees reduite a la periode et aux sites selectiones
  sumarizedData <- reactive({
    get_summary(masting[
      masting$Year >= input$range[1]
      & masting$Year <= input$range[2]
      & masting$Site %in% input$select_sites,
    ])
  })

  # This will be used for the map.
  # Position moyenne des sites selectiones
  # (pas utilise)
  positionData <- reactive({
    get_mean_positions_site(masting[masting$Site %in% input$select_sites, ])
  })

  # This will be used for the map.
  # Mise a jour de la taille des cercles
  scale_circle <- reactive({
    input$circle_size[1]
  })

  # Echelle de couleur
  pal <- colorNumeric(colorRamp(c("blue", "red"), interpolate = "spline"), NULL)

  # Fonction echelle
  # (pas utilise)
   echelle <- function(x){
    if (length(x) > 0)  {
      (scale_circle() - 1) * x + 1
    }
    else {
      x
    }
  }

  # Fonction echelle racine caree
  # -----------------------------
  echelle_sqrt <- function(x) {
    print(length(x))
    if (length(x) > 0) {
      sqrt((scale_circle() - 1) * x)
    }
    else {
      x
    }
  }

  # Affichage des markers et des cercles sur la carte
  # -------------------------------------------------
  observe({
    leafletProxy("map", data = sumarizedData()) %>%
      clearShapes() %>%
      addMarkers(label = ~paste(" ", Arbre), popup = ~paste(Arbre, ":<br>croissance terriere moyenne = ",meanBAI,"<br>taux fructif moyen = ",meantauxfructif,"<br>nb moyen de fruits par m2 = ",meanTotal_Fruits_per_m2))  %>%
      addCircles(radius = ~echelle_sqrt(meanTotal_Fruits_per_m2), color = ~pal(meanTotal_Fruits_per_m2),stroke= FALSE, group ="Fruits_m2" )  %>%
      #addCircles(radius = ~echelle_sqrt(meanBAI), color = ~pal(meanBAI), dashArray = "50", label = ~paste(" ", Arbre), popup = ~paste(Arbre, ":<br>croissance terriere moyenne = ",meanBAI,"<br>nb moyen de fruits par m2 = ",meanTotal_Fruits_per_m2), group ="Croissance" )
      addCircles(radius = ~echelle_sqrt(meanBAI), color = ~pal(meanBAI), fill = FALSE, group ="Croissance" )
  })


  # Action download
  output$download <- downloadHandler(
    filename = function() {
      paste0("selected_data.csv")
    },
    content = function(file) {
      saved_data<- select_in_map(input)
      write.csv(saved_data, file)
    }
  )

  # Output  de plotPerYear
  output$plotPerYear <- renderPlot({
    data_plot <- select_in_map(input)
    if (nrow(data_plot) > 0) {
      plot_years_var(
        data_plot,
        "Total_Fruits_per_m2",
        "Nb de fruits au m2 au cours du temps\n(arbres visible sur la carte)",
        "Nb de fruits au m2"
      )
    }
  })

  # Output de plotSitePerYear
  output$plotSitePerYear <- renderPlot({
    data_plot <- filteredData()
    if (nrow(data_plot) > 0) {
      toto <- get_summary_site(data_plot, "Total_Fruits_per_m2")
      plot_site_years_var(
        toto,
        "Moyenne_sur_les_arbres",
        "Nb de fruit par m2 (moyenne par site)",
        "Nb de fruit par m2"
      )
    }
  })

  # Output de plotSitePerYearBAI 
  output$plotSitePerYearBAI <- renderPlot({
    data_plot <- filteredData()
    if (nrow(data_plot) > 0) {
      toto <- get_summary_site(data_plot, "BAI")
      plot_site_years_var(
        toto,
        "Moyenne_sur_les_arbres", 
        "croissance terriere (moyenne par site)", 
        "BAI"
      )
    }
  })

  # Output de plotHisto
  output$plotHisto <- renderPlot({
    data_plot <- sumarizedData()
    if (nrow(data_plot) > 0) {
      plot_barplot_var(
        data_plot,
        "meanTotal_Fruits_per_m2",
        "Nb de fruits au m2 moyen par arbre"
      )
    }
  })

  # Output de plotHistoMax
  output$plotHistoMax <- renderPlot({
    data_plot <- sumarizedData()
    if (nrow(data_plot) > 0) {
      plot_barplot_var(
        data_plot,
        "maxTotal_Fruits_per_m2",
        "Nb de fruits au m2 maximum par arbre"
      )
    }
  })


  # output$lat <- renderPrint({
  #     input$lat
  #   })

  #   output$long <- renderPrint({
  #     input$long
  #   })

  #   output$geolocation <- renderPrint({
  #     input$
  

}

shinyApp(ui, server)
