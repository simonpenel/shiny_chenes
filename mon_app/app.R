library(shiny)
library(leaflet)

library(RColorBrewer)
library(shinydashboard)

library(shinyWidgets)
library(shinyjs)

library(jsonlite)

# Masting data
options(encoding="latin1")

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

  tags$script('
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);

        function onError (err) {
          Shiny.onInputChange("geolocation", false);
        }

        function onSuccess (position) {
          setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.onInputChange("geolocation", true);
            Shiny.onInputChange("lat", coords.latitude);
            Shiny.onInputChange("long", coords.longitude);
          }, 1100)
        }
      });
              '),

leafletOutput("map", width = "100%", height = "100%"),

#absolutePanel(bottom = 50, right = 300, width = 400,  height = 150,
#class = "panel panel-default", draggable = TRUE,
#fluidRow(column(width = 2,
#                verbatimTextOutput("lat"),
#                verbatimTextOutput("long"),
#                verbatimTextOutput("geolocation"))
#),
#),

absolutePanel(bottom = 10, left = 10, width = 400,  height = 200,
class = "panel panel-default", draggable = TRUE,





    #downloadButton("download"),

    sliderInput("range", "Year", min(masting$Year), max(masting$Year),
      value = range(masting$Year), step = 1, sep ="", width=600
    ),

    sliderInput("circle_size", "Circle size", 1, 50,
     value = 5, step = 1, sep ="", width=600
    ),

    absolutePanel( class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 180, left = 10, bottom = "auto",
      width = 120, height = "auto",
      prettyCheckboxGroup("select_sites", "Sites", choices = sites, selected=sites,status="primary"),
      actionButton('unselect_all', 'Unselect all',
        style="opacity: .80; color:black; background-color: white; border-color: white"),
      actionButton('select_all', 'Select all',
        style="opacity: .80; color: black; background-color: white; border-color: white"),

    ),

    fixedPanel(bottom=10, right = 350,
        actionButton('plotBtn2', 'Show/Hide Plot 2',
        style="opacity: .80; color: #fff; background-color: #a662e3; border-color: #a153e5")
    ),
    fixedPanel(bottom=10, right = 180,
        actionButton('barplotBtn1', 'Show/Hide Barplot 1',
        style="opacity: .80; color: #fff; background-color: #a662e3; border-color: #a153e5")
    ),
    fixedPanel(bottom=10, right = 10,
        actionButton('barplotBtn2', 'Show/Hide Barplot 2',
        style="opacity: .80; color: #fff; background-color: #a662e3; border-color: #a153e5")
    ),
    fixedPanel(bottom=10, right = 500,
        actionButton('plotBtn1', 'Show/Hide Plot 1',
        style="opacity: .80; color: #fff; background-color: #a662e3; border-color: #a153e5")
    ),


    absolutePanel(id = 'plotyear', class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 10, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
      plotOutput("plotPerYear", height = 250),
    ),
    absolutePanel(id = 'barplot1', class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 260, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
      plotOutput("plotHisto", height = 250),
    ),
    absolutePanel(id = 'barplot2', class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 510, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
      plotOutput("plotHistoMax", height = 250),
    ),
    absolutePanel(id = 'plotsiteyear', class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 10, left = "auto", right = 350, bottom = "auto",
      width = 330, height = "auto",
      plotOutput("plotSitePerYear", height = 250),
    ),

    absolutePanel(id = 'plotsiteyearBAI', class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 260, left = "auto", right = 350, bottom = "auto",
      width = 330, height = "auto",
      plotOutput("plotSitePerYearBAI", height = 250),
    ),

  ),
)


mean_var <- function(x, var,data) {
    mean(data[data$Arbre == x,][[var]])
    }

max_var <- function(x, var,data) {
        max(data[data$Arbre == x,][[var]])
        }


get_summary <- function(m){
  sum_mast  <- data.frame(Arbre=m$Arbre,Latitude=m$Latitude,Longitude=m$Longitude)
  sum_mast <- unique(sum_mast)
  arbres <- sum_mast$Arbre
  test = sapply(arbres,mean_var, var="tauxfructif", data = m)
  sum_mast$meantauxfructif = c(test)
  test = sapply(arbres,mean_var, var="Total_Flowers_per_m2", data = m)
  sum_mast$meanTotal_Flowers_per_m2 = test
  test = sapply(arbres,mean_var, var="Total_Fruits_per_m2", data = m)
  sum_mast$meanTotal_Fruits_per_m2 = c(test)
  test = sapply(arbres,max_var, var="tauxfructif", data = m)
  sum_mast$maxtauxfructif = c(test)
  test = sapply(arbres,max_var, var="Total_Fruits_per_m2", data = m)
  sum_mast$maxTotal_Fruits_per_m2 = c(test)
  test = sapply(arbres,mean_var, var="BAI", data = m)
  sum_mast$meanBAI = c(test)
  sum_mast

}


# Select the data per site


# Select the data on the area, the year, the variable and the species
select_in_map <- function(input) {
  bounds <- input$map_bounds
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)
  selected_data<-subset(masting,
    Latitude >= latRng[1] & Latitude <= latRng[2] &
    Longitude >= lngRng[1] & Longitude <= lngRng[2] &
    Year >= input$range[1] &
    Year <= input$range[2] &
    Site %in% input$select_sites
    )
  selected_data
}

mean_years_select_in_map <- function(input) {
    select_in_map(input)
}

# Fonction qui renvoie une df avec la valeur moyenne sur les arbres par site/an
# ------------------------------------------------------------------------------
get_summary_site <- function(m,var){
  sum_site  <- data.frame(Site=m$Site,Year=m$Year,Moyenne_sur_les_arbres=0)
  sum_site <- unique(sum_site)
  sites <- unique(sum_site$Site)
  test <- lapply(sites,extract_site,data=m,sum_site=sum_site,var=var)
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
extract_site <- function(site,data,sum_site,var) {
  test <- data[data$Site == site ,]
  years <- unique(test$Year)
  mean_year <- sapply(years,mean_over_trees,data=test,var=var)
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

  if (nt > 1) {
  for(j in 2:nt) {
    site_data <-df[[j]]
    ymax <- max(site_data[[var]])
    xmax <- max(site_data$Year)
    xmin <- min(site_data$Year)
    ymax_all <- max(c(ymax,ymax_all))
    xmax_all <- max(c(xmax,xmax_all))
    xmin_all  <- min(c(xmin,xmin_all))
  }
  }

  site_data <-df[[1]]
  site_data <-site_data[order(site_data$Year),,drop=FALSE]

  ylim <- c(0,ymax_all)
  xlim <- c(xmin_all - 3, xmax_all + 1)

  plot(site_data$Year, site_data[[var]], type = type, pch = pch, col = col[1],
   xlim=xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = main,
   xaxp=c(xmin_all,xmax_all,xmax_all - xmin_all))

  liste_sites <- c(unique(site_data$Site))

  if (nt > 1) {
  for(j in 2:nt) {
    site_data <-df[[j]]
    site_data <-site_data[order(site_data$Year),,drop=FALSE]
    points(site_data$Year, site_data[[var]], type = type, pch = pch, col = col[j])
    liste_sites <- append(liste_sites, unique(site_data$Site))
  }
  }
  legend("topleft", legend=liste_sites,box.lty=0,
       col=col, lty=1, cex=0.8)
}


# Plot of the  production along years
plot_years_var <- function(df,var, main, ylab, type = "b", pch = 19,
xlab = "Year", leg = TRUE, posleg = "topleft", ...){
arbres = unique(df$Arbre)
arbres
nt   <- length(arbres)
col <- hcl.colors(nt, "Dark 2")
arbre <- df[df$Arbre==arbres[1],]
ylim <- c(0, max(df[[var]]))
xlim <- c(min(df$Year)-1, max(df$Year)+1)
plot(arbre$Year, arbre[[var]], type = type, pch = pch, col = col[1], xlim=xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = main, ...)
for(j in 2:nt) {
arbre <- df[df$Arbre==arbres[j],]
points(arbre$Year, arbre[[var]], type = type, pch = pch, col = col[j])
}
}


# Plot barplot
plot_barplot <- function(df, ...){
  par(mar=c(8,4,1,1))

  #"v_mean = df$meantauxfructif
  #v_max = df$maxtauxfructif

  v_mean = df$meanTotal_Fruits_per_m2
  v_max = df$maxTotal_Fruits_per_m2
  meanmax = matrix(c(v_mean,v_max),nc=2, byrow=F)
  #colnames(meanmax) = df$Arbre

  #rownames(meanmax) <- c("Mean","Max")
#barplot(meanmax,col=c(5:6),beside=T)
  barplot(df$meanTotal_Fruits_per_m2,names.arg=df$Arbre,las=2 )
}

plot_barplot_var <- function(df,var,main, ...){
  par(mar=c(8,4,1,1))
  barplot(df[[var]],names.arg=df$Arbre, las=2, main = main )
}


# SERVER
server <- function(input, output, session) {

  observeEvent(input$plotBtn2, {
    toggle('plotyear')
  })

  observeEvent(input$plotBtn1, {
    toggle('plotsiteyear')
    toggle('plotsiteyearBAI')
  })

  observeEvent(input$barplotBtn1, {
    toggle('barplot1')
  })

  observeEvent(input$barplotBtn2, {
    toggle('barplot2')
  })

  observeEvent(input$unselect_all, {
  updateCheckboxGroupInput(session,"select_sites","Sites:",choices=sites)
  })

  observeEvent(input$select_all, {
  updateCheckboxGroupInput(session,"select_sites","Sites:",choices=sites,selected=sites)
  })


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
    completedColor = "red") %>%
      addLayersControl(
    overlayGroups = c("Fruits_m2","Croissance"),
    options = layersControlOptions(collapsed = FALSE)
  )  %>%
  # hideGroup("Croissance")%>%
    fitBounds(~min(Longitude) - 0.001 , ~min(Latitude) - 0.001, ~max(Longitude) + 0.001 , ~max(Latitude) + 0.001)
  })


  # This will be used for the map.
  filteredData <- reactive({
    masting[masting$Year >= input$range[1]
    & masting$Year <= input$range[2]
    & masting$Site %in% input$select_sites, ]
  })

  # This will be used for the map.
  sumarizedData <- reactive({
    get_summary(
      masting[masting$Year >= input$range[1]
    & masting$Year <= input$range[2]
    & masting$Site %in% input$select_sites, ])
  })

  scale_circle <- reactive({
    input$circle_size[1]
  })



pal <- colorNumeric(colorRamp(c("blue", "red"), interpolate="spline"),NULL)



  echelle <- function(x){
      print("X=")
      print(x)
      print(length(x))
      if (length(x) > 0)  {
        (scale_circle() - 1 ) * x + 1
      }
      else {
        x
      }
    }

  echelle_sqrt <- function(x){
      print("X=")
      print(x)
      print(length(x))
      if (length(x) > 0)  {
        sqrt((scale_circle() - 1 ) * x )
      }
      else {
        x
      }
    }


  observe({
    #leafletProxy("map", data = mean_years_filteredData()) %>%
    #leafletProxy("map", data = get_summary(filteredData())) %>%
      leafletProxy("map", data = sumarizedData()) %>%
      clearShapes() %>%
      addCircles(radius = ~echelle_sqrt(meanTotal_Fruits_per_m2), color = ~pal(meanTotal_Fruits_per_m2),stroke= FALSE, label = ~paste(" ", Arbre), popup = ~paste(Arbre, ":<br>BAI moyen = ",meanBAI,"<br>taux fructif moyen = ",meantauxfructif,"<br>nb moyen de fruits par m2 = ",meanTotal_Fruits_per_m2), group ="Fruits_m2" )  %>%
      #addCircles(radius = ~echelle_sqrt(meanBAI), color = ~pal(meanBAI), dashArray = "50", label = ~paste(" ", Arbre), popup = ~paste(Arbre, ":<br>BAI moyen = ",meanBAI,"<br>nb moyen de fruits par m2 = ",meanTotal_Fruits_per_m2), group ="Croissance" )
      addCircles(radius = ~echelle_sqrt(meanBAI), color = ~pal(meanBAI), fill = FALSE, label = ~paste(Arbre, ":<br>BAI moyen = ",meanBAI,"<br>taux fructif moyen = ",meantauxfructif,"<br>nb moyen de fruits par m2 = ",meanTotal_Fruits_per_m2), group ="Croissance" )

  })


  # Output for the download
  output$download <- downloadHandler(
    filename = function() {
      paste0("selected_data.csv")
    },
    content = function(file) {
      saved_data<- select_in_map(input)
      write.csv(saved_data, file)
    }
  )

  # Output the plot
  output$plotPerYear <- renderPlot({
    data_plot <- select_in_map(input)
    if (nrow(data_plot) > 0) {
    plot_years_var(data_plot,"Total_Fruits_per_m2","Nombre de fruits au m2 au cours du temps","Nombre de fruits au m2")
  }
  })

  # Output the plot
  output$plotSitePerYear <- renderPlot({
    data_plot <-filteredData()
    if (nrow(data_plot) > 0) {

      toto <- get_summary_site(data_plot,"Total_Fruits_per_m2")
      plot_site_years_var(toto,"Moyenne_sur_les_arbres", "Nb de fruit par m2 (moyenne)", "Nb de fruit par m2")
  }
  })

  # Output the plot
  output$plotSitePerYearBAI <- renderPlot({
    data_plot <-filteredData()
    if (nrow(data_plot) > 0) {

      toto <- get_summary_site(data_plot,"BAI")
      plot_site_years_var(toto,"Moyenne_sur_les_arbres", "BAI (moyenne)", "BAI")
  }
  })

  # Output the plot
  output$plotHisto <- renderPlot({
    data_plot <- sumarizedData()
    if (nrow(data_plot) > 0) {
      plot_barplot_var(data_plot,"meanTotal_Fruits_per_m2", "Nombre de fruits au m2 moyen par arbre")
  }
  })

  # Output the plot
  output$plotHistoMax <- renderPlot({
    data_plot <- sumarizedData()
    if (nrow(data_plot) > 0) {
      plot_barplot_var(data_plot,"maxTotal_Fruits_per_m2","Nombre de fruits au m2 maximum par arbre")
  }
  })


  output$lat <- renderPrint({
      input$lat
    })

    output$long <- renderPrint({
      input$long
    })

    output$geolocation <- renderPrint({
      input$geolocation
    })
}

shinyApp(ui, server)
