library(shiny)
library(leaflet)

library(RColorBrewer)
library(shinydashboard)

library(shinyWidgets)
library(shinyjs)

# Masting data
options(encoding="latin1")

masting <- read.csv("merged.csv",sep=";")
masting$meantauxfructif <- masting$tauxfructif * 0

print("toto")
sites <-unique(masting$Site)
print(sites)

# UI
ui <- bootstrapPage(

  tags$style(type = "text/css",
    "html, body {width:100%;height:100%}
    #controls { background-color: #ddd; opacity: 0.85;"
  ),

  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(bottom = 10, left = 10, width = 400, class = "panel panel-default", draggable = TRUE,


    #prettyCheckboxGroup("select_sites", "Sites", choices = sites, selected=sites,status="primary"),

    #downloadButton("download"),

    sliderInput("range", "Year", min(masting$Year), max(masting$Year),
      value = range(masting$Year), step = 1, sep ="", width=600
    ),

    absolutePanel( class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 100, left = 10, bottom = "auto",
      width = 120, height = "auto",
      prettyCheckboxGroup("select_sites", "Sites", choices = sites, selected=sites,status="primary"),

    ),

    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 10, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
      plotOutput("histCentile", height = 250),
    )

  ),
)

mean_var <- function(x, var) {
    mean(masting[masting$Arbre == x,][[var]])
    }

get_summary <- function(m){
  sum_mast  <- data.frame(Arbre=m$Arbre,Latitude=m$Latitude,Longitude=m$Longitude)
  sum_mast <- sum_mast[!duplicated(sum_mast), ]
  arbres <- sum_mast$Arbre

  test = lapply(arbres,mean_var, var="tauxfructif")
  sum_mast$meantauxfructif = test
  test = lapply(arbres,mean_var, var="Total_Flowers_per_m2")
  sum_mast$meanTotal_Flowers_per_m2 = test
  test = lapply(arbres,mean_var, var="Total_Fruits_per_m2")
  sum_mast$meanTotal_Fruits_per_m2 = test
  print(sum_mast)
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
# Plot of the fruits production
plot_fruits <- function(df, type = "b", pch = 19,
ylab = "Fruits per m2", xlab = "Year", leg = TRUE, posleg = "topleft",
main = "Production de fruit au cours du temps", ...){
arbres = unique(df$Arbre)
arbres
nt   <- length(arbres)
col <- hcl.colors(nt, "Dark 2")
arbre <- df[df$Arbre==arbres[1],]
ylim <- c(0, max(df$Total_Fruits_per_m2))
plot(arbre$Year, arbre$Total_Fruits_per_m2, type = type, pch = pch, col = col[1], ylim = ylim, xlab = xlab, ylab = ylab, main = main, ...)
for(j in 2:nt) {
arbre <- df[df$Arbre==arbres[j],]
points(arbre$Year, arbre$Total_Fruits_per_m2, type = type, pch = pch, col = col[j])
}
}


# SERVER
server <- function(input, output, session) {

pal <- colorNumeric(
  palette = "magma",
  domain = masting$Total_Flowers_per_m2)

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(filteredData()) %>% addTiles(options = tileOptions(minZoom = 0, maxZoom = 25)) %>%
    fitBounds(~min(Longitude) - 0.001 , ~min(Latitude) - 0.001, ~max(Longitude) + 0.001 , ~max(Latitude) + 0.001)
  })


  # This will be used for the map.
  filteredData <- reactive({
    masting[masting$Year >= input$range[1]
    & masting$Year <= input$range[2]
    & masting$Site %in% input$select_sites, ]
  })

  mean_years_filteredData <- reactive({
    brut <- masting[masting$Year >= input$range[1]
        & masting$Year <= input$range[2]
        & masting$Site %in% input$select_sites, ]
    sites = unique(brut$Site)
    arbres = unique(brut$Arbre)
    print("Sites:")
    print(sites)
    sel_site <- function(x) {brut[brut$Site == x,]}
    mean_sel_site <- function(x) {mean(brut[brut$Site == x,]$tauxfructif)}
    mean_sel_arbre <- function(x) {mean(brut[brut$Arbre == x,]$tauxfructif)}
    set_mean_arbre <- function(x) {brut[brut$Arbre == x,]$meantauxfructif=mean(brut[brut$Arbre == x,]$tauxfructif)}

    test = lapply(arbres,mean_sel_arbre)
    #brut$meantauxfructif = test
    print("test")
    print(test)
    print(brut$meantauxfructif)
    #brut = subset(brut, select = -c(Year,Total_Flowers_per_m2, tauxfructif) )
    test=lapply(arbres,set_mean_arbre)
    print(test)
    mean <- mean(brut$tauxfructif)
    #print(mean)
    print(brut)
    brut
  })


  observe({
    #leafletProxy("map", data = mean_years_filteredData()) %>%
    leafletProxy("map", data = get_summary(filteredData())) %>%
      clearShapes() %>%
      #addCircles(radius = ~meantauxfructif,color = ~pal(meantauxfructif),  group ="Cone" )
      addCircles(radius = ~meantauxfructif,  group ="Cone" )
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
  output$histCentile <- renderPlot({
    data_plot <- select_in_map(input)
    if (nrow(data_plot) > 0) {
    #plot(data_plot$Year,data_plot$tauxfructif,type="b")
    plot_fruits(data_plot)
  }

  })

}

shinyApp(ui, server)
