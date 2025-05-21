library(shiny)
library(leaflet)

library(RColorBrewer)
library(shinydashboard)

library(shinyWidgets)
library(shinyjs)

# Masting data
options(encoding="latin1")

masting <- read.csv("merged.csv",sep=";")

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


    #downloadButton("download"),

    sliderInput("range", "Year", min(masting$Year), max(masting$Year),
      value = range(masting$Year), step = 1, sep ="", width=600
    ),

    sliderInput("circle_size", "Circle size", 1, 50,
     value = 5, step = 1, sep ="", width=600
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


mean_var <- function(x, var,data) {
    mean(data[data$Arbre == x,][[var]])
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
        scale_circle() * x
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
      addCircles(radius = ~echelle(meantauxfructif), color = ~pal(meantauxfructif), popup = ~paste(Arbre, ":<br>taux fructif moyen = ",meantauxfructif,"<br>nb moyen de fruits par m2 = ",meanTotal_Fruits_per_m2), group ="Cone" )
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
