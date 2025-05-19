library(shiny)
library(leaflet)

library(RColorBrewer)
library(shinydashboard)

library(shinyWidgets)
library(shinyjs)

# Masting data
options(encoding="latin1")

masting <- read.csv("merged.csv",sep=";")

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

  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~tauxfructif,color = ~pal(Total_Flowers_per_m2),  group ="Cone" )
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
