
library(devtools)
devtools::install_github("ropensci/rnaturalearthhires")
library(dplyr)
library(osmdata)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(shiny)
library(leaflet)
library(readxl)
library(ggmap)
library(leaflet)
library(RColorBrewer)
library(leaflet.extras)
library(rsconnect)


ui <- fluidPage(
  titlePanel("Fast Food to Grocery Store Ratio in NJ"),
  sidebarLayout(
    sidebarPanel(
      textInput("street", "Street Address", "290 George St"),
      textInput("city", "City", "New Brunswick"),
      textInput("state", "State", "NJ"),
      textInput("zip", "ZIP Code", "08901"),
      actionButton("go", "Get Ratio"),
      textOutput("ratioText"),
      selectInput("heatmapType", "Toggle Population Heatmap:", 
                  choices = list("Off" = "off", "Population" = "population"), selected = "off"),
      HTML("<h3>What does this ratio mean?</h3>
           <p>The ratio represents the number of fast food outlets compared to grocery stores within a specified radius (one mile) from the entered address. A higher ratio indicates a greater prevalence of fast food options relative to grocery stores, which might reflect on the local food environment and accessibility to different types of food.</p>")
    ),
    mainPanel(
      leafletOutput("njMap", height = "700px")
    )
  )
)

server <- function(input, output) {
  output$njMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -74.5, lat = 40.0, zoom = 7) %>%
      addPolygons(
        data = nj_boundaries, 
        color = "#444444",
        weight = 2,
        fillOpacity = 0.2,  
        fillColor = "transparent"
      ) %>%
      addCircleMarkers(
        data = combined_df_map,
        color = ~ifelse(Category == "Fast Food", "yellow", "green"),
        popup = ~paste(Name, "<br>", Category),
        opacity = 1, fillOpacity = 0.8, radius = 1
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("yellow", "green"),
        labels = c("Fast Food", "Grocery"),
        title = "Category"
      )
  })
  
  observe({
    leafletProxy("njMap") %>% clearGroup("Population Heatmap")
    
    if(input$heatmapType == "population") {
      req(nrow(merged_data_sf) > 0)
      
      coords <- st_coordinates(merged_data_sf)
      population <- merged_data_sf$Population  
      
      leafletProxy("njMap") %>%
        addHeatmap(lng = coords[,1], lat = coords[,2], intensity = population, radius = 20, blur = 15, group = "Population Heatmap")
    }
  })
  
  observeEvent(input$go, {
    full_address <- paste(input$street, input$city, input$state, input$zip, sep = ", ")
    geocode_result <- ggmap::geocode(full_address, output = "latlona", source = "google")
    
    if (!is.na(geocode_result$lat) && !is.na(geocode_result$lon)) {
      result <- calculate_ratio(geocode_result$lat, geocode_result$lon, combined_df_map, 1609.34) 
      
      output$ratioText <- renderText({
        if (is.na(result$ratio)) {
          paste("No grocery stores found within the specified radius.",
                "\nFast Food Count: ", result$fast_food_count,
                "\nGrocery Count: ", result$grocery_count)
        } else {
          paste("Fast Food to Grocery Store Ratio: ", format(result$ratio, digits = 2),
                "\nFast Food Count: ", result$fast_food_count,
                "\nGrocery Count: ", result$grocery_count)
        }
      })
    } else {
      output$ratioText <- renderText("Could not geocode the address. Please try again.")
    }
  })
}
shinyApp(ui = ui, server = server)


library(rsconnect)
rsconnect::deployApp('~/finalproject')





