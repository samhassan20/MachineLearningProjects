install.packages("htmltools")

library(shiny)
library(leaflet)
library(htmltools)
library(rsconnect)

plane_hijack <-read.csv("plane.csv")
plane_hijack$summary <- as.character(plane_hijack$summary)
plane_hijack$summary[plane_hijack$summary==""] <- "No Summary Available"
ui <- bootstrapPage(titlePanel("Plane Hijack Visualization(1970-2015)"),
                    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                    leafletOutput("worldmap", width = "100%", height = "100%"),
                    absolutePanel(top = 10, right = 10,
                                selectInput(inputId = "year", "Select Year:",
                                             choices = unique(plane_hijack$iyear))))

server <- function(input, output, session) {
  points <- eventReactive(input$year,{
    x <-subset(plane_hijack, plane_hijack$iyear == input$year)
    cbind(x$longitude, x$latitude)
  }, ignoreNULL = TRUE)
  msg <- eventReactive(input$year,{
    x <-subset(plane_hijack, plane_hijack$iyear == input$year)
    x$summary
  }, ignoreNULL = TRUE)
  
  killings <- eventReactive(input$year,{
    x <-subset(plane_hijack, plane_hijack$iyear == input$year)
    x$nkill 
  }, ignoreNULL = TRUE)
  output$worldmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points(), popup = paste("People Killed:-",killings(),"<br>",msg()) )
  })
}
shinyApp(ui = ui, server = server)

