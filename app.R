library(sf)
library(shiny)
library(mapdeck)

if(!file.exists("casualties_active_london.Rds")) {
  download.file("https://github.com/saferactive/saferactiveshiny/releases/download/0.0.2/casualties_active_london.Rds",
                "casualties_active_london.Rds")
}

crashes = readRDS("casualties_active_london.Rds")
crashes$year = lubridate::year(crashes$date)

geographic_levels = c("region", "police force", "local authority", "constituency", "heatmap")

ui <- fluidPage(
  sidebarPanel(
    selectInput(inputId = "level", label = "Level of aggregation", choices = geographic_levels, selected = "region"),
    sliderInput(inputId = "years", "years", 2000, 2030, value = c(2014, 2018)),
    selectInput(inputId = "measure", label = "Measure of Safety (not yet implemented)", choices = c("Cycling KSI/bkm", "Walking KSI/bkm", "Cycling KSI absolute")),
    actionButton(inputId = "roads", label = "Roads viz"),
    actionButton(inputId = "ui", label = "Additional user interface here!")
  )
  , mainPanel(
    mapdeckOutput(outputId = "map"),
    textOutput(outputId = "text")
  )
)

server <- function(input, output) {
  
  crashes_year = reactive(
    crashes[crashes$year >= input$years[1] & crashes$year <= input$years[2], ]
  )

    mapdeck::set_token("pk.eyJ1Ijoicm9iaW5sb3ZlbGFjZXAiLCJhIjoiY2p5OTJjcjdwMGY3ZzNtbzBsemRxdG1rZiJ9.67PMwFHfzG8RL2TwBVYsrw")
  
  ## initialise a map
  output$map <- renderMapdeck({
    mapdeck( location = c(-2, 54), zoom = 5 )
  })
  
  # use an observer to add and remove layers
  observeEvent({input$roads},{

    if ( input$roads %% 2 == 1 ) {

      mapdeck_update(map_id = "map") %>%
        add_path(
          data = roads
          , layer_id = "myRoads"
          , stroke_colour = "RIGHT_LOC"
          , update_view = FALSE
        )
    } else {
      mapdeck_update(map_id = "map") %>%
        clear_path(layer_id = "myRoads")
    }
  })
  
  observeEvent({
    input$level
    input$years
    },{

    if ( input$level == "heatmap" ) {

      mapdeck_update(map_id = "map") %>%
        add_heatmap(crashes_year(), layer_id = "heatmap", update_view = FALSE)
    } else {
      mapdeck_update(map_id = "map") %>%
        clear_heatmap(layer_id = "heatmap")
    }
  })
  
  output$text = renderText(input$years)
  
}

shinyApp(ui, server)

# package and upload the app from root directory:
# zip("shinyapp1.zip", files = "shinyapp1")
# piggyback::pb_upload("shinyapp1.zip", repo = "saferactive/saferactive.github.io")
# piggyback::pb_download_url("shinyapp1.zip", repo = "saferactive/saferactive.github.io")
# shiny::runUrl("https://github.com/saferactive/saferactive.github.io/releases/download/0.0.1/shinyapp1.zip")
