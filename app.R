library(sf)
library(shiny)
library(plotly)
library(mapdeck)
library(tidyverse)

if(!file.exists("casualties_active_london.Rds")) {
  download.file("https://github.com/saferactive/saferactiveshiny/releases/download/0.0.2/casualties_active_london.Rds",
                "casualties_active_london.Rds")
}

crashes = readRDS("casualties_active_london.Rds")
crashes$year = lubridate::year(crashes$date)
crashes$month = lubridate::round_date(crashes$date, unit = "month")

geographic_levels = c("region", "police force", "local authority", "constituency", "heatmap")
casualty_types = c(Walking = "Pedestrian", Cycling = "Cyclist")

ui <- fluidPage(
  sidebarPanel(
    selectInput(inputId = "casualty_type", label = "Casualty type", choices = casualty_types),
    selectInput(inputId = "level", label = "Level of aggregation", choices = geographic_levels, selected = "region"),
    sliderInput(inputId = "years", "years", 2000, 2030, value = c(2014, 2018)),
    selectInput(inputId = "measure", label = "Measure of Safety (not yet implemented)", choices = c("Cycling KSI/bkm", "Walking KSI/bkm", "Cycling KSI absolute")),
    actionButton(inputId = "roads", label = "Roads viz"),
    actionButton(inputId = "ui", label = "Additional user interface here!")
  )
  , mainPanel(
    mapdeckOutput(outputId = "map"),
    textOutput(outputId = "text"),
    plotly::plotlyOutput(outputId = "plot1")
    # plotOutput(outputId = "plot1")
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
  

# time series plot --------------------------------------------------------

  output$plot1 = plotly::renderPlotly(expr = {
  # output$plot1 = renderPlot(expr = {
    
    cas_monthly = crashes_year() %>%
      sf::st_drop_geometry() %>% 
      group_by(month) %>%
      summarise(
        Total_Slight = sum(accident_severity == "Slight"),
        Pedestrian_Slight = sum(accident_severity == "Slight" & casualty_type == "Pedestrian", na.rm = TRUE),
        Cyclist_Slight = sum(accident_severity == "Slight" & casualty_type == "Cyclist", na.rm = TRUE),
        Total_Serious = sum(accident_severity == "Serious"),
        Pedestrian_Serious = sum(accident_severity == "Serious" & casualty_type == "Pedestrian", na.rm = TRUE),
        Cyclist_Serious = sum(accident_severity == "Serious" & casualty_type == "Cyclist", na.rm = TRUE),
        Total_Fatal = sum(accident_severity == "Fatal"),
        Pedestrian_Fatal = sum(accident_severity == "Fatal" & casualty_type == "Pedestrian", na.rm = TRUE),
        Cyclist_Fatal = sum(accident_severity == "Fatal" & casualty_type == "Cyclist", na.rm = TRUE),
      )
    cas_monthly = cas_monthly[-1, ]
    cas_monthly = cas_monthly[-nrow(cas_monthly), ]
    cas_long = gather(cas_monthly, Collision_type, Number, -month)
    cas_long = cas_long %>% separate(data = ., col = Collision_type, into = c("Mode", "Severity"), sep = "_")
    
    # Alternative with colours for crash types
    cas_long = cas_long %>% 
      filter(Mode == input$casualty_type)
    cas_long$Number = cas_long$Number * 12
    p1 = ggplot(cas_long) +
      geom_line(aes(month, Number, colour = Severity)) +
      scale_y_log10() +
      ylab("Casualties/year") +
      xlab("") + 
      theme_minimal()
    
    p1
    
  })
  
}

shinyApp(ui, server)

# package and upload the app from root directory:
# zip("shinyapp1.zip", files = "shinyapp1")
# piggyback::pb_upload("shinyapp1.zip", repo = "saferactive/saferactive.github.io")
# piggyback::pb_download_url("shinyapp1.zip", repo = "saferactive/saferactive.github.io")
# shiny::runUrl("https://github.com/saferactive/saferactive.github.io/releases/download/0.0.1/shinyapp1.zip")
