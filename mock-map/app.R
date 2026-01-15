library(shiny)
library(dplyr)
library(leaflet)
library(lubridate)

#From GH

# ---- Load data ----
df <- read.csv("case_movements_long.csv", stringsAsFactors = FALSE)

df <- df %>%
  mutate(
    day = dmy(day),
    date_last_attended = dmy(date_last_attended),
    type = factor(type, levels = c("home", "work", "visited", "cooling tower"))
  )

# ---- Icons ----
icon_home <- makeIcon("icons/home.png", 30, 30)
icon_work <- makeIcon("icons/work.png", 30, 30)
icon_visited <- makeIcon("icons/visited.png", 30, 30)
icon_tower <- makeIcon("icons/cooling_tower.png", 34, 34)

icons <- iconList(
  home = icon_home,
  work = icon_work,
  visited = icon_visited,
  `cooling tower` = icon_tower
)

# ---- UI ----
ui <- fluidPage(
  
  titlePanel("Legionellosis Outbreak – Exposures and Cooling Towers Map"),
  
  sidebarLayout(
    sidebarPanel(
      
      dateInput(
        inputId = "day",
        label = "Select date of outbreak",
        value = max(df$day),
        min = min(df$day),
        max = max(df$day),
        format = "dd/mm/yyyy"
      ),
      
      checkboxGroupInput(
        inputId = "type",
        label = "Location type",
        choices = levels(df$type),
        selected = levels(df$type)
      )
    ),
    
    mainPanel(
      leafletOutput("map", height = 750)
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  filtered_df <- reactive({
    df %>%
      filter(
        day <= input$day,
        type %in% input$type
      )
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(
        lng = mean(df$longitude, na.rm = TRUE),
        lat = mean(df$latitude, na.rm = TRUE),
        zoom = 12
      )
  })
  
  observe({
    leafletProxy("map", data = filtered_df()) %>%
      clearMarkers() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        icon = ~icons[as.character(type)],
        popup = ~paste0(
          "<b>PHESS ID:</b> ", phess_id, "<br>",
          "<b>Type:</b> ", type, "<br>",
          "<b>Date last attended:</b> ",
          ifelse(is.na(date_last_attended), "N/A",
                 format(date_last_attended, "%d/%m/%Y")),
          "<br>",
          "<b>Known as of:</b> ", format(day, "%d/%m/%Y")
        )
      )
  })
}

shinyApp(ui, server)
