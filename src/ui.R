library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(shinyalert)
library(tidyverse)
library(DT)
library(purrr)

tooltip_text <- "Abundance is Catch Per Unit Effort (CPUE).
It represents the number of fish caught per unit of fishing 
effort, such as per net.
\n
Size is L90, a metric used to describe the size distribution 
of fish populations. It refers to the length at which 90% of 
the fish in a population are equal to or smaller than that size."

# Define UI for application
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"), # Link to the external CSS file
    tags$script(src = "playMusic.js")  # Load the custom JavaScript file
  ),
  
  tabsetPanel(
    # First tab: Map
    tabPanel("Map",
             div(id = "mapContainer",
                 leafletOutput("swedenMap", width = "100%", height = "100%"),  # Map container
                 
                 div(id = "sidebar",  # Sidebar for controls
                     h4("Map Controls"),
                     
                     # Marker input
                     radioButtons("marker", "Marker:",
                                  choices = c("Circles", "Fish"),
                                  selected = "Circles",
                                  inline = TRUE),
                     
                     # Species input
                     selectInput("species", "Species", choices = NULL),
                     
                     # Size/abundance input
                     div(
                       style = "display: flex; align-items: center;",
                       selectInput("indicator", 
                                   span("Indicator", class = "help", "(?)", title = tooltip_text), 
                                   choices = c("Abundance [CPUE]", "Size [L90]"), selected = "Abundance"),
                       tags$style(".help { font-size: 14px; color: #FFFFF; margin-left: 5px; }")
                     ),
                     
                     # Year input
                     sliderInput("year", "Year:",
                                 min = 2000, max = 2020,
                                 value = 2015, sep = ""),
                     br(),
                     
                     # Display plot
                     plotOutput("barplot", height = "200px")
                 )
             ),
             
             # Placeholder for dynamic legend
             div(id = "legendContainer",
                 class = "legend-container",
                 h4(class = "legend-title", "Marker Legend"),
                 uiOutput("legendSmall"),
                 uiOutput("legendMedium"),
                 uiOutput("legendLarge"),
                 uiOutput("legendExtraLarge")
             )
    ),
    
    # Second tab: Data Explorer
    tabPanel("Data Explorer",
             DTOutput("data_table")
    )
  ),
  
  # Additional UI components (outside tabs)
  div(class = "switch-container",  # Toggle switch overlay with opaque white background
      tags$label("Sound:",
                 tags$input(type = "checkbox", id = "toggle_music", checked = TRUE)
      )
  ),
  
  div(id = "fish_puns",
      actionButton("fish_puns_btn", "Fish Pun"))
)
