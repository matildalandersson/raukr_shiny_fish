# Define server logic
server <- function(input, output, session) {
  
  # Read data file
  data <- read.csv("../Results_20232002.csv")
  fish_puns <- readLines("../data/fish_puns.txt")
  
  ## Map Tab ##
  # Update species choices
  species <- data$Species %>% unique() %>% sort()
  updateSelectInput(session, "species", choices = species, selected = "Perch")
  
  max_year <- max(data$Year)
  min_year <- min(data$Year)
  middle_year <- round((max_year + min_year) / 2, 0)
  updateSliderInput(session, "year",
                    min = min_year - 1,
                    max = max_year,
                    value = middle_year)
  
  # Reactive value to store the previous indicator
  previous_indicator <- reactiveVal(NULL)
  
  
  # Update dataframe based on choices
  reactive_data <- reactive({
    req(input$species, input$year)
    
    df <- data[which(data$Species == input$species & data$Year == input$year), ]
    
    if (is.null(df) || nrow(df) == 0) {
      shinyalert(title = "No Data",
                 text = paste("Something is fishy here. No data for", input$species, "in", input$year),
                 type = "error")
      
      return(NULL)
    }
    
    return(df)
  })
  
  range_values <- reactive({
    df <- reactive_data()
    
    if (input$indicator == "Abundance [CPUE]") {
      df_col <- "CPUE"
    } else if (input$indicator == "Size [L90]") {
      df_col <- "L90_cm"
    }
    
    # Remove rows where the selected column is NA
    df <- df[!is.na(df[[df_col]]), ]
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    # Compute min, max, and range
    minimum <- min(df[[df_col]], na.rm = TRUE)
    maximum <- max(df[[df_col]], na.rm = TRUE)
    range <- maximum - minimum
    
    # Check for Inf or NaN values
    if (is.infinite(minimum) || is.infinite(maximum) || is.nan(minimum) || is.nan(maximum)) {
      return(NULL)  # Return NULL if any value is Inf or NaN
    }
    
    # Compute quartile values
    val_00 <- round(minimum, 2)
    val_25 <- round(0.25 * range + minimum, 2)
    val_50 <- round(0.50 * range + minimum, 2)
    val_75 <- round(0.75 * range + minimum, 2)
    val_100 <- round(maximum, 2)
    
    values <- c(val_00, val_25, val_50, val_75, val_100)
    return(values)
  })
  
  reactive_marker_data <- reactive({
    df <- reactive_data()
    indicator <- input$indicator
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    if (indicator == "Abundance [CPUE]"){
      max_CPUE <- max(df$CPUE)
      min_CPUE <- min(df$CPUE)
      range_CPUE <- max_CPUE - min_CPUE
      
      display_df_sizes <- df %>%
        mutate(size_category = case_when(
          CPUE > (0.75 * range_CPUE + min_CPUE) ~ 20,
          CPUE > (0.5 * range_CPUE + min_CPUE) ~ 15,
          CPUE > (0.25 * range_CPUE + min_CPUE) ~ 10,
          TRUE ~ 5
        ))
    } else if (indicator == "Size [L90]"){
      if (previous_indicator() == "Abundance [CPUE]") {
        shinyalert(title = "Removing Data Points",
                   text = paste("Your data is fishy! To use the size indicator, every location with less than 50", input$species, "caught have been removed"),
                   type = "warning")
      }
      
      display_df <- df %>%
        subset(!is.na(L90_cm))
      
      if (is.null(display_df) || nrow(display_df) == 0) {
        shinyalert(title = "No Data",
                   text = paste0("Someone stole all of your fish! Every location has caught less than 50 ", input$species, ", so everything has been removed"),
                   type = "error")
        return(NULL)
      }
      
      max_L90 <- max(display_df$L90_cm)
      min_L90 <- min(display_df$L90_cm)
      range_L90 <- max_L90 - min_L90
      
      display_df_sizes <- display_df %>%
        mutate(size_category = case_when(
          L90_cm > (0.75 * range_L90 + min_L90) ~ 20,
          L90_cm > (0.5 * range_L90 + min_L90) ~ 15,
          L90_cm > (0.25 * range_L90 + min_L90) ~ 10,
          TRUE ~ 5
        ))
    }
    
    # Update previous_indicator with the current indicator
    previous_indicator(input$indicator)
    
    return(display_df_sizes)
  })
  
  plot_data <- reactive({
    info <- input$swedenMap_marker_click
    req(info)
    if (input$indicator == "Abundance [CPUE]"){
      plot_col <- "CPUE"
    } else if (input$indicator == "Size [L90]"){
      plot_col <- "L90_cm"
    }
    
    # Find the row in the data that matches the clicked marker's coordinates
    marker_df <- reactive_marker_data()
    req(marker_df)
    
    marker_data <- marker_df %>%
      filter(Longitude == info$lng, Latitude == info$lat)
    
    location <- marker_data$Location
    species <- marker_data$Species
    
    test_data <<- data
    
    df <- data %>% complete(Species, Year, Location)
    df <- df[which(df$Location == location & df$Species == species), ]
    
    
    df$plot_column <- df[[plot_col]]
    
    df <- df %>%
      mutate(plot_column = replace_na(plot_column, 0))
    
    return(df)
  })
  
  # The map
  output$swedenMap <- renderLeaflet({
    leaflet() %>%                                     # Create a leaflet map object
      addTiles() %>%                                  # Default OpenStreetMap tiles
      setView(lng = 18.6435, lat = 60.1282, zoom = 5) # Somewhere in Sweden
  })
  
  # The marker (fish vs circles)
  observe({
    df <- reactive_marker_data()
    marker <- input$marker
    
    if (is.null(df) || nrow(df) == 0) {
      leafletProxy("swedenMap", data = df) %>%
        clearMarkers()
      
      return(NULL)
    }
    
    if (marker == "Circles") {
      leafletProxy("swedenMap", data = df) %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = ~Longitude, lat = ~Latitude,     # Set longitude and latitude
          label = ~Location,                     # Label each marker,
          color = "#FA8072",
          radius = ~size_category,               # Radius size
          stroke = FALSE, fillOpacity = 0.6,     # Border and fill opacity
          popup = ~paste0("<b>", Location, "</b><br>Number: ", count, "</b><br>Abundance: ", round(CPUE, 2), 
                          "</b><br>Size: ", L90_cm, "</b><br>Years of data: ", Years)
        )
    } else if (marker == "Fish") {
      fish_multiplier = 3
      
      leafletProxy("swedenMap", data = df) %>%
        clearMarkers() %>%
        addMarkers(
          lng = ~Longitude, lat = ~Latitude,     # Set longitude and latitude
          icon = ~icons(
            iconUrl = "https://as1.ftcdn.net/v2/jpg/06/70/58/68/1000_F_670586814_zPsLZ38T5wtVC4vDKIGNHCN8aYXPSqo1.webp",
            iconWidth = ~size_category * fish_multiplier,      # Scale icon width
            iconHeight = ~size_category * fish_multiplier,     # Scale icon height
            iconAnchorX = ~size_category * fish_multiplier,
            iconAnchorY = ~size_category * fish_multiplier,
            popupAnchorX = 0,
            popupAnchorY = 0
          ),
          label = ~Location,                     # Label each marker
          popup = ~paste0("<b>", Location, "</b><br>Number: ", count, "</b><br>Abundance: ", round(CPUE, 2), 
                          "</b><br>Size: ", L90_cm, "</b><br>Years of data: ", Years)
        )
    }
  })
  
  # Render legend items
  output$legendSmall <- renderUI({
    legend_text <- paste("S: ", range_values()[1], " - ", range_values()[2])
    div(class = "legend-item",
        div(class = "legend-circle small"),
        legend_text)
  })
  
  output$legendMedium <- renderUI({
    legend_text <- paste("M: ", range_values()[2], " - ", range_values()[3])
    div(class = "legend-item",
        div(class = "legend-circle medium"),
        legend_text)
  })
  
  output$legendLarge <- renderUI({
    legend_text <- paste("L: ", range_values()[3], " - ", range_values()[4])
    div(class = "legend-item",
        div(class = "legend-circle large"),
        legend_text)
  })
  
  output$legendExtraLarge <- renderUI({
    legend_text <- paste("XL: ", range_values()[4], " - ", range_values()[5])
    div(class = "legend-item",
        div(class = "legend-circle extra-large"),
        legend_text)
  })
  
  output$barplot <- renderPlot({
    df <- plot_data()
    
    if (is.null(df) || nrow(df) == 0) {
      title <- "Please choose a map location"
      df <- data.frame(
        Year = cos(1:10000) - sin(1:10000)^2 / sqrt(2),
        plot_column = cos(1:10000) * sin(1:10000)
      )
      
      p <- ggplot(df, aes(x = Year, y = plot_column)) +
        geom_point() +
        labs(title = title, x = "Year", y = "Indicator") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = "white")
        ) +
        scale_x_continuous(breaks = df$Year)
    } else {
      title <- unique(df$Location)
      
      p <- ggplot(df, aes(x = Year, y = plot_column)) +
        geom_bar(stat = "identity", fill = "#FA8072", color = "black", width = 0.7) +
        labs(title = title, x = "Year", y = "Indicator") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          panel.grid.major = element_line(color = "gray", linewidth = 0.5),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white")
        ) +
        scale_x_continuous(breaks = df$Year)
    }
    
    return(p)
  })
  
  # Initialize music state
  observe({
    if (input$toggle_music) {
      session$sendCustomMessage(type = "playMusic", message = list(volume = 0.5))
    } else {
      session$sendCustomMessage(type = "stopMusic", message = list())
    }
  })
  
  
  # Define action for "Fish Puns" button
  observeEvent(input$fish_puns_btn, {
    # Randomly select a fish pun
    random_pun <- sample(fish_puns, 1)
    
    # Show the pun in a pop-up
    showModal(
      modalDialog(
        title = "Fish Pun",
        random_pun,
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  ## Data Explorer Tab ##
  # Cleaning up data
  table_data <- data %>%
    select(-Ntot) %>%                    
    mutate(across(c(Longitude, Latitude, CPUE), \(x) round(x, 2))) %>% 
    rename(Count = count, 'Number of Nets' = Effort_Nnets, 
           L90 = L90_cm, 'Years of available data' = Years)
  
  # Render table in tab
  output$data_table <- renderDT({
    datatable(
      table_data,
      options = list(
        scrollY = "460px",
        scrollCollapse = TRUE,
        paging = TRUE
      )
    )
  })
  
}