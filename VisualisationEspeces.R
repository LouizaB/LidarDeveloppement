library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  
  titlePanel("Tree Inventory Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a CSV file"),
      tags$hr(),
      checkboxInput("tree_plot", "Show Tree Plot", value = TRUE),
      checkboxInput("height_hist", "Show Height Histogram", value = TRUE),
      checkboxInput("species_boxplot", "Show Species Boxplot", value = TRUE)
    ),
    
    mainPanel(
      plotOutput("treePlot"),
      plotOutput("heightHist"),
      plotOutput("speciesBoxplot"),
      plotOutput("rasterPlot"),
      uiOutput("additionalPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$file, {
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    
    # Tree plot
    output$treePlot <- renderPlot({
      req(input$file)
      req(input$tree_plot)
      if (input$tree_plot) {
        ggplot(data, aes(x = x, y = y, group = s)) +
          geom_point(aes(color = s, size = d)) +
          coord_sf(datum = 2154) +
          scale_color_manual(values = rainbow(length(unique(data$s)))) +
          scale_radius(name = "Diameter") +
          geom_text(aes(label = n, size = 20), hjust = 0, vjust = 1) +
          labs(color = "Species")
      }
    })
    
    # Height histogram
    output$heightHist <- renderPlot({
      req(input$file)
      req(input$height_hist)
      if (input$height_hist) {
        hist(data$h, xlab = "Height", main = "Height Histogram", col = "lightblue")
      }
    })
    
    # Species boxplot
    output$speciesBoxplot <- renderPlot({
      req(input$file)
      req(input$species_boxplot)
      if (input$species_boxplot) {
        ggplot(data, aes(x = s, y = h)) +
          geom_boxplot(fill = "lightgreen") +
          labs(x = "Species", y = "Height", title = "Species Boxplot")
      }
    })
    
    # Raster plot
    output$rasterPlot <- renderPlot({
      req(input$file)
      if (input$file) {
        # Your code to generate and plot the raster goes here
        # For example:
        output_raster <- terra::rast(resolution = 0.5,
                                     xmin = ROI_range$xmin - ROI_buffer,
                                     xmax = ROI_range$xmax + ROI_buffer,
                                     ymin = ROI_range$ymin - ROI_buffer,
                                     ymax = ROI_range$ymax + ROI_buffer,
                                     crs = sf::st_crs(las_chablais3)$wkt)
        plot(output_raster)
      }
    })
  })
  
  output$additionalPlot <- renderUI({
    req(input$file)
    tagList(
      tags$iframe(src = "additional_plot.html", height = "500px", width = "100%")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
