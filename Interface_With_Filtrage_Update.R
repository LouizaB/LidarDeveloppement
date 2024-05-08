# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                tags$head(
                  tags$style(
                    HTML(
                      "
                      .nav-tabs > li > a {
                        padding: 10px 15px; /* Ajuster le rembourrage des onglets */
                        width: 150px; /* Largeur fixe des onglets */
                        text-align: center; /* Centrer le texte des onglets */
                      }
                      .tab-content {
                        height: 600px; /* Hauteur fixe du contenu des onglets */
                      }
                      .sidebarPanel {
                        width: 300px; /* Largeur fixe du sidebarPanel */
                      }
                      "
                    )
                  )
                ),
                navbarPage(
                  "Analyse des données LiDAR",
                  tabPanel("Visualisation LAS",
                           titlePanel("Visualisation"),
                           sidebarLayout(
                             sidebarPanel(
                               fileInput("file", "Sélectionner le fichier LAS"),
                               actionButton("load_las", "Visualiser LAS")
                             ),
                             mainPanel(
                               plotOutput("chm_plot"),
                               plotOutput("apices_plot"),
                               rglwidgetOutput("rgl_widget"),
                               imageOutput("imagePlot")
                             )
                           )
                  ),
                  
                  tabPanel("Position",
                           mainPanel(
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
                             ),
                             style = "width: 900px;"
                           )
                  ),
                  tabPanel("Filtrage",
                           titlePanel("CHM Viewer"),
                           sidebarLayout(
                             sidebarPanel(
                               fileInput("file_filtrage", "Choose LAS file"),
                               selectInput("algorithm_filtrage", "Choose Algorithm",
                                           choices = c("p2r", "p2r(subcircle)", "dsmtin", "lidR::pitfree")),
                               sliderInput("resolution_filtrage", "Resolution (m)", min = 0.1, max = 1, value = 0.5),
                               conditionalPanel(
                                 condition = "input.algorithm_filtrage == 'p2r(subcircle)'",
                                 sliderInput("subcircle_filtrage", "Subcircle Radius (m)", min = 0.05, max = 0.5, value = 0.15)
                               ),
                               conditionalPanel(
                                 condition = "input.algorithm_filtrage == 'lidR::pitfree'",
                                 sliderInput("param1_filtrage", "Param1", min = 0.1, max = 1, value = 0.5),
                                 sliderInput("param2_filtrage", "Param2", min = 0.1, max = 1, value = 0.5)
                               )
                             ),
                             mainPanel(
                               plotOutput("plotCHM_filtrage")
                             )
                           )
                  ),
                  
                  tabPanel("Normalisation",
                           titlePanel("Interactive LAS Decimation Tool"),
                           sidebarLayout(
                             sidebarPanel(
                               fileInput("lasfile", "Upload LAS File (.laz)"),
                               selectInput("algorithm", "Select Algorithm", 
                                           choices = c("None" = "", "random" = "random", "homogenize" = "homogenize", 
                                                       "highest" = "highest", "lowest" = "lowest", "random_per_voxel" = "random_per_voxel")),
                               uiOutput("dynamicUI"),  # Dynamic UI for input parameters
                               actionButton("updatePlot", "Update Plot")
                             ),
                             mainPanel(
                               plotOutput("plotOutput")
                             )
                           )
                  ),
                  
                  
                  tabPanel("Coloriage",
                           mainPanel(
                             imageOutput("test")
                           )
                  )
                  
                ))

# Define server function  
server <- function(input, output, session) {
  
  # Partie pour le filtrage
  observeEvent(input$file_filtrage, {
    las <- lidR::readLAS(input$file_filtrage$datapath)
    output$plotCHM_filtrage <- renderPlot({
      if (input$algorithm_filtrage == "lidR::pitfree" && (is.null(input$param1_filtrage) || is.null(input$param2_filtrage))) {
        # Gérer le cas où les paramètres pour lidR::pitfree ne sont pas définis
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "")
        text(0.5, 0.5, "Please select parameters for lidR::pitfree", cex = 2)
      } else {
        chm <- rasterize_canopy(las, 
                                res = input$resolution_filtrage, 
                                algorithm = switch(input$algorithm_filtrage,
                                                   "p2r" = p2r(),
                                                   "p2r(subcircle)" = p2r(subcircle = input$subcircle_filtrage),
                                                   "dsmtin" = dsmtin(max_edge = input$max_edge_filtrage),
                                                   "lidR::pitfree" = pitfree(max_edge = c(input$param1_filtrage, input$param2_filtrage))))
        plot(chm, main = "Canopy Height Model")
      }
    })
  })
  
  
  # Partie pour l'affichage d'une image
  output$test <- renderImage({
    list(src = "./plot_before_coloring.png",
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  # Partie pour la visualisation LAS
  observeEvent(input$load_las, {
    req(input$file)
    las <- lidR::readLAS(input$file$datapath)
    chm <- lidR::rasterize_canopy(las, res = 0.5)
    segms <- lidaRtRee::tree_segmentation(chm)
    apices <- lidaRtRee::tree_extraction(segms)
    output$chm_plot <- renderPlot({
      plot(chm, main = "Canopy Height Model")
    })
    output$segments_plot <- renderPlot({
      plot(segms$segments_id, main = "Segments")
    })
    output$apices_plot <- renderPlot({
      plot(apices$meanI, main = "Apices")
    })
    output$rgl_widget <- renderRglwidget({
      rglwidget(rgl::plot3d(las))
    })
    output$coloredImage <- renderImage({
      img <- readPNG("./plot_before_coloring.png")
      list(src = img, contentType = "image/png", width = "100%")
    }, deleteFile = FALSE)
  })
  
  #Partie position
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

  ###############################
  
  
  # Partie pour la normalisation
  reactive_las <- reactive({
    req(input$lasfile)
    readLAS(input$lasfile$datapath, select = "xyz")
  })
  
  output$dynamicUI <- renderUI({
    if (input$algorithm == "random" || input$algorithm == "homogenize") {
      numericInput("param", "Parameter", value = if (input$algorithm == "random") 10 else 5)
    } else if (input$algorithm %in% c("highest", "lowest", "random_per_voxel")) {
      numericInput("param", "Parameter", value = 1)
    }
  })
  
  observeEvent(input$updatePlot, {
    req(reactive_las(), input$algorithm, input$param)
    las <- reactive_las()
    algorithm <- switch(input$algorithm,
                        "random" = random(input$param),
                        "homogenize" = homogenize(input$param),
                        "highest" = highest(input$param),
                        "lowest" = lowest(input$param),
                        "random_per_voxel" = random_per_voxel(input$param),
                        stop("Please select a valid algorithm"))
    decimated_las <- decimate_points(las, algorithm)
    output$plotOutput <- renderPlot({
      plot(decimated_las)
    })
  }, ignoreInit = TRUE)
}

# Create Shiny object
shinyApp(ui = ui, server = server)
