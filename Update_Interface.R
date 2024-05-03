library(shiny)
library(shinythemes)
library(shinyWidgets)
library(lidaRtRee)
library(rgl)
library(terra)
library(sf)
library(ade4)
library(ggplot2)
# Define UI
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
                             # Paste the UI code here
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
                             # Ajuster la largeur du sidebarPanel
                             style = "width: 900px;"
                           )
                  ),
                  tabPanel("Filtrage",
                           titlePanel("Filtrage"),
                           sidebarLayout(
                             sidebarPanel(
                               fileInput("file", "Choisir le fichier Laz"),
                               selectInput("filter", "Choisir la technique de filtrage",
                                           choices = c("CHM" = "chm", "DTM" = "dtm", "DSM" = "dsm")),
                               actionButton("show", "Afficher l'image")
                             ),
                             mainPanel(
                               plotOutput("image")
                             )
                           )
                  ),
                  tabPanel("Normalisation",
                           mainPanel(
                             
                           )
                  ),
                  
                  
                  tabPanel("Coloriage",
                           mainPanel(
                             # Use imageOutput to place the image on the page
                             imageOutput("test")
                           )
                  ),
                  
                )
)

# Define server function  
server <- function(input, output, session) {
  observeEvent(input$show, {
    inFile <- input$file
    print(inFile) # Debugging
    if (is.null(inFile)) {
      print("File is null") # Debugging
      return(NULL)
    }
    print("Reading LAS file") # Debugging
    las <- lidR::readLAS(inFile$datapath)
    
    # Define output_raster object
    output_raster <- terra::rast(resolution = 0.5,
                                 xmin = min(las$X),
                                 xmax = max(las$X),
                                 ymin = min(las$Y),
                                 ymax = max(las$Y),
                                 crs = st_crs(las)$wkt)
    
    # Compute DTM, DSM, and CHM
    print("Computing DTM") # Debugging
    dtm <- rasterize_terrain(las, output_raster, tin())
    print("Computing DSM") # Debugging
    dsm <- rasterize_canopy(las, output_raster, p2r())
    print("Computing CHM") # Debugging
    chm <- dsm - dtm
    
    # Plot selected image
    output$image <- renderPlot({
      switch(input$filter,
             "chm" = plot(chm, main = "Canopy Height Model"),
             "dtm" = plot(dtm, main = "Digital Terrain Model"),
             "dsm" = plot(dsm, main = "Digital Surface Model"))
    })
  })
  
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
  
  output$test <- renderImage({
    # Return a list containing the filename
    list(src = "./plot_before_coloring.png",
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  observeEvent(input$load_las, {
    req(input$file)
    # Lire le fichier LAS
    las <- lidR::readLAS(input$file$datapath)
    
    # Calculer le CHM
    chm <- lidR::rasterize_canopy(las, res = 0.5)
    
    # Calculer les segments
    segms <- lidaRtRee::tree_segmentation(chm)
    
    # Calculer les apices
    apices <- lidaRtRee::tree_extraction(segms)
    
    # Plot du CHM
    output$chm_plot <- renderPlot({
      plot(chm, main = "Modèle de hauteur du couvert")
    })
    
    # Plot des segments
    output$segments_plot <- renderPlot({
      plot(segms$segments_id, main = "Segments")
    })
    
    # Plot des apices
    output$apices_plot <- renderPlot({
      plot(apices$meanI, main = "Apices")
    })
    
    # Plot en 3D
    output$rgl_widget <- renderRglwidget({
      rglwidget(rgl::plot3d(las))
    })
    
    # Charger l'image pour l'onglet "Coloriage"
    output$coloredImage <- renderImage({
      # Charger l'image
      img <- readPNG("./plot_before_coloring.png")
      
      # Retourner les informations de l'image
      list(src = img, contentType = "image/png", width = "100%")
    }, deleteFile = FALSE)
  })
  
  output$imagePlot <- renderPlot({
    # Récupérer les valeurs sélectionnées par l'utilisateur
    filtering <- input$filtering
    segmentation <- input$segmentation
    normalization <- input$normalization
    
    # Utiliser les valeurs sélectionnées pour afficher l'image correspondante
    # Ici, on suppose que vous avez déjà des fonctions ou des méthodes pour visualiser les images
    # Utilisez les valeurs de filtering, segmentation et normalization pour appeler ces fonctions/méthodes avec les paramètres appropriés
    # Par exemple :
    # image <- visualiser_image(filtering, segmentation, normalization)
    # plot(image)
    # Remplacez visualiser_image() par vos fonctions/méthodes réelles pour charger et visualiser les images
    # Assurez-vous que vos fonctions/méthodes prennent les options de filtrage, segmentation et normalisation comme paramètres
    
    # Pour cet exemple, nous affichons juste un message indiquant les options sélectionnées
    message <- paste("Technique de filtrage:", filtering, "\n",
                     "Technique de segmentation:", segmentation, "\n",
                     "Technique de normalisation:", normalization)
    cat(message)
  })
  
  # Définition des fonctions tdh, chm et hnt
  dtm <- function(las_chablais3) {
    # Compute terrain model
    dtm <- lidR::rasterize_terrain(las_chablais3, output_raster, lidR::tin())
    return(dtm)
  }
  
  dsm <- function(las_chablais3) {
    # Compute surface model
    dsm <- lidR::rasterize_canopy(las_chablais3, output_raster, lidR::p2r())
    return(dsm)
  }
  
  chm <- function(las_chablais3) {
    # Compute terrain model
    dtm <- lidR::rasterize_terrain(las_chablais3, output_raster, lidR::tin())
    # Compute surface model
    dsm <- lidR::rasterize_canopy(las_chablais3, output_raster, lidR::p2r())
    # Compute canopy height model
    chm <- dsm - dtm
    return(chm)
  }
  
  observeEvent(input$update, {
    chosenFilter <- input$filtering
    chosenSegmentation <- input$segmentation
    chosenNormalization <- input$normalization
    
    if (chosenFilter != "Aucune") {
      if (chosenFilter == "dsm") {
        result <- dtm(las_chablais3)
      } else if (chosenFilter == "dtm") {
        result <- chm(las_chablais3)
      } else if (chosenFilter == "chm") {
        result <- dsm(las_chablais3)
      }
    }
    
    if (chosenSegmentation != "Aucune") {
      if (chosenFilter == "") {
        result <- dtm(las_chablais3)
      } else if (chosenFilter == "") {
        result <- chm(las_chablais3)
      } else if (chosenFilter == "") {
        result <- dsm(las_chablais3)
      }
    }
    
    if (chosenNormalization != "Aucune") {
      if (chosenFilter == "") {
        result <- dtm(las_chablais3)
      } else if (chosenFilter == "") {
        result <- chm(las_chablais3)
      } else if (chosenFilter == "") {
        result <- dsm(las_chablais3)
      }
    }
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)