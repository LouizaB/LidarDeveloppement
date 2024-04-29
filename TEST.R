# Load R packages
library(shiny)
library(shinythemes)
library(lidaRtRee)
library(rgl)
library(terra)
library(sf)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  "Analyse des données LiDAR",
                  tabPanel("Visualisation LAS",
                           sidebarLayout(
                             sidebarPanel(
                               fileInput("file", "Sélectionner le fichier LAS"),
                               actionButton("load_las", "Visualiser LAS")
                             ),
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("CHM", plotOutput("chm_plot")),
                                 tabPanel("Segments", plotOutput("segments_plot")),
                                 tabPanel("Apices", plotOutput("apices_plot")),
                                 tabPanel("Projection 3D", rglwidgetOutput("rgl_widget"))
                               )
                             )
                           )
                  ), # tabPanel
                  tabPanel("Navbar 2", "This panel is intentionally left blank"),
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                ) # navbarPage
) # fluidPage

# Define server function  
server <- function(input, output) {
  
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
  })
} # server

# Create Shiny object
shinyApp(ui = ui, server = server)
