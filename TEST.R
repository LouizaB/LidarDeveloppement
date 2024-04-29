library(shiny)
library(shinythemes)
library(shinyWidgets)
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
                               actionButton("load_las", "Visualiser LAS"),
                               selectInput("filtering", "Techniques de filtrage :", 
                                           choices = c("Aucune","HNT", "CHM", "TDM"), selected = "HNT"),
                               selectInput("segmentation", "Techniques de segmentation de données :", 
                                           choices = c("Aucune","Segmentation daplont", "Segmentation silva", "CNN"), selected = "Segmentation daplont"),
                               selectInput("normalization", "Techniques de normalisation :", 
                                           choices = c("Aucune","Homogène", "Aléatoire", "Elévation"), selected = "Homogène"),
                               actionButton("update", "Appliquer")
                             ),
                             mainPanel(
                               plotOutput("chm_plot"),
                               plotOutput("segments_plot"),
                               plotOutput("apices_plot"),
                               rglwidgetOutput("rgl_widget"),
                               plotOutput("imagePlot")
                             )
                           )
                  ), # tabPanel
                  tabPanel("Navbar 2", "This panel is intentionally left blank"),
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                ) # navbarPage
) # fluidPage

# Define server function  
server <- function(input, output, session) {
  
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
} # server

# Create Shiny object
shinyApp(ui = ui, server = server)
