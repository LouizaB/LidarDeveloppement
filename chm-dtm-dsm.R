library(shiny)
library(lidR)
library(sf)

# Définition de l'interface utilisateur
ui <- fluidPage(
  titlePanel("Visualisation des images LiDAR"),
  
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
)

# Définition du serveur
server <- function(input, output) {
  observeEvent(input$show, {
    req(input$file)
    
    # Charger le fichier LAS
    las <- readLAS(input$file$datapath)
    
    # Obtenir le CRS du fichier LAS
    las_crs <- st_crs(las)
    
    # Définir le CRS du fichier LAS
    st_crs(las) <- st_crs(output_raster)
    
    # Appliquer la technique de filtrage choisie
    output_image <- switch(input$filter,
                           "dsm" = plot(rasterize_canopy(las, output_raster, p2r())),
                           "dtm" = plot(rasterize_terrain(las, output_raster, tin())),
                           "chm" = plot(rasterize_canopy(las, output_raster, p2r()) - rasterize_terrain(las, output_raster, tin()))
    )
    
    output$image <- renderPlot({
      output_image
    })
  })
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)
