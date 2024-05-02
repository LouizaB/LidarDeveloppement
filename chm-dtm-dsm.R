library(shiny)
library(terra)
library(lidR)

# Define UI
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

# Define server logic
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
}
# Run the application
shinyApp(ui = ui, server = server)
