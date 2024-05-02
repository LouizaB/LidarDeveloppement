library(shiny)
library(terra)

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
    if (is.null(inFile)) {
      return(NULL)
    }
    las <- readLAS(inFile$datapath)
    
    # Compute DTM, DSM, and CHM
    dtm <- rasterize_terrain(las, output_raster, tin())
    dsm <- rasterize_canopy(las, output_raster, p2r())
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
