ui <- fluidPage(
  
  # Title
  titlePanel("Volcano Plot"),
  
  # Creates sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for intputs ----
    sidebarPanel(
      
      # Stores excel data file
      fileInput("data", h4("Data File")),
      
      # Stores number of groups in experiment
      numericInput("num_groups", h5("Number of Groups"), value=1, min=1),
      
      # Stores number of replicates per group
      numericInput("num_replicates", h5("Replicates per Group"), value=1, min=1),
      
      # Updates user inputs
      submitButton("Refresh", icon("refresh"))
    ),
    
    # Main panel for displaying outputs  ----
    mainPanel(
      
      # Sets up different tabs
      tabsetPanel(type="tabs",
                  
                  # Tab for inputing TMT Labels 
                  tabPanel("TMT Label Map",
                           uiOutput("tmt")),
                  
                  
                  # Tab displaying Volcano Plot
                  tabPanel("Raw Data",
                           dataTableOutput("data_tidy")),
                  
                  tabPanel("Plot3",
                           textOutput("samples")))
    )
  )
)

# Defines server logic  ----
server <- function(input, output) 
{
  # Determines plex size of experiment ---- 
  plex <- reactive({
    as.numeric(input$num_groups) * as.numeric(input$num_replicates)
  })
  
  output$plex <- renderText({
    paste("Plex is", plex())
  })
  
  
  
  # Produces prompts to enter TMT mapping  ----
  samples <- reactive({
    samples <- tagList()
    samples[[1]] <- tagList()
    samples[[1]][[1]] <- textInput("c", paste("Sample",3,"Name: "))
    samples[[1]][[2]] <- textInput("e", paste("Sample",3,"Label: "))
    samples

  })
  
  output$tmt <- renderUI({
    samples()
  })
  
  output$samples <- renderText({
    input[["c"]]
  })
  
  # Replaces missing values in dataset with 0  ----
  data_rm0 <- reactive({
    input$data$datapath %>%
      read_excel() %>%
      mutate_all(funs(replace(.,is.na(.),0)))
  })
  
  
  
  
  
  
  
  
  
  output$data_tidy <- renderDataTable({
    data_rm0()
  })
  
  
  
  
  
  
  
  
  
}


# Runs the app  ---
shinyApp(ui=ui, server=server)