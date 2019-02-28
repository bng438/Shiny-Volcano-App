# Brandon Ng
#
# Script for Shiny Volcano App


# Installs Packages  ----
packages <- c("plyr", "tidyverse", "plotly", 
              "gridExtra", "ggseqlogo", "DT",
              "RDocumentation", "runjags", "pracma", "shiny")

for (i in seq_along(packages))
{
  if(!requireNamespace(packages[i]))
  {
    install.packages(packages[i])
  }
}


# Clears Global Workspace  ----
rm(list = ls())


# Loads Packages  ----
library(plyr)
library(tidyverse)
library(plotly)
library(readxl)
library(gridExtra)
library(ggseqlogo)
library(DT)
library(runjags)
library(pracma)
library(shiny)


# T-test function  ----
pVal <- function(dt, grp1, grp2)
{
  x <- dt[grp1]
  y <- dt[grp2]
  
  # Performs t-test using the mean of x and y
  result <- t.test(x, y, 
                   alternative="two.sided",
                   mu=0, paired=FALSE, var.equal=TRUE)
  
  # Extracts and returns p-values from the results
  return(result$p.value)
}


# Log2 fold-change function  ----
foldChange <- function(dt, grp1, grp2)
{
  x <- dt[grp1] %>% unlist %>% as.numeric() %>% mean() %>% log2()
  y <- dt[grp2] %>% unlist %>% as.numeric() %>% mean() %>% log2()
  
  fold_change <- (x - y)
  return(fold_change)
}



# Applies specified function for all combinations of groups  ----
toAllGroups <- function(dat, num_repl, func)
{
  result <- as.data.frame(matrix(0,nrow(dat)))
  
  for (i in seq(1, (ncol(dat) - (2 * num_repl - 1)), by=num_repl))
  {
    for (j in seq((num_repl + i), (ncol(dat) - num_repl + 1), by=num_repl))
    {
      result[(length(result) + 1)] <-
        adply(.data=dat, .margins=1, .fun=get(func),
              grp1=c(i : (i + num_repl - 1)),
              grp2=c(j : (j + num_repl - 1))) %>%
        as_tibble() %>% select("V1")
      
      # Determines column heading depending on function user inputed
      if (strcmp(func, "pVal"))
      {
        func_heading <- "Pval"
      }
      else 
      {
        func_heading <- "Fold-change"
      }
      
      names(result)[length(result)] <-
        paste(func_heading,
              substr(names(dat[i]), 1, length(names(dat[i]))),
              "vs",
              substr(names(dat[j]), 1, length(names(dat[j]))))
      
    }
  }
  return(select(result, -1))
}


# Removes NAN error values  ----
is.nan.data.frame <- function(x)
{
  do.call(cbind, lapply(x,is.nan))
}




# Defines UI  -----
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
      
      numericInput("pval_threshold", h5("Pval Threshold"), value=.05, min=0),
      numericInput("fc_threshold", h5("Log2 Fold-change Threshold"), value=1, min=0),
      
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
                  
                  tabPanel("Raw Data",
                           dataTableOutput("data_tidy")),
                  
                  tabPanel("Normalized",
                           dataTableOutput("data_normalized")),
                  
                  tabPanel("-Log10 Pval",
                           dataTableOutput("data_log_pval")),
                  
                  tabPanel("Log2 Fold-change",
                           dataTableOutput("data_fc")),
                  
                  tabPanel("Volcano",
                           plotlyOutput("volcano")))
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
  
  
  # Produces prompts to enter TMT mapping  ----
  tmt_prompt <- reactive({
    samples <- tagList()
    for (i in 1:plex())
    {
      samples[[i]] <- tagList()
      samples[[i]][[1]] <- textInput(letters[i], paste("Sample",i,"Name:"))
      samples[[i]][[2]] <- textInput(letters[(i+plex())], paste("Sample",i,"Label:"))
    }
    samples
  })
  
  output$tmt <- renderUI({
    tmt_prompt()
  })
  
  
  # Gets sample name and labels user provided  ----
  getSamples <- reactive({
    samples <- as.data.frame(matrix(0,2,plex()))
    for (i in 1:plex())
    {
      samples[i] <- c(input[[letters[i]]],
                      input[[letters[(i+plex())]]])
    }
    samples
  })
  
  
  # Replaces missing values in dataset with 0  ----
  data_rm0 <- reactive({
    input$data$datapath %>%
      read_excel() %>%
      mutate_all(funs(replace(.,is.na(.),0)))
  })
  
  
  # Renames column names with corresponding sample name  ----
  data_rename <- reactive({
    samples <- getSamples()
    dat <- data_rm0()
    
    for (i in 1:ncol(dat))
    {
      for (j in 1:ncol(samples))
      {
        if (grepl(samples[2,j], names(dat)[i], ignore.case=TRUE))
        {
          names(dat)[i] <- samples[1,j]
        }
      }
    }
    dat
  })
  
  
  # Removes all columns except protein abundances  ----
  data_selected <- reactive({
    samples <- getSamples()
    dat <- data_rename()
    selected <- as.data.frame(matrix(0,nrow(dat),plex()))
    
    for (i in 1:ncol(samples))
    {
      names(selected)[i] <- samples[1,i]
      selected[i] <- dat[samples[1,i]]
    }
    selected
  })
  
  
  # Normalizes data about median  ----
  data_normalized <- reactive({
    dat <- data_selected()
    samples <- getSamples()
    sums <- colSums(dat)
    median <- median(sums)
    percent_median <- median / sums
    data_normalized <- as.data.frame(matrix(0,nrow(dat),plex()))
    
    for(i in 1:ncol(dat))
    {
      data_normalized[i] <- dat[,i] * percent_median[i]
      names(data_normalized)[i] <- names(dat)[i]
    }
    data_normalized
  })
  
  
  # Performs t-test among all groups  ----
  data_pval <- reactive({
    toAllGroups(data_normalized(), input$num_replicates, "pVal")
  })
  
  
  # Calculates negative log of p-values  ----
  data_log_pval <- reactive({ 
    -log10(data_pval())
  })
  
  
  # Calculates Log2 fold-change among all groups  ----
  data_fc <- reactive({
    toAllGroups(data_normalized(), input$num_replicates, "foldChange")
  })
  
  
  # Merges -log2 of pvals and log10 fold-change values together  ----
  data_merged <- reactive({
    data_fc <- data_fc()
    data_log_pval <- data_log_pval()
    cbind(data_fc, data_log_pval)
  })
  
  
  # Replaces NAN error values with 0
  data_rm_nan <- reactive({
    merged <- data_merged()
    merged[is.nan(merged)] <- 0
    merged
  })
  
  
  # Creates volcano plot
  volcano <- reactive({
    dat <- data_rm_nan()
    og_data <- data_rm0()
    num_comparisons <- ncol(dat) / 2
    dat <- cbind(og_data["Description"], dat)

    
    createPlot <- function(index)
    {
      plot_ly(x=dat[[index+1]], 
              y=dat[[index+num_comparisons+1]],
              text=dat[[1]],
              height=1000) %>%
        add_markers(symbol=I(1)) %>%
        layout(xaxis=list(title="Log2 Fold-change"), 
               yaxis=list(title="-Log2 P-val"))
    }
    
    # Recurssive plotting
    plotList <- function(nplot) 
    {
      lapply(seq_len(nplot), createPlot)
    }
    subplot(plotList(num_comparisons),
            nrows=num_comparisons,
            shareX=TRUE, 
            shareY=TRUE,
            titleX=TRUE,
            titleY=TRUE)
  })
  
  
  
  
  # Renders all outputs  ----
  output$data_tidy <- renderDataTable({
    data_selected()
  })
  
  output$data_normalized <- renderDataTable({
    data_normalized()
  })
  
  output$data_log_pval <- renderDataTable({
    data_log_pval()
  })
  
  output$data_fc <- renderDataTable({
    data_fc()
  })
  
  output$volcano <- renderPlotly({
    volcano()
  })
  
  
}


# Runs the app  ---
shinyApp(ui=ui, server=server)