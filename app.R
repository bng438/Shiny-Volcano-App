# Brandon Ng
#
# Script for Shiny Volcano App


# Installs Packages  ----
packages <- c("plyr", "tidyverse", "plotly", 
              "gridExtra", "ggseqlogo", "DT",
              "RDocumentation", "runjags", "pracma",
              "shiny", "data.table")

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
library(data.table)


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


# Fold-change function  ----
foldChange <- function(dt, grp1, grp2)
{
  x <- dt[grp1] %>% unlist %>% as.numeric() %>% mean()
  y <- dt[grp2] %>% unlist %>% as.numeric() %>% mean()
  
  fold_change <- (x / y)
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
      # Applies the specified function func to all rows in the
      # two groups currently being compared
      result[(length(result) + 1)] <-
        adply(.data=dat, .margins=1, .fun=get(func),
              grp1=c(i : (i + num_repl - 1)),
              grp2=c(j : (j + num_repl - 1))) %>%
        as_tibble() %>% select("V1")
      
      # Determines column heading depending on function specified
      if (strcmp(func, "pVal"))
      {
        func_heading <- "Pval"
      }
      else 
      {
        func_heading <- "Fold-change"
      }
      
      # Renames column heading
      names(result)[length(result)] <-
        paste(func_heading,
              substr(names(dat[i]), 1, nchar(names(dat[i]))-1),
              "vs",
              substr(names(dat[j]), 1, nchar(names(dat[j]))-1))
    }
  }
  return(select(result,-1))
}


# Identifies NAN error values  ----
is.nan.data.frame <- function(x)
{
  do.call(cbind, lapply(x,is.nan))
}


# Replaces NAN error values with 0  ----
removeNan <- function(dat)
{
  dat[is.nan(dat)] <- 0
  return(dat)
}


# Merges datasets such that corresponding columns are next to each other  ----
# I.e. 1st column of dat2 is next to 1st column of dat1, etc...
merge <- function(dat1, dat2)
{
  resorted <- as.data.frame(matrix(0,nrow(dat1)))
  for (i in 1:ncol(dat1))
  {
    resorted[(length(resorted)+1)] <- dat1[i]
    resorted[(length(resorted)+1)] <- dat2[i]
  }
  return(select(resorted,-1))
}


# Rounds values to desired number of decimal places  ----
roundValues <- function(dat,x)
{
  mutate_all(dat, round, x)
}



# Defines UI  -----
ui <- fluidPage(
  
  # Title
  titlePanel("TMT Statistical Analysis"),
  
  # Creates sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for intputs ----
    sidebarPanel(
      
      # Stores excel data file
      fileInput("data", h4("Data File")),
      
      # Stores tmt label map
      fileInput("tmt", h4("Tmt Map")),
      
      # Stores number of groups in experiment
      numericInput("num_groups", h5("Number of Groups"),value=3,min=1),
      
      # Stores number of replicates per group
      numericInput("num_replicates", h5("Replicates per Group"),value=3,min=1,max=9),
      
      # Stores p-val threshold
      numericInput("pval_threshold", h5("Pval Threshold"), value=.05, min=0),
      
      # Stores fold-change threshold
      numericInput("fc_threshold", h5("Fold-change Threshold"), value=1.414, min=0),
      
      # Text box detailing limitations to script
      helpText("Data file should not contain abundance ratios")
    ),
    
    # Main panel for displaying outputs  ----
    mainPanel(
      
      # Sets up different tabs
      tabsetPanel(type="tabs",
                  
                  # Displays raw protein abundance values
                  tabPanel("Raw Data",
                           dataTableOutput("data_tidy")),
                  
                  # Displays normalized protein abundance values
                  tabPanel("Normalized",
                           dataTableOutput("data_normalized")),
                  
                  # Displays fold-change and pvals for selected comparison group
                  tabPanel("Fold-change & Pval",
                           uiOutput("comparison_group"),
                           dataTableOutput("comparison_group_data")),
                  
                  # Displays volcano plot for all combination of groups
                  tabPanel("Volcano",
                           plotlyOutput("volcano")),
                  
                  # Displays significant proteins
                  tabPanel("Significant Proteins",
                           uiOutput("sig_comparison_group"),
                           dataTableOutput("sig_protein_data")),
                  
                  # Displays miscellaneous statistical values
                  tabPanel("Misc",
                           dataTableOutput("percent_median"))
      )
      
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
  
  
  # Gets sample names and labels user provided  ----
  getSamples <- reactive({
    samples <- input$tmt$datapath %>% read_excel()
    as.data.frame(samples)
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
      for (j in 1:nrow(samples))
      {
        if (grepl(samples[j,2], names(dat)[i], ignore.case=TRUE))
        {
          names(dat)[i] <- samples[j,1]
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
    
    for (i in 1:nrow(samples))
    {
      names(selected)[i] <- samples[i,1]
      selected[i] <- dat[samples[i,1]]
    }
    selected
  })
  
  
  # Normalizes data about median  ----
  data_normalized <- reactive({
    dat <- data_selected()
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
  
  
  # Calculates negative log10 of p-values  ----
  data_log_pval <- reactive({ 
    -log10(data_pval())
  })
  
  
  # Calculates fold-change among all groups  ----
  data_fc <- reactive({
    toAllGroups(data_normalized(), input$num_replicates, "foldChange")
  })
  
  
  # Calculates log2 of fold-change  ----
  data_log_fc <- reactive({
    log2(data_fc())
  })
  
  
  # Creates volcano plot  ----
  volcano <- reactive({
    dat <- cbind(data_log_fc(),data_log_pval()) %>% removeNan()
    og_data <- data_rm0()
    pval <- -log10(input$pval_threshold)
    fc <- log2(input$fc_threshold)
    
    # Determines number of group comparisons
    num_comparisons <- ncol(dat) / 2
    
    # Attaches protein descriptions to the merged dataset
    # which contains fold-change and pval
    dat <- cbind(og_data["Description"], dat)
    
    
    # Creates individual volcano plot  ----
    createPlot <- function(index)
    {
      # plot_ly: Plots fold-change on x-axis and pval on y-axis
      # text: Specifies that protein description appear when a 
      #       datapoint is hovered over
      # name: Specifies the name that appears on the legend for
      #       that specific dataset
      plot_ly(x=dat[[index+1]], 
              y=dat[[index+num_comparisons+1]],
              text=dat[[1]],
              name=substr(names(dat)[index+1],12,nchar(names(dat)[index+1]))) %>%
        
        # Specifies that each datapoint is a hollow circle
        add_markers(symbol=I(1)) %>%
        
        # Creates a horiztonal line layered over the origianl
        # plot, representing the p-val threshold
        add_lines(y=pval,
                  line=list(color="pink"),
                  showlegend=FALSE,
                  hoverinfo= "text",
                  text="P-val Threshold") %>%
        
        # Creates a vertical line layered over the original
        # plot, representing positive fold-change threshold
        add_lines(x=fc,
                  line=list(color="pink"),
                  showlegend=FALSE,
                  hoverinfo="text",
                  text="Fold-change Threshold") %>%
        
        # Creates a vertical line layered over the original
        # plot, representing negative fold-change threshold
        add_lines(x=-fc,
                  line=list(color="pink"),
                  showlegend=FALSE,
                  hoverinfo="text",
                  text="Fold-change Threshold") %>%
        
        # Specifies x and y axis titles
        layout(xaxis=list(title="Log2 Fold-change"),
               yaxis=list(title="-Log10 p-val"))
    }
    
    # Creates a list of plots for group comparisons
    plotList <- function(nplot) 
    {
      lapply(seq_len(nplot), createPlot)
    }
    
    # Creates single plot containing all group comparison plots
    subplot(plotList(num_comparisons),
            shareX=TRUE,
            shareY=TRUE,
            titleX=TRUE,
            titleY=TRUE)
  })
  
  
  # Determines names of all comparison groups  ----
  getComparisonNames <- reactive({
    names <- colnames(data_fc())
    # Removes "fold-change" originally found in column names
    names <- lapply(names, function(x) {substr(x,12,nchar(x))})
    names
  })
  
  
  # Produces prompt to select which comparison group to view  ----
  comparison_group_prompt <- reactive({
    names <- cbind(getComparisonNames(),"all")
    selectInput("compare_group","Comparison Group:",
                choices=names)
  })
  
  
  # Produces prompt to select which comparison group to view w/o "all" option ----
  sig_comparison_prompt <- reactive({
    names <- getComparisonNames()
    selectInput("sig_compare_group","Comparison Group:",
                choices=names)
  })
  
  
  # Produces data table of pval & fc of selected comparison group  ----
  comparison_group_data <- reactive({
    
    # Selected comparison group
    comp_grp <- input$compare_group
    
    # Data set containing protein description
    raw_data <- data_rm0()
    
    fc_data <- data_fc()
    pval_data <- data_pval()
    
    if (input$compare_group == "all")
    {
      # Merge function puts corresponding pvals next to corresponding fold-changes
      merged <- merge(fc_data,pval_data) %>% removeNan() %>% roundValues(.,4)
      comp_group_data <- cbind(raw_data["Description"],merged)
    }
    else
    {
      # Determines which comparison group user selected
      for (i in 1:ncol(fc_data))
      {
        if (grepl(comp_grp, names(fc_data)[i], ignore.case=TRUE))
        {
          merged <- cbind(fc_data[i],pval_data[i]) %>% removeNan() %>% roundValues(.,4)
          comp_group_data <- cbind(raw_data["Description"],merged)
        }
      }
    }
    comp_group_data
  })
  
  
  # Produces data table of significant proteins  ----
  sig_protein_data <- reactive({
    
    # Selected comparison group
    comp_grp <- input$sig_compare_group
    
    # Data set containing protein description
    raw_data <- data_rm0()
    
    fc_data <- data_fc() %>% removeNan() %>% roundValues(.,4)
    pval_data <- data_pval() %>% removeNan() %>% roundValues(.,4)
    
    # Determines which comparison group user selected
    for (i in 1:ncol(fc_data))
    {
      if (grepl(comp_grp, names(fc_data)[i], ignore.case=TRUE))
      {
        sig_protein <- getSigProt(cbind(raw_data["Description"],fc_data[i],pval_data[i]))
      }
    }
    # write.csv(sig_protein,file=paste(comp_grp,".csv"))
    sig_protein
  })
  
  # Determines proteins beyond pval and fold-change thresholds  ----
  getSigProt <- function(dat)
  {
    fc <- input$fc_threshold
    pval <- input$pval_threshold
    keep <- 0
    
    for (i in 1:nrow(dat))
    {
      if (dat[i,3] < pval & (dat[i,2] > fc | dat[i,2] < 1/fc) & (dat[i,2] != 0))
      {
        keep <- c(keep,i)
      }
    }
    return(dat[keep,])
  }
  
  # Misc.: Determines percent median of all samples  ----
  percent_median <- reactive({
    dat <- data_selected()
    sums <- colSums(dat)
    median <- median(sums)
    percent_median <- median / sums
    as.data.frame(percent_median)
  })
  
  
  # Renders all outputs  ----
  
  # Renders dataset with only protein abundances
  output$data_tidy <- renderDataTable({
    data_selected()
  })
  
  # Renders dataset of protein abundances after being normalized about median
  output$data_normalized <- renderDataTable({
    roundValues(data_normalized(),3)
  })
  
  # Renders prompt to select comparison group
  output$comparison_group <- renderUI({
    comparison_group_prompt()
  })
  
  # Renders table of fold-changes & pvals for selected comparison group
  output$comparison_group_data <- renderDataTable({
    comparison_group_data()
  })
  
  # Renders volcano plots for all combinations of groups
  output$volcano <- renderPlotly({
    volcano()
  })
  
  # Renders prompt to select comparison group for significant protein
  output$sig_comparison_group <- renderUI({
    sig_comparison_prompt()
  })
  
  # Renders table of significant proteins
  output$sig_protein_data <- renderDataTable({
    sig_protein_data()
  })
  
  # Renders table of pecent medians of all samples
  output$percent_median <- renderDataTable({
    percent_median()
  })
  
}


# Runs the app   ----
shinyApp(ui=ui, server=server)