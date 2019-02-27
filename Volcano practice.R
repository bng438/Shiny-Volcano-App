# Brandon Ng
#
# Script for transforming exported data into volcano plot


# Installs Packages  ------------------------------
packages <- c("plyr", "tidyverse", "plotly", 
              "gridExtra", "ggseqlogo",
              "RDocumentation", "runjags", "pracma", "shiny")

for (i in seq_along(packages))
{
  if(!requireNamespace(packages[i]))
  {
    install.packages(packages[i])
  }
}


# Clears Global Workspace  ------------------------------
rm(list = ls())


# Loads Packages  ------------------------------
library(plyr)
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggseqlogo)
library(runjags)
library(pracma)
library(base)
library(plotly)
library(shiny)


# Imports Data   ------------------------------
dat <- read_excel("Copy.xlsx")


p1 <- plot_ly(x=dat[[2]], y=dat[[5]],
              text = dat[[1]]) %>%
  add_markers(symbol=I(1)) %>%
  layout(xaxis=list(range=c(-2,2)), yaxis=list(range=c(0,5.2)))


p2 <- plot_ly(x=dat[[3]], y=dat[[6]], 
              type="scatter",
              mode="markers",
              text = dat[[1]]) %>%
  layout(xaxis=list(range=c(-2,2)), yaxis=list(range=c(0,5.2)))


p3 <- plot_ly(x=dat[[4]], y=dat[[7]], 
              type="scatter",
              mode="markers",
              text = dat[[1]]) %>%
  layout(xaxis=list(range=c(-2,2)), yaxis=list(range=c(0,5.2)))


plotList <- function(nplot) 
{
  if (nplot != 0)
  {
    subplot(,plotList(nplot))
  }
  else
  {
    plot_ly()
  }
}


for (i in 1:((ncol(dat)-1)/2))
{
  plot_ly(x=dat[[i+1]], y=dat[[i+4]],
          text=dat[[1]]) %>%
    add_markers(symbol=I(1)) %>%
    layout(xaxis=list(range=c(-2.2)), yaxis=list(range=c(0,5.2)))
}










volcano <- reactive({
  dat <- data_rm_nan()
  og_data <- data_rm0()
  dat2 <- cbind(og_data["Description"], dat)
  
  p1 <- plot_ly(x=dat2[[2]], y=dat2[[5]],
                text = dat2[[1]]) %>%
    add_markers(symbol=I(1)) %>%
    layout(xaxis=list(range=c(-2,2)), yaxis=list(range=c(0,5.2)))
  
  p2 <- plot_ly(x=dat2[[3]], y=dat2[[6]],
                text = dat2[[1]]) %>%
    add_markers(symbol=I(1)) %>%
    layout(xaxis=list(range=c(-2,2)), yaxis=list(range=c(0,5.2)))
  
  p3 <- plot_ly(x=dat2[[4]], y=dat2[[7]],
                text = dat2[[1]]) %>%
    add_markers(symbol=I(1)) %>%
    layout(xaxis=list(range=c(-2,2)), yaxis=list(range=c(0,5.2)))
  
  subplot(p1,p2,p3)
  
})




