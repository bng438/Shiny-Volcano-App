# Brandon Ng
#
# Script for transforming exported data into volcano plot


# Installs Packages  ------------------------------
packages <- c("plyr", "tidyverse", "plotly", "pheatmap", 
              "gridExtra", "VennDiagram", "ggseqlogo",
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
library(ggplot2)
library(readxl)
library(gplots)
library(pheatmap)
library(gridExtra)
library(VennDiagram)
library(ggseqlogo)
library(runjags)
library(pracma)
library(base)
library(plotly)
library(shiny)


# Imports Data   ------------------------------
dat <- read_excel("Volcano practice dataset.xlsx")


p1 <- plot_ly(x=dat[[2]], y=dat[[5]],
              text = dat[[1]]) %>%
  add_markers(symbol=I(1)) %>%
  layout(xaxis=list(range=c(-2,2)), yaxis=list(range=c(0,5.2)))


p2 <- plot_ly(x=dat[[4]], y=dat[[7]], 
              type="scatter",
              mode="markers",
              text = dat[[1]]) %>%
  layout(xaxis=list(range=c(-2,2)), yaxis=list(range=c(0,5.2)))


