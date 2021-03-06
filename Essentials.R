# Brandon Ng
#
# Script for downloading and installing packages


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
