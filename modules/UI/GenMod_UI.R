#----------------Module: UI ---------------------------

# Guane: Shiny app for phylogenetic comparative methods
# Author: Viviana Romero 
# Creation Date: July 7 2023 
# Generic modules

#----------------Module: UI ---------------------------

library(shiny)
# Button Reset panels
resetPanelsUI <- function(id) {
  tagList(
    actionButton(inputId = NS(id, "Resetpanels"),
                 width = "2%",shiny::icon("gear")) ,br(),
  )
}

# Read phylogeny

PhyloInputUI <- function(id){
  tagList(
    fileInput(inputId = NS(id, "loadTree"),
              label =  strong("Load Tree"),
              multiple = F,
              accept = c(".tre",".tree",".phy",".nex", ".nexus"),
              width = "100%"))
}

#Read CSV

csvInputUI <- function(id){
  tagList(
    fileInput(inputId = NS(id, "loadCsv"),
              label =  strong("Load csv"),
              multiple = F,
              accept = c(".csv",".txt"),
              width = "100%"))
}


# Button check tree and csv names 


checkNamesUI <- function(id){
  tagList(
    checkboxInput(
      inputId = NS(id, "checkNames"),
      label = "Check tree & csv data names", 
      value = FALSE
    )
  )
} 


#Display Info

infoUI <- function(id){
  tagList(
    verbatimTextOutput(NS(id, "InfoFunc"))
  )
}

#Plot in Data Panel

plotUI <- function(id){
  plotOutput(outputId = NS(id, "plotFunc"))
}

