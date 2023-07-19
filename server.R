# Guane: a shiny app for phylogenetic comparative methods
# Author: Viviana Romero Alarcon
# Creation date: July 7 2023
# Server File


shinyServer(function(input, output,session) {
 
  ######################################################################
  # Data panel: Ancestral State Reconstruction: Discrete Characters    #
  ######################################################################

  source("Server/Ancestral_State_reconstruction/Dicrete_Character/ANC_Data.R", local = T)

  ######################################################################
  # Data panel: Ancestral State Reconstruction: Discrete Characters    #
  ######################################################################


  # R code
  output$knitDoc <- renderUI({
    input$eval
    HTML(knitr::knit2html(text = isolate(input$rmd), quiet = TRUE,force_v1 = T))
  })
  


#Finish server
})








