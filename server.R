# Guane: a shiny app for phylogenetic comparative methods
# Author: Viviana Romero Alarcon
# Creation date: July 7 2023
# Server File


function(input, output,session) {
  
######################################################################
# Data panel: Ancestral State Reconstruction: Discrete Characters    #
######################################################################
  

#------------------ Reactive Variables --------------------
  
VarDisCharDT <- reactiveValues()
VarDisCharDT$infobox <- NULL
VarDisCharDT$strbox <- NULL
VarDisCharDT$plotbox <- NULL

#------------------ str/message/error print --------------------

output$infoDisCharDT <- renderPrint({
  req(VarDisCharDT$infobox)
  print(VarDisCharDT$infobox)
})

output$srtDisCharDT <- renderPrint({
  req(VarDisCharDT$strbox)
  print(str(VarDisCharDT$strbox))
})

#------------------ enable/disable/reset --------------------


observeEvent(input$ResetDisCharDT, {
  VarDisCharDT$infobox <<- NULL
  VarDisCharDT$strbox <<- NULL
  VarDisCharDT$plotbox <<- 0
  reset("DisCharDTReset")
})


#------------------ Plot Tree --------------------

output$plotDisCharDT <- renderPlot({
  if(is.null(treeDisCharDT())){
    return()
  }else if (VarDisCharDT$plotbox == 0){
    return()} 
  else{
    return(plot.phylo(treeDisCharDT()))
  }
})



################### Process ############################

#------------------ Read phylogeny --------------------
  
treeDisCharDT <-eventReactive(input$loadTreeDisCharDT,{
  if (input$EgTreeDisCharDT == T){
    readRDS("examples/anoleTree.RDS")
  }else{
    req(input$treeDisCharDT)
    validate(need(input$treeDisCharDT$datapath != "", "Please select a file or active example"))
    if (is.nexus(input$treeDisCharDT$datapath) == T) {
      read.nexus(file = input$treeDisCharDT$datapath)
    } else {
      read.tree(file = input$treeDisCharDT$datapath)
    }
  }
})



# Send Info
observeEvent(input$loadTreeDisCharDT, {
  VarDisCharDT$plotbox <- 1 # active plotbox
  VarDisCharDT$infobox <- VarDisCharDT$strbox <- treeDisCharDT()
  
})




#------------------ Read Data --------------------

csvDisCharDT <-eventReactive(input$loadCSVDisCharDT,{
  if (input$EgCSVDisCharDT == T){
    readRDS("examples/anoleData.RDS")
  }else{
    req(input$csvDisCharDT)
    validate(need(input$csvDisCharDT$datapath != "", "Please select a file or active example"))
    read.csv(file = input$csvDisCharDT$datapath, header = T, row.names = 1)
  }
})



# Send Info
observeEvent(input$loadCSVDisCharDT, {
  VarDisCharDT$infobox <- head(csvDisCharDT())
  VarDisCharDT$strbox <- list("phy" = treeDisCharDT(), "data" = csvDisCharDT())

})




#------------------ Check names --------------------


ckcnames <- eventReactive(input$chckNamesDisCharDT,{
  
  ckcnames <- name.check(treeDisCharDT(), csvDisCharDT())
  if(length(ckcnames)== 1){
    ckcnames <- "OK, no mismatch in names"
  } else{
    ckcnames$message <- "Please, resolve the mismatch"
  }
  return(ckcnames)
})

observeEvent(input$chckNamesDisCharDT,{
  
  VarDisCharDT$infobox <- ckcnames()
})


#Finish server
}








