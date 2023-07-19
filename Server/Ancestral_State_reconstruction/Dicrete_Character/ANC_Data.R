# Guane: a shiny app for phylogenetic comparative methods
# Author: Viviana Romero Alarcon
# Creation date: July 7 2023
# Server: Ancestral State Reconstruction: Discrete Characters 


#------------------ Reactive Variables --------------------

VarDisCharDT <- reactiveValues()
VarDisCharDT$infobox <- NULL
VarDisCharDT$strbox <- NULL
VarDisCharDT$plotbox <- NULL

#------------------ str/message/error print --------------------

output$infoDisCharDT <- renderPrint({
  req(VarDisCharDT$infobox)
  if (is.null(VarDisCharDT$infobox)) return()
  print(VarDisCharDT$infobox)
})

output$srtDisCharDT <- renderPrint({
  req(VarDisCharDT$strbox)
  if (is.null(VarDisCharDT$strbox)) return()
  print(str(VarDisCharDT$strbox))
})

#------------------ enable/disable/reset --------------------


observeEvent(input$ResetDisCharDT, {
  VarDisCharDT$infobox <- NULL
  VarDisCharDT$strbox <- NULL
  VarDisCharDT$plotbox <- 0
  reset("DisCharDTReset")
})


#------------------ Plot Tree --------------------

HeightDisCharDT <- reactive(input$HeightDisCharDT[1])
WidthDisCharDT <- reactive(input$WidthDisCharDT[1])

output$plotDisCharDT <- renderPlot( height = HeightDisCharDT  , width = WidthDisCharDT,{
  
  if(is.null(treeDisCharDT())){
    return()
  }else if (VarDisCharDT$plotbox == 0){
    return()} 
  else{
    if(input$ladderizeDisCharDT == "regular"){
      tree <-treeDisCharDT()
    }else if(input$ladderizeDisCharDT == "ascending"){
      tree <- ladderize(treeDisCharDT(), right = F)
    }else if(input$ladderizeDisCharDT == "descending"){
      tree <- ladderize(treeDisCharDT(), right = T)
    }
    
    
    
    
    if(input$typeDisCharDT == "fan" | input$typeDisCharDT == 'radial'  ){
      plot.phylo(tree,plot = input$ViewPlotDisCharDT,
                 use.edge.length = input$edgeLenghtDisCharDT,
                 type = input$typeDisCharDT, 
                 edge.width = input$edgeWidthDisCharDT,
                 edge.lty = as.numeric(input$edgetlyDisCharDT),
                 rotate.tree = input$rotateTreeDisCharDT,#
                 open.angle = input$openAngleDisCharDT,#
                 show.node.label = input$nodelabelDisCharDT,
                 node.width = input$nodeWidthDisCharDT,
                 node.lty = input$odeltyDisCharDT,
                 show.tip.label = input$tipLabelsDisCharDT,
                 align.tip.label = input$aligntiplabelDisCharDT,
                 font = as.numeric(input$fontDisCharDT),
                 cex = input$cexDisCharDT,
                 adj = as.numeric(input$adjDisCharDT),
                 srt = input$srtDisCharDT,
                 label.offset = input$labeloffsetDisCharDT,
                 underscore = input$labeloffsetDisCharDT,
                 lab4ut = input$lab4utDisCharDT,
                 #direction = input$directionDisCharDT,
                 no.margin = input$nomarginDisCharDT,
                 #x.lim = input$xlimDisCharDT,
                 #y.lim = input$ylimDisCharDT,
                 direction = input$directionDisCharDT
                 
                 
                 
                 
      )
    }else{
      plot.phylo(tree,plot = input$ViewPlotDisCharDT,
                 use.edge.length = input$edgeLenghtDisCharDT,
                 type = input$typeDisCharDT, 
                 edge.width = input$edgeWidthDisCharDT,
                 edge.lty = as.numeric(input$edgetlyDisCharDT),
                 show.node.label = input$nodelabelDisCharDT,
                 node.width = input$nodeWidthDisCharDT,
                 node.lty = input$odeltyDisCharDT,
                 show.tip.label = input$tipLabelsDisCharDT,
                 align.tip.label = input$aligntiplabelDisCharDT,
                 font = as.numeric(input$fontDisCharDT),
                 cex = input$cexDisCharDT,
                 adj = as.numeric(input$adjDisCharDT),
                 srt = input$srtDisCharDT,
                 label.offset = input$labeloffsetDisCharDT,
                 underscore = input$labeloffsetDisCharDT,
                 lab4ut = input$lab4utDisCharDT,
                 no.margin = input$nomarginDisCharDT,
                 #x.lim = input$xlimDisCharDT,
                 #y.lim = input$ylimDisCharDT,
                 direction = input$directionDisCharDT
                 
                 
      )
    }
    
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




#------------------ Check names and Solve --------------------

# Check names
ckcnames <- eventReactive(input$chckNamesDisCharDT == T,{
  req(treeDisCharDT())
  ckcnames <- geiger::name.check(treeDisCharDT(), csvDisCharDT())
  
  if(length(ckcnames)== 1){
    ckcnames <- "OK, no mismatch in names"
    
  } else{
    ckcnames$message <- "Please, resolve the mismatch"
    output$RslvNamesDisCharDT <- renderUI({
      actionButton(inputId = "MismatchDischarDT" ,label = "Resolve mismatch")
    })
  }
  return(ckcnames)
})

# Send message to infobox
observeEvent(input$chckNamesDisCharDT,{
  VarDisCharDT$infobox <- ckcnames()
})

#Solve mismatch

MatchDisCharDT <- eventReactive(input$MismatchDischarDT, {
  req(ckcnames())
  geiger::treedata(treeDisCharDT(), csvDisCharDT())
})

# Send message to infobox
observeEvent(input$MismatchDischarDT,{
  
  treeDisCharDT <- reactive({req(MatchDisCharDT())
    MatchDisCharDT()$phy})
  csvDisCharDT <- reactive({req(MatchDisCharDT())
    MatchDisCharDT()$data})
  
  VarDisCharDT$infobox <- paste("Tips in tree:",length(treeDisCharDT()$phy$tip.label),";", "Tips in data:",nrow(csvDisCharDT()-1))
  VarDisCharDT$strbox <-  MatchDisCharDT()
  
})


#------------------ Choose character and type --------------------

#Dynamic input selection (Variables)
#Note: when there is only one option you should use list(), but there are more than one you could code names() straightforward
#
observeEvent(csvDisCharDT(), {
  numvar <- which(lapply(csvDisCharDT(), class) == 'numeric' | lapply(csvDisCharDT(), class) == 'integer')
  factvar <- which(lapply(csvDisCharDT(), class) == 'factor' | lapply(csvDisCharDT(), class) == 'character')
  if (length(numvar) == 0) {
    if (length(factvar) > 1) {
      updateSelectInput(session, "chooseVarDisCharDT",
                        choices=list('Select','Discrete Char'= names(factvar)))
    } else {
      updateSelectInput(session, "chooseVarDisCharDT",
                        choices=list('Select','Discrete Char'= list(names(factvar))))
    }
  } else if (length(factvar) == 0) {
    if (length(numvar) > 1){
      updateSelectInput(session, "chooseVarDisCharDT",
                        choices=list('Select','Continuou Char'= names(numvar)))
    }else{
      updateSelectInput(session, "chooseVarDisCharDT",
                        choices=list('Select','Continuou Char'= list(names(numvar))))
    }
  } else {
    if (length(factvar) > 1 & length(numvar) == 1) {
      updateSelectInput(session, "chooseVarDisCharDT",selected = NULL,
                        choices = list('Select','Discrete Char'= names(factvar),
                                       'Continuou Char'= list(names(numvar))))
    } else if (length(factvar) == 1 & length(numvar) > 1) {
      updateSelectInput(session, "chooseVarDisCharDT",selected = NULL,
                        choices=list('Select','Discrete Char'= list(names(factvar)),
                                     'Continuou Char'= names(numvar)))
    } else if (length(factvar) == 1 & length(numvar) == 1) {
      updateSelectInput(session, "chooseVarDisCharDT",selected = NULL,
                        choices=list('Select','Discrete Char'= list(names(factvar)),'
                                               Continuou Char'= list(names(numvar))))
    } else {
      updateSelectInput(session, "chooseVarDisCharDT",selected = NULL,
                        choices=list('Select','Discrete Char'= names(factvar),
                                     'Continuou Char'= names(numvar)))
    }
  }
})

# choose character

charDischarDT <- eventReactive( input$chooseVarDisCharDT,{
  col <-which(colnames(csvDisCharDT()) == input$chooseVarDisCharDT)
  return(csvDisCharDT()[,col])
})

