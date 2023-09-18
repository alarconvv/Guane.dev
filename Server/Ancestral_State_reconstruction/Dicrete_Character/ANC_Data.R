# Guane: a shiny app for phylogenetic comparative methods
# Author: Viviana Romero Alarcon
# Creation date: July 7 2023
# Server: Ancestral State Reconstruction: Discrete Characters 


#------------------ Reactive Variables --------------------

VarDisDT <- reactiveValues()
VarDisDT$infobox <- NULL
VarDisDT$strbox <- NULL
VarDisDT$plotbox <- NULL

#------------------ str/message/error print --------------------

output$infoDisDT <- renderPrint({
  req(VarDisDT$infobox)
  if (is.null(VarDisDT$infobox)) return()
  print(VarDisDT$infobox)
})

output$srtDisDT <- renderPrint({
  req(VarDisDT$strbox)
  if (is.null(VarDisDT$strbox)) return()
  print(str(VarDisDT$strbox))
})

#------------------ enable/disable/reset --------------------


observeEvent(input$ResetDisDT, {
  VarDisDT$infobox <- NULL
  VarDisDT$strbox <- NULL
  VarDisDT$plotbox <- 0
  reset("DisCharDTReset")
})


#------------------ Plot Tree --------------------

HeightDisDT <- reactive(input$HeightDisDT[1])
WidthDisDT <- reactive(input$WidthDisDT[1])

output$plotDisDT <- renderPlot( height = HeightDisDT  , width = WidthDisDT,{
  
  if(is.null(treeDisDT())){
    return()
  }else if (VarDisDT$plotbox == 0){
    return()} 
  else{
    if(input$ladderizeDisDT == "regular"){
      tree <-treeDisDT()
    }else if(input$ladderizeDisDT == "ascending"){
      tree <- ladderize(treeDisDT(), right = F)
    }else if(input$ladderizeDisDT == "descending"){
      tree <- ladderize(treeDisDT(), right = T)
    }
    
    
    
    
    if(input$typeDisDT == "fan" | input$typeDisDT == 'radial'  ){
      plot.phylo(tree,plot = input$ViewPlotDisDT,
                 use.edge.length = input$edgeLenghtDisDT,
                 type = input$typeDisDT, 
                 edge.width = input$edgeWidthDisDT,
                 edge.lty = as.numeric(input$edgetlyDisDT),
                 rotate.tree = input$rotateTreeDisDT,#
                 open.angle = input$openAngleDisDT,#
                 show.node.label = input$nodelabelDisDT,
                 node.width = input$nodeWidthDisDT,
                 node.lty = input$odeltyDisDT,
                 show.tip.label = input$tipLabelsDisDT,
                 align.tip.label = input$aligntiplabelDisDT,
                 font = as.numeric(input$fontDisDT),
                 cex = input$cexDisDT,
                 adj = as.numeric(input$adjDisDT),
                 srt = input$srtDisDT,
                 label.offset = input$labeloffsetDisDT,
                 underscore = input$labeloffsetDisDT,
                 lab4ut = input$lab4utDisDT,
                 #direction = input$directionDisDT,
                 no.margin = input$nomarginDisDT,
                 #x.lim = input$xlimDisDT,
                 #y.lim = input$ylimDisDT,
                 direction = input$directionDisDT
                 
                 
                 
                 
      )
    }else{
      plot.phylo(tree,plot = input$ViewPlotDisDT,
                 use.edge.length = input$edgeLenghtDisDT,
                 type = input$typeDisDT, 
                 edge.width = input$edgeWidthDisDT,
                 edge.lty = as.numeric(input$edgetlyDisDT),
                 show.node.label = input$nodelabelDisDT,
                 node.width = input$nodeWidthDisDT,
                 node.lty = input$odeltyDisDT,
                 show.tip.label = input$tipLabelsDisDT,
                 align.tip.label = input$aligntiplabelDisDT,
                 font = as.numeric(input$fontDisDT),
                 cex = input$cexDisDT,
                 adj = as.numeric(input$adjDisDT),
                 srt = input$srtDisDT,
                 label.offset = input$labeloffsetDisDT,
                 underscore = input$labeloffsetDisDT,
                 lab4ut = input$lab4utDisDT,
                 no.margin = input$nomarginDisDT,
                 #x.lim = input$xlimDisDT,
                 #y.lim = input$ylimDisDT,
                 direction = input$directionDisDT
                 
                 
      )
    }
    
  }
})

################### Process ############################

#------------------ Read phylogeny --------------------

treeDisDT <-eventReactive(input$loadTreeDisDT,{
  if (input$EgTreeDisDT == T){
    readRDS("examples/anoleTree.RDS")
  }else{
    req(input$treeDisDT)
    validate(need(input$treeDisDT$datapath != "", "Please select a file or active example"))
    if (is.nexus(input$treeDisDT$datapath) == T) {
      read.nexus(file = input$treeDisDT$datapath)
    } else {
      read.tree(file = input$treeDisDT$datapath)
    }
  }
})



# Send Info
observeEvent(input$loadTreeDisDT, {
  VarDisDT$plotbox <- 1 # active plotbox
  VarDisDT$infobox <- VarDisDT$strbox <- treeDisDT()
  
})




#------------------ Read Data --------------------

csvDisDT <-eventReactive(input$loadCSVDisDT,{
  if (input$EgCSVDisDT == T){
    readRDS("examples/anoleData.RDS")
  }else{
    req(input$csvDisDT)
    validate(need(input$csvDisDT$datapath != "", "Please select a file or active example"))
    read.csv(file = input$csvDisDT$datapath, header = T, row.names = 1)
  }
})



# Send Info
observeEvent(input$loadCSVDisDT, {
  VarDisDT$infobox <- head(csvDisDT())
  VarDisDT$strbox <- list("phy" = treeDisDT(), "data" = csvDisDT())
  
})




#------------------ Check names and Solve --------------------

# Check names
ckcnames <- eventReactive(input$chckNamesDisDT == T,{
  req(treeDisDT())
  ckcnames <- geiger::name.check(treeDisDT(), csvDisDT())
  
  if(length(ckcnames)== 1){
    ckcnames <- "OK, no mismatch in names"
    
  } else{
    ckcnames$message <- "Please, resolve the mismatch"
    output$RslvNamesDisDT <- renderUI({
      actionButton(inputId = "MismatchDisDT" ,label = "Resolve mismatch")
    })
  }
  return(ckcnames)
})

# Send message to infobox
observeEvent(input$chckNamesDisDT,{
  VarDisDT$infobox <- ckcnames()
})

#Solve mismatch

MatchDisDT <- eventReactive(input$MismatchDisDT, {
  req(ckcnames())
  geiger::treedata(treeDisDT(), csvDisDT())
})

# Send message to infobox
observeEvent(input$MismatchDisDT,{
  
  treeDisDT <- reactive({req(MatchDisDT())
    MatchDisDT()$phy})
  csvDisDT <- reactive({req(MatchDisDT())
    MatchDisDT()$data})
  
  VarDisDT$infobox <- paste("Tips in tree:",length(treeDisDT()$phy$tip.label),";", "Tips in data:",nrow(csvDisDT()-1))
  VarDisDT$strbox <-  MatchDisDT()
  
})


#------------------ Choose character and type --------------------

#Dynamic input selection (Variables)
#Note: when there is only one option you should use list(), but there are more than one you could code names() straightforward
#
observeEvent(csvDisDT(), {
  numvar <- which(lapply(csvDisDT(), class) == 'numeric' | lapply(csvDisDT(), class) == 'integer')
  factvar <- which(lapply(csvDisDT(), class) == 'factor' | lapply(csvDisDT(), class) == 'character')
  if (length(numvar) == 0) {
    if (length(factvar) > 1) {
      updateSelectInput(session, "chooseVarDisDT",
                        choices=list('Select','Discrete Char'= names(factvar)))
    } else {
      updateSelectInput(session, "chooseVarDisDT",
                        choices=list('Select','Discrete Char'= list(names(factvar))))
    }
  } else if (length(factvar) == 0) {
    if (length(numvar) > 1){
      updateSelectInput(session, "chooseVarDisDT",
                        choices=list('Select','Continuou Char'= names(numvar)))
    }else{
      updateSelectInput(session, "chooseVarDisDT",
                        choices=list('Select','Continuou Char'= list(names(numvar))))
    }
  } else {
    if (length(factvar) > 1 & length(numvar) == 1) {
      updateSelectInput(session, "chooseVarDisDT",selected = NULL,
                        choices = list('Select','Discrete Char'= names(factvar),
                                       'Continuou Char'= list(names(numvar))))
    } else if (length(factvar) == 1 & length(numvar) > 1) {
      updateSelectInput(session, "chooseVarDisDT",selected = NULL,
                        choices=list('Select','Discrete Char'= list(names(factvar)),
                                     'Continuou Char'= names(numvar)))
    } else if (length(factvar) == 1 & length(numvar) == 1) {
      updateSelectInput(session, "chooseVarDisDT",selected = NULL,
                        choices=list('Select','Discrete Char'= list(names(factvar)),'
                                               Continuou Char'= list(names(numvar))))
    } else {
      updateSelectInput(session, "chooseVarDisDT",selected = NULL,
                        choices=list('Select','Discrete Char'= names(factvar),
                                     'Continuou Char'= names(numvar)))
    }
  }
})

# choose character

charDisDT <- eventReactive( input$chooseVarDisDT,{
  col <-which(colnames(csvDisDT()) == input$chooseVarDisDT)
  return(csvDisDT()[,col])
})

