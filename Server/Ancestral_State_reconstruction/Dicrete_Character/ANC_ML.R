# Guane: a shiny app for phylogenetic comparative methods
# Author: Viviana Romero Alarcon
# Creation date: September 11 2023
# Server: Ancestral State Reconstruction: Discrete Characters: Analysis 

#------------------ Reactive Variables --------------------

VarDisML <- reactiveValues()
VarDisML$infobox <- NULL
VarDisML$strbox <- NULL
VarDisML$plotbox <- NULL
#---
VarDisML$matrix0 <- NULL
VarDisML$Stmatrix <- NULL
VarDisML$counter <- 1

#------------------ str/message/error print --------------------

output$infoDisML <- renderPrint(expr = {
  req(cancelOutput =  VarDisML$infobox)
  if (is.null(x = VarDisML$infobox)) return()
  print(x = VarDisML$infobox)
})

output$srtDisML <- renderPrint(expr = {
  req(cancelOutput = VarDisML$strbox)
  if (is.null(x = VarDisML$strbox)) return()
  print(x = str(object = VarDisML$strbox))
})

#------------------ enable/disable/reset --------------------


observeEvent(eventExpr = input$DisMLReset, {
  VarDisML$infobox <- NULL
  VarDisML$strbox <- NULL
  VarDisML$plotbox <- 0
  reset("DisMLReset")
})



#------------------ Load/refresh variables --------------------

#tree
treeDisML <- eventReactive(eventExpr = input$loadDisML,{
  validate(errorClass = need(expr = try(expr = treeDisDT()),"Please, load or refresh data"))
  treeDisDT()
}
)

#Character
dataDisML <- eventReactive(eventExpr = input$loadDisML,{
  validate(errorClass = need(expr = try(expr = charDisDT()),"Please, load or refresh data"))
  
  setNames(object = charDisDT(),row.names(x = charDisDT()))
}
)


# Send Info
observeEvent(eventExpr = input$loadDisML, {
  VarDisML$plotbox <- 1 # active plotbox
  VarDisML$infobox <- VarDisML$strbox <- list(treeDisML(), dataDisML())
  
})



# n states
nStates <- eventReactive(eventExpr = dataDisML(),{
  length(x = levels(x = dataDisML()))
  })

# depend on nstates display option of models

observeEvent(eventExpr = nStates(),{
  if ( nStates()[1]== 2 | nStates()[1]== 1){
    # update model list
    updateSelectInput(session = session,inputId =  "ModDisML",
                      choices=c("ER" = "ER", 
                                "ARD" = "ARD", 
                                "Ireversible01" = "Ireversible01",
                                "Ireversible10" = "Ireversible10"))
    
  }else{
    # update model list for 3 states
    updateSelectInput(session = session, inputId = "ModDisML",
                      choices=c("ER" = "ER", "ARD" = "ARD", "SYM" = "SYM"))
    
    
    # display  add button action
    output$addModel3States <- renderUI({
      actionButton(inputId = 'addModDisML',label = 'Add new model')
    })
    
  }
}) 


#----------- Create new model
observeEvent(eventExpr = !is.null(x = input$ModDisML),{
  
  #Create a matrix0
  VarDisML$matrix0 <- matrix(data = NA,
                            nrow = nStates()[1],
                            ncol =  nStates()[1],
                            dimnames=list(levels(x = dataDisML()),
                                            levels(x = dataDisML())))

  # Create ER model
  if (  'ER'  %in% input$ModDisML){
    VarDisML$Stmatrix$ER <- VarDisML$matrix0
    
    row.names(x = VarDisML$Stmatrix$ER) <- levels(x = dataDisML())
    colnames(x = VarDisML$Stmatrix$ER) <- levels(x = dataDisML())
    
    VarDisML$Stmatrix$ER[lower.tri(x = VarDisML$Stmatrix$ER)] <- 1
    VarDisML$Stmatrix$ER[upper.tri(x = VarDisML$Stmatrix$ER)] <- 1
  }
  
  # Create ARD model
  if ('ARD' %in% input$ModDisML){
    VarDisML$Stmatrix$ARD <- VarDisML$matrix0
    
    row.names(x = VarDisML$Stmatrix$ARD) <- levels(x = dataDisML())
    colnames(x = VarDisML$Stmatrix$ARD) <- levels(x = dataDisML())
    
    VarDisML$Stmatrix$ARD[lower.tri(x = VarDisML$Stmatrix$ARD)] <- 2
    VarDisML$Stmatrix$ARD[upper.tri(x = VarDisML$Stmatrix$ARD)] <- 1
  }
  
  # Create SYM model
  if ('SYM' %in% input$ModelsDisML){
    VarDisML$Stmatrix$SYM <- VarDisML$matrix0
    
    row.names(x = VarDisML$Stmatrix$SYM) <- levels(x = dataDisML())
    colnames(x = VarDisML$Stmatrix$SYM) <- levels(x = dataDisML())
    
    VarDisML$Stmatrix$SYM[lower.tri(x = VarDisML$Stmatrix$SYM)] <- 1:length(x = VarDisML$Stmatrix$SYM[lower.tri(x = VarDisML$Stmatrix$SYM)])
    VarDisML$Stmatrix$SYM[upper.tri(x = VarDisML$Stmatrix$SYM)] <- 1:length(x = VarDisML$Stmatrix$SYM[lower.tri(x = VarDisML$Stmatrix$SYM)])
  }
  
  # Create  Ireversible01
  if ('Ireversible01' %in% input$ModelsDisML){
    VarDisML$Stmatrix$Ireversible01 <- VarDisML$matrix0
    
    row.names(x = VarDisML$Stmatrix$Ireversible01) <- levels(x = dataDisML())
    colnames(x = VarDisML$Stmatrix$Ireversible01) <- levels(x = dataDisML())
    
    VarDisML$Stmatrix$Ireversible01[lower.tri(x = VarDisML$Stmatrix$Ireversible01)] <- 1
    VarDisML$Stmatrix$Ireversible01[upper.tri(x = VarDisML$Stmatrix$Ireversible01)] <- 0
  }
  
  # Create Ireversible10
  if ('Ireversible10' %in% input$ModelsDisML){
    VarDisML$Stmatrix$Ireversible10 <- VarDisML$matrix0
    
    row.names(VarDisML$Stmatrix$Ireversible10) <- levels(dataDisML())
    colnames(VarDisML$Stmatrix$Ireversible10) <- levels(dataDisML())
    
    VarDisML$Stmatrix$Ireversible10[lower.tri(VarDisML$Stmatrix$Ireversible10)] <- 0
    VarDisML$Stmatrix$Ireversible10[upper.tri(VarDisML$Stmatrix$Ireversible10)] <- 1
  }
})



# if add model button is clicked
observeEvent(input$addModDisML > 0 ,{

  # add matrix for a new model
  output$w1 <- renderRHandsontable({
    VarDisML$matrix0[lower.tri(VarDisML$matrix0)] <- as.integer(1)
    VarDisML$matrix0[upper.tri(VarDisML$matrix0)] <- as.integer(1)
    rhandsontable(VarDisML$matrix0,readOnly = F)
  })
  
  
})



#Submitting models

observeEvent(input$SubmAddModel > 0,{
  
  # star counter
  VarDisML$counter[1] <- VarDisML$counter[1] + 1
  
  VarDisML$Stmatrix$x <- hot_to_r(input$w1)
  row.names(VarDisML$Stmatrix$x) <- levels(dataDisML())
  colnames(VarDisML$Stmatrix$x) <- levels(dataDisML())
  
  names(VarDisML$Stmatrix)[length(VarDisML$Stmatrix)] <- paste('UserModel',as.character(VarDisML$counter[1] - 1),sep = '')
  
  updateSelectInput(session, "ModDisML",
                    choices = names(VarDisML$Stmatrix),selected = names(VarDisML$Stmatrix))
  
  
})



# info panel
observeEvent(!is.null(input$ModDisML),{

  VarDisML$infobox <- names(VarDisML$Stmatrix)
  VarDisML$strbox <- VarDisML$Stmatrix
})



#'
#'next time add to run mono and polymorphic character


#------------------ Plot Tree --------------------

HeightDisML <- reactive(input$HeightDisML[1])
WidthDisML <- reactive(input$WidthDisML[1])


output$plotDisML <- renderPlot( height = HeightDisML  , width = WidthDisML,{
  
  if(is.null(treeDisML())){
    return()
  }else if (VarDisML$plotbox == 0){
    return()} 
  else{
    if(input$ladderizeDisML == "regular"){
      tree <-treeDisML()
    }else if(input$ladderizeDisML == "ascending"){
      tree <- ladderize(treeDisML(), right = F)
    }else if(input$ladderizeDisML == "descending"){
      tree <- ladderize(treeDisML(), right = T)
    }
    
    
    
    
    if(input$typeDisML == "fan" | input$typeDisML == 'radial'  ){
      plot.phylo(tree,plot = input$ViewPlotDisML,
                 use.edge.length = input$edgeLenghtDisML,
                 type = input$typeDisML, 
                 edge.width = input$edgeWidthDisML,
                 edge.lty = as.numeric(input$edgetlyDisML),
                 rotate.tree = input$rotateTreeDisML,#
                 open.angle = input$openAngleDisML,#
                 show.node.label = input$nodelabelDisML,
                 node.width = input$nodeWidthDisML,
                 node.lty = input$odeltyDisML,
                 show.tip.label = input$tipLabelsDisML,
                 align.tip.label = input$aligntiplabelDisML,
                 font = as.numeric(input$fontDisML),
                 cex = input$cexDisML,
                 adj = as.numeric(input$adjDisML),
                 srt = input$srtDisML,
                 label.offset = input$labeloffsetDisML,
                 underscore = input$labeloffsetDisML,
                 lab4ut = input$lab4utDisML,
                 #direction = input$directionDisML,
                 no.margin = input$nomarginDisML,
                 #x.lim = input$xlimDisML,
                 #y.lim = input$ylimDisML,
                 direction = input$directionDisML
                 
                 
                 
                 
      )
    }else{
      plot.phylo(tree,plot = input$ViewPlotDisML,
                 use.edge.length = input$edgeLenghtDisML,
                 type = input$typeDisML, 
                 edge.width = input$edgeWidthDisML,
                 edge.lty = as.numeric(input$edgetlyDisML),
                 show.node.label = input$nodelabelDisML,
                 node.width = input$nodeWidthDisML,
                 node.lty = input$odeltyDisML,
                 show.tip.label = input$tipLabelsDisML,
                 align.tip.label = input$aligntiplabelDisML,
                 font = as.numeric(input$fontDisML),
                 cex = input$cexDisML,
                 adj = as.numeric(input$adjDisML),
                 srt = input$srtDisML,
                 label.offset = input$labeloffsetDisML,
                 underscore = input$labeloffsetDisML,
                 lab4ut = input$lab4utDisML,
                 no.margin = input$nomarginDisML,
                 #x.lim = input$xlimDisML,
                 #y.lim = input$ylimDisML,
                 direction = input$directionDisML
                 
                 
      )
    }
    
  }
})
