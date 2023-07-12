#----------------Module: Server ---------------------------

# Guane: Shiny app for phylogenetic comparative methods
# Author: Viviana Romero 
# Creation Date: July 7 2023 
# Generic modules

#----------------Module: Server ---------------------------
library(shiny)
# Read phylogeny
PhyloInputServer <- function(id){
  moduleServer( id,
    function(input, output, session){
     
      # ask if it is nexus or not
      if (is.nexus( input$loadTree$datapath) == T) {
        read.nexus(file = input$loadTree$datapath)
      } else {
        tree <- read.tree(file = input$loadTree$datapath)
      }
   
    }
  )
}


# Read csv
csvInputServer <- function(id){
  moduleServer( id,
                function(input, output, session){
                  #get file
                  userFile <- reactive({
                    req(input$loadCsv)
                  })
                read.csv(file = userFile()$datapath,header = T, row.names = 1)
                  
                }
  )
}

# Display Info

infoServer <- function(id,info){
  moduleServer(id,
               function(input, output, session){
              
             output$InfoFunc <- renderPrint({
               
               # if (class(info) == 'data.frame' || class(info) == 'matrix' || class(info) == 'table') {
               #  obj <- head(info, 10L)
               # }else {
               if (is.null(info)) return()
               print(info)
             #  }
               
             })
               }
  )
}

#Plot in Data Panel

plotServer <- function(id, tree){
  moduleServer(id,
    function(input, output, session){
      output$plotFunc <- renderPlot( #height = heightDt  , width = widthDt,
        {
          if (is.null(tree)) return()
          
          plot.phylo(tree,# show.tip.label = input$tipLabels[1],
                                  #cex = input$tipSize[1],use.edge.length = input$branchLength[1], type = input$plotType,
                                 edge.width = 0.8,edge.color = 'grey40')
         
        })
    }
  )
}
#heightDt <- reactive(input$PlotHeightDt[1])
#widthDt <- reactive(input$PlotWidthDt[1])





