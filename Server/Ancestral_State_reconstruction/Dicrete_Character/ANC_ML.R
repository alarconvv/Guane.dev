# Guane: a shiny app for phylogenetic comparative methods
# Author: Viviana Romero Alarcon
# Creation date: September 11 2023
# Server: Ancestral State Reconstruction: Discrete Characters: Analysis 

#------------------ Reactive Variables --------------------

VarDisCharANA <- reactiveValues()
VarDisCharANA$infobox <- NULL
VarDisCharANA$strbox <- NULL
VarDisCharANA$plotbox <- NULL



#------------------ str/message/error print --------------------

output$infoDisCharANA <- renderPrint({
  req(VarDisCharANA$infobox)
  if (is.null(VarDisCharANA$infobox)) return()
  print(VarDisCharANA$infobox)
})

output$srtDisCharANA <- renderPrint({
  req(VarDisCharANA$strbox)
  if (is.null(VarDisCharANA$strbox)) return()
  print(str(VarDisCharANA$strbox))
})

#------------------ enable/disable/reset --------------------


observeEvent(input$ResetDisCharANA, {
  VarDisCharANA$infobox <- NULL
  VarDisCharANA$strbox <- NULL
  VarDisCharANA$plotbox <- 0
  reset("DisCharDTReset")
})



#------------------ Load/refresh variables --------------------

#tree
treeMLDisCharANA <- eventReactive(input$loadDisCharANA,{
  validate(need(try(treeDisCharDT()),"Please, your tree must be ultrametric and binary "))
  treeDisCharDT()
}
)

#Character
charMLDisCharANA <- eventReactive(input$loadDisCharANA,{
  validate(need(try(charDischarDT()),"Please, your tree must be ultrametric and binary "))
  
  setNames(charDischarDT(),row.names(CcharDischarDT()))
}
)

# n states
nStates <- eventReactive(charMLDisCharANA,{
  length(levels(charMLDisCharANA))
  })

# depend on nstates display option of models

observeEvent(nStates(),{
  if ( nStates()[1]== 2 | nStates()[1]== 1){
    # update model list
    updateSelectInput(session, "ModMLDisCharANA",
                      choices=c('ER'='ER', 'ARD'='ARD', 'Ireversible01'='Ireversible01','Ireversible10'='Ireversible10'))
    
  }else{
    # update model list for 3 states
    updateSelectInput(session, "ModelsDisML",
                      choices=c('ER'='ER', 'ARD'='ARD', 'SYM'='SYM'))
    
    
    # display  add buttonaction
    output$addModel3States <- renderUI({
      actionButton('AddModelDisML','Add new model')
    })
    
  }
}) 

