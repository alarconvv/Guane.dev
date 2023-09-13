# Guane: a shiny app for phylogenetic comparative methods
# Author: Viviana Romero Alarcon
# Creation date: July 7 2023
# UI File




#------------------ Libraries ---------------
library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(shinyjs)
library(esquisse)
library(shinyAce)
#-----------
library(ape)
library(phytools)
library(geiger)




#------------------ Sources ---------------

source("Functions/GenFunctions.R")
source("Theme/Guane_theme.R")


#------------------ UI ---------------
shinyUI(navbarPage(title = div( "", img(src = "Picture1.png",
                                      id = "simulation",
                                      height = "75px",
                                      width = "70px",
                                      style = "position: relative; margin:0px 0px; display:center-align;")),
                 id = "GnavbarPage",
                 selected = "Intro",
                 position = "static-top",
                 inverse = FALSE,
                 collapsible = TRUE,
                 fluid = TRUE,
                 windowTitle = "Guane",
                 theme = theme,
                 
                 # Module  to modify adminlte
                 header=  tags$style(HTML(".nav>li>a:active, .nav>li>a:focus, .nav>li>a:hover { background: #0d4d6100;}")),
          
                 #---------------- Collapsible Menu ---------------
                 
           #---------------- Intro Panel ---------------
                 tabPanel(id = "Gintro",title =  "Intro"),
                 
           #---------------- Method Panels ---------------
                 # Collapsible panels
                 navbarMenu( title = "Methods",menuName = "GnavbarMenu",
                                                  #---------------- Phylo Signal ---------------
                             tabPanel(id = "PhySignal", title = "Phylogenetic Signal",
                                      tabsetPanel(id="TabsPhySignal",type="tabs",
                                                  #-------------------------------------------Phylo Signal DATA ----------------------------------------------------
                                                  tabPanel(id="PhySignalDT",title = "DATA",
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "PhySignalDTReset",
                                                               fluidRow(
                                                                 column(10, 
                                                                        fluidRow(
                                                                          column(3,sidebarPanel(width = "100%",
                                                                                                p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                hr(),
                                                                                                
                                                                                                
                                                                          )),
                                                                          column(9,fluidRow(column(8,card(height = "680px",full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                                          card_body( plotOutput("plotPhySignalDT", inline = T)),
                                                                          )),
                                                                          column(4, card(full_screen = TRUE, card_header( strong("Data Structure")),
                                                                                         verbatimTextOutput("srtPhySignalDT")),
                                                                                 card(full_screen = TRUE,
                                                                                      card_header( strong("Messages/Errors")),
                                                                                      verbatimTextOutput("infoPhySignalDT"))
                                                                                 
                                                                          ))))),
                                                                 column(2, sidebarPanel(width = "100%",p("Graphic Controls", style = "align:center;text-align:center; font-weight: bold"),hr(),
                                                                                        awesomeCheckbox(inputId = "ViewPlotPhySignalDT",label = strong("Display Plot"), 
                                                                                                        value = T,status = "info"), hr(),
                                                                                        sliderInput(inputId = "HeightPhySignalDT" ,
                                                                                                    label = " Height",min =0 ,max = 10000,value = 400),
                                                                                        sliderInput(inputId = "WidthPhySignalDT" ,
                                                                                                    label = " Width",min =0 ,max = 10000,value = 600 ), 
                                                                                        hr(),
                                                                                        accordion_panel( title = h5(" \ Tree and edge design"),
                                                                                                         value = "SetTreePhySignalDT",
                                                                                                         
                                                                                                         
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Lables"),
                                                                                                         value = "labels PhySignalDT",
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Margins"),
                                                                                                         value = "marginsPhySignalDT",
                                                                                                         
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Nodes"),
                                                                                                         value = "NodesPhySignalDT",
                                                                                                         
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        
                                                                 ),fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold")))
                                                                 ))
                                                               
                                                               # Finish Div: PhySignalDTReset
                                                           ),
                                                           
                                                           actionButton("ResetPhySignalDT",width = "100%",label = "Restart initial values")
                                                           # Finish tabPanel: PhySignalDT
                                                  ),
                                                  #-------------------------------------------Phylo Signal ANALYSIS ----------------------------------------------------
                                                  tabPanel(id="PhySignalANA",title = "ANALYSIS",
                                                           tabsetPanel(id="PillsDiverModANA",type="pills",
                                                                       
                                                  #-------------------------------------------Phylo Signal ANALYSIS: Maxumum Likelihood ----------------------------------------------------
                                                                       tabPanel(id="MLPhySignalANA",title = "Maxumum Likelihood",
                                                            # Use Div to reset panels
                                                            useShinyjs(),
                                                            div(id = "MLPhySignalANAReset",
                                                                fluidRow(
                                                                  column(10, 
                                                                         fluidRow(
                                                                           column(3,sidebarPanel(width = "100%",
                                                                                                 p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                 hr(),
                                                                                                 
                                                                                                 
                                                                           )),
                                                                           column(9,fluidRow(column(8,card(height = "680px",full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                                           card_body( plotOutput("plotMLPhySignalANA", inline = T)),
                                                                           )),
                                                                           column(4, card(full_screen = TRUE, card_header( strong("Data Structure")),
                                                                                          verbatimTextOutput("srtMLPhySignalANA")),
                                                                                  card(full_screen = TRUE,
                                                                                       card_header( strong("Messages/Errors")),
                                                                                       verbatimTextOutput("infoMLPhySignalANA"))
                                                                                  
                                                                           ))))),
                                                                  column(2, sidebarPanel(width = "100%",p("Graphic Controls", style = "align:center;text-align:center; font-weight: bold"),hr(),
                                                                                         awesomeCheckbox(inputId = "ViewPlotMLPhySignalANA",label = strong("Display Plot"), 
                                                                                                         value = T,status = "info"), hr(),
                                                                                         sliderInput(inputId = "HeightMLPhySignalANA" ,
                                                                                                     label = " Height",min =0 ,max = 10000,value = 400),
                                                                                         sliderInput(inputId = "WidthMLPhySignalANA" ,
                                                                                                     label = " Width",min =0 ,max = 10000,value = 600 ), 
                                                                                         hr(),
                                                                                         accordion_panel( title = h5(" \ Tree and edge design"),
                                                                                                          value = "SetTreeMLPhySignalANA",
                                                                                                          
                                                                                                          
                                                                                                          open = FALSE,icon = icon("greater-than")),
                                                                                         
                                                                                         hr(),
                                                                                         
                                                                                         accordion_panel( title = h5(" \ Lables"),
                                                                                                          value = "labels MLPhySignalANA",
                                                                                                          open = FALSE,icon = icon("greater-than")),
                                                                                         
                                                                                         hr(),
                                                                                         
                                                                                         accordion_panel( title = h5(" \ Margins"),
                                                                                                          value = "marginsMLPhySignalANA",
                                                                                                          
                                                                                                          open = FALSE,icon = icon("greater-than")),
                                                                                         
                                                                                         
                                                                                         hr(),
                                                                                         
                                                                                         accordion_panel( title = h5(" \ Nodes"),
                                                                                                          value = "NodesMLPhySignalANA",
                                                                                                          
                                                                                                          open = FALSE,icon = icon("greater-than")),
                                                                                         
                                                                                         
                                                                  ),fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold")))
                                                                  ))
                                                                
                                                                # Finish Div: MLPhySignalANAReset
                                                            ),
                                                            
                                                            actionButton("ResetMLPhySignalANA",width = "100%",label = "Restart initial values")
                                                            #Finish TabPanel: MLPhySignalANA           
                                                            ),
                                                            
                                                            
                                                  #-------------------------------------------Phylo Signal ANALYSIS: MCMC ----------------------------------------------------
                                                            tabPanel(id="BIPhySignalANA",title = "Stochastic mapping",
                                                                     # Use Div to reset panels
                                                                     useShinyjs(),
                                                                     div(id = "BIPhySignalANAReset",
                                                                         fluidRow(
                                                                           column(10, 
                                                                                  fluidRow(
                                                                                    column(3,sidebarPanel(width = "100%",
                                                                                                          p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                          hr(),
                                                                                                          
                                                                                                          
                                                                                    )),
                                                                                    column(9,fluidRow(column(8,card(height = "680px",full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                                                    card_body( plotOutput("plotBIPhySignalANA", inline = T)),
                                                                                    )),
                                                                                    column(4, card(full_screen = TRUE, card_header( strong("Data Structure")),
                                                                                                   verbatimTextOutput("srtBIPhySignalANA")),
                                                                                           card(full_screen = TRUE,
                                                                                                card_header( strong("Messages/Errors")),
                                                                                                verbatimTextOutput("infoBIPhySignalANA"))
                                                                                           
                                                                                    ))))),
                                                                           column(2, sidebarPanel(width = "100%",p("Graphic Controls", style = "align:center;text-align:center; font-weight: bold"),hr(),
                                                                                                  awesomeCheckbox(inputId = "ViewPlotBIPhySignalANA",label = strong("Display Plot"), 
                                                                                                                  value = T,status = "info"), hr(),
                                                                                                  sliderInput(inputId = "HeightBIPhySignalANA" ,
                                                                                                              label = " Height",min =0 ,max = 10000,value = 400),
                                                                                                  sliderInput(inputId = "WidthBIPhySignalANA" ,
                                                                                                              label = " Width",min =0 ,max = 10000,value = 600 ), 
                                                                                                  hr(),
                                                                                                  accordion_panel( title = h5(" \ Tree and edge design"),
                                                                                                                   value = "SetTreeBIPhySignalANA",
                                                                                                                   
                                                                                                                   
                                                                                                                   open = FALSE,icon = icon("greater-than")),
                                                                                                  
                                                                                                  hr(),
                                                                                                  
                                                                                                  accordion_panel( title = h5(" \ Lables"),
                                                                                                                   value = "labels BIPhySignalANA",
                                                                                                                   open = FALSE,icon = icon("greater-than")),
                                                                                                  
                                                                                                  hr(),
                                                                                                  
                                                                                                  accordion_panel( title = h5(" \ Margins"),
                                                                                                                   value = "marginsBIPhySignalANA",
                                                                                                                   
                                                                                                                   open = FALSE,icon = icon("greater-than")),
                                                                                                  
                                                                                                  
                                                                                                  hr(),
                                                                                                  
                                                                                                  accordion_panel( title = h5(" \ Nodes"),
                                                                                                                   value = "NodesBIPhySignalANA",
                                                                                                                   
                                                                                                                   open = FALSE,icon = icon("greater-than")),
                                                                                                  
                                                                                                  
                                                                           ),fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold")))
                                                                           ))
                                                                         
                                                                         # Finish Div: BIPhySignalANAReset
                                                                     ),
                                                                     
                                                                     actionButton("ResetBIPhySignalANA",width = "100%",label = "Restart initial values")
                                                                     #Finish TabPanel: BIPhySignalANA           
                                                            ),
                                                            
                                                            
                                                            #Finish TabsetPanel: PillsPhySignalANA
                                                            )
                                                           # Finish tabPanel: PhySignalANA
                                                           )
                                                  
                                                  
                                                  
                                                  # Finish tabsetPanel: TabsPhySignal
                                                  )
                                      # Finish tabPanel: PhySignal
                                      ),
                             
                             "----",
                             "Ancestral State Estimation",
                                                  #-------------------------------------------ANS Discrete Characters----------------------------------------------------
                             tabPanel(id = "DisChar", title = "Discrete Characters",
                                      tabsetPanel(id="TabsDisChar",type="tabs",
                                                  #-------------------------------------------ANS Discrete Characters DATA ----------------------------------------------------
                                                  tabPanel(id="DisCharDT",title = "DATA",
                                                           
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "DisCharDTReset",
                                                               fluidRow(
                                                                 column(10, 
                                                                        fluidRow(
                                                                          column(3,sidebarPanel(width = "100%",
                                                                                                p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                hr(),
                                                                                                fileInput(inputId = "treeDisCharDT",
                                                                                                          label = strong("Load tree"),
                                                                                                          width = "100%",
                                                                                                          accept = c(".tree",".tre",".nexus",".phy",".nex")),
                                                                                                
                                                                                                fluidRow(column(6,aling="center",
                                                                                                                materialSwitch(inputId = "EgTreeDisCharDT",label = "Use example",
                                                                                                                               value = FALSE,status = "info",right = TRUE)),
                                                                                                         column(6,align= "right",actionButton(inputId = "loadTreeDisCharDT",
                                                                                                                                              label = NULL,icon =icon(name = "upload"), 
                                                                                                                                              width = "25%"))),
                                                                                                hr(),
                                                                                                fileInput(inputId = "csvDisCharDT",
                                                                                                          label = strong('Load Character(s)'),
                                                                                                          width = "100%",
                                                                                                          accept = c(".csv",".txt",".text")),
                                                                                                fluidRow(column(6,aling="center",
                                                                                                                materialSwitch(inputId = "EgCSVDisCharDT",label = "Use example",
                                                                                                                               value = FALSE,status = "info",right = TRUE)),
                                                                                                         column(6,align= "right",actionButton(inputId = "loadCSVDisCharDT",
                                                                                                                                              label = NULL,icon =icon(name = "upload"), 
                                                                                                                                              width = "25%"))),
                                                                                                hr(),
                                                                                                awesomeCheckbox(inputId = "chckNamesDisCharDT",label = strong("Check tree & data names"), 
                                                                                                  value = F,status = "info"),
                                                                                                uiOutput(outputId = "RslvNamesDisCharDT"),
                                                                                                hr(),
                                                                                                selectInput(inputId = "chooseVarDisCharDT",label = strong("Select  your discrete character"), choices = "Select"),
                                                                                                selectInput(inputId = "chooseTupeDisCharDT",label = strong("Select characther type"), choices = c("Select","monomorphic", "polymorphic"),selected = NULL,multiple = F),
                                                                                                
                                                                                                
                                                                                                
                                                               ),),
                                                               column(9,fluidRow(column(8,card(height = "680px",full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                               card_body( plotOutput("plotDisCharDT", inline = T)),
                                                                                              )),
                                                                                 column(4, card(full_screen = TRUE, card_header( strong("Data Structure")),
                                                                                                verbatimTextOutput("srtDisCharDT")),
                                                                                        card(full_screen = TRUE,
                                                                                             card_header( strong("Messages/Errors")),
                                                                                                            verbatimTextOutput("infoDisCharDT")
                                                                                        )
                                                                                        
                                                                                 ))))),
                                                               column(2, sidebarPanel(width = "100%",p("Graphic Controls", style = "align:center;text-align:center; font-weight: bold"),hr(),
                                                                                      awesomeCheckbox(inputId = "ViewPlotDisCharDT",label = strong("Display Plot"), 
                                                                                                      value = T,status = "info"), hr(),
                                                                                      sliderInput(inputId = "HeightDisCharDT" ,
                                                                                                  label = " Height",min =0 ,max = 10000,value = 400),
                                                                                      sliderInput(inputId = "WidthDisCharDT" ,
                                                                                                  label = " Width",min =0 ,max = 10000,value = 600 ), 
                                                                                      hr(),
                                                                                      accordion_panel( title = h5(" \ Tree and edge design"),
                                                                                                       value = "SetTreeDisCharDT",
                                                                                                       pickerInput(selected = "regular",
                                                                                                                   inputId = "ladderizeDisCharDT",
                                                                                                                   label = strong('Type'),
                                                                                                                   choices = c("regular", 
                                                                                                                               "ascending", "descending")
                                                                                                       ),
                                                                                                       
                                                                                                       pickerInput(selected = "phylogram",
                                                                                                                   inputId = "typeDisCharDT",
                                                                                                                   label = strong('Sort Tree'),
                                                                                                                   choices = c("phylogram" , 
                                                                                                                               "cladogram", "fan","unrooted","radial","tidy"), 
                                                                                                       ),
                                                                                                       
                                                                                                       pickerInput(selected = "rightwards",
                                                                                                                   inputId = "directionDisCharDT",
                                                                                                                   label = strong('Direction'),
                                                                                                                   choices = c("rightwards" , 
                                                                                                                               "leftwards", "upwards",
                                                                                                                               "downwards"), 
                                                                                                       ),
                                                                                                   
                                                                                                       conditionalPanel(condition= "input.typeDisCharDT == 'fan' || input.typeDisCharDT == 'radial' ",
                                                                                                                        numericInput(inputId = "rotateTreeDisCharDT",
                                                                                                                                     label = "Rotate tree",value = 0 ,min =-360,
                                                                                                                                     max =360 ,step =0.1 ,width = "40%"),
                                                                                                                        numericInput(inputId = "openAngleDisCharDT",
                                                                                                                                     label = "Open angle",
                                                                                                                                     value = 0.1 ,min =0.1,
                                                                                                                                     max =360 ,step =0.1 ,width = "40%"),
                                                                                                                        ),
                                                                                      
                                                                                      awesomeCheckbox(inputId = "edgeLenghtDisCharDT",
                                                                                                      label = strong("Use edge lengths"), 
                                                                                                      value = T,status = "info"),
                                                                                      
                                                                                      numericInput(inputId = "edgeWidthDisCharDT",
                                                                                                   label = "set edge width",value = 1 ,
                                                                                                   min =0.1 ,max =10 ,step =0.01 ,width = "40%"),
                                                                                      
                                                                                      selectInput(selected = "plain",
                                                                                                  inputId = "edgetlyDisCharDT",
                                                                                                  label = strong('Line Type'),
                                                                                                  choices = c( "plain" = "1" ,  "dashed" = "2", 
                                                                                                              "dotted" = "3",  "dotdash" = "4", 
                                                                                                               "longdash" = "5" , "twodash" = "6")
                                                                                      ),
                                                                                      
                                                                                      open = FALSE,icon = icon("greater-than")),
                                                                                      
                                                                                      hr(),

                                                                                      accordion_panel( title = h5(" \ Lables"),
                                                                                                       value = "lablesDisCharDT",
                                                                                                       awesomeCheckbox(inputId = "tipLabelsDisCharDT",label = strong("Show tip lables"), 
                                                                                                                       value = T,status = "info"),
                                                                                                       awesomeCheckbox(inputId = "aligntiplabelDisCharDT",
                                                                                                                       label = strong("Align tip label"), 
                                                                                                                       value = F,status = "info"),
                                                                                                       awesomeCheckbox(inputId = "underscoreDisCharDT",
                                                                                                                       label = strong("Underscore"), 
                                                                                                                       value = F,status = "info"),
                                                                                                       selectInput(selected = "plain text",
                                                                                                                   inputId = "fontDisCharDT",
                                                                                                                   label = strong('Font'),
                                                                                                                   choices = c( "plain text" = "1" ,  "bold" = "2", 
                                                                                                                                "italic" = "3",  "bold-italic" = "4")),
                                                                                                       numericInput(inputId = "cexDisCharDT",
                                                                                                                    label = "size labels",value = 1 ,
                                                                                                                    min =0.1 ,max =10 ,step =0.001 ,width = "40%"),
                                                                                                       selectInput(selected = NULL,
                                                                                                                   inputId = "adjDisCharDT",
                                                                                                                   label = strong('Justification'),
                                                                                                                   choices = c( "left-justification" = "0" ,  "centering" = "0.5", 
                                                                                                                                "right-justification" = "1", NULL)),
                                                                                                      numericInput(inputId = "srtDisCharDT",
                                                                                                                   label = "Label rotation",
                                                                                                                   value = 0.1 ,min =0.1,
                                                                                                                   max =360 ,step =0.1 ,width = "40%"),
                                                                                                      numericInput(inputId = "labeloffsetDisCharDT",
                                                                                                                   label = "Label offset",value = 0.01 ,
                                                                                                                   min =0.1 ,max =10 ,step =0.001 ,width = "40%"),
                                                                                                      selectInput(selected = "horizontal",
                                                                                                                  inputId = "lab4utDisCharDT",
                                                                                                                  label = strong('Labels for unroot'),
                                                                                                                  choices = c( "horizontal" ,  "axial")),
                                                                                                      open = FALSE,icon = icon("greater-than")),

                                                                                      hr(),
                                                                                      
                                                                                      accordion_panel( title = h5(" \ Margins"),
                                                                                                       value = "marginsDisCharDT",
                                                                                                       awesomeCheckbox(inputId = "nomarginDisCharDT",
                                                                                                                       label = strong("No margin"), 
                                                                                                                       value = T,status = "info"),
                                                                                                       
                                                                                                       # numericInput(inputId = "xlimDisCharDT",
                                                                                                       #              label = "x lim",value = 0.1,
                                                                                                       #              min =0.1 ,max =10 ,step =0.001 ,width = "40%"),
                                                                                                       # numericInput(inputId = "ylimDisCharDT",
                                                                                                       #              label = "y lim",value = 0.1,
                                                                                                       #              min =0.1 ,max =10 ,step =0.001 ,width = "40%"),
                                                                                                       open = FALSE,icon = icon("greater-than")),
                                                                                      
                                                                                      
                                                                                      hr(),
                                                                                      
                                                                                      accordion_panel( title = h5(" \ Nodes"),
                                                                                                       value = "NodesDisCharDT",
                                                                                                       awesomeCheckbox(inputId = "nodelabelDisCharDT",
                                                                                                                       label = strong("Show node label"), 
                                                                                                                       value = F,status = "info"),
                                                                                                       
                                                                                                       numericInput(inputId = "nodeWidthDisCharDT",
                                                                                                                    label = "Node width",value = 1 ,
                                                                                                                    min =0.1 ,max =10 ,step =0.01 ,width = "40%"),
                                                                                                       
                                                                                                       selectInput(selected = "plain",
                                                                                                                   inputId = "nodeltyDisCharDT",
                                                                                                                   label = strong('Line Type'),
                                                                                                                   choices = c( "plain" = "1" ,  "dashed" = "2", 
                                                                                                                                "dotted" = "3",  "dotdash" = "4", 
                                                                                                                                "longdash" = "5" , "twodash" = "6")),
                                                                                                       open = FALSE,icon = icon("greater-than")),
                                                                                      
                                                                                      
                                                                                      ),fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold")))
                                                                      ))
                                                               
                                                               # Finish Div: DisCharDTReset
                                                           ),
                                                           
                                                           actionButton("ResetDisCharDT",width = "100%",label = "Restart initial values")
                                                           # Finish tabPanel: DisCharDT
                                                  ),
                                                  
                                                  #-------------------------------------------ANS Discrete Characters ANALYSIS ----------------------------------------------------
                                                  tabPanel(id="DisCharANA",title = "ANALYSIS",
                                                           
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "DisCharANAReset",
                                                               fluidRow(
                                                                 column(10, 
                                                                        fluidRow(
                                                                          column(3,sidebarPanel(width = "100%",
                                                                                                p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                hr(),
                                                                                                actionButton(inputId = "loadMLDisCharANA",label = "Load/Refresh data" ,width = "100%"),hr(),
                                                                                                
                                                                                                selectInput("select", label = "Estimation", 
                                                                                                            choices = list("Marginal" = "marginal", "Joint" = "joint"), 
                                                                                                            selected = "marginal"),
                                                                                                selectInput("ModMLDisCharANA", "Set models",choices = NULL,multiple = TRUE),
                                                                                                uiOutput("addModMLDisCharANA"),
                                                                                                
                                                                                                
                                                                                                #----------------- nextime add matrix
                                                                        
                                                                                                
                                                                                                
                                                                          )),
                                                                          column(9,fluidRow(column(8,card(height = "680px",full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                                          card_body( plotOutput("plotDisCharANA", inline = T)),
                                                                          )),
                                                                          column(4, card(full_screen = TRUE, card_header( strong("Data Structure")),
                                                                                         verbatimTextOutput("srtDisCharANA")),
                                                                                 card(full_screen = TRUE,
                                                                                      card_header( strong("Messages/Errors")),
                                                                                      verbatimTextOutput("infoDisCharANA"))
                                                                                 
                                                                          ))))),
                                                                 column(2, sidebarPanel(width = "100%",p("Graphic Controls", style = "align:center;text-align:center; font-weight: bold"),hr(),
                                                                                        awesomeCheckbox(inputId = "ViewPlotDisCharANA",label = strong("Display Plot"), 
                                                                                                        value = T,status = "info"), hr(),
                                                                                        sliderInput(inputId = "HeightDisCharANA" ,
                                                                                                    label = " Height",min =0 ,max = 10000,value = 400),
                                                                                        sliderInput(inputId = "WidthDisCharANA" ,
                                                                                                    label = " Width",min =0 ,max = 10000,value = 600 ), 
                                                                                        hr(),
                                                                                        accordion_panel( title = h5(" \ Tree and edge design"),
                                                                                                         value = "SetTreeDisCharANA",
                                                                                                        
                                                                                                         
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Lables"),
                                                                                                         value = "labels DisCharANA",
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Margins"),
                                                                                                         value = "marginsDisCharANA",
                                                                                                        
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Nodes"),
                                                                                                         value = "NodesDisCharANA",
                                                                                                         
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        
                                                                 ),fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold")))
                                                                 ))
                                                               
                                                               # Finish Div: DisCharANAReset
                                                           ),
                                                           
                                                           actionButton("ResetDisCharANA",width = "100%",label = "Restart initial values")
                                                           # Finish tabPanel: DisCharANA
                                                  ),
                                                  
                                                  
                                                  
                                                  # Finish tabsetPanel: TabsDisChar
                                      )
                                      # Finish tabPanel: DisChar
                             ),
                             
                                                  #-------------------------------------------ANS Continuous Characters ----------------------------------------------------
                             tabPanel(id = "ConChar", title = "Continuous Characters",
                                      tabsetPanel(id="TabsConChar",type="tabs",
                                                  #-------------------------------------------ANS Continuous Characters DATA----------------------------------------------------
                                                  tabPanel(id="ConCharDT",title = "DATA",
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "ConCharDTReset",
                                                               fluidRow(
                                                                 column(10, 
                                                                        fluidRow(
                                                                          column(3,sidebarPanel(width = "100%",
                                                                                                p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                hr(),
                                                                                                
                                                                                                
                                                                          )),
                                                                          column(9,fluidRow(column(8,card(height = "680px",full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                                          card_body( plotOutput("plotConCharDT", inline = T)),
                                                                          )),
                                                                          column(4, card(full_screen = TRUE, card_header( strong("Data Structure")),
                                                                                         verbatimTextOutput("srtConCharDT")),
                                                                                 card(full_screen = TRUE,
                                                                                      card_header( strong("Messages/Errors")),
                                                                                      verbatimTextOutput("infoConCharDT"))
                                                                                 
                                                                          ))))),
                                                                 column(2, sidebarPanel(width = "100%",p("Graphic Controls", style = "align:center;text-align:center; font-weight: bold"),hr(),
                                                                                        awesomeCheckbox(inputId = "ViewPlotConCharDT",label = strong("Display Plot"), 
                                                                                                        value = T,status = "info"), hr(),
                                                                                        sliderInput(inputId = "HeightConCharDT" ,
                                                                                                    label = " Height",min =0 ,max = 10000,value = 400),
                                                                                        sliderInput(inputId = "WidthConCharDT" ,
                                                                                                    label = " Width",min =0 ,max = 10000,value = 600 ), 
                                                                                        hr(),
                                                                                        accordion_panel( title = h5(" \ Tree and edge design"),
                                                                                                         value = "SetTreeConCharDT",
                                                                                                         
                                                                                                         
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Lables"),
                                                                                                         value = "labels ConCharDT",
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Margins"),
                                                                                                         value = "marginsConCharDT",
                                                                                                         
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Nodes"),
                                                                                                         value = "NodesConCharDT",
                                                                                                         
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        
                                                                 ),fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold")))
                                                                 ))
                                                               
                                                               # Finish Div: ConCharDTReset
                                                           ),
                                                           
                                                           actionButton("ResetConCharDT",width = "100%",label = "Restart initial values")
                                                           # Finish tabPanel: ConCharDT
                                                  ),
                                                  #-------------------------------------------ANS Continuous Characters ANALYSIS ----------------------------------------------------
                                                  tabPanel(id="ConCharANA",title = "ANALYSIS",
                                                           tabsetPanel(id="PillsDiverModANA",type="pills",
                                                                       
                                                  #-------------------------------------------ANS Characters ANALYSIS: Maxumum Likelihood ----------------------------------------------------
                                                                       tabPanel(id="MLConCharANA",title = "Maxumum Likelihood",
                                                                                # Use Div to reset panels
                                                                                useShinyjs(),
                                                                                div(id = "MLConCharANAReset",
                                                                                    fluidRow(
                                                                                      column(10, 
                                                                                             fluidRow(
                                                                                               column(3,sidebarPanel(width = "100%",
                                                                                                                     p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                                     hr(),
                                                                                                                     
                                                                                                                     
                                                                                               )),
                                                                                               column(9,fluidRow(column(8,card(height = "680px",full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                                                               card_body( plotOutput("plotMLConCharANA", inline = T)),
                                                                                               )),
                                                                                               column(4, card(full_screen = TRUE, card_header( strong("Data Structure")),
                                                                                                              verbatimTextOutput("srtMLConCharANA")),
                                                                                                      card(full_screen = TRUE,
                                                                                                           card_header( strong("Messages/Errors")),
                                                                                                           verbatimTextOutput("infoMLConCharANA"))
                                                                                                      
                                                                                               ))))),
                                                                                      column(2, sidebarPanel(width = "100%",p("Graphic Controls", style = "align:center;text-align:center; font-weight: bold"),hr(),
                                                                                                             awesomeCheckbox(inputId = "ViewPlotMLConCharANA",label = strong("Display Plot"), 
                                                                                                                             value = T,status = "info"), hr(),
                                                                                                             sliderInput(inputId = "HeightMLConCharANA" ,
                                                                                                                         label = " Height",min =0 ,max = 10000,value = 400),
                                                                                                             sliderInput(inputId = "WidthMLConCharANA" ,
                                                                                                                         label = " Width",min =0 ,max = 10000,value = 600 ), 
                                                                                                             hr(),
                                                                                                             accordion_panel( title = h5(" \ Tree and edge design"),
                                                                                                                              value = "SetTreeMLConCharANA",
                                                                                                                              
                                                                                                                              
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Lables"),
                                                                                                                              value = "labels MLConCharANA",
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Margins"),
                                                                                                                              value = "marginsMLConCharANA",
                                                                                                                              
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Nodes"),
                                                                                                                              value = "NodesMLConCharANA",
                                                                                                                              
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             
                                                                                      ),fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold")))
                                                                                      ))
                                                                                    
                                                                                    # Finish Div: MLConCharANAReset
                                                                                ),
                                                                                
                                                                                actionButton("ResetMLConCharANA",width = "100%",label = "Restart initial values")
                                                                                #Finish TabPanel: MLConCharANA           
                                                                       ),
                                                                       
                                                                       
                                                  #-------------------------------------------ANS ANALYSIS: MCMC ----------------------------------------------------
                                                                       tabPanel(id="BIConCharANA",title = "Stochastic mapping",
                                                                                # Use Div to reset panels
                                                                                useShinyjs(),
                                                                                div(id = "BIConCharANAReset",
                                                                                    fluidRow(
                                                                                      column(10, 
                                                                                             fluidRow(
                                                                                               column(3,sidebarPanel(width = "100%",
                                                                                                                     p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                                     hr(),
                                                                                                                     
                                                                                                                     
                                                                                               )),
                                                                                               column(9,fluidRow(column(8,card(height = "680px",full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                                                               card_body( plotOutput("plotBIConCharANA", inline = T)),
                                                                                               )),
                                                                                               column(4, card(full_screen = TRUE, card_header( strong("Data Structure")),
                                                                                                              verbatimTextOutput("srtBIConCharANA")),
                                                                                                      card(full_screen = TRUE,
                                                                                                           card_header( strong("Messages/Errors")),
                                                                                                           verbatimTextOutput("infoBIConCharANA"))
                                                                                                      
                                                                                               ))))),
                                                                                      column(2, sidebarPanel(width = "100%",p("Graphic Controls", style = "align:center;text-align:center; font-weight: bold"),hr(),
                                                                                                             awesomeCheckbox(inputId = "ViewPlotBIConCharANA",label = strong("Display Plot"), 
                                                                                                                             value = T,status = "info"), hr(),
                                                                                                             sliderInput(inputId = "HeightBIConCharANA" ,
                                                                                                                         label = " Height",min =0 ,max = 10000,value = 400),
                                                                                                             sliderInput(inputId = "WidthBIConCharANA" ,
                                                                                                                         label = " Width",min =0 ,max = 10000,value = 600 ), 
                                                                                                             hr(),
                                                                                                             accordion_panel( title = h5(" \ Tree and edge design"),
                                                                                                                              value = "SetTreeBIConCharANA",
                                                                                                                              
                                                                                                                              
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Lables"),
                                                                                                                              value = "labels BIConCharANA",
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Margins"),
                                                                                                                              value = "marginsBIConCharANA",
                                                                                                                              
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Nodes"),
                                                                                                                              value = "NodesBIConCharANA",
                                                                                                                              
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             
                                                                                      ),fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold")))
                                                                                      ))
                                                                                    
                                                                                    # Finish Div: BIConCharANAReset
                                                                                ),
                                                                                
                                                                                actionButton("ResetBIConCharANA",width = "100%",label = "Restart initial values")
                                                                                #Finish TabPanel: BIConCharANA           
                                                                       ),
                                                                       
                                                                       
                                                                       #Finish TabsetPanel: PillsConCharANA
                                                           )
                                                           # Finish tabPanel: ConCharANA
                                                  )
                                                  
                                                  
                                                  
                                                  # Finish tabsetPanel: TabsConChar
                                      )
                                      # Finish tabPanel: ConChar
                             ),
                             
                             "----",
                             "Basic Diversification Analyses",
                                                  #-------------------------------------------LTT ----------------------------------------------------
                             tabPanel(id = "Ltt", title = "Phylogenetic Signal",
                                      tabsetPanel(id="TabsLtt",type="tabs",
                                                  #-------------------------------------------LTT DATA ----------------------------------------------------
                                                  tabPanel(id="LttDT",title = "DATA",
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "LttDTReset",
                                                               fluidRow(
                                                                 column(10, 
                                                                        fluidRow(
                                                                          column(3,sidebarPanel(width = "100%",
                                                                                                p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                hr(),
                                                                                                
                                                                                                
                                                                          )),
                                                                          column(9,fluidRow(column(8,card(height = "680px",full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                                          card_body( plotOutput("plotLttDT", inline = T)),
                                                                          )),
                                                                          column(4, card(full_screen = TRUE, card_header( strong("Data Structure")),
                                                                                         verbatimTextOutput("srtLttDT")),
                                                                                 card(full_screen = TRUE,
                                                                                      card_header( strong("Messages/Errors")),
                                                                                      verbatimTextOutput("infoLttDT"))
                                                                                 
                                                                          ))))),
                                                                 column(2, sidebarPanel(width = "100%",p("Graphic Controls", style = "align:center;text-align:center; font-weight: bold"),hr(),
                                                                                        awesomeCheckbox(inputId = "ViewPlotLttDT",label = strong("Display Plot"), 
                                                                                                        value = T,status = "info"), hr(),
                                                                                        sliderInput(inputId = "HeightLttDT" ,
                                                                                                    label = " Height",min =0 ,max = 10000,value = 400),
                                                                                        sliderInput(inputId = "WidthLttDT" ,
                                                                                                    label = " Width",min =0 ,max = 10000,value = 600 ), 
                                                                                        hr(),
                                                                                        accordion_panel( title = h5(" \ Tree and edge design"),
                                                                                                         value = "SetTreeLttDT",
                                                                                                         
                                                                                                         
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Lables"),
                                                                                                         value = "labels LttDT",
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Margins"),
                                                                                                         value = "marginsLttDT",
                                                                                                         
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Nodes"),
                                                                                                         value = "NodesLttDT",
                                                                                                         
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        
                                                                 ),fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold")))
                                                                 ))
                                                               
                                                               # Finish Div: LttDTReset
                                                           ),
                                                           
                                                           actionButton("ResetLttDT",width = "100%",label = "Restart initial values")
                                                           # Finish tabPanel: LttDT
                                                  ),
                                                  #-------------------------------------------LTT ANALYSIS ----------------------------------------------------
                                                 
                                                          
                                                                       tabPanel(id="LttANA",title = "ANALYSIS",
                                                                                # Use Div to reset panels
                                                                                useShinyjs(),
                                                                                div(id = "LttANAReset",
                                                                                    fluidRow(
                                                                                      column(10, 
                                                                                             fluidRow(
                                                                                               column(3,sidebarPanel(width = "100%",
                                                                                                                     p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                                     hr(),
                                                                                                                     
                                                                                                                     
                                                                                               )),
                                                                                               column(9,fluidRow(column(8,card(height = "680px",full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                                                               card_body( plotOutput("plotLttANA", inline = T)),
                                                                                               )),
                                                                                               column(4, card(full_screen = TRUE, card_header( strong("Data Structure")),
                                                                                                              verbatimTextOutput("srtLttANA")),
                                                                                                      card(full_screen = TRUE,
                                                                                                           card_header( strong("Messages/Errors")),
                                                                                                           verbatimTextOutput("infoLttANA"))
                                                                                                      
                                                                                               ))))),
                                                                                      column(2, sidebarPanel(width = "100%",p("Graphic Controls", style = "align:center;text-align:center; font-weight: bold"),hr(),
                                                                                                             awesomeCheckbox(inputId = "ViewPlotLttANA",label = strong("Display Plot"), 
                                                                                                                             value = T,status = "info"), hr(),
                                                                                                             sliderInput(inputId = "HeightLttANA" ,
                                                                                                                         label = " Height",min =0 ,max = 10000,value = 400),
                                                                                                             sliderInput(inputId = "WidthLttANA" ,
                                                                                                                         label = " Width",min =0 ,max = 10000,value = 600 ), 
                                                                                                             hr(),
                                                                                                             accordion_panel( title = h5(" \ Tree and edge design"),
                                                                                                                              value = "SetTreeLttANA",
                                                                                                                              
                                                                                                                              
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Lables"),
                                                                                                                              value = "labels LttANA",
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Margins"),
                                                                                                                              value = "marginsLttANA",
                                                                                                                              
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Nodes"),
                                                                                                                              value = "NodesLttANA",
                                                                                                                              
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             
                                                                                      ),fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold")))
                                                                                      ))
                                                                                    
                                                                                    # Finish Div: LttANAReset
                                                                                ),
                                                                                
                                                                                actionButton("ResetLttANA",width = "100%",label = "Restart initial values")
                                                                           
                                                           # Finish tabPanel: LttANA
                                                  )
                                                  
                                                  
                                                  
                                                  # Finish tabsetPanel: TabsLtt
                                      )
                                      # Finish tabPanel: Ltt
                             ),
                                                  #-------------------------------------------Diversification Models-----------------------
                             tabPanel(id = "DivMod", title = "Phylogenetic Signal",
                                      tabsetPanel(id="TabsDivMod",type="tabs",
                                                  #-------------------------------------------DivMod DATA ----------------------------------------------------
                                                  tabPanel(id="DivModDT",title = "DATA",
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "DivModDTReset",
                                                               fluidRow(
                                                                 column(10, 
                                                                        fluidRow(
                                                                          column(3,sidebarPanel(width = "100%",
                                                                                                p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                hr(),
                                                                                                
                                                                                                
                                                                          )),
                                                                          column(9,fluidRow(column(8,card(height = "680px",full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                                          card_body( plotOutput("plotDivModDT", inline = T)),
                                                                          )),
                                                                          column(4, card(full_screen = TRUE, card_header( strong("Data Structure")),
                                                                                         verbatimTextOutput("srtDivModDT")),
                                                                                 card(full_screen = TRUE,
                                                                                      card_header( strong("Messages/Errors")),
                                                                                      verbatimTextOutput("infoDivModDT"))
                                                                                 
                                                                          ))))),
                                                                 column(2, sidebarPanel(width = "100%",p("Graphic Controls", style = "align:center;text-align:center; font-weight: bold"),hr(),
                                                                                        awesomeCheckbox(inputId = "ViewPlotDivModDT",label = strong("Display Plot"), 
                                                                                                        value = T,status = "info"), hr(),
                                                                                        sliderInput(inputId = "HeightDivModDT" ,
                                                                                                    label = " Height",min =0 ,max = 10000,value = 400),
                                                                                        sliderInput(inputId = "WidthDivModDT" ,
                                                                                                    label = " Width",min =0 ,max = 10000,value = 600 ), 
                                                                                        hr(),
                                                                                        accordion_panel( title = h5(" \ Tree and edge design"),
                                                                                                         value = "SetTreeDivModDT",
                                                                                                         
                                                                                                         
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Lables"),
                                                                                                         value = "labels DivModDT",
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Margins"),
                                                                                                         value = "marginsDivModDT",
                                                                                                         
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        
                                                                                        hr(),
                                                                                        
                                                                                        accordion_panel( title = h5(" \ Nodes"),
                                                                                                         value = "NodesDivModDT",
                                                                                                         
                                                                                                         open = FALSE,icon = icon("greater-than")),
                                                                                        
                                                                                        
                                                                 ),fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold")))
                                                                 ))
                                                               
                                                               # Finish Div: DivModDTReset
                                                           ),
                                                           
                                                           actionButton("ResetDivModDT",width = "100%",label = "Restart initial values")
                                                           # Finish tabPanel: DivModDT
                                                  ),
                                                  #-------------------------------------------DivMod ANALYSIS ----------------------------------------------------
                                                  tabPanel(id="DivModANA",title = "ANALYSIS",
                                                           tabsetPanel(id="PillsDiverModANA",type="pills",
                                                                       
                                                  #-------------------------------------------DivMod ANALYSIS: Maxumum Likelihood ----------------------------------------------------
                                                                       tabPanel(id="MLDivModANA",title = "Maxumum Likelihood",
                                                                                # Use Div to reset panels
                                                                                useShinyjs(),
                                                                                div(id = "MLDivModANAReset",
                                                                                    fluidRow(
                                                                                      column(10, 
                                                                                             fluidRow(
                                                                                               column(3,sidebarPanel(width = "100%",
                                                                                                                     p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                                     hr(),
                                                                                                                     
                                                                                                                     
                                                                                               )),
                                                                                               column(9,fluidRow(column(8,card(height = "680px",full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                                                               card_body( plotOutput("plotMLDivModANA", inline = T)),
                                                                                               )),
                                                                                               column(4, card(full_screen = TRUE, card_header( strong("Data Structure")),
                                                                                                              verbatimTextOutput("srtMLDivModANA")),
                                                                                                      card(full_screen = TRUE,
                                                                                                           card_header( strong("Messages/Errors")),
                                                                                                           verbatimTextOutput("infoMLDivModANA"))
                                                                                                      
                                                                                               ))))),
                                                                                      column(2, sidebarPanel(width = "100%",p("Graphic Controls", style = "align:center;text-align:center; font-weight: bold"),hr(),
                                                                                                             awesomeCheckbox(inputId = "ViewPlotMLDivModANA",label = strong("Display Plot"), 
                                                                                                                             value = T,status = "info"), hr(),
                                                                                                             sliderInput(inputId = "HeightMLDivModANA" ,
                                                                                                                         label = " Height",min =0 ,max = 10000,value = 400),
                                                                                                             sliderInput(inputId = "WidthMLDivModANA" ,
                                                                                                                         label = " Width",min =0 ,max = 10000,value = 600 ), 
                                                                                                             hr(),
                                                                                                             accordion_panel( title = h5(" \ Tree and edge design"),
                                                                                                                              value = "SetTreeMLDivModANA",
                                                                                                                              
                                                                                                                              
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Lables"),
                                                                                                                              value = "labels MLDivModANA",
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Margins"),
                                                                                                                              value = "marginsMLDivModANA",
                                                                                                                              
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Nodes"),
                                                                                                                              value = "NodesMLDivModANA",
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             
                                                                                      ),fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold")))
                                                                                      ))
                                                                                    
                                                                                    # Finish Div: MLDivModANAReset
                                                                                ),
                                                                                
                                                                                actionButton("ResetMLDivModANA",width = "100%",label = "Restart initial values")
                                                                                #Finish TabPanel: MLDivModANA           
                                                                       ),
                                                                       
                                                                       
                                                  #-------------------------------------------DivMod ANALYSIS: MCMC ----------------------------------------------------
                                                                       tabPanel(id="BIDivModANA",title = "Stochastic mapping",
                                                                                # Use Div to reset panels
                                                                                useShinyjs(),
                                                                                div(id = "BIDivModANAReset",
                                                                                    fluidRow(
                                                                                      column(10, 
                                                                                             fluidRow(
                                                                                               column(3,sidebarPanel(width = "100%",
                                                                                                                     p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                                     hr(),
                                                                                                                     
                                                                                                                     
                                                                                               )),
                                                                                               column(9,fluidRow(column(8,card(height = "680px",full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                                                               card_body( plotOutput("plotBIDivModANA", inline = T)),
                                                                                               )),
                                                                                               column(4, card(full_screen = TRUE, card_header( strong("Data Structure")),
                                                                                                              verbatimTextOutput("srtBIDivModANA")),
                                                                                                      card(full_screen = TRUE,
                                                                                                           card_header( strong("Messages/Errors")),
                                                                                                           verbatimTextOutput("infoBIDivModANA"))
                                                                                                      
                                                                                               ))))),
                                                                                      column(2, sidebarPanel(width = "100%",p("Graphic Controls", style = "align:center;text-align:center; font-weight: bold"),hr(),
                                                                                                             awesomeCheckbox(inputId = "ViewPlotBIDivModANA",label = strong("Display Plot"), 
                                                                                                                             value = T,status = "info"), hr(),
                                                                                                             sliderInput(inputId = "HeightBIDivModANA" ,
                                                                                                                         label = " Height",min =0 ,max = 10000,value = 400),
                                                                                                             sliderInput(inputId = "WidthBIDivModANA" ,
                                                                                                                         label = " Width",min =0 ,max = 10000,value = 600 ), 
                                                                                                             hr(),
                                                                                                             accordion_panel( title = h5(" \ Tree and edge design"),
                                                                                                                              value = "SetTreeBIDivModANA",
                                                                                                                              
                                                                                                                              
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Lables"),
                                                                                                                              value = "labels BIDivModANA",
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Margins"),
                                                                                                                              value = "marginsBIDivModANA",
                                                                                                                              
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             
                                                                                                             hr(),
                                                                                                             
                                                                                                             accordion_panel( title = h5(" \ Nodes"),
                                                                                                                              value = "NodesBIDivModANA",
                                                                                                                              
                                                                                                                              open = FALSE,icon = icon("greater-than")),
                                                                                                             
                                                                                                             
                                                                                      ),fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold")))
                                                                                      ))
                                                                                    
                                                                                    # Finish Div: BIDivModANAReset
                                                                                ),
                                                                                
                                                                                actionButton("ResetBIDivModANA",width = "100%",label = "Restart initial values")
                                                                                #Finish TabPanel: BIDivModANA           
                                                                       ),
                                                                       
                                                                       
                                                                       #Finish TabsetPanel: PillsDivModANA
                                                           )
                                                           # Finish tabPanel: DivModANA
                                                  )
                                                  
                                                  
                                                  
                                                  # Finish tabsetPanel: TabsDivMod
                                      )
                                      # Finish tabPanel: DivMod
                             ),
                             # Finish navbarmenu: GnavbarPage
                 ),
                                                  #-------------------------- R code panel ------------------------------
                 tabPanel(id="Rcode",title= "R code",
                          fluidRow( 
                            ))

# Finish  navbarPage ALL APP
))




