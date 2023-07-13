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
#-----------
library(ape)
library(phytools)
library(geiger)




#------------------ Sources ---------------

source("Functions/GenFunctions.R")
source("Theme/Guane_theme.R")


#------------------ UI ---------------
navbarPage(title = div( "", img(src = "Picture1.png",
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
                 
                 #Intro Panel
                 tabPanel(id = "Gintro",title =  "Intro"),
                 
                 # Collapsible panels
                 navbarMenu( title = "Methods",menuName = "GnavbarMenu",
                             tabPanel(id = "PhySignal", title = "Phylogenetic Signal",
                                      tabsetPanel(id="TabsPhySignal",type="tabs",
                                                  tabPanel(id="PhySignalDT",title = "DATA",
                                                           #-------------------------------------------Phylo Signal DATA ----------------------------------------------------
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "PhySignalDTReset",
                                                               fluidRow(column(10, fluidRow(column(3, fluidRow(sidebarPanel(width = "100%",
                                                                                                                            p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                                            hr(),
                                                                                                                            
                                                               )),
                                                               br(),
                                                               fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold"),hr()))),
                                                               column(9,fluidRow(column(8,card(full_screen = TRUE, card_header( strong("Main Plot")))),
                                                                                 column(4, card(full_screen = TRUE, card_header( strong("Data Structure"))),
                                                                                        card(full_screen = TRUE, card_header( strong("Messages/Errors")))))))),
                                                               column(2, sidebarPanel(width = "100%",strong("Graphic controls"),hr())))
                                                               
                                                               # Finish Div: PhySignalDTReset
                                                           ),
                                                           
                                                           # Finish tabPanel: PhySignalDT
                                                  ),
                                                  #-------------------------------------------Phylo Signal ANALYSIS ----------------------------------------------------
                                                  tabPanel(id="PhySignalANA",title = "ANALYSIS",
                                                           
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "PhySignalANAReset",
                                                               fluidRow(column(10, fluidRow(column(3, fluidRow(sidebarPanel(width = "100%",
                                                                                                                            p("Setting up", style = "align:center;text-align:center; font-weight: bold"),hr())),
                                                                                                   br(),
                                                                                                   fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold"),hr()))),
                                                                                            column(9,fluidRow(column(7,card(full_screen = TRUE, card_header( strong("Main Plot")))),
                                                                                                              column(5, card(full_screen = TRUE, card_header( strong("Data Structure"))),
                                                                                                                     card(full_screen = TRUE, card_header( strong("Messages/Errors")))))))),
                                                                        column(2, sidebarPanel(width = "100%",strong("Graphic controls"),hr())))
                                                               
                                                               # Finish Div: PhySignalANAReset
                                                           ),
                                                           
                                                           # Finish tabPanel: PhySignalANA
                                                  )
                                                  # Finish tabsetPanel: TabsPhySignal
                                      )
                                      # Finish tabPanel: PhySignal
                             ),
                             
                             "----",
                             "Ancestral State Estimation",
                             #-------------------------------------------ANS Discrete Characters DATA ----------------------------------------------------
                             tabPanel(id = "DisChar", title = "Discrete Characters",
                                      tabsetPanel(id="TabsDisChar",type="tabs",
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
                                                                                                
                                                                                                
                                                                                                
                                                               ),
                                                               br(),
                                                               fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold"),hr()))),
                                                               column(9,fluidRow(column(8,card(full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                               card_body( plotOutput("plotDisCharDT")),
                                                                                              )),
                                                                                 column(4, card(full_screen = TRUE, card_header( strong("Data Structure")),
                                                                                                verbatimTextOutput("srtDisCharDT")),
                                                                                        card(full_screen = TRUE,
                                                                                             card_header( strong("Messages/Errors")),
                                                                                                            verbatimTextOutput("infoDisCharDT")
                                                                                        )
                                                                                        
                                                                                 ))))),
                                                               column(2, sidebarPanel(width = "100%",strong("Graphic controls"),hr(),
                                                                                      awesomeCheckbox(inputId = "ViewPlotDisCharDT",label = strong("Display Plot"), 
                                                                                                      value = T,status = "info"), hr(),
                                                                                      accordion_panel( title = h5(" \ Tree and edge design"),
                                                                                                       value = "SetTreeDisCharDT",
                                                                                                       pickerInput(selected = "phylogram",
                                                                                                                   inputId = "typeDisCharDT",
                                                                                                                   label = strong('Sort Tree'),
                                                                                                                   choices = c("phylogram" , 
                                                                                                                               "cladogram", "fan","unrooted","radial","tidy"), 
                                                                                                       ),
                                                                                                       
                                                                                                       pickerInput(selected = "regular",
                                                                                                                   inputId = "ladderizeDisCharDT",
                                                                                                                   label = strong('Type'),
                                                                                                                   choices = c("regular", 
                                                                                                                               "ascending", "descending")
                                                                                                                   ),
                                                                                      awesomeCheckbox(inputId = "edgeLenghtDisCharDT",label = strong("Use edge lengths"), 
                                                                                                      value = T,status = "info"),
                                                                                      
                                                                                      numericInput(inputId = "edgeWidthDisCharDT",label = "set edge width",value = 1 ,min =0.1 ,max =10 ,step =0.001 ,width = "40%"),
                                                                                      
                                                                                      selectInput(selected = "plain",
                                                                                                  inputId = "edgetlyDisCharDT",
                                                                                                  label = strong('Line Type'),
                                                                                                  choices = c( "plain" = "1" ,  "dashed" = "2", 
                                                                                                              "dotted" = "3",  "dotdash" = "4", 
                                                                                                               "longdash" = "5" , "twodash" = "6")
                                                                                      ),
                                                                                      
                                                                                      open = FALSE,icon = icon("greater-than")),
                                                                                      
                                                                                      hr(),
                                                                                      accordion_panel( title = h5(" \ Nodes"),
                                                                                                       value = "NodesDisCharDT",
                                                                                                       open = FALSE,icon = icon("greater-than")),
                                                                                      
                                                                                      
                                                                                      hr(),
                                                                                      
                                                                                       
                                                                                      
                                                                                      accordion_panel( title = h5(" \ Lables"),
                                                                                                       value = "lablesDisCharDT",
                                                                                                       awesomeCheckbox(inputId = "tipLabelsDisCharDT",label = strong("Show tip lables"), 
                                                                                                                       value = T,status = "info"),
                                                                                                       open = FALSE,icon = icon("greater-than")),
                                                                                      
                                                                                      
                                                                                      hr(),
                                                                                      
                                                                                      accordion_panel( title = h5(" \ Margins"),
                                                                                                       value = "marginsDisCharDT",
                                                                                                       
                                                                                                       open = FALSE,icon = icon("greater-than")),
                                                                                      
                                                                                      
                                                                                      hr(),
                                                                                      
                                                                                      
                                                                                      )))
                                                               
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
                                                               fluidRow(column(10, fluidRow(column(3, fluidRow(sidebarPanel(width = "100%",
                                                                                                                            p("Setting up", style = "align:center;text-align:center; font-weight: bold"),hr())),
                                                                                                   br(),
                                                                                                   fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold"),hr()))),
                                                                                            column(9,fluidRow(column(7,card(full_screen = TRUE, card_header( strong("Main Plot")))),
                                                                                                              column(5, card(full_screen = TRUE, card_header( strong("Data Structure"))),
                                                                                                                     card(full_screen = TRUE, card_header( strong("Messages/Errors")))))))),
                                                                        column(2, sidebarPanel(width = "100%",strong("Graphic controls"),hr())))
                                                               
                                                               # Finish Div:DisCharANAReset
                                                           ),
                                                          
                                                           # Finish tabPanel:DisCharANA
                                                  )
                                                  # Finish tabsetPanel: TabsDisChar
                                      )
                                      # Finish tabPanel: DisChar
                             ),
                             
                             #-------------------------------------------ANS Continuous Characters DATA ----------------------------------------------------
                             tabPanel(id = "ContChr", " Continuous Characters",
                                      tabsetPanel(id="TabsContChr",type="tabs",
                                                  tabPanel(id="ContChrDT",title = "DATA",
                                                           
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "ContChrReset",
                                                               fluidRow(column(10, fluidRow(column(3, fluidRow(sidebarPanel(width = "100%",
                                                                                                                            p("Setting up", style = "align:center;text-align:center; font-weight: bold"),hr(),
                                                                                                                            
                                                               )),
                                                               br(),
                                                               fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold"),hr()))),
                                                               column(9,fluidRow(column(8,card(full_screen = TRUE, card_header( strong("Main Plot")))),
                                                                                 column(4, card(full_screen = TRUE, card_header( strong("Data Structure"))),
                                                                                        card(full_screen = TRUE, card_header( strong("Messages/Errors")))))))),
                                                               column(2, sidebarPanel(width = "100%",strong("Graphic controls"),hr())))
                                                               
                                                               # Finish Div: ContChrDTReset
                                                           ),
                                                          
                                                           # Finish tabPanel: ContChrDT
                                                  ),
                                                  
                                                  tabPanel(id="ContChrANA",title = "ANALYSIS",
                                                           
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "ContChrANAReset",
                                                               fluidRow(column(10, fluidRow(column(3, fluidRow(sidebarPanel(width = "100%",
                                                                                                                            p("Setting up", style = "align:center;text-align:center; font-weight: bold"),hr())),
                                                                                                   br(),
                                                                                                   fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold"),hr()))),
                                                                                            column(9,fluidRow(column(7,card(full_screen = TRUE, card_header( strong("Main Plot")))),
                                                                                                              column(5, card(full_screen = TRUE, card_header( strong("Data Structure"))),
                                                                                                                     card(full_screen = TRUE, card_header( strong("Messages/Errors")))))))),
                                                                        column(2, sidebarPanel(width = "100%",strong("Graphic controls"),hr())))
                                                               
                                                               # Finish Div:ContChrANAReset
                                                           ),
                                                           
                                                           # Finish tabPanel: ContChrANA
                                                  )
                                                  # Finish tabsetPanel: TabsContChr
                                      )
                                      # Finish tabPanel: ContChr
                             ),
                             
                             "----",
                             "Basic Diversification Analyses",
                             
                             tabPanel(id = "LTT", title = "Lineages Throught the time",
                                      tabsetPanel(id="TabsLTT",type="tabs",
                                                  tabPanel(id="LTTDT",title = "DATA",
                                                           
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "ContChrReset",
                                                               fluidRow(column(10, fluidRow(column(3, fluidRow(sidebarPanel(width = "100%",
                                                                                                                            p("Setting up", style = "align:center;text-align:center; font-weight: bold"),hr())),
                                                                                                   br(),
                                                                                                   fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold"),hr()))),
                                                                                            column(9,fluidRow(column(8,card(full_screen = TRUE, card_header( strong("Main Plot")))),
                                                                                                              column(4, card(full_screen = TRUE, card_header( strong("Data Structure"))),
                                                                                                                     card(full_screen = TRUE, card_header( strong("Messages/Errors")))))))),
                                                                        column(2, sidebarPanel(width = "100%",strong("Graphic controls"),hr())))
                                                               
                                                               # Finish Div: LTTDTReset
                                                           ),
                                                          
                                                           # Finish tabPanel: LTTDT
                                                  ),
                                                  
                                                  tabPanel(id="LTTANA",title = "ANALYSIS",
                                                           
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "LTTANAReset",
                                                               fluidRow(column(10, fluidRow(column(3, fluidRow(sidebarPanel(width = "100%",
                                                                                                                            p("Setting up", style = "align:center;text-align:center; font-weight: bold"),hr())),
                                                                                                   br(),
                                                                                                   fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold"),hr()))),
                                                                                            column(9,fluidRow(column(7,card(full_screen = TRUE, card_header( strong("Main Plot")))),
                                                                                                              column(5, card(full_screen = TRUE, card_header( strong("Data Structure"))),
                                                                                                                     card(full_screen = TRUE, card_header( strong("Messages/Errors")))))))),
                                                                        column(2, sidebarPanel(width = "100%",strong("Graphic controls"),hr())))
                                                               
                                                               # Finish Div:LTTReset
                                                           ),
                                                         
                                                           # Finish tabPanel: LTTANA
                                                  )
                                                  # Finish tabsetPanel: TabsLTT
                                      )
                                      
                             ),
                             
                             tabPanel(id = "DivMod", "Diversification Models",
                                      tabsetPanel(id="TabsDivMod",type="tabs",
                                                  tabPanel(id="DivModDT",title = "DATA",
                                                           
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "DivModReset",
                                                               fluidRow(column(10, fluidRow(column(3, fluidRow(sidebarPanel(width = "100%",
                                                                                                                            p("Setting up", style = "align:center;text-align:center; font-weight: bold"),hr())),
                                                                                                   br(),
                                                                                                   fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold"),hr()))),
                                                                                            column(9,fluidRow(column(8,card(full_screen = TRUE, card_header( strong("Main Plot")))),
                                                                                                              column(4, card(full_screen = TRUE, card_header( strong("Data Structure"))),
                                                                                                                     card(full_screen = TRUE, card_header( strong("Messages/Errors")))))))),
                                                                        column(2, sidebarPanel(width = "100%",strong("Graphic controls"),hr())))
                                                               
                                                               # Finish Div: DivModReset
                                                           ),
                                                          
                                                           # Finish tabPanel: DivModDT
                                                  ),
                                                  
                                                  tabPanel(id="DivModANA",title = "ANALYSIS",
                                                           
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "DivModReset",
                                                               fluidRow(column(10, fluidRow(column(3, fluidRow(sidebarPanel(width = "100%",
                                                                                                                            p("Setting up", style = "align:center;text-align:center; font-weight: bold"),hr())),
                                                                                                   br(),
                                                                                                   fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold"),hr()))),
                                                                                            column(9,fluidRow(column(7,card(full_screen = TRUE, card_header( strong("Main Plot")))),
                                                                                                              column(5, card(full_screen = TRUE, card_header( strong("Data Structure"))),
                                                                                                                     card(full_screen = TRUE, card_header( strong("Messages/Errors")))))))),
                                                                        column(2, sidebarPanel(width = "100%",strong("Graphic controls"),hr())))
                                                               
                                                               # Finish Div:DivModReset
                                                           ),
                                                           
                                                           # Finish tabPanel: DivModANA
                                                  )
                                                  # Finish tabsetPanel: TabsDivMod
                                      )
                                      
                             )
                             # Finish  navbarMenu
                 ),
                 
                 tabPanel(id="Rcode","Code",verbatimTextOutput("CodePanel"))
                 # Finish navbarPage: GnavbarPage
)


