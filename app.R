# Guane: a shiny app for phylogenetic comparative methods
# Author: Viviana Romero Alarcon
# Creation date: July 7 2023
# App file




#------------------ Libraries ---------------
library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(shinyjs)
library(ape)
library(phytools)



#------------------ Sources ---------------

source("modules/Theme/Guane_theme.R")
source("modules/UI/GenMod_UI.R")
source("modules/Server/GenMod_Server.R")
source("modules/Functions/GenFunctions.R")

#------------------ UI ---------------
ui <- navbarPage(title = div( "", img(src = "Picture1.png",
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
                                                                                                                            PhyloInputUI("PhySignalDT")
                                                                                                                            )),
                                                                                                   br(),
                                                                                                   fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold"),hr()))),
                                                                                            column(9,fluidRow(column(8,card(full_screen = TRUE, card_header( strong("Main Plot")))),
                                                                                                              column(4, card(full_screen = TRUE, card_header( strong("Data Structure"))),
                                                                                                                     card(full_screen = TRUE, card_header( strong("Messages/Errors")))))))),
                                                                        column(2, sidebarPanel(width = "100%",strong("Graphic controls"),hr())))
                                                               
                                                               # Finish Div: PhySignalDTReset
                                                               ),
                                                           resetPanelsUI('ResetPhySignalDT'),
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
                                                           resetPanelsUI('ResetPhySignalANA'),
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
                                               div(id = "DisCharReset",
                                                   fluidRow(column(10, fluidRow(column(3, fluidRow(sidebarPanel(width = "100%",
                                                                                                                p("Setting up", style = "align:center;text-align:center; font-weight: bold"),
                                                                                                                hr(),
                                                                                                               
                                                                                                                PhyloInputUI("PhyloDisCharDT"),
                                                                                                                
                                                                                                                fluidRow(column(6,aling="center",
                                                                                                                                materialSwitch(inputId = "ExTreeDisCharDT",label = "Phylo example",
                                                                                                                                               value = FALSE,status = "info",right = TRUE)),
                                                                                                                         column(6,align= "right",actionButton(inputId = "importTreeDisCharDT",
                                                                                                                                                              label = NULL,icon =icon(name = "upload"), 
                                                                                                                                                              width = "20%"))),
                                                                                                                hr(),
                                                                                                                
                                                                                                                csvInputUI("csvDisCharDT"),
                                                                                                                
                                                                                                                fluidRow(column(6,aling="center",
                                                                                                                                materialSwitch(inputId = "ExCVSDisCharDT",label = "CSV example",
                                                                                                                                               value = FALSE,status = "info",right = TRUE)),
                                                                                                                         column(6,align= "right",actionButton(inputId = "importCSVDisCharDT",
                                                                                                                                                              label = NULL,icon =icon(name = "upload"), 
                                                                                                                                                              width = "20%"))),
                                                                                                                hr(),
                                                                                                                
                                                                                                                checkNamesUI("chkNameDisCharDT")
                                                                                                                
                                                                                                                
                                                                                                                )),
                                                                                       br(),
                                                                                       fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold"),hr()))),
                                                                                column(9,fluidRow(column(8,card(full_screen = TRUE, card_header( strong("Main Plot")),
                                                                                                                plotUI("plotTreeDisCharDT"))),
                                                                                                  column(4, card(full_screen = TRUE, card_header( strong("Data Structure"))),
                                                                                                         card(full_screen = TRUE,
                                                                                                              card_header( strong("Messages/Errors")),
                                                                                                             infoUI("infoDisCharDT")
                                                                                                              )
                                                                                                         
                                                                                                         ))))),
                                                            column(2, sidebarPanel(width = "100%",strong("Graphic controls"),hr())))
                                                   
                                                   # Finish Div: DisCharDTReset
                                               ),
                                               resetPanelsUI('ResetDisCharDT'),
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
                                               resetPanelsUI('ResetDisCharANA'),
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
                                                           resetPanelsUI('ResetContChrDT'),
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
                                                           resetPanelsUI('ResetContChrANA'),
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
                                                           resetPanelsUI('ResetLTTDT'),
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
                                                           resetPanelsUI('ResetLTTANA'),
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
                                                           resetPanelsUI('ResetDivModDT'),
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
                                                           resetPanelsUI('ResetDivModANA'),
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
  

  
  
#------------------ Server ---------------
server <- function(input, output,session) {
  
  #### ancestral state reconstruction discrete characters
  
  #Vectors
  VarDisCharDT <- reactiveValues()
  VarDisCharDT$info <- NULL
  
  # Info vector
  infoServer("infoDisCharDT", VarDisCharDT$info)

  # Read Phylo
  treeDisCharDT <- eventReactive(input$importTreeDisCharDT,{
    if (input$ExTreeDisCharDT == T){
    readRDS(file = "examples/anoleTree.RDS")
    }else{
    PhyloInputServer("PhyloDisCharDT")}
  })
  
  # Record info Tree
  
  observeEvent(input$importTreeDisCharDT, {
    VarDisCharDT$info <- treeDisCharDT()
  })
  
  # read characters
  
  StatesDisChar <- eventReactive(input$importCSVDisCharDT,{
    if (input$importCSVDisCharDT == T){
      readRDS(file = 'data/anoleData.RDS')
    }else{
      csvInputServer(id = "csvDisCharDT")}
  })

  
  # plot phylo
  
  
    plotServer("plotTreeDisCharDT", tree = treeDisCharDT())
 
 
  
  
  #Finish server
}


#------------------ Run App---------------
shinyApp(ui = ui, server = server)
