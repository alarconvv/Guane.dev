# Guane: a shiny app for phylogenetic comparative methods
# Author: Viviana Romero Alarcon
# Creation date: July 7 2023
# App file




#------------------ Libraries ---------------
library(shiny)
library(bslib)
#library(dplyr)
library(shinyjs)



#------------------ Sources ---------------

source("Modules/Theme/Guane_theme.R")

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
                                                           "dropdown()",
                                                           # Use Div to reset panels
                                                           useShinyjs(),
                                                           div(id = "PhySignalDTReset",
                                                               fluidRow(column(10, fluidRow(column(3, fluidRow(sidebarPanel(width = "100%",
                                                                                                                            p("Setting up", style = "align:center;text-align:center; font-weight: bold"),hr())),
                                                                                                   br(),
                                                                                                   fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold"),hr()))),
                                                                                            column(9,fluidRow(column(8,card(full_screen = TRUE, card_header( strong("Main Plot")))),
                                                                                                              column(4, card(full_screen = TRUE, card_header( strong("Data Structure"))),
                                                                                                                     card(full_screen = TRUE, card_header( strong("Messages/Errors")))))))),
                                                                        column(2, sidebarPanel(width = "100%",strong("Graphic controls"),hr())))
                                                               
                                                               # Finish Div: PhySignalDTReset
                                                               )
                                                           # Finish tabPanel: PhySignalDT
                                                           ),
                                                  
                                                  tabPanel(id="PhySignalANA",title = "ANALYSIS",
                                                           "dropdown()",
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
                                                           )
                                                           # Finish tabPanel: PhySignalANA
                                                           )
                                                  # Finish tabsetPanel: TabsPhySignal
                                                  )
                                      # Finish tabPanel: PhySignal
                                      ),
                             
                             "----",
                             "Ancestral State Estimation",
                             
                             tabPanel(id = "DisChar", title = "Discrete Characters",
                                      tabsetPanel(id="TabsDisChar",type="tabs",
                                      tabPanel(id="DisCharDT",title = "DATA",
                                               "dropdown()",
                                               # Use Div to reset panels
                                               useShinyjs(),
                                               div(id = "DisCharReset",
                                                   fluidRow(column(10, fluidRow(column(3, fluidRow(sidebarPanel(width = "100%",
                                                                                                                p("Setting up", style = "align:center;text-align:center; font-weight: bold"),hr())),
                                                                                       br(),
                                                                                       fluidRow(sidebarPanel(width = "100%",p("Download", style = "align:center;text-align:center; font-weight: bold"),hr()))),
                                                                                column(9,fluidRow(column(8,card(full_screen = TRUE, card_header( strong("Main Plot")))),
                                                                                                  column(4, card(full_screen = TRUE, card_header( strong("Data Structure"))),
                                                                                                         card(full_screen = TRUE, card_header( strong("Messages/Errors")))))))),
                                                            column(2, sidebarPanel(width = "100%",strong("Graphic controls"),hr())))
                                                   
                                                   # Finish Div: DisCharDTReset
                                               )
                                               # Finish tabPanel: DisCharDT
                                      ),
                                      
                                      tabPanel(id="DisCharANA",title = "ANALYSIS",
                                               "dropdown()",
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
                                               )
                                               # Finish tabPanel:DisCharANA
                                      )
                                      # Finish tabsetPanel: TabsDisChar
                             )
                             # Finish tabPanel: DisChar
                             ),
                             
                             tabPanel(id = "ContChr", " Continuous Characters",
                                      tabsetPanel(id="TabsContChr",type="tabs",
                                                  tabPanel(id="ContChrDT",title = "DATA",
                                                           "dropdown()",
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
                                                               
                                                               # Finish Div: ContChrDTReset
                                                           )
                                                           # Finish tabPanel: ContChrDT
                                                  ),
                                                  
                                                  tabPanel(id="ContChrANA",title = "ANALYSIS",
                                                           "dropdown()",
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
                                                           )
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
                                                           "dropdown()",
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
                                                           )
                                                           # Finish tabPanel: LTTDT
                                                  ),
                                                  
                                                  tabPanel(id="LTTANA",title = "ANALYSIS",
                                                           "dropdown()",
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
                                                           )
                                                           # Finish tabPanel: LTTANA
                                                  )
                                                  # Finish tabsetPanel: TabsLTT
                                      )
                                      
                                      ),
                             
                             tabPanel(id = "DivMod", "Diversification Models",
                                      tabsetPanel(id="TabsDivMod",type="tabs",
                                                  tabPanel(id="DivModDT",title = "DATA",
                                                           "dropdown()",
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
                                                           )
                                                           # Finish tabPanel: DivModDT
                                                  ),
                                                  
                                                  tabPanel(id="DivModANA",title = "ANALYSIS",
                                                           "dropdown()",
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
                                                           )
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
  
 
}


#------------------ Run App---------------
shinyApp(ui = ui, server = server)
