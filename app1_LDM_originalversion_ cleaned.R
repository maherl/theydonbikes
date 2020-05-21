#install packages if necessary
#install.packages("shiny")
#install.packages("extrafont")
#install.packages("rsconnect")
#install.packages("fingertipsR")
#install.packages("shinyLP")
#install.packages("shinyBS")




#libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plyr)
library(dplyr)
library(knitr)
library(extrafont)
library(rsconnect)
library(plotly)
library(fingertipsR)
library(miniUI)
library(shinyLP)
library(shinyBS)


##================##
##----Get Data----##
##================##

# Cleanup
#rm(list=ls())

#setwd("C:/Users/maher05/OneDrive - Cancer Research UK/2018/ED Dashboard")
load(file="./shiny_v2.RData")


## app.R ##
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "Early Diagnosis", tags$li(class = "dropdown", 
                  actionButton("help", "", icon = icon("question-circle"), style='padding-right:60px; font-size:200%; background-color: #ffffff; border-color: #ffffff'))), 
               
  
  dashboardSidebar(
    tags$head(
      tags$style(HTML(".content-wrapper, .right-side{background-color: #ffffff;"))
    ),
    
    sidebarMenu(
      menuItem("Home", tabName = "hometab", icon = icon('th')),
      menuItem("Incidence by Stage", tabName = "incidence", icon = icon("signal")),
      menuItem("Screening", tabName = "screening", icon = icon("camera")),
      menuItem("Emergency Presentations", tabName = "referrals", icon = icon("fast-forward")),
      menuItem("Routes to Diagnosis", tabName = "rtd", icon = icon("map-signs")),
      menuItem("Routes to Diagnosis by Stage", tabName = "rtdinfo", icon = icon("info-circle")),
#      menuItem("62 Day Wait", tabName = "62day", icon = icon("bell")),
      menuItem("Diagnostic Workforce", tabName = "diagwork", icon = icon("odnoklassniki")),
      menuItem("Diagnostic Activity", tabName = "diagact", icon = icon("paper-plane")),
      menuItem("Diagnostic Waiting Times", tabName = "diagwait", icon = icon("medkit"))
    )
  ),
  
  dashboardBody(
     tags$head(
       tags$style(HTML('
          .skin-black .main-header .logo {
          background-color: #ffffff;
          }
          .skin-black .main-header .logo:hover {
          background-colour: #ffffff;
          }
          .skin-black .main-header .navbar {
          background-colour: #00b6ed;
          }
          .skin-black .main-sidebar .sidebar .sidebar-menu a{
          background-colour: #00b6ed;
          colour: #00b6ed;
          }
          .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
          background-color: #00b6ed;
          }
          .skin-blue .main-header .navbar .sidebar-toggle:hover{
          background-color: #00b6ed;
          }
          .box.box-solid.box-primary>.box-header{
color:#fff;
background:#222d32
          }
          '))
     ),
##=================##    
##----Tab Items----##
##=================##

tabItems(
  
##================##
##----Home Tab----##
##================##  
      
tabItem(tabName = "hometab",
        fluidRow(
            h1("Early Diagnosis Data Hub", align = "center"),
            h4( "produced by the Cancer Intelligence team", align ="center"),
              
            br(),
              
            img(src = "logoCRUK.jpg", height = 141, width = 300, style = "display: block; margin-left: auto; margin-right: auto;")),
              
            br(),
           
        fluidRow(
          column(width = 12,
                   box(width = 2, solidHeader = TRUE, tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}')))),
                   box(width = 8, solidHeader = TRUE, align = "center", tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'))),
                   h5(HTML("Please contact <a href='mailto:stats.signoff@cancer.org.uk?Subject=ED%20Data%20Hub%20Help' target='_top'>Stats Signoff</a> if you have any questions about the data and their interpretation, and to check whether you've used them in the right context."), 
                   br(),
                   br(),
                   "Additional information and helfpul tips are available by clicking on the help icon above."),
                   box(width = 2, solidHeader = TRUE))
            
          
          ))),

##==============================##
##----Incidence by Stage Tab----##
##==============================##

tabItem(tabName= "incidence",
        
        h2("Incidence by Stage"),
        h5("Caution should be taken when comparing incidence by stage data across countries due to variations in time periods, completeness of data, and staging definitions."),
        h5("The proportion of cases can be calculated with or without cases diagnosed at an unknown stage. Please contact us for advice on which way would be most suitable for your context."),
        
 
        #fluidRow(
          #box(width =11, column(width =12, uiOutput("note")), tags$head(tags$style("#note{font-size: 12px;}")))
        #),      
          br(),     
#England    
        fluidRow(
          column(width = 8,
                 box(width = NULL, plotlyOutput("ENG", height = 350), title = "England", solidHeader = TRUE, status = "primary")),
          
          column(width = 3,
                 box(width = NULL, selectInput(inputId = "cancersite", label = "Select Cancer Type:",
                      choices = CancerSitesENG),
                      checkboxInput(inputId = "known", label = "Include Unknowns", value = TRUE)),
                box(width = NULL, uiOutput("tab"), tags$head(tags$style("#tab{font-size: 13px;}")))
          )
          
        ),

#Scotland

       fluidRow(
          column(width = 8,
                box(width = NULL, plotlyOutput("SCT", height = 350), title = "Scotland", solidHeader = TRUE, status = "primary")),
          
          column(width = 3,
                box(width = NULL, selectInput(inputId = "cancersite2", label = "Select Cancer Type:",
                      choices = c("Breast (female)", "Colorectal", "Lung")), 
                      checkboxInput(inputId = "known2", label = "Include Unknowns", value = TRUE)),
                #box(width = NULL, uiOutput("note2"), tags$head(tags$style("#note2{font-size: 12px;}"))),
                box(width = NULL, uiOutput("tab2"), tags$head(tags$style("#tab2{font-size: 13px;}")))
    )
    ),
    
#Northern Ireland

        fluidRow(
          column(width = 8,
                box(width = NULL, plotlyOutput("NIR", height = 350), title = "Northern Ireland", solidHeader = TRUE, status = "primary")),
      
          column(width = 3,
                box(width = NULL, selectInput(inputId = "cancersite3", label = "Select Cancer Type:",
                      choices = CancerSitesNIR),
                      checkboxInput(inputId = "known3", label = "Include Unknowns", value = TRUE)),
                box(width = NULL, uiOutput("tab3"), tags$head(tags$style("#tab3{font-size: 13px;}")))
    )
    ),

#Wales

        fluidRow(
          column(width = 8,
                box(width = NULL, plotlyOutput("WAL", height = 350), title = "Wales", solidHeader = TRUE, status = "primary")),
          
          column(width = 3,
                box(width = NULL, selectInput(inputId = "cancersite4", label = "Select Cancer Type:",
                      choices = CancerSitesWAL),
                      checkboxInput(inputId = "known4", label = "Include Unknowns", value = TRUE)),
                box(width = NULL, uiOutput("tab4"), tags$head(tags$style("#tab4{font-size: 13px;}")))
          )   
      )
    ),

##=====================##
##----Screening Tab----##
##=====================##

tabItem(tabName = "screening",
        h2("Screening"),
        br(),
        
#Bowel screening

      fluidRow(
        column(width = 8,
               box(width = NULL, title ="Bowel Screening", solidHeader = TRUE, status = "primary", plotlyOutput("bowelplot"))),
        
        column(width = 3,
               box(width = NULL, uiOutput("tab5"), tags$head(tags$style("#tab5{font-size: 13px;}"))),
               box(width = NULL, uiOutput("note3"), tags$head(tags$style("#note3{font-size: 13px;}"))))),
#Breast screening

      fluidRow(
        column(width = 8,
               box(width = NULL, title ="Breast Screening", solidHeader = TRUE, status = "primary", plotlyOutput("breastplot"))),
        
        column(width = 3,
               box(width = NULL, uiOutput("tab6"), tags$head(tags$style("#tab6{font-size: 13px;}"))),
               box(width = NULL, uiOutput ("BreastNote"), tags$head(tags$style("BreastNote{font-size: 13px;")))
      )),

#Cervical screening
      fluidRow(
        column(width = 8,
               box(width = NULL, title = "Coverage of Cervical Screening in the UK (2016)", solidHeader = TRUE, status = "primary", tableOutput("cervicaltable"))),
      
        column(width = 3,
               box(width = NULL, selectInput(inputId = "agerange", label = "Age Range",
                      selected = "25-64",
                      choices = c("25-64", "25-49", "50-64"))),
              box(width = NULL, uiOutput("tab19"), tags$head(tags$style("#tab19{font-size: 13px;}"))),
              box(width = NULL, uiOutput("comingsoon2"), tags$head(tags$style("#note3{font-size: 13px;}")))))
    
),

##===================================##
##----Emergency Presentations Tab----##
##===================================##

    tabItem(tabName = "referrals",
            h2("Emergency Presentations"),
            h5("There are currently no routinely published data available for Scotland, Northern Ireland and Wales."),
            
            br(),
            
#England            
      fluidRow(
        column(width = 8,
              box(width = NULL, title = "England", solidHeader = TRUE, status = "primary", plotlyOutput("referralplot"))),
        
        column(width = 3,
              box(width = NULL, uiOutput("cautiontag")), 
              box(width = NULL, uiOutput("tag")))
        )

#Scotland
#            fluidRow(
#              box(title = "Scotland",
#                  status = "primary", solidHeader = TRUE,
#                  helpText("There is currently no routinely published data available for this measure."))
#            ),
#            fluidRow(
#              box(title = "Northern Ireland",
#                  status = "primary", solidHeader = TRUE,
#                  helpText("There is currently no routinely published data available for this measure."))
#            ),
#            fluidRow(
#              box(title = "Wales",
#                  status = "primary", solidHeader = TRUE,
#                  helpText("There is currently no routinely published data available for this measure."))
#            )),
#    tabItem(tabName= "dwt",
#            fluidRow(
#              box(
#                helpText("There is currently no routinely published data available for this measure.")
#              )
#            )
            ),

##===============================##
##----Routes to Diagnosis Tab----##
##===============================##

    tabItem(tabName= "rtd",
            h2("Routes to Diagnosis"),
            br(),

#England
                       
      fluidRow(
        column(width = 8,
            box(width = NULL, height = "auto", plotlyOutput("routes"), title = "England",solidHeader = TRUE, status = "primary")),
        
        column(width = 3,
            box(width = NULL, selectInput(inputId = "cancersite5", label = "Select Cancer Type:",
                choices = CancerSitesRTD, selected = "All Cancers Combined (excl. NMSC)")),
            box(width = NULL, uiOutput("tab9"), tags$head(tags$style("#tab{font-size: 12px;}"))),
            box(width = NULL, uiOutput("tab7"), tags$head(tags$style("#tab{font-size: 12px;}"))))
        ),

#Scotland
      fluidRow(
        column(width = 8,
            box(width = NULL, title = "Scotland", solidHeader = TRUE, status = "primary", helpText("There are currently no routinely published data available."))
              )),

#Northern Ireland
      fluidRow(
        column(width = 8,
            box(width = NULL, title = "Northern Ireland", solidHeader = TRUE, status = "primary", helpText("There are currently no routinely published data available. However, there is work underway to look at how to collect and track this information for Northern Ireland. Michael Chapman, CRUK, is on the board of this project."))
              )),

#Wales
      fluidRow(
        column(width = 8,
            box(width = NULL, title = "Wales", solidHeader = TRUE, status = "primary", helpText("There are currently no routinely published data available. However, there is work in progress by WCISU and Macmillan."))
            ))
    ),

##========================================##
##----Routes to Diagnosis by Stage Tab----##
##========================================##
    tabItem(tabName = "rtdinfo",
            h2("Routes to Diagnosis by Stage"),
            br(),
            
      fluidRow(
        column(width = 8, 
               box(width = NULL, uiOutput("routesinfoimg"))),
              
        column(width = 3,
              box(width = NULL, selectInput(inputId = "routesinfo", label = "Select Cancer Type",
                                    choices = c("All Cancers Combined", "Lung", "Bowel")), style="display: block; margin-left: auto; margin-right: auto;"),
              box(width = NULL, uiOutput("tab10")))
        )),

##=======================##
##----62 Day Wait Tab----##
##=======================##

#tabItem(tabName= "62day",
#            fluidRow(
#              box(
#                plotlyOutput("cwt"), title = "62 Day Wait", status = "primary", solidHeader = TRUE
#              ),
#              box(uiOutput("tab11"), tags$head(tags$style("#tab4{font-size: 13px;}")))
#            )
#    ),

##================================##
##----Diagnostic Workforce Tab----##
##================================##

    tabItem(tabName = "diagwork",
            h2("Diagnostic Workforce"),
            h5("This only shows workforce data related to radiologists. Please be aware that CRUK is also concerned about other areas of the diagnositc workforce (eg, endoscopists and pathologists)."),
            br(),
            
            fluidRow(
              column(width = 8,
                     box(width = NULL, plotlyOutput("radiologyvacant", height = 250), title = "United Kingdom",  solidHeader = TRUE, status = "primary")),
              
              column(width = 3,
                     #box(width = NULL, title = "Caution", solidHeader = FALSE, helpText("This only shows data related to radiologists, not the entire diagnostic workforce. Please be aware that CRUK is also concerned about other areas of the diagnostic workforce such as endoscopists and pathologists.")),
                     box(width = NULL, uiOutput("tab8"), tags$head(tags$style("#tab{font-size: 13px;}")))
                     ))
              ),

##===============================##
##----Diagnostic Activity Tab----##
##===============================##

    tabItem(tabName = "diagact",
            h2("Diagnostic Activity"),
            h5("Please note that the below diagnostic activities are not cancer specific but are counts of all chest x-rays, colonoscopies and gastrocopies in England."),
            br(),
            
            fluidRow(
              column(width = 8,
              box(width = NULL, plotlyOutput("test", height = 250), title = "England", solidHeader = TRUE, status = "primary")),
              
              column(width = 3,
              box(width = NULL, uiOutput("tab12"), tags$head(tags$style("#tab4{font-size: 13px;}"))))
            ),
            
            fluidRow(
              column(width = 8,
                box(width = NULL, plotlyOutput("colon", height = 250), title = "England", solidHeader = TRUE, status = "primary")),
              column(width = 3,
                box(width = NULL, uiOutput("tab13"), tags$head(tags$style("#tab4{font-size: 13px;}"))))
            ),
            fluidRow(
              column(width = 8,
      box(width = NULL, plotlyOutput("gastro", height = 250), title = "England", solidHeader = TRUE, status = "primary")),
              column(width = 3,
      box(width = NULL, uiOutput("tab14"), tags$head(tags$style("#tab4{font-size: 13px;}"))))
              ),
      fluidRow(
        column(width = 8,
        box(width = NULL, title = "Other UK Nations", solidHeader = TRUE, status = "primary",
            helpText("Data to be scoped and added for the other UK nations")))
      )),

##====================================##
##----Diagnostic Waiting Times Tab----##
##====================================##

    tabItem(tabName = "diagwait",
            h2("Diagnostic Waiting Times"),
            br(),
            
            fluidRow(
              column(width = 8,
                      box(width = NULL, plotlyOutput("mri", height = 250), title = "England", status = "primary", solidHeader = TRUE)),
              column(width = 3,
                      box(width = NULL, uiOutput("tab15"), tags$head(tags$style("#tab4{font-size: 13px;}"))))
            ),
             
            fluidRow(
               column(width = 8,
                      box(width = NULL, plotlyOutput("ct", height = 250), title = "England", status = "primary", solidHeader = TRUE)),
               column(width = 3,
                      box(width = NULL, uiOutput("tab16"), tags$head(tags$style("#tab4{font-size: 13px;}"))))
             ),
            
             fluidRow(
               column(width = 8,
                      box(width = NULL, plotlyOutput("ultra", height = 250), title = "England", status = "primary", solidHeader = TRUE)),
               column(width = 3,
               box(width = NULL, uiOutput("tab17"), tags$head(tags$style("#tab4{font-size: 13px;}"))))
             ),
            
            fluidRow(
              column(width = 8,
              box(width = NULL, title = "Other UK Nations",
                  status = "primary", solidHeader = TRUE,
                  helpText("Data to be scoped and added for the other UK nations")))
            )
            )
  )))
##################
##==============##
##----SERVER----##
##==============##
##################

server <- function(input, output) {
  
  observeEvent(input$help, {
    showModal(modalDialog(title = strong("Welcome to the Early Diagnosis Data Hub"), br(), br(),
                      fluidRow(column(width = 1, icon("info-circle", "fa-2x"), align = "center"),
                              (column(width = 11, "Additional information of each data point is available by hovering over the data points on the graphs."))), 
                      br(),
                      fluidRow(column(width = 1, icon("list", "fa-2x"), align = "center"),    
                              (column(width = 11, "Data that will be available soon on the Data Hub are listed under 'Coming soon' on the right side of each tab."))),
                      br(),
                      fluidRow(column(width = 1, icon("calendar", "fa-2x"), align = "center"),
                              (column(width = 11, (HTML("The percentage of patients receiving treatment within 62 days of referral data will be available soon. If you urgently require the data, please contact <a href='mailto:stats.signoff@cancer.org.uk?Subject=ED%20Data%20Hub%20Help' target='_top'>Stats Signoff</a>."))))),
                      br(),
                      fluidRow(column(width = 1, icon("search-plus", "fa-2x"), align = "center"),   
                              (column(width = 11, "To zoom in on an area of a graph, click and drag over that area. To zoom back out, double-click on the graph."))),
                      br(),
                      fluidRow(column(width = 1, icon("envelope", "fa-2x"), align = "center"),
                              (column(width = 11,(HTML("Please contact <a href='mailto:stats.signoff@cancer.org.uk?Subject=ED%20Data%20Hub%20Help' target='_top'>Stats Signoff</a> if you have any questions about the data and their interpretation, and to check whether you've used them in the right context."))))),  
                      br(),
         
         easyClose = TRUE, size = "l",
      footer = modalButton("Close"))
    )
  })
  
  four_colours <- c('#99E2F8', '#00B6ED','#1985d2', '#3254b6')
  five_colours <- c('#99E2F8', '#00B6ED','#1985d2', '#3254b6', '#4b239b')
  names(four_colours) <- c("1","2","3","4")
  names(five_colours) <- c("1","2","3","4","Unknown")
  
  colours <- reactive({
  ifelse(input$known == TRUE, five_colours, four_colours)
  })
  
  output$landing <-  renderUI({   #This is where the image is set
      img(src = "landing_image.png", width = "500px", height = "400px")
  })
  
  # Create list of colours 
  crukColours <- c('#99E2F8', '#00B6ED','#1985d2', '#3254b6', '#4b239b')
  


  
##=================================##
##----Incidence by Stage Output----##
##=================================##


#England  
  
  EnglandStage <- reactive({
    EnglandIncidencebyStage %>% filter(CancerSite ==input$cancersite, DateRangeName == "2016", CountryKey=="ENG", GeographyTypeKey == "Country")
  })
   
  output$ENG <- renderPlotly({
#adds data to plotly chart
    p <- plot_ly(EnglandStage())
    ifelse(input$known == TRUE, 
    p <- add_trace(p, x = ~StageKey, y = ~PercentageOfAllStages, type = 'bar', marker = list(color = crukColours), color= ~StageKey, colors = crukColours, hoverinfo = 'text', text = ~paste("</br> Stage: ", EnglandStage()$StageKey,"</br> Proportion: ", EnglandStage()$PercentageOfAllStages, "</br> Number of Cases: ", EnglandStage()$NumberOfCases)), p <- add_trace(p, x = ~StageKey, y = ~PercentOfKnownStages, type = 'bar', color= ~StageKey, colors = crukColours, hoverinfo = 'text', text = ~paste("</br> Stage: ", EnglandStage()$StageKey, "</br> Proportion: ", EnglandStage()$PercentOfKnownStages,"</br> Number of Cases: ", EnglandStage()$NumberOfCases)))  

#changes layout to CRUK branded style        
    p <- p %>%       
      layout(margin=list(t=50, l=75, b=50, r=25),
             title = paste("Incidence of", input$cancersite, "Cancer <br> by Stage at Diagnosis (2016)"),
             titlefont=list(family="Arial", size=18, color="#2E008B"),
             xaxis = list(title = '<b>Stage</b>', 
                          titlefont=list(family="Arial", size=16, color="#2E008B"),
                          tickfont=list(family="Arial", size=11, color="#343434")), 
             yaxis = list(side="left",
                          title = '<b>Proportion of Cases (%) </b>', 
                          tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                          titlefont=list(family="Arial", size=16, color="#2E008B"),
                          tickfont=list(family="Arial", size=14, color="#343434"), range =c(0,100)),
             showlegend = FALSE)
    
    ggplotly(p)
  })
#output$note <- renderUI({
#tagList("Note:  Caution should be taken if comparing the incidence by stage data across countries due to the difference in time periods reported, completeness of the data, and potential differences in staging definitions. The proportion of cases by stage at diagnosis can be calculated with and without including cases with an unknown stage. Contact us for advice on which way would be the most suitable for your context. To find the actual number of patients diagnosed at each stage (not just the percentage), hover over the data bars.")
#})
  url <- a("National Cancer Registration and Analysis Service (NCRAS)", href="http://www.ncin.org.uk/publications/survival_by_stage", target="_blank")
  output$tab <- renderUI({
    tagList(strong("Source:"), url, br(), strong("Next publication:"), ("Circa. Jan 2019"), br(), strong("Local breakdown:"), ("Available at CCG level"), br(), br(), ("*All cancer refers to listed cancers in drop down box only"))
  })

#Scotland  
    
ScotlandStage <- reactive({  
  IncidencebyStage %>% filter(CancerSite ==input$cancersite2, DateRangeName == "2015-2016", CountryKey=="SCT", GeographyTypeKey == "Country")
})
  output$SCT <- renderPlotly({
    p <- plot_ly(ScotlandStage())
    ifelse(input$known2 == TRUE, 
           p <- add_trace(p, x = ~StageKey, y = ~PercentageOfAllStages, type = 'bar', marker = list(color = crukColours), color= ~StageKey, colors = crukColours, hoverinfo = 'text', text = ~paste("</br> Stage: ", ScotlandStage()$StageKey, "</br> Proportion: ", ScotlandStage()$PercentageOfAllStages, "</br> Number of Cases: ", ScotlandStage()$NumberOfCases)), p <- add_trace(p, x = ~StageKey, y = ~PercentOfKnownStages, type = 'bar', color= ~StageKey, colors = crukColours, hoverinfo = 'text', text = ~paste("</br> Stage: ", ScotlandStage()$StageKey, "</br> Proportion: ", ScotlandStage()$PercentOfKnownStages,"</br> Number of Cases: ", ScotlandStage()$NumberOfCases)))  
    
    p <- p %>%       
      layout(margin=list(t=50, l=75, b=50, r=25),
             title = paste("Incidence of", input$cancersite2, "Cancer <br> by Stage at Diagnosis (2015-16)"),
             titlefont=list(family="Arial", size=18, color="#2E008B"),
             xaxis = list(title = '<b>Stage</b>', 
                          titlefont=list(family="Arial", size=16, color="#2E008B"),
                          tickfont=list(family="Arial", size=11, color="#343434")), 
             yaxis = list(side="left",
                          title = '<b>Proportion of Cases (%)</b>', 
                          tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                          titlefont=list(family="Arial", size=16, color="#2E008B"),
                          tickfont=list(family="Arial", size=14, color="#343434"), range =c(0,100)),
             showlegend = FALSE)
    
    ggplotly(p)
  })
  url2 <- a("ISD Scotland, Detect Cancer Early (DCE) Data,  2017", href="http://www.isdscotland.org/Health-Topics/Cancer/Detect-Cancer-Early/", target="_blank")
  output$tab2 <- renderUI({
    tagList(strong("Source:"), url2, ("(see original source for more up-to-date data)"), br(), strong("Local breakdown:"), ("Available at NHS Board and Cancer Network level"), br(), strong("Note:"), ("Data for some additional cancer types are available upon request. They are not published here due to complications with different time periods and staging systems."))
  })
#output$note2 <- renderUI({
#tagList("Note: Data for some additional cancer sites for Scotland are available upon request. They are not published here due to complications with differences in time periods and staging systems.")
#})

#some cancer types need the person figure, whereas others need just the male or female figure. This creates lists of which cancers have male figures and which have females figures so that we can filter appropriately in the reactive statement below
cancerTypeIncludeMale <- unique(paste(IncidencebyStage[IncidencebyStage$GenderKey == "M", ]$CancerSite, sep =""))
cancerTypeIncludeMale <- cancerTypeIncludeMale[cancerTypeIncludeMale != "Breast"]
cancerTypeIncludeFemale <- unique(IncidencebyStage[IncidencebyStage$GenderKey == "F", ]$CancerSite, sep ="")  
  
#Northern Ireland

NI_gender <- reactive({
  ifelse(input$cancersite3 %in% cancerTypeIncludeMale && !(input$cancersite3 %in% cancerTypeIncludeFemale), "M", ifelse(input$cancersite3 %in% cancerTypeIncludeFemale&& !(input$cancersite3 %in% cancerTypeIncludeMale), "F", "P"))
})  
   
  NIStage <- reactive({
    IncidencebyStage %>% filter(CancerSite ==input$cancersite3, DateRangeName == "2011-2015", CountryKey=="NIR", GeographyTypeKey == "Country", GenderKey == NI_gender())
  })
  
  output$NIR <- renderPlotly({
    p <- plot_ly(NIStage())
    ifelse(input$known3 == TRUE, 
           p <- add_trace(p, x = ~StageKey, y = ~PercentageOfAllStages, type = 'bar', marker = list(color = crukColours), color= ~StageKey, colors = crukColours, hoverinfo = 'text', text = ~paste("</br> Stage: ", NIStage()$StageKey,"</br> Proportion: ", NIStage()$PercentageOfAllStages, "</br> Number of Cases: ", NIStage()$NumberOfCases)), p <- add_trace(p, x = ~StageKey, y = ~PercentOfKnownStages, type = 'bar', color= ~StageKey, colors = crukColours, hoverinfo = 'text', text = ~paste("</br> Stage: ", NIStage()$StageKey, "</br> Proportion: ", NIStage()$PercentOfKnownStages,"</br> Number of Cases: ", NIStage()$NumberOfCases)))  
    
    p <- p %>%       
      layout(margin=list(t=50, l=75, b=50, r=25),
             title = paste("Incidence of", input$cancersite3, "Cancer <br> by Stage at Diagnosis (2011-15)"),
             titlefont=list(family="Arial", size=18, color="#2E008B"),
             xaxis = list(title = '<b>Stage</b>', 
                          titlefont=list(family="Arial", size=16, color="#2E008B"),
                          tickfont=list(family="Arial", size=11, color="#343434")), 
             yaxis = list(side="left",
                          title = '<b>Proportion of Cases (%)</b>', 
                          tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                          titlefont=list(family="Arial", size=16, color="#2E008B"),
                          tickfont=list(family="Arial", size=14, color="#343434"), range =c(0,100)),
             showlegend = FALSE)
    
    ggplotly(p)
  })
  url3 <- a("Northern Ireland Cancer Registry, Queens University Belfast, 2017", href="http://www.qub.ac.uk/research-centres/nicr/CancerInformation/official-statistics/", target="_blank")
  output$tab3 <- renderUI({
    tagList(strong("Source:"), url3, ("(see original source for more up-to-date data)"), br(), strong("Local breakdown:"), ("Not available at local level"), br(), br(), ("*All cancer refers to listed cancers in drop down box only"))
  })
  
#Wales
  
  WalesStage <- reactive({  
  IncidencebyStage %>% filter(CancerSite ==input$cancersite4, DateRangeName == "2015", GeographyTypeKey == "Country", DeprivationQuintile == "All Quintiles", GenderKey == "P", AgeBand == "All ages")
  })
  
  output$WAL <- renderPlotly({
    p <- plot_ly(WalesStage())
    ifelse(input$known4 == TRUE, 
           p <- add_trace(p, x = ~StageKey, y = ~PercentageOfAllStages, type = 'bar', marker = list(color = crukColours), color= ~StageKey, colors = crukColours, digits = 1, hoverinfo = 'text', text = ~paste("</br> Stage: ", WalesStage()$StageKey, "</br> Proportion: ", WalesStage()$PercentageOfAllStages, "</br> Number of Cases: ", WalesStage()$NumberOfCases)), p <- add_trace(p, x = ~StageKey, y = ~PercentOfKnownStages, type = 'bar', color= ~StageKey, colors = crukColours, hoverinfo = 'text', text = ~paste("</br> Stage: ", WalesStage()$StageKey, "</br> Proportion: ", WalesStage()$PercentOfKnownStages,"</br> Number of Cases: ", WalesStage()$NumberOfCases)))  
    
    p <- p %>%       
      layout(margin=list(t=50, l=75, b=50, r=25),
             title = paste("Incidence of", input$cancersite4, "Cancer <br> by Stage at Diagnosis (2015)"),
             titlefont=list(family="Arial", size=18, color="#2E008B"),
             xaxis = list(title = '<b>Stage</b>', 
                          titlefont=list(family="Arial", size=16, color="#2E008B"),
                          tickfont=list(family="Arial", size=11, color="#343434")), 
             yaxis = list(side="left",
                          title = '<b>Proportion of Cases (%)</b>', 
                          tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                          titlefont=list(family="Arial", size=16, color="#2E008B"),
                          tickfont=list(family="Arial", size=14, color="#343434"), range =c(0,100)),
             showlegend = FALSE)
    
    ggplotly(p) 
  })
  
  url4 <-a("Welsh Cancer Intelligence and Surveillance Unit", href="http://www.wcisu.wales.nhs.uk/cancer-incidence-by-stage-at-diagnosis-i-1", target="_blank")
  output$tab4 <- renderUI({
    tagList(strong("Source:"), url4, br(), strong("Local breakdown:"), ("Available at Health Board level"))
  })  
  
##========================##
##----Screening Output----##
##========================## 
  
  screening <- NationalScreeningBaseYear2018
  screening <- screening %>% plyr::rename(c("DateRangeName" ="Date","CoveragePercent" = "Coverage %", "CountryKey" = "Country"))  
 
  cervical_data <- reactive({
     screening[screening$ScreeningTypeKey == "Cervical" & screening$AgeRangeName == input$agerange, c("Date", "Coverage %", "Country")] 
  })
screening <- NationalScreeningBaseYear2018 %>% mutate(numeric_date = as.numeric(substr(DateRangeName,1,4)))

##scotland daterangename different
screening <- screening %>% plyr::rename(c("DateRangeName" ="Date","CoveragePercent" = "Coverage %", "CountryKey" = "Country"))
test <-screening[screening$ScreeningTypeKey == "Breast" & screening$AgeRangeName == "50-70"& screening$Country != "SCT", c("AgeRangeName", "UptakePercent", "Country", "numeric_date", "NumberTested")] %>% group_by(Country)
test2 <-screening[screening$ScreeningTypeKey == "Breast" & screening$DateRangeType == "3 Year rolling" & screening$Country == "SCT" & is.na(screening$DeprivationQuintile), c("AgeRangeName", "UptakePercent", "Country", "numeric_date", "NumberTested")] %>% group_by(Country)
full_data <- rbind(test, test2)

bowel <-screening[screening$ScreeningTypeKey == "Bowel" & screening$DateRangeType == "Year" & is.na(screening$DeprivationDecile) & is.na(screening$DeprivationQuintile) &screening$GenderKey == "P" &screening$AgeRangeName =="60-74", c("AgeRangeName", "UptakePercent", "Country", "numeric_date", "NumberTested")] %>% group_by(Country)

bowel_SCT <-screening[screening$ScreeningTypeKey == "Bowel" & screening$DateRangeType == "2 Years" & is.na(screening$DeprivationQuintile) &screening$GenderKey == "P" &screening$AgeRangeName =="50-74" & screening$Country == "SCT", c("AgeRangeName", "UptakePercent", "Country", "numeric_date", "Date")] %>% group_by(Country)
bowel_SCT$numeric_date <- gsub("- [[:alpha:]][[:alpha:]][[:alpha:]] [[:digit:]][[:digit:]][[:digit:]][[:digit:]]$" ,"" , bowel_SCT$Date)
bowel_SCT$numeric_date <- gsub("[[:alpha:]][[:alpha:]][[:alpha:]] " ,"" , bowel_SCT$numeric_date)
bowel_SCT$numeric_date <- as.numeric(bowel_SCT$numeric_date)
full_bowel <- rbind(bowel, bowel_SCT)
#full_bowel$numeric_date

output$note3 <- renderUI({
  p(strong("Coming soon:"), br(), "- NIR data (see source above for available data)", br(), "- Historical trend data for SCT and WAL")
  
})

output$bowelplot <- renderPlotly({
  p <- plot_ly(full_bowel)
  p <- add_trace(p, x = ~numeric_date, y = ~UptakePercent, type = 'scatter', mode = 'lines+markers', color= ~Country, colors = c("ENG" = '#00B6ED',
                                                                                                                                 #"NIR" = '#4b239b',
                                                                                                                                 "SCT" = '#f24cae',"WAL" = '#7f7f7f'), hoverinfo = 'text', text = ~paste("</br> Country: ", full_bowel$Country, "</br> Uptake Percentage: ", round(full_bowel$UptakePercent, digits = 1), "</br> Year: ", full_bowel$numeric_date, "</br> Number Tested: ", round(full_bowel$NumberTested, digits = 1)))
  
  p <- p %>%       
    layout(margin=list(t=50, l=75, b=50, r=25),
           title = "Uptake of Bowel Screening in the UK (2009-2016)",
           titlefont=list(family="Arial", size=18, color="#2E008B"),
           xaxis = list(title = '<b>Year</b>', 
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=11, color="#343434")), 
           yaxis = list(side="left",
                        title = '<b>Uptake (%)</b>', 
                        tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=14, color="#343434"), range = c(0,100))
    )
  ggplotly(p) 
})
  
url5eng <- a("PHE Fingertips", href = "https://fingertips.phe.org.uk/profile/cancerservices/data#page/3/gid/1938132830/pat/46/par/E39000030/ati/153/are/E38000010/iid/92601/age/280/sex/4", target="_blank")
url5ni <- a("Public Health Agency",href = "http://www.cancerscreening.hscni.net/pdf/Bowel_Factsheet_241017.pdf", target="_blank")
url5sct <- a("ISD Scotland", href="http://www.isdscotland.org/Health-Topics/Cancer/Publications/data-tables2017.asp?id=2095#2095", target="_blank")
url5wls <- a("Public Health Wales",href="http://www.bowelscreening.wales.nhs.uk/opendoc/320238", target="_blank")

output$tab5 <- renderUI({
  
  tagList(p(strong("England source:"), url5eng, br(), strong("Local breakdown:"), "Available at CCG/NHS sub region level"), p(strong("Northern Ireland source:"), url5ni, br(), strong("Local breakdown:"), "Available at Health & Social Care Trust level"), p(strong("Scotland source:"), url5sct, br(), strong("Local breakdown:"), "Available at Health Board level"), p(strong("Wales source:"), url5wls, br(), strong("Local breakdown:"), "Available at LA/Health Board level"))
})

output$cervicaltable <- renderTable({
  data <- cervical_data()
})

url19eng <- a("NHS Digital, 2017", href="http://digital.nhs.uk/pubs/cervical1617", target="_blank")
url19wls <- a("Cervical Screening Wales, 2017", href="http://www.cervicalscreeningwales.wales.nhs.uk/opendoc/315537", target="_blank")
url19sct <- a("ISD, 2017", href="http://www.isdscotland.org/Health-Topics/Cancer/Cervical-Screening/", target="_blank")
url19ni <- a("NHS Digital, 2017, Cervical Screening Programme Bulletin", href="http://digital.nhs.uk/data-and-information/publications/statistical/cervical-screening-programme/cervical-screening-programme-england-2016-17", target="_blank")

url19 <- a("NCRAS outputs", href="http://www.ncin.org.uk/publications/routes_to_diagnosis", target="_blank")  
output$tab19 <- renderUI({
  tagList(p(strong("England Source:"), url19eng, "(see original source for more up-to-date data)"), p(strong("Northern Ireland Source:"), url19ni), p(strong("Scotland Source:"), url19sct, "(see original source for more up-to-date data)"), p(strong("Wales Source:"), url19wls))
})

output$comingsoon2 <- renderUI({
  p(strong("Coming soon:"), br(), "- Historical trend data")
  
})


output$breastplot <- renderPlotly({
  p <- plot_ly(full_data)
  p <- add_trace(p, x = ~numeric_date, y = ~UptakePercent, type = 'scatter', mode = 'lines+markers', color= ~Country, colors = c("ENG" = '#00B6ED',"NIR" = '#4b239b',"SCT" = '#f24cae',"WAL" = '#7f7f7f'), hoverinfo = 'text', text = ~paste("</br> Country: ", full_data$Country, "</br> Uptake Percentage: ", round(full_data$UptakePercent, digits = 1), "</br> Year: ", full_data$numeric_date, "</br> Number Tested: ", round(full_data$NumberTested, digits = 1)))
  
  p <- p %>%       
    layout(margin=list(t=50, l=75, b=50, r=25),
           title = "Uptake of Breast Screening in the UK (2006-2016)",
           titlefont=list(family="Arial", size=18, color="#2E008B"),
           xaxis = list(title = '<b>Year</b>', 
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=11, color="#343434")), 
           yaxis = list(side="left",
                        title = '<b>Uptake (%)</b>', 
                        tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=14, color="#343434"), range = c(0,100))
    )
  
  p 
})

url6eng <- a("NHS Digital, 2017", href="https://digital.nhs.uk/data-and-information/publications/statistical/breast-screening-programme/", target="_blank")
url6wls <- a("Breast Test Wales, 2017", href="http://www.breasttestwales.wales.nhs.uk/reports-1", target="_blank")
url6sct <- a("ISD, 2017", href="http://www.isdscotland.org/Health-Topics/Cancer/Breast-Screening/", target="_blank")
url6ni <- a("Northern Ireland Breast Screening Programme, 2017", href="http://www.cancerscreening.hscni.net/pdf/BREAST_ANNUAL_REPORT_2012-13_Version_3.0_22_Aug_14.pdf", target="_blank")

#output$BreastNote <- renderUI({
  #p(strong("Local Breakdown:"), "LA/Regions (ENG), LA/Health Boards (WAL), NHS Board (SCT), Unit (NIR)")
  
#})

output$tab6 <- renderUI({
  tagList(p(strong("England source:"), url6eng, "(see original source for more up-to-date data)", br(), strong("Local breakdown:"), "Available at LA/Region level"), p(strong("Northern Ireland source:"), url6ni, br(), strong("Local breakdown:"), "Available at Unit level"),  p(strong("Scotland source:"), url6sct, br(), strong("Local breakdown:"), "Available at NHS Board level"), p(strong("Wales source:"), url6wls, br(), strong("Local breakdown:"), "Available at LA/Health Board level"))
})
##=====================================##
##----Emergency Presentation Output----##
##=====================================##  

output$referralplot <- renderPlotly({
  p <- plot_ly(england_emer_pres)
  p <- add_trace(p, x = ~Timeperiod, y = ~Value, type = 'scatter', mode = 'lines+markers', colors= "#00b6ed", hoverinfo = 'text', 
                 text = ~paste("</br> Emergency Presentations per 100,000: ", round(england_emer_pres$Value, digits = 1), "</br> Year: ", england_emer_pres$Timeperiod))
  
  p <- p %>%
    layout(margin=list(t=50, l=75, b=50, r=25),
           title = ("Number of Patients (per 100,000) Diagnosed with <br> Cancer via an Emergency Presentation (2009-2017)"),
           titlefont=list(family="Arial", size=18, color="#2E008B"),
           xaxis = list(title = '<b>Year</b>',
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=11, color="#343434")),
           yaxis = list(side="left",
                        title = '<b>Number per 100,000</b>',
                        tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=14, color="#343434"), range = c(0,150))
    )
  
  p
})
url5 <- a("PHE Fingertips", href="https://fingertips.phe.org.uk/profile/cancerservices/data#page/0", target="_blank")
output$cautiontag <- renderUI({
  tagList(strong("Caution:"), "For the most reliable source of emergency presentation data see 'Routes to Diagnosis' tab. ")
})
output$tag <- renderUI({
  tagList(strong("Data source: "), url5, br(strong("Local breakdown:"), "Available at NHS sub-region, STP, CCG and GP practice level"))
})
##==================================##
##----Routes to Diagnosis Output----##
##==================================##
#some cancer types need the person figure, whereas others need just the male or female figure. This creates lists of which cancers have male figures and which have females figures (for RtD) so that we can filter appropriately in the reactive statement below    
cancerTypeIncludeMale_routes <- unique(paste(routestodiagnosis[routestodiagnosis$GenderKey == "M", ]$CancerSite, sep =""))
cancerTypeIncludeMale_routes <- cancerTypeIncludeMale_routes[cancerTypeIncludeMale_routes != "Breast"]
cancerTypeIncludeFemale_routes <- unique(routestodiagnosis[routestodiagnosis$GenderKey == "F", ]$CancerSite, sep ="")  

routes_gender <- reactive({
  ifelse(input$cancersite5 %in% cancerTypeIncludeMale_routes && !(input$cancersite5 %in% cancerTypeIncludeFemale_routes), "M", ifelse(input$cancersite5 %in% cancerTypeIncludeFemale_routes && !(input$cancersite5 %in% cancerTypeIncludeMale_routes), "F", "P"))
})  

RTD <- reactive({  
  routestodiagnosis %>% filter(CancerSite ==input$cancersite5, GenderKey == routes_gender())
})

output$routes<- renderPlotly({
  p <- plot_ly(RTD())
  p <- add_trace(p, x = ~DateRangeName, y = ~PercentageViaScreening*100, type = 'scatter', mode = 'lines+markers', colors = '#00B6ED', hoverinfo = 'text', text = ~paste("</br> Year: ", RTD()$DateRangeName, "</br> Percentage: ", round(RTD()$PercentageViaScreening*100, digits = 1), "</br> Number of Cases: ", RTD()$NumberViaScreening), name = "Screening")
  p <- add_trace(p, x = ~DateRangeName, y = ~PercentageViaTwoWeekWait*100, type = 'scatter', mode = 'lines+markers', line = list(color ='#4b239b'), hoverinfo = 'text', text = ~paste("</br> Year: ", RTD()$DateRangeName, "</br> Percentage: ", round(RTD()$PercentageViaTwoWeekWait*100, digits = 1), "</br> Number of Cases: ", RTD()$NumberViaTwoWeekWait), name = "Two Week Wait")
  p <- add_trace(p, x = ~DateRangeName, y = ~PercentageViaGPReferral*100, type = 'scatter', mode = 'lines+markers', line = list(color = '#f24cae'), hoverinfo = 'text', text = ~paste("</br> Year: ", RTD()$DateRangeName, "</br> Percentage: ", round(RTD()$PercentageViaGPReferral*100, digits = 1), "</br> Number of Cases: ", RTD()$NumberViaGPReferral), name = "GP Referral")
  p <- add_trace(p, x = ~DateRangeName, y = ~PercentageViaOtherOutpatient*100, type = 'scatter', mode = 'lines+markers', line = list(color = '#99e2f8'), hoverinfo = 'text', text = ~paste("</br> Year: ", RTD()$DateRangeName, "</br> Percentage: ", round(RTD()$PercentageViaOtherOutpatient*100, digits = 1), "</br> Number of Cases: ", RTD()$NumberViaOtherOutpatient), name = "Other Outpatient")
  p <- add_trace(p, x = ~DateRangeName, y = ~PercentageViaInpatientElective*100, type = 'scatter', mode = 'lines+markers', line = list(color = '#ab99d1'),  hoverinfo = 'text', text = ~paste("</br> Year: ", RTD()$DateRangeName, "</br> Percentage: ", round(RTD()$PercentageViaInpatientElective*100, digits = 1), "</br> Number of Cases: ", RTD()$NumberViaInpatientElective), name = "Inpatient Elective")
  p <- add_trace(p, x = ~DateRangeName, y = ~PercentageViaEmergencyPresentation*100, type = 'scatter', mode = 'lines+markers', line = list(color = '#f799d1'), hoverinfo = 'text', text = ~paste("</br> Year: ", RTD()$DateRangeName, "</br> Percentage: ", round(RTD()$PercentageViaEmergencyPresentation*100, digits = 1), "</br> Number of Cases: ", RTD()$NumberViaEmergencyPresentation), name = "Emergency Presentation")
  
  p <- p %>%       
    layout(margin=list(t=50, l=75, b=50, r=25),
           title = paste("Proportion of", input$cancersite5, "Cancer Diagnoses <br> by Presentation Route in England (2006-2015)"),
           titlefont=list(family="Arial", size=18, color="#2E008B"),
           xaxis = list(title = '<b>Year</b>', 
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=11, color="#343434"),
                        tickprefix = "\r\n "), 
           yaxis = list(side="left",
                        title = '<b>Proportion of all diagnoses (%)</b>', 
                        tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=13, color="#343434"), range = c(0,100)
           ),
          
           legend = list(orientation = "h")
    )
  
  p
})

url9 <- a("National Cancer Registration and Analysis Service (NCRAS)", href="http://www.ncin.org.uk/publications/routes_to_diagnosis", target="_blank")
url18 <- a("NCRAS outputs", href="http://www.ncin.org.uk/publications/routes_to_diagnosis", target="_blank")  
output$tab9 <- renderUI({
  tagList(strong("Source:"), url9, br(strong("Local breakdown:"), "Available by Cancer Alliance and CCG (limited) level in 'Workbook (b)' of the",url18))
})

url7 <- a("Emergency Presentation Visualisations", href="https://data.healthdatainsight.org.uk/apps/routes_to_diagnosis/route_emergency_both/", target="_blank")
output$tab7 <- renderUI({
  tagList(strong("Note:"), "Emergency presentations can be further broken down into sub-routes to provide further insight: Accident and Emergency presentations, GP emergency presentations (emergency referral), inpatient emergencies and outpatient emergencies. See:", url7)
})  
##===========================================##
##----Routes to Diagnosis by Stage Output----##
##===========================================##  

output$routesinfoimg <-  renderUI({   #This is where the image is set
  if(input$routesinfo == "All Cancers Combined"){
    img(src = "Route_to_diagnosis_and_stage_for_ALL_cancers RGB.png", width = "370px", height = "500px")
  }
  else if(input$routesinfo == "Bowel"){
    img(src = "Bowel.jpg", width = "370px", height = "500px")
  }
  
  else if(input$routesinfo == "Lung"){
    img(src = "Lung.jpg", width = "370px", height = "500px")
  }
})

url10 <- a("National Cancer Registration and Analysis Service (NCRAS)", href="http://www.ncin.org.uk/publications/routes_to_diagnosis", target="_blank")
output$tab10 <- renderUI({
  tagList(strong("Source:"), url10)
})

##==========================##
##----62 Day Wait Output----##
##==========================##
output$cwt <- renderPlotly({
  p <- plot_ly(CWT62Days)
  p <- add_trace(p, x = ~chartdate, y = ~PercentageWithin62Days, type = 'scatter', mode = 'lines+markers', color= ~CountryKey, colors = c("ENG" = '#00B6ED',"NIR" = '#4b239b',"SCT" = '#f24cae',"WAL" = '#7f7f7f'), hoverinfo = 'text', 
                 text = ~paste("</br> Value: ", round(CWT62Days$PercentageWithin62Days, digits = 1)))
  
  p <- p %>%
    layout(margin=list(t=50, l=75, b=50, r=25),
           title = "Percentage of Cancer Patients Treated within 62 Days",
           titlefont=list(family="Arial", size=18, color="#2E008B"),
           xaxis = list(title = '<b></b>',
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=11, color="#343434")),
           yaxis = list(side="left",
                        title = '<b>Percentage (%)</b>',
                        tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=14, color="#343434"), range = c(0,100))
    )
  
  p
})

url11eng <- a("NHS England", href="https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-waiting-times/", target="_blank")
url11wls <- a("Stats Wales", href="https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Waiting-Times/Cancer-Waiting-Times", target="_blank")
url11sct <- a("ISD Scotland", href="http://www.isdscotland.org/Health-Topics/Waiting-Times/Cancer/", target="_blank")
url11ni <- a("Department of Health", href="https://www.health-ni.gov.uk/articles/cancer-waiting-times", target="_blank")
output$tab11 <- renderUI({
  tagList(p(strong("England Source:"), url11eng), p(strong("Wales Source:"), url11wls), p(strong("Scotland Source:"), url11sct), p(strong("Northern Ireland Source:"), url11ni), p("Local Breakdown: NHS Trusts (ENG), Health Boards (WLS), Health Board (SCT), HSCT (NI) "))
})

##===================================##
##----Diagnostic Workforce Output----##
##===================================##

output$radiologyvacant <- renderPlotly({
  p <- plot_ly(diagwork)
  p <- add_trace(p, x = ~Year, y = ~VacancyRate*100, type = 'scatter', mode = 'lines+markers', colors= "#00b6ed", hoverinfo = 'text', 
                 text = ~paste("</br> Value: ", round(diagwork$VacancyRate*100, digits = 1)))
  
  p <- p %>%
    layout(margin=list(t=50, l=75, b=50, r=25),
           title = "Vacancy Rate of Unfilled Consultant Radiologist Posts (WTE) <br> in the UK (2011-2016)",
           titlefont=list(family="Arial", size=18, color="#2E008B"),
           xaxis = list(title = '<b>Year</b>',
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=11, color="#343434")),
           yaxis = list(side="left",
                        title = '<b>Percentage (%)</b>',
                        tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=14, color="#343434"), range = c(0,100))
    )
  
  p
})

url8 <- a("Clinical Radiology UK workforce census, 2016 report", href="https://www.rcr.ac.uk/clinical-radiology/service-delivery/rcr-radiology-workforce-census", target="_blank")
output$tab8 <- renderUI({
  tagList(strong("Source:"), url8, "(see original source for more up-to-date data)", br(), strong("Note:"), ("For historical trends, look at table 19 in the 2015 report."), p("Data for 2013 not published."))
})
# output$chest1 <- renderInfoBox({
#   infoBox(
#     "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
#     color = "purple"
#   )
# })
# output$chest2 <- renderInfoBox({
#   infoBox(
#     "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
#     color = "yellow"
#   )
# })

##==================================##
##----Diagnostic Activity Output----##
##==================================##

output$test <- renderPlotly({
  p <- plot_ly(diagact_v2)
  p <- add_trace(p, x = ~Year, y = ~All/1000000, type = 'scatter', mode = 'lines+markers', colors= "#00b6ed", hoverinfo = 'text', 
                 text = ~paste("</br> Number: ", diagact_v2$All, "</br> Year: ", diagact_v2$Year))
  
  p <- p %>%
    layout(margin=list(t=50, l=75, b=50, r=25),
           title = "Number of Chest X-Rays in England (2012/13-2016/17)",
           titlefont=list(family="Arial", size=18, color="#2E008B"),
           xaxis = list(title = '<b>Year</b>',
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=11, color="#343434")),
           yaxis = list(side="left",
                        title = '<b>Number of X-Rays (in millions)</b>',
                        tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=14, color="#343434"), range = c(0,10))
    )
  
  p
})

url12 <- a("NHS England, Diagnostic Imaging Dataset 2017-18", href="https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostic-imaging-dataset/diagnostic-imaging-dataset-2017-18-data/", target="_blank")
output$tab12 <- renderUI({
  tagList(strong("Source:"), url12, "(see original source for more up-to-date data)")
})
output$colon <- renderPlotly({
  p <- plot_ly(colon_annual)
  p <- add_trace(p, x = ~Year, y = ~AnnualTotal, type = 'scatter', mode = 'lines+markers', colors= "#00b6ed", hoverinfo = 'text', 
                 text = ~paste("</br> Number: ", round(colon_annual$AnnualTotal, 0), "</br> Year: ", colon_annual$Year))
  
  p <- p %>%
    layout(margin=list(t=50, l=75, b=50, r=25),
           title = "Number of Colonoscopies in England (2007-2017)",
           titlefont=list(family="Arial", size=18, color="#2E008B"),
           xaxis = list(title = '<b>Year</b>',
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=11, color="#343434")),
           yaxis = list(side="left",
                        title = '<b>Number of Colonoscopies</b>',
                        tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=14, color="#343434"))
    )
  
  p
})
url13 <- a("NHS England, Monthly Diagnostics Data 2018-19", href="https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2018-19/", target="_blank")
output$tab13 <- renderUI({
  tagList(strong("Source:"), url13, "(see original source for more up-to-date data)")
})
output$gastro <- renderPlotly({
  p <- plot_ly(gastro_annual)
  p <- add_trace(p, x = ~Year, y = ~AnnualTotal, type = 'scatter', mode = 'lines+markers', colors= "#00b6ed", hoverinfo = 'text', 
                 text = ~paste("</br> Number: ", round(gastro_annual$AnnualTotal, 0), "</br> Year: ", gastro_annual$Year))
  
  p <- p %>%
    layout(margin=list(t=50, l=75, b=50, r=25),
           title = "Number of Gastroscopies in England (2007-2017)",
           titlefont=list(family="Arial", size=18, color="#2E008B"),
           xaxis = list(title = '<b>Year</b>',
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=11, color="#343434")),
           yaxis = list(side="left",
                        title = '<b>Number of Gastroscopies</b>',
                        tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=14, color="#343434"))
    )
  
  p
})
url14 <- a("NHS England, Monthly Diagnostics Data 2018-19", href="https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2018-19/", target="_blank")
output$tab14 <- renderUI({
  tagList(strong("Source:"), url14, "(see original source for more up-to-date data)")
})

##=======================================##
##----Diagnostic Waiting Times Output----##
##=======================================##

output$mri <- renderPlotly({
  p <- plot_ly(waiting_annual)
  p <- add_trace(p, x = ~Year, y = ~MRI_annual, type = 'scatter', mode = 'lines+markers', colors= "#00b6ed", hoverinfo = 'text', 
                 text = ~paste("</br> Number: ", round(waiting_annual$MRI_annual, 0), "</br> Year: ", waiting_annual$Year,  "</br> % waiting 6+ weeks: ", round(waiting_annual$MRI_perc, 2), "%"))
  
  p <- p %>%
    layout(margin=list(t=50, l=75, b=50, r=25),
           title = "Number of Patients Waiting 6 Weeks or Longer <br> for an MRI Scan in England (2006-2017)",
           titlefont=list(family="Arial", size=18, color="#2E008B"),
           xaxis = list(title = '<b>Year</b>',
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=11, color="#343434"),
                        range = c(2008, max(waiting_annual$Year))),
           yaxis = list(side="left",
                        title = '<b>No. People Waiting 6+ Weeks</b>',
                        tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=14, color="#343434"),
                        range = c(0, 50000))
    )
  
  p
})
url15 <- a("NHS England, Monthly Diagnostics Data 2018-19", href="https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2018-19/", target="_blank")
output$tab15 <- renderUI({
  tagList(strong("Source:"), url15, "(see original source for more up-to-date data)")
})

output$ct <- renderPlotly({
  p <- plot_ly(waiting_annual)
  p <- add_trace(p, x = ~Year, y = ~ct_annual, type = 'scatter', mode = 'lines+markers', colors= "#00b6ed", hoverinfo = 'text', 
                 text = ~paste("</br> Number: ", round(waiting_annual$ct_annual, 0), "</br> Year: ", waiting_annual$Year, "</br> % waiting 6+ weeks: ", round(waiting_annual$ct_perc, 2), "%"))
  
  p <- p %>%
    layout(margin=list(t=50, l=75, b=50, r=25),
           title = "Number of Patients Waiting 6 Weeks or Longer <br> for a CT Scan in England (2006-2017)",
           titlefont=list(family="Arial", size=18, color="#2E008B"),
           xaxis = list(title = '<b>Year</b>',
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=11, color="#343434"),
                        range = c(2008, max(waiting_annual$Year))),
           yaxis = list(side="left",
                        title = '<b>No. People Waiting 6+ Weeks</b>',
                        tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=14, color="#343434"),
                        range = c(0, 50000))
    )
  
  p
})

url16 <- a("NHS England, Monthly Diagnostics Data 2018-19", href="https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2018-19/", target="_blank")
output$tab16 <- renderUI({
  tagList(strong("Source:"), url16, "(see original source for more up-to-date data)")
}) 

output$ultra <- renderPlotly({
  p <- plot_ly(waiting_annual)
  p <- add_trace(p, x = ~Year, y = ~ultra_annual, type = 'scatter', mode = 'lines+markers', colors= "#00b6ed", hoverinfo = 'text', 
                 text = ~paste("</br> Number: ", round(waiting_annual$ultra_annual, 0), "</br> Year: ", waiting_annual$Year,  "</br> % waiting 6+ weeks: ", round(waiting_annual$ultra_perc, 2), "%"))
  
  p <- p %>%
    layout(margin=list(t=50, l=75, b=50, r=25),
           title = "Number of Patients Waiting 6 Weeks or Longer <br> for a Non-Obstetric Ultrasounds (2006-2017)",
           titlefont=list(family="Arial", size=18, color="#2E008B"),
           xaxis = list(title = '<b>Year</b>',
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=11, color="#343434"),
                        range = c(2008, max(waiting_annual$Year))),
           yaxis = list(side="left",
                        title = '<b>No. People Waiting 6+ Weeks</b>',
                        tickformat=',.0f', hoverformat=',.0f', tickprefix="  ",
                        titlefont=list(family="Arial", size=16, color="#2E008B"),
                        tickfont=list(family="Arial", size=14, color="#343434"),
                        range = c(0, 50000))
    )
  
  p
})

url17 <- a("NHS England, Monthly Diagnostics Data 2018-19", href="https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2018-19/", target="_blank")
output$tab17 <- renderUI({
  tagList(strong("Source:"), url17, "(see original source for more up-to-date data)")
})
}

shinyApp(ui, server)


##=====================##
##----Deploying App----##
##=====================##
#to re-deploy any changes in shiny app to server, run the two lines of code below


