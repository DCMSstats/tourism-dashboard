#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
source("Fetch_Data.R")
source("Clean_Data.R")
source("Output_Data.R")
library(shiny)

# Define UI for application that draws a histogram
shinyUI(
fluidPage(theme = "bootstrap.css",
          
          tags$head( tags$style( " .title { background:url('banner.png'); background-repeat: no-repeat; background-size: 100% 200%; color: white;
                               font-family: Optima, Segoe, 'Segoe UI', Candara, Calibri, Arial, sans-serif;
	                                font-size: 24px;
                                 font-style: normal;
                                 font-variant: normal;
                                 font-weight: 2000;
                                 line-height: 26.4px} " ) ), 
          tags$head(tags$style(
            type="text/css",
            "#image img {max-width: 100%; width: auto; height: auto}"
          )),
          
  titlePanel(windowTitle = "DCMS Tourism Dashboard",
             title =
               div(
                 img(
                   src = "DCMSlogo.png",
                   height = 100,
                   width = 150,
                   style = "margin:10px 10px"
                 ),
       "DCMS Tourism Dashboard",
                class = 'title'
               )                  
         ),
    
    navbarPage(id = "Navbar",
                  fluid = TRUE,
                  theme = shinythemes::shinytheme("spacelab"),
                  footer = helpText(
                    "Love the dashboard? Hate it? Got a suggestion to improve it? Found a bug? We'd love to hear from you",
                    a(href="https://www.surveymonkey.co.uk/", target="_blank", "here")
                  ),
                  tags$style(type="text/css", "body {padding-top: 0px;}"),
  
    # Show a plot of the generated distribution
  
    tabPanel("About",
             
             fluidRow(column(3,div(tags$style(
               type="text/css",
               "#image img {max-width: auto; max-height: auto}"
             ),div(img(src = "Sidepanel.png"), id = "image")),tags$br()),column(9,tags$p(class = "intro",
                    "The DCMS Tourism Dashboard is a one-stop shop for all
                    information about tourism. It brings together a range of tourism 
                    datasets produced by DCMS, ONS, VisitBritain and VisitEngland into one easy-to-use tool. 
                    Information is presented using dynamic graphs and data tables."),
                    tags$br("Main subject-area groupings of tourism data are shown on 
                           the toolbar above. To navigate around this site, left-click 
                            one of the subject areas and then select one of the related 
                            categories in the drop down list."),
                    tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                    tags$p("You can use the button below to get the latest data for the dashboard.
                            If you know that new data is available, and that the dashboard hasn't updated
                            to reflect this, then please get in touch with the Evidence and Analysis Unit. 
                            Please be patient as the data updates as this will take a minute or so.")
                    ),
             fluidRow(column(1),column(12,
               actionButton('Fetch', 'Check for new data')
             ))))),
    navbarMenu("Overall",
               tabPanel("Economic Contribution",
                        fluidRow(column(4,
                          div(class = "well",selectInput("EconomicDT","Choose a measure:",choices = c("GVA","Employment")),
                          tags$p(tags$b("Click on the below buttons to download elements of this page:")),
                          fluidRow(column(12,downloadButton("downloadEconomicPlot","Download Plot "),
                                   downloadButton("downloadEconomicTable","Download Table"))
                                   )),
                          fluidRow(tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                           
                                           tags$p(class = "intro-divider",tags$b("Notes:"),
                                                  tags$p("1. GVA figures and spend figures are expressed in current prices (i.e. not accounting for inflation)"),
                                                  tags$p("2. A definition of Tourism is provided on the 'More Information' panel"))),
                                         tags$p(class = "intro-divider",tags$b("Sources:"),
                                          htmlOutput("EconomicSources")
                                          ))
                          ),
                          column(8,
                                 plotly::plotlyOutput("EconomicPlot",height = "auto"),
                                 DT::dataTableOutput("EconomicTable")
                          ) 
                        ), fluidRow(tags$br())),
  
  tabPanel("Visits and Spend",
           fluidRow(column(4,
                   div(class = "well",selectInput("VisitSpendDT","Choose tourism type:",choices = c("Inbound tourism (UK)","Domestic overnight tourism (GB)","Domestic day tourism (GB)")),
                       tags$p(tags$b("Click on the below buttons to download elements of this page:")),
                   fluidRow(column(12,downloadButton("downloadVisitSpendPlot","Download Plot "),
                                   downloadButton("downloadVisitSpendTable","Download Table"))
                   )),
               fluidRow(tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                
                                tags$p(class = "intro-divider",tags$b("Notes:"),
                                       tags$p("1. Domestic figures are for GB; inbound figures are for the UK"),
                                       htmlOutput("VisitSpendNotes"))),
                        tags$p(class = "intro-divider",tags$b("Sources:"),
                               htmlOutput("VisitSpendSources")
                        ))
               ),
             column(8,
               plotly::plotlyOutput("VisitSpendPlot"),
               DT::dataTableOutput("VisitSpendTable")
             ) 
             ), fluidRow(tags$br()))
  ),
  
             tabPanel("Regional Metrics",
               column(4,div(class = "well",radioButtons("MapInputData","Please choose a metric:",c("GVA" = "GVA","Employment" = "Employment")),
                            tags$p(tags$b("Click on a region for more information and then click on either of the buttons below to download the map and/or table:")),
                            fluidRow(column(12,downloadButton("downloadRegionalPlot","Download Map "),
                                            downloadButton("downloadRegionalTable","Download Table"))
                            ),tags$br(tags$b("Please note that the map may take a few seconds to download"))),
                      fluidRow(tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                       
                                       tags$p(class = "intro-divider",tags$b("Notes:"),
                                              tags$p("1. GVA figures and spend figures are expressed in current prices (i.e. not accounting for inflation)"),
                                              tags$p("2. A definition of Tourism is provided on the 'More Information' panel"))),
                               tags$p(class = "intro-divider",tags$b("Sources:"),
                                      htmlOutput("RegionalSources")
                               ))
                      
                      ),
               column(8,leafletOutput("RegionalGVAPlot"),
                      DT::dataTableOutput("RegionalGVATable"),fluidRow(tags$br()))
             ),
            tabPanel("More Information",column(4,div(class = "well",tags$h5(tags$b("Definitions")),
                                                                    tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                                                    tags$ol(class = "intro-divider",tags$li("Gross value added (GVA) is the measure of the economic value of goods and services produced in an area, industry or sector of an economy. It is similar conceptually to GDP but GDP cannot be estimated at a sector level. At a national level the relationship between GVA and GDP is given by:
",tags$b("GVA + taxes on products - subsidies on products = GDP")),
                                                                    tags$br(tags$li('"Tourism Industries”, as used for employment data, is a wider definition of tourism than we use for GVA. We use this employment estimate because it is much more current (it is available for 2014 rather than the 2013 employment estimate from the ONS tourism satellite account). The industries included within tourism industries can be seen in annex A of the Characteristics of tourism industries, 2014 release.'
)))
))),
                     column(8,div(class = "well",tags$h5(tags$b("Statistical Release Calendar")),
                                                tags$hr(style="border-color: red;border-top: 3px solid #F511BC;","This release calendar highlights any relevant upcoming statistical releases for the next two months "),
                                                tags$br(""),
                                                 DT::dataTableOutput("ReleaseCalendar"))))
             
             
             
             
             
             
             )
  
  
  
  )

)


