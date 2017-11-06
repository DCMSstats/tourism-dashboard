#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    ## Create reactive objects
  Rplot_Economic<-reactive(plot_Economic(input$EconomicDT))
  Rtable_Economic<-reactive(Output_Economic(input$EconomicDT))
  
  Rplot_VisitSpend<-reactive(if(input$VisitSpendDT == "Inbound tourism (UK)"){
    
                              plot_ONSVisitSpend(start=2010)
    
                              } else if(input$VisitSpendDT == "Domestic overnight tourism (GB)"){
                              
                              plot_GBTS()})
  
  Rtable_VisitSpend<-reactive(if(input$VisitSpendDT == "Inbound tourism (UK)"){
    
    Output_VisitSpend(Clean_ONSVisitSpend()/1000,Clean_ONSVisitSpend(Mode = "Quarters")/1000,"Visits (millions)","Spend (£bn)")
    
    } else if(input$VisitSpendDT == "Domestic overnight tourism (GB)"){
      
      Output_VisitSpend2(Clean_GBTSVisitSpend(),"Visits (millions)","Spend (£bn)")})
  
  VisitSpendData<-reactive({if(input$VisitSpendDT == "Inbound tourism (UK)"){
    
                                Clean_ONSVisitSpend()
    
                            } else if(input$VisitSpendDT == "Domestic overnight tourism (GB)"){
                              
                              Clean_GBTSVisitSpend()}})
  
  
  output$EconomicPlot <- plotly::renderPlotly({Rplot_Economic()})
  output$EconomicTable <- DT::renderDataTable({Rtable_Economic()}, options = list(dom = 't'))

  output$VisitSpendPlot<-plotly::renderPlotly({Rplot_VisitSpend()})
  output$VisitSpendTable<-DT::renderDataTable({Rtable_VisitSpend()}, options = list(dom = 't')) 

  ## Reactive notes
  #GBTSDT<-eventReactive(input$Fetch,{reactive(Output_VisitSpend(Clean_GBTSVisitSpend(),"Visits (millions)","Spend (£bn)"))},ignoreNULL=FALSE)
  #GBTS<-eventReactive(input$Fetch,{Clean_GBTSVisitSpend()},ignoreNULL = FALSE)
  #GBTSMaxYear<-reactive(colnames(GBTS())[length(GBTS())])
  #GBTSNote<-reactive(paste("2. GBTS figures are available up to ", paste(GBTSMaxYear(), "however spend data is not yet available")))
  #GBTShref<-reactive(paste("https://www.visitbritain.org/gb-tourism-survey-",paste(GBTSMaxYear(),"-overview",sep=""),sep=""))
  #GBTSText<-reactive(paste("GBTS Travel Survey: ",GBTSMaxYear(),sep=""))
  
  #Sources<-eventReactive(input$Fetch,{read.csv("./User_Sources/DataSources.csv")},ignoreNULL=FALSE)
  #Sources_DCMS<-reactive(unique(as.vector(Sources()[which(Sources()$Source=="DCMS"),4])))
  #Sources_ONS<-reactive(unique(as.vector(Sources()[which(Sources()$Source=="ONS"),4])))
  
  #output$VisitSpendNotes<-renderUI({
    #if(length(GBTS()[which(is.na(GBTS()[,length(GBTS())])),length(GBTS())])>0){tags$p(GBTSNote())}
    #})

  #output$EconomicSources<-renderUI({lapply(Sources_DCMS(),function(x) SourceURL(x,"DCMS"))})
  #output$VisitSpendSources<-renderUI({list(lapply(Sources_ONS(),function(x) SourceURL(x,"ONS")),
                                           #list(tags$p(tags$a(href = GBTShref(),GBTSText()))))})

  ##Download buttons
  
  output$downloadEconomicPlot<-downloadHandler(filename = function(){paste0(input$EconomicDT," Plot.png")},
                                               content = function(file){plotly_IMAGE(Rplot_Economic(),format="png",out_file = file)})
  
  output$downloadEconomicTable<-downloadHandler(filename = function(){paste0(input$EconomicDT," Table.csv")},
                                                content = function(file){write.csv(Rtable_Economic()$x$data[,2:ncol(Rtable_Economic()$x$data)],file = file)})
  
  output$downloadVisitSpendPlot<-downloadHandler(filename = function(){paste0(input$VisitSpendDT," Plot.png")},
                                               content = function(file){plotly_IMAGE(Rplot_VisitSpend(),format="png",out_file = file)})
  
  output$downloadVisitSpendTable<-downloadHandler(filename = function(){paste0(input$VisitSpendDT," Table.csv")},
                                                content = function(file){write.csv(Rtable_VisitSpend()$x$data[,2:ncol(Rtable_VisitSpend()$x$data)],file = file)})
  
  
  observeEvent(input$Fetch,{withProgress(message = "Updating dashboard", value = 0,{d1<-Fetch_DCMSData()
                                                                                    incProgress(1/6,detail = d1)
                                                                                    d2<-Fetch_ONSData()
                                                                                    incProgress(1/6,detail = d2)
                                                                                    d3<-Fetch_VB_IPSData()
                                                                                    incProgress(1/6,detail = d3)
                                                                                    d4<-Fetch_VB_GBTSData()
                                                                                    incProgress(1/6,detail = d4)
                                                                                    d5<-Fetch_StatsWalesData()
                                                                                    incProgress(1/6,detail = d5)
                                                                                    incProgress(1/6,detail = "Download complete")})})  
  
  ##Chloropleth Map Code
  
  ##Loads basemap
  
  RBaseMap<-reactive({BaseMap()})
  

  
  ##Pulls in Ordinance Survey shapefile and spatial polygons dataframe  
  area2<-isolate({OSPolygons()})
  area2data<-area2@data
  
  ##Get regional value dataset and append to the NUTS1 
  Data <- reactive({Clean_RegionalValue(input$MapInputData)})
  AreaData <- reactive({data.frame(area2data, Data()[match(area2data$NAME, Data()$NUTS1),2:4])})
  AreaDataLabel<-reactive({if(input$MapInputData=="GVA"){round(as.numeric(AreaData()[,5])/1000,2)}else{as.numeric(AreaData()[,5])}})
  AreaDataLabel2<-reactive({paste0(round(100*as.numeric(AreaData()[,7]),1),"%")})
  
  datacol<-reactive({as.numeric(AreaData()[,5])})
  maxval<-reactive({max(100*ceiling(datacol()/100))})
  minval<-reactive({min(100*floor(datacol()/100))})
  step = reactive({100*floor(((maxval() - minval())/700))})
  bins<-reactive({c(seq(minval(),minval()+7*step(),by = step()),maxval())})
  
  maxvallab<-reactive({if(input$MapInputData=="GVA"){round(maxval()/1000,2)}else{maxval()}})
  minvallab<-reactive({if(input$MapInputData=="GVA"){round(minval()/1000,2)}else{minval()}})
  
  colors<-reactive({if(input$MapInputData == "GVA"){c('#f7fbff',
                                                      '#deebf7',
                                                      '#c6dbef',
                                                      '#9ecae1',
                                                      '#6baed6',
                                                      '#4292c6',
                                                      '#2171b5',
                                                      '#084594')
  }else{c('#fee0d2',  #an example color scheme. you can substitute your own colors
          '#fcbba1',
          '#fc9272',
          '#fb6a4a',
          '#ef3b2c',
          '#cb181d',
          '#a50f15',
          '#67000d')
  }})
  
  textcolor<-reactive({if(input$MapInputData == "GVA"){"blue"}else{"Salmon"}})
  
  palette <- reactive({colorBin(colors(), 
                                bins = bins())(as.numeric(AreaData()[,5]))})
  
  popupvar<-reactive({if(input$MapInputData=="GVA"){"% of Regional GVA"}else{"% of all jobs"}})
  
  popup <- reactive({paste0("<span style='color: ",textcolor(),";'><strong>Region: </strong></span>", 
                            AreaData()$NAME, 
                            "<br><span style='color: ",textcolor(),";'><strong>",input$MapInputData,": </strong></span>", 
                            format(AreaDataLabel(),big.mark = ","),
                            "<br><span style='color: ",textcolor(),";'><strong>",popupvar(),": </strong></span>", 
                            AreaDataLabel2())
  })
  
  title<-reactive({if(input$MapInputData == "GVA"){paste0("Region<br>",input$MapInputData," (£bn)")}else{paste0("Region<br>",input$MapInputData)}})
  
  labels<-reactive({c(format(maxvallab(),big.mark = ","),"","","","","","",format(minvallab(),big.mark=","))})
  
  
  AddNUTS1<-reactive({function(map){map<-map %>% clearControls() %>% addPolygons(data = area2,
                                                             fillColor = ~palette(),  ## we want the polygon filled with 
                                                             ## one of the palette-colors
                                                             ## according to the value in student1$Anteil
                                                             fillOpacity = 0.6,         ## how transparent do you want the polygon to be?
                                                             color = "darkgrey",       ## color of borders between districts
                                                             weight = 1.5,            ## width of borders
                                                             popup = popup(),         ## which popup?
                                                             layerId = AreaData()$NAME,
                                                             highlight = highlightOptions(
                                                               weight = 3,
                                                               color = "#666",
                                                               dashArray = "",
                                                               fillOpacity = 0.7,
                                                               bringToFront = TRUE)) %>%
    addLegend(position = 'topleft', ## choose bottomleft, bottomright, topleft or topright
              colors = colors()[length(colors()):1], 
              labels = labels(),  ## legend labels (only min and max)
              opacity = 0.6,      ##transparency again
              title = title() )
                                            }})
  
  FullMap<-reactive({AddNUTS1()(BaseMap())})
  
  output$RegionalGVAPlot<-renderLeaflet({FullMap()})
  
  DT<-eventReactive({input$RegionalGVAPlot_shape_click 
                      input$MapInputData},{
  
   if(length(input$RegionalGVAPlot_shape_click)==0){Output_RegionalTable("London","GVA")}else{Output_RegionalTable(input$RegionalGVAPlot_shape_click[1],input$MapInputData)}})

  output$RegionalGVATable<-DT::renderDataTable(DT())
  
  filetext1<-reactive(if(length(input$RegionalGVAPlot_shape_click)==0){"London"}else{input$RegionalGVAPlot_shape_click[1]})

  filetext2<-reactive(if(input$MapInputData=="GVA"){"Spend"}else{"Visits"})
  
  output$downloadRegionalTable<-downloadHandler(filename = function(){paste0(filetext1()," ",filetext2()," Data.csv")},
                                                content = function(file){write.csv(DT()$x$data[,2:ncol(DT()$x$data)],file = file)})
  output$downloadRegionalPlot<-downloadHandler(filename = function(){paste0(filetext1()," ",input$MapInputData," Chloropleth Map.png")},
                                               content = function(file){mapshot(FullMap(),file = file)})

  ##Release Calendar Code
  
  output$ReleaseCalendar<-DT::renderDataTable(Output_ReleaseCalendar())
  
  })


