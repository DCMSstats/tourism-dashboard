source("Clean_Data.R")
library(plotly)
library(leaflet)
library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(rmapshaper)
library(mapview)
library(webshot)
library(htmltools)

plot_Economic<-function(Data, sheetno = 2){
  
  TS<-Clean_Economic(Data,sheetno)
  
  rng<-1:ncol(TS)
  
  TS_x<-as.numeric(t(data.frame(colnames(TS))))
  
  if(Data == "GVA")
  {TS_y1<-t(TS[1,])[rng,]/1000
  TS_y2<-t(TS[2,])[rng,]/1000
  ytitle<-"GVA (\u00A3)"
  yrange<-c(0, round(max(TS_y1/10),0)*10+5)
  aytitle<-"GVA (\u00A3)"
  ayrange<-c(0,round(max(TS_y2/200),0)*200+200)
  name1<-"Tourism Direct GVA (\u00A3)"
  name2<-"Other Direct GVA (\u00A3)"
  format = "none"}
  else
  {TS_y1<-t(TS[1,])[rng,]
  TS_y2<-t(100*TS[1,]/(TS[1,]+TS[2,]))[rng,]
  ytitle<-"Employment"
  yrange<-c(0,floor(max(TS_y1)/100)*100+200)
  aytitle<-"Percentage of UK Employment"
  ayrange<-c(0,floor(max(TS_y2))+1)
  name1<-"Employment in direct tourism"
  name2<-"Direct tourism as a percentage of UK employment"
  format = "B"}
  
  ay <- list(
    tickfont = list(color = "black"),
    showgrid = FALSE,
    overlaying = "y",
    side = "right",
    title = aytitle,
    range = ayrange,
    exponentformat = format)
  
  m <- list(
    l = 50,
    r = 50,
    b = 50,
    t = 10,
    pad = 4
  )
  
  p <- plot_ly() %>%
    add_lines(x = ~TS_x, y = ~TS_y1, name = name1,color = I("mediumvioletred")) %>%
    add_lines(x = ~TS_x, y = ~TS_y2, name = name2, yaxis = "y2",color = I("midnightblue")) %>%
    config(displayModeBar = F) %>%
    layout(
      yaxis2 = ay,
      legend = list(x = 0.1, y = 0.1),
      xaxis = list(showgrid = FALSE, title = ""),
      yaxis = list(title = ytitle, range = yrange,exponentformat = "none"),
      margin = m,
      autosize = TRUE
    )
  
  return(p)
}

plot_ONSVisitSpend<-function(start,Mode,TP){
  
 TS<-Clean_ONSVisitSpend(Mode = Mode,start = start,ver = "v1") 
  
 rng<-1:ncol(TS)
  
 if(Mode == "Monthly"){
   
 TS_x<-dmy(paste0("01/",substr(as.character(colnames(TS)),6,8),"/", substr(as.character(colnames(TS)),1,4)))
   
 }else{
   
 TS_x<-as.character(colnames(TS))
 
 }
  
  TS_y1<-TS[1,rng]/(10^3)
  TS_y2<-TS[2,rng]/(10^3)
  ytitle<-"Visits (millions)"
  yrange<-c(0, 5*floor(max(TS_y1)/5)+5)
  aytitle<-"Spend (\u00A3bn)"
  ayrange<-c(0,5*floor(max(TS_y2)/5)+5)
  name1<-"Visits (millions)"
  name2<-"Spend (\u00A3bn)"

  ay <- list(
    tickfont = list(color = "black"),
    showgrid = FALSE,
    overlaying = "y",
    side = "right",
    title = aytitle,
    range = ayrange,
    exponentformat = "none")
  
  if(Mode == "Monthly"){
    type = 'date'
    legend = list(x = 0.04,y=0.04)
  }else{
  type = 'category'
  legend = list(x = 0.1,y=0.1)}
  
  m <- list(
    l = 50,
    r = 50,
    b = 50,
    t = 10,
    pad = 4
  )
  
  p <- plot_ly() %>%
    add_lines(x = ~TS_x, y = ~TS_y1, name = name1,color = I("mediumvioletred")) %>%
    add_lines(x = ~TS_x, y = ~TS_y2, name = name2, yaxis = "y2",color = I("midnightblue")) %>%
    config(displayModeBar = F) %>%
    layout(
      yaxis2 = ay,
      legend = legend,
      xaxis = list(showgrid = FALSE, title = "",type = type),
      yaxis = list(title = ytitle, range = yrange, exponentformat = "none"),
      margin = m
    )
  
  return(p)  
  
  
}

plot_GBTS<-function(Data,Mode){
  
  TS<-Clean_VB_GBTSModev1(Data,Mode) 
  
  length <- nrow(TS)
  
  rng<-seq(1,length)
  
  if(Mode == "Monthly"){
  
    TS_x<-dmy(TS[,1])  
    
  }else{
    
    TS_x<-as.character(TS[,1])
  }
  
  TS_y1<-TS[rng,2]
  TS_y2<-TS[rng,3]
  
  ytitle<-"Trips (millions)"
  yrange<-c(0, 5*round(max(TS_y1)/5,0)+5)
  aytitle<-"Spend (\u00A3bn)"
  ayrange<-c(0,5*round(max(TS_y2[which(!is.na(TS_y2))])/5,0)+5)
  name1<-"Trips (millions)"
  name2<-"Spend (\u00A3bn)"
  
  ay <- list(
    tickfont = list(color = "black"),
    showgrid = FALSE,
    overlaying = "y",
    side = "right",
    title = aytitle,
    range = ayrange,
    exponentformat = "none")
  
  if(Mode == "Monthly"){
    type = 'date'
    legend = list(x = 0.04,y=0.04)
  }else{
    type = 'category'
    legend = list(x = 0.1,y=0.1)}
  
  m <- list(
    l = 50,
    r = 50,
    b = 50,
    t = 10,
    pad = 4
  )
  
  p <- plot_ly() %>%
    add_lines(x = ~TS_x, y = ~TS_y1, name = name1,color = I("mediumvioletred")) %>%
    add_lines(x = ~TS_x, y = ~TS_y2, name = name2, yaxis = "y2",color = I("midnightblue")) %>%
    config(displayModeBar = F) %>%
    layout(
      yaxis2 = ay,
      legend = legend,
      xaxis = list(showgrid = FALSE, title = "",type = type),
      yaxis = list(title = ytitle, range = yrange, exponentformat = "none"),
      margin = m
    )
  
  return(p)  
  
  
}

Output_VisitSpend<-function(Annual_Data,Quarterly_Data,series1,series2){
  
  l1<-ncol(Annual_Data)
  rownames(Annual_Data)<-c(series1,series2)
  
  l2<-ncol(Quarterly_Data)
  rownames(Quarterly_Data)<-c(series1,series2)
  
  Table<-data.frame(Annual_Data[,(l1-1):l1],Quarterly_Data[,(l2-1):l2])
  colnames(Table)<-gsub(".Q"," Q",gsub("X","",colnames(Table)))
  Table<-round(Table,1)
  
  Table$Annual_Change<-as.vector(round(100*((Table[,2]/Table[,1])-1),1))  
  Table$Annual_Change[which(!is.na(Table$Annual_Change))]<-paste(Table$Annual_Change[which(!is.na(Table$Annual_Change))],"%",sep="")

  Table$Quarterly_Change<-as.vector(round(100*((Table[,4]/Table[,3])-1),1))  
  Table$Quarterly_Change[which(!is.na(Table$Quarterly_Change))]<-paste(Table$Quarterly_Change[which(!is.na(Table$Quarterly_Change))],"%",sep="")

  colnames(Table)[5:6]<-c("Change","Change ")
  Table<-Table[,c(1,2,5,3,4,6)]
  
  
    sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Metric'),
        th(colspan = 3,class = 'dt-center sorting', 'Latest calendar year'),
        th(colspan = 3,class = 'dt-center sorting', 'Latest quarterly year')
      ),
      tr(
        lapply(colnames(Table),th)
      )
    )
  ))
  
  return(DT::datatable(Table, container = sketch, rownames = TRUE,options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = 1:3)))))
  
}

Output_ONSVisitSpend<-function(Mode = "Annual",start = 2010,units = 10^3,series1,series2,title,y1=2014,y2=2015,TP = NA){
  
  Data = Clean_ONSVisitSpend(Mode = Mode, start = start,TP = TP)/units
  
  l1<-which(substr(colnames(Data),1,4)==y1)
  l2<-which(substr(colnames(Data),1,4)==y2)
  
  if(is.na(TP)){currentchoice1 = y1
                  currentchoice2 = y2
  }else{currentchoice1<-paste0(y1," ",TP)
       currentchoice2<-paste0(y2," ",TP)}
  
  rownames(Data)<-c(series1,series2)
  
  Table<-as.data.frame(Data)
  colnames(Table)<-gsub("'.'"," ",gsub(".Q"," Q",gsub("X","",colnames(Table))))
  
  sketch = htmltools::tags$table(
    class = 'display',
    tags$thead(
      tags$tr(
        tags$th(rowspan = 2, 'Metric'),
        tags$th(colspan = ncol(Table)+1,class = 'dt-center sorting',title)
      ),
      tags$tr(
        lapply(c(colnames(Table),paste0("% Change (",currentchoice1," to ",currentchoice2,")")),tags$th)
      )
    )
  )
  
  Table$Change<-as.vector(round(100*((Table[,l2]/Table[,l1])-1),1))  
  Table$Change[which(!is.na(Table$Change))]<-paste(Table$Change[which(!is.na(Table$Change))],"%",sep="")
  Table[1:2,1:(ncol(Table)-1)]<-round(Table[1:2,1:(ncol(Table)-1)],1)
  
  return(DT::datatable(Table, container = sketch, rownames = TRUE,options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = 1:3)))))
  
}

Output_GBTSVisitSpend<-function(Data,units=1,series1,series2,title,y1,y2,TP = NA){
  
  Data = Clean_VB_GBTSModev2(Data,TP = TP)
  
  l1<-which(grepl(y1,Data[,1]))
  l2<-which(grepl(y2,Data[,1]))
  
  if(is.na(TP)){currentchoice1 = y1
  currentchoice2 = y2
  }else{currentchoice1<-paste0(y1," ",TP)
  currentchoice2<-paste0(y2," ",TP)}
  
  rownames(Data)<-Data[,1]
  Data<-Data[,2:3]
  
  colnames(Data)<-c(series1,series2)
  
  Table<-as.data.frame(t(Data))/units

  sketch = htmltools::tags$table(
    class = 'display',
    tags$thead(
      tags$tr(
        tags$th(rowspan = 2, 'Metric'),
        tags$th(colspan = ncol(Table)+1,class = 'dt-center sorting',title)
      ),
      tags$tr(
        lapply(c(colnames(Table),paste0("% Change (",currentchoice1," to ",currentchoice2,")")),tags$th)
      )
    )
  )
  
  Table$Change<-as.vector(round(100*((Table[,l2]/Table[,l1])-1),1))  
  Table$Change[which(!is.na(Table$Change))]<-paste(Table$Change[which(!is.na(Table$Change))],"%",sep="")
  Table[1:2,1:(ncol(Table)-1)]<-round(Table[1:2,1:(ncol(Table)-1)],1)
  
  return(DT::datatable(Table, container = sketch, rownames = TRUE,options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = 1:3)))))
  
}

Output_Economic<-function(Data_str,y1 = 2014,y2 = 2015){
  
  DT<-Clean_Economic(Data_str)
  l1<-which(as.integer(colnames(DT))==y1)
  l2<-which(as.integer(colnames(DT))==y2)
  
  if(Data_str == "GVA"){sketch = htmltools::withTags(table(
    class = 'display',
    align = 'right',
    thead(
      tr(
        th(rowspan = 2, 'Category'),
        th(colspan = ncol(DT)+1,class = 'dt-center sorting', 'GVA (\u00A3bn)')
      ),
      tr(
        lapply(c(colnames(DT),paste0("% Change (",y1," to ",y2,")")),th)
      )
    )
  ))
  DT<-DT/1000
  
  }else{
    sketch = htmltools::withTags(table(
      class = 'display',
      align = 'right',
      thead(
        tr(
          th(rowspan = 2, 'Category'),
          th(colspan = ncol(DT)+1,class = 'dt-center sorting', 'Employment (000s)')),
        tr(
          lapply(c(colnames(DT),paste0("% Change (",y1," to ",y2,")")),th)
        )
      )
    ))
    DT<-rbind(DT[1,],DT[1,]+DT[2,])
    rownames(DT)<-c("Direct Tourism","All industries")}
  
  DT$Change<-round(100*((DT[,l2]/DT[,l1])-1),1)
  DT$Change[which(!is.na(DT$Change))]<-paste(DT$Change[which(!is.na(DT$Change))],"%",sep="")
  DT[,1:(ncol(DT)-1)]<-round(DT[,1:(ncol(DT)-1)],1)
  
  if(Data_str == "Employment"){DT<-rbind(DT[1,],rep("-",3),DT[2,])
  row.names(DT)[2]<-"Tourism industries"}
  
  return(DT::datatable(DT, container = sketch, rownames = TRUE,options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = 1:3)))))
  
}

SourceURL<-function(x,Dep){
  
if(Dep=="DCMS"){href = paste("https://www.gov.uk/government/statistics/",tolower(gsub(" ","-",gsub(":","",x))),sep="")}

  else if(Dep =="ONS"){
    
    if(grepl("TSA",x)){
    
        href = paste("https://www.ons.gov.uk/economy/nationalaccounts/satelliteaccounts/datasets/",tolower(gsub(" ","",gsub(":","",x))),sep="")}
    
          else if(grepl("GMAA",x)|grepl("GMAK",x)){
          
            href = paste("https://www.ons.gov.uk/peoplepopulationandcommunity/leisureandtourism/timeseries/",tolower(gsub(" ","",gsub(": ","/",x))),sep="")
              
            x<-gsub("GMAK","OS visits to UK: Earnings NSA",gsub("GMAA","OS visits to UK: All visits NSA",paste0(x," Time Series")))
            
          }else{
    
            href = paste("https://www.ons.gov.uk/peoplepopulationandcommunity/leisureandtourism/articles/",tolower(gsub(" ","",gsub(": ","/",x))),sep="")}}
    
ax<-tags$a(href = href,x)
pax<-tags$p(ax)

return(pax)    
}

BaseMap<-function(){
  
  mymap <- leaflet() %>% 
  setView(-5,55,5)%>%
  addProviderTiles("OpenStreetMap.BlackAndWhite")
  
  return(mymap)}
  
OSPolygons<-function(){
  
  area2<-readOGR(dsn = "./Data", layer = "NUTS1b")
  area2@proj4string<-CRS("+init=EPSG:7405")
  area2<-spTransform(area2,CRS("+init=EPSG:4326"))
  
  return(area2)
  
}

  
Output_RegionalTable<-function(Region,Metric){
  
  RegionList<-Clean_RegionalTables()
  
  RegionList[[1]]$`DO Visits`<-1000*RegionList[[1]]$`DO Visits`
  RegionList[[2]]$`DO Visits`<-1000*RegionList[[2]]$`DO Visits`
  
  if(Metric=="GVA"){cols<-seq(3,ncol(RegionList[[1]]),by = 2)
                    text<-"Spend (\u00A3bn)"
                    } else {
                    cols<-seq(2,ncol(RegionList[[1]]),by = 2)
                    text<-"Visits (Millions)"
                    }

  LatestRegion<-RegionList[[1]][which(RegionList[[1]][,1]==Region),cols]
  Latest_1Region<-RegionList[[2]][which(RegionList[[2]][,1]==Region),cols]
  
  Dataframe<-data.frame(round(as.numeric(t(Latest_1Region))/1000,2),round(as.numeric(t(LatestRegion))/1000,2))

  
  colnames(Dataframe)<-names(RegionList)[2:1]
  rownames(Dataframe)<-c("Inbound (UK)","Domestic Overnight (GB)")
  
  Dataframe$Change<-round(100*(Dataframe[,2]/Dataframe[,1]-1),1)
  Dataframe$Change[which(!is.na(Dataframe$Change))]<-paste0(Dataframe$Change[which(!is.na(Dataframe$Change))],"%")

  sketch = htmltools::withTags(table(
    class = 'display',
    align = 'right',
    thead(
      tr(
        th(rowspan = 2, text),
        th(colspan = 3,class = 'dt-center sorting', Region)
      ),
      tr(
        lapply(colnames(Dataframe),th)
      )
    )
  ))
  
  return(DT::datatable(Dataframe, container = sketch, rownames = TRUE,options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = 1:3)))))
  
  }  
  
Output_ReleaseCalendar<-function(){
  
  RC<-Clean_ReleaseCalendar()
  
  RC_DT<-DT::datatable(RC, rownames = FALSE,options = list(dom = 't'))
  
  return(RC_DT)
}  
  
