library(lubridate)
library(officer)
library(tidyr)
library(dplyr)
library(RCurl)
library(XML)

#Function downloads all DCMS user inputted data sources

Fetch_DCMSData<-function(Source = "./User_Sources/DCMSDataSources.csv"){
  
  #reads in user inputted data sources and converts URLs to characters
  Source_df<-read.csv(Source)
  #does [,1] mean the first column in the csv file?
  Source_df[,1]<-as.character(Source_df[,1])  
  
  #runs through all URLs and downloads corresponding xlsx file and saves it to the data folder
  #Filename is determined by user input
  sapply(Source_df[,1], function (x) 
    
    download.file(url = x, 
                  destfile = paste0("./Data/",Source_df[which(Source_df[,1]==x),2],".xlsx"),
                  mode="wb")
  )
  
  RC_URL<-"https://www.gov.uk/government/statistics/statistics"
  RC_URL_Content<-getURL(RC_URL)
  parsed_RC_Content<-htmlParse(RC_URL_Content)
  RC_links<-unlist(xpathSApply(parsed_RC_Content,path = "//a",xmlGetAttr,"href"))
  
  csv_files<-RC_links[which(grepl(".csv",RC_links))]
  csv_at_end<-substr(csv_files,nchar(csv_files)-3,nchar(csv_files))
  RC_csv_file<-csv_files[which(csv_at_end==".csv")]
  
  download.file(url = paste0("https://www.gov.uk",RC_csv_file), 
                destfile = "./Data/ReleaseCalendar.csv",
                mode="wb")
  
  ONS_Tourism_RC_URL<-"https://www.gov.uk/government/statistics/announcements?utf8=%E2%9C%93&keywords=tourism"
  ONS_Tourism_RC_UR_Content<-getURL(ONS_Tourism_RC_URL)
  parsed_ONS_RC_Content<-htmlParse(ONS_Tourism_RC_UR_Content)
  
  ONS_RC_Stats<-unlist(xpathSApply(parsed_ONS_RC_Content,path = "//h3//a",xmlValue))
  
  ONS_RC_Stats_Dates<-unlist(xpathSApply(parsed_ONS_RC_Content,path = "//ul//li",xmlValue))
  ONS_RC_Stats_Dates<-ONS_RC_Stats_Dates[which((grepl("(provisional)",ONS_RC_Stats_Dates)
                                               |grepl("(confirmed)",ONS_RC_Stats_Dates)))]
  
  ONS_RC_Stats_Dates<-gsub("pm "," pm",gsub("am "," am",gsub("\\(provisional\\)","",gsub("\\(confirmed\\)","",ONS_RC_Stats_Dates))))
  ONS_RC_Stats_Dates<-format(strptime(ONS_RC_Stats_Dates,"%d %b %Y %H:%M %p"),"%d-%m-%Y")
  
  RC<-data.frame(Organisation = rep("ONS",length(ONS_RC_Stats)),Publication = ONS_RC_Stats, Date = ONS_RC_Stats_Dates)
  
  RC<-RC[which(!(grepl("Wales",RC$Publication)|grepl("Scotland",RC$Publication))),]
  
  write.csv(RC,file = "./Data/ONSReleaseCalendar.csv")
  
  #prints message to console when complete
  return(print("DCMS datasets have been downloaded"))
  
}

#Function downloads all ONS data sources

Fetch_ONSData<-function(Source = "./User_Sources/ONSDataSources.csv"){
  
  ##UK Level Datasets
  
  #Reads in time series and dataset sources
  ONS_Sources<-read.csv(Source)
  
  #Creates filetype column. Entries will be '.csv' unless URL ends with '.xls' string
  ONS_Sources$filetype<-rep(".csv",length(rownames(ONS_Sources)))
  FiletypeCheck<-substr(ONS_Sources[,1],nchar(as.character(ONS_Sources[,1]))-3,nchar(as.character(ONS_Sources[,1])))
  ONS_Sources$filetype[which(FiletypeCheck==".xls")]<-".xls"
  
  #runs through all URLs and downloads corresponding file and saves it to the data folder.
  #Filename is determined by csv file and filetype is determined by filetype column.
  ONS_URLs<-ONS_Sources[,1]
    sapply(ONS_URLs, function (x) 
    
    download.file(url = as.character(x), 
                  destfile = paste0("./Data/",ONS_Sources[which(ONS_URLs==x),2], ONS_Sources[which(ONS_URLs==x),3]),
                  mode="wb")
  )
  
  ##Regional Level GVA Data
  
  #Creates a datarange of all years from 2013 (latest available data) to the current year  
  YearRange<-seq(2013,as.integer(format(Sys.Date(), "%Y")))  
  
  #Strings required to create URL to get the regional data
  Regional_p1<-"https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/leisureandtourism/datasets/regionalvalueoftourismestimatesfornuts1andnuts2areas/"
  Regional_p2<-"/regionalreferencetables.xls"
  
  #Creates a vector whose values are FALSE if URL doesn't exist and the year if URL does exist
  URL_Exists<-sapply(YearRange,function(x) if(RCurl::url.exists(paste0(Regional_p1,x,Regional_p2))==TRUE)
  {as.character(x)}
  else{FALSE})
  
  #Works out the latest year which data is available for
  URL_Exists<-URL_Exists[which(URL_Exists!=FALSE)]
  LatestDataYear<-as.integer(URL_Exists[length(URL_Exists)])
  LatestDataURL<-paste0(Regional_p1,LatestDataYear,Regional_p2)
  
  #Downloads latest regional data
  download.file(url = LatestDataURL, 
                destfile = "./Data/TourismRegionalGVA.xls",
                mode="wb")
  
 ##prints message to console when complete
 return(print("ONS datasets have been downloaded"))
}

Fetch_VB_GBTSData<-function(add_string = ""){
  
  YearRange<-seq(2014,as.integer(format(Sys.Date(), "%Y")))  
  
  Data_Dir<-"https://www.visitbritain.org/sites/default/files/vb-corporate/Documents-Library/documents/England-documents/gb_all_trip_purposes_"  
  
  URL_Exists1<-sapply(YearRange,function(x) if(RCurl::url.exists(paste0(Data_Dir,x,".xlsx"))==TRUE)
  {as.character(x)}
  else{FALSE})  
  
  URL_Exists2<-sapply(YearRange,function(x) if(RCurl::url.exists(paste0(Data_Dir,x,"_0.xlsx"))==TRUE)
  {gsub("[[:space:]]","",paste(x,"_0"))}
  else{FALSE})
  
  URL_Exists3<-sapply(YearRange,function(x) if(RCurl::url.exists(paste0(Data_Dir,x,add_string,".xlsx"))==TRUE)
  {gsub("[[:space:]]","",paste(x,add_string))}
  else{FALSE})
  
  URL_Exists<-URL_Exists1
  URL_Exists[which(URL_Exists1==FALSE)]<-URL_Exists2[which(URL_Exists1==FALSE)]
  URL_Exists[which(URL_Exists == FALSE)]<-URL_Exists3[which(URL_Exists == FALSE)]
  URL_Exists<-URL_Exists[which(URL_Exists!=FALSE)]
  
  LatestDataYear<-as.integer(gsub("_0","",URL_Exists[length(URL_Exists)]))
  
  GBTS<-read.csv("./Data/GBTS.csv")
  DORV<-read.csv("./Data/DORegionalVisits.csv")
  DORS<-read.csv("./Data/DORegionalSpend.csv")
  
  DataList<-list(GBTS,DORV,DORS)
  
  for(x in 1:3){if(colnames(DataList[[x]])[1]=="X"){DataList[[x]]=DataList[[x]][,2:ncol(DataList[[x]])]}
                colnames(DataList[[x]]) = suppressWarnings(as.integer(gsub("X","",colnames(DataList[[x]]))))
                colnames(DataList[[x]])[is.na(colnames(DataList[[x]]))]="NUTS1.Regions"}                
                                                      
  GBTS<-DataList[[1]]
  DORV<-DataList[[2]]
  DORS<-DataList[[3]]
  
  RegionList<-DORV[,1]
  
  MissingDataTest<-length(which(is.na(GBTS[,ncol(GBTS)])))>0 & colnames(GBTS)[ncol(GBTS)]==LatestDataYear
  YearTest<-length(which(colnames(GBTS)==LatestDataYear))==0
  
  if(MissingDataTest == TRUE | YearTest == TRUE){
    
    LatestDataURL<-paste0(Data_Dir,LatestDataYear,".xlsx")
    filepath<-paste0("./Data/GBTS",LatestDataYear,".xlsx")
    
    download.file(url = LatestDataURL, 
                  destfile = filepath,
                  mode="wb")
    
    LatestGBTS<-readxl::read_excel(filepath,sheet = 2)
    
    TripsCol<-min(which(grepl("Trips*",colnames(LatestGBTS))))
    AllTripsRow<-min(which(grepl("All trip purposes*",LatestGBTS[[1]])))
    LatestGBTSTrips<-as.numeric(LatestGBTS[[TripsCol]][AllTripsRow])
    LRDORV<-LatestGBTS[match(RegionList, LatestGBTS[[1]]),TripsCol]
    
    if(MissingDataTest==TRUE){newcol=ncol(GBTS)}else{newcol=ncol(GBTS)+1}  
    
    GBTS[1,newcol]<-LatestGBTSTrips
    DORV[,newcol+1]<-as.numeric(LRDORV[[1]])
    colnames(DORV)[newcol+1]<-LatestDataYear
    
    if(length(which(grepl("Spend*",colnames(LatestGBTS))))>0){
      
      SpendCol<-min(which(grepl("Spend*",colnames(LatestGBTS))))
      LRDORS<-LatestGBTS[match(RegionList, LatestGBTS[[1]]),SpendCol]
      
      LatestGBTSSpend<-as.numeric(LatestGBTS[[SpendCol]][AllTripsRow])
      
      GBTS[2,newcol]<-LatestGBTSpend  
      DORS[,newcol+1]<-as.numeric(LRDORS[[1]])
      colnames(DORS)[newcol+1]<-LatestDataYear
    }
    
    colnames(GBTS)[newcol]<-LatestDataYear
    
  }
  write.csv(GBTS,file = "./Data/GBTS.csv")
  write.csv(DORV,file = "./Data/DORegionalVisits.csv")
  write.csv(DORS,file = "./Data/DORegionalSpend.csv")
  
  return(print("VB GBTS data has been downloaded"))
}

Fetch_VB_PPTXData<-function(url,series){
  
  months<-c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  
  url2<-getURL(url)
  parsed<-htmlParse(url2)
  links<-xpathSApply(parsed,path = "//a",xmlGetAttr,"href")
  links<-unlist(links)
  
  ppt_files<-links[which((grepl("ppt",links) & grepl("summary",links)))]
  
  Year<-unique(as.numeric(unlist(strsplit(gsub("[^0-9]", "", ppt_files), ""))))
  Year<-1000*Year[1]+100*Year[2]+10*Year[3]+Year[4]
  
  sys.year = as.numeric(format(Sys.Date(), "%Y"))
  if(!(Year %in% seq(sys.year - 5, sys.year +5))){Year <- sys.year}
  
  MonthCheck<-t(sapply(months,function(x) grepl(x,sub("summary","",ppt_files))))
  MonthCheck<-as.data.frame(apply(MonthCheck, 1, function(r) any(r == TRUE)))
  MonthCheck<-rownames(MonthCheck)[which(MonthCheck==TRUE)]
  
  for(month in MonthCheck){
    
    Current_Month<-ppt_files[which(grepl(month,sub("summary","",ppt_files)))]
    
    month_int<-which(months == month)
    
    Date<-as.Date(paste0(Year,"-",month_int,"-",1))
    
    download.file(url = paste0("https://www.visitbritain.org",Current_Month),
                  destfile = paste0("./Data/",Date," ",series,".pptx"),
                  mode = "wb")
    
    
  }
  
  print(paste0("VB latest ",series," data has been downloaded"))
  
}

#Fetch Regional Data

Fetch_VB_IPSData<-function(IPS_startyear=1999){
  
  DataDownloaded = FALSE
  
  while(DataDownloaded == FALSE){
    
  Path1<-"https://www.visitbritain.org/sites/default/files/vb-corporate/Documents-Library/documents/regional_spread_by_year_flat_file_"
  Path2<-"_-_"
  Path3<-".xls"
  
  YearRange<-seq(IPS_startyear,as.integer(format(Sys.Date(), "%Y"))) 
  
  URL_Exists<-sapply(YearRange,function(x) if(RCurl::url.exists(paste0(Path1,IPS_startyear,Path2,x,Path3))==TRUE)
  {x}
  else{FALSE})  
  
  URL_Exists<-URL_Exists[which(URL_Exists!=FALSE)]
  
  if(length(URL_Exists)==0){IPS_startyear = IPS_startyear+1
                            DataDownloaded = FALSE} else {
  
  URL_year<-max(URL_Exists)
  
  URL_LatestData<-paste0(Path1,IPS_startyear,Path2,URL_year,Path3)
  filepath<-"./Data/VB_IPSData.xls"
  
  #Downloads latest regional data
  download.file(url = URL_LatestData, 
                destfile = filepath,
                mode="wb")
  
  return(print("VB IPS data has been downloaded"))
  
  DataDownloaded = TRUE}}
  
}

#Fetch StatsWales Data (JSON)

Fetch_StatsWalesData<-function(){
  
  url<-"http://open.statswales.gov.wales/en-gb/dataset/tour0007"
  
  json_data1<-jsonlite::fromJSON(txt = url)
  json_data2<-jsonlite::fromJSON(txt = json_data1$odata.nextLink)

  json_data<-rbind(json_data1$value,json_data2$value)
  
  Visits<-json_data[which(json_data$Month_ItemName_ENG=="December" & json_data$Purpose_ItemName_ENG == "All" & json_data$Measure_ItemName_ENG == "Trips, millions" & json_data$Geography_ItemName_ENG == "Wales"),]
  Spend<-json_data[which(json_data$Month_ItemName_ENG=="December" & json_data$Purpose_ItemName_ENG == "All" & json_data$Measure_ItemName_ENG == "Spend, \u00a3millions" & json_data$Geography_ItemName_ENG == "Wales"),]
  
  Wales<-rbind(Visits$Data, Spend$Data)
  colnames(Wales)<-as.vector(Visits$Year_ItemName_ENG)
  rownames(Wales)<-c("Visits","Spend")
  
  write.csv(Wales,file = "./Data/StatsWales.csv")
  
  return(print("StatsWales data has been downloaded"))
}

#Function downloads all data sources 

Fetch_Data<-function(SourceDCMS = "./User_Sources/DCMSDataSources.csv",
                     SourceONS = "./User_Sources/ONSDataSources.csv",
                     add_string="",IPS_startyear = 1999,
                     GBTSURL = "https://www.visitbritain.org/great-britain-tourism-survey-latest-monthly-overnight-data"){
                     #GBDVURL = "https://www.visitbritain.org/gb-day-visits-survey-latest-results")
  
  #Runs all 'Fetch' functions
  Fetch_DCMSData(Source = SourceDCMS)
  Fetch_ONSData(Source = SourceONS)
  Fetch_VB_GBTSData(add_string = add_string)
  Fetch_VB_IPSData(IPS_startyear = IPS_startyear)
  Fetch_VB_PPTXData(url = GBTSURL,"GBTS")
  #Fetch_VB_PPTXData(url = GBDVURL,"GBDV")
  Fetch_StatsWalesData()
  
}
  
  
  
  