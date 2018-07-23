library(lubridate)
library(officer)
library(tidyr)
library(dplyr)
library(zoo)

#Function to clean generic dataframes by removing all columns with at max two non-NA entries

CleanDF<-function(DF,length = 2){
  
  #Column index range
  DFnew<-1:nrow(DF)
  
  #Iterate through all columns in DF
  for(i in 1:ncol(DF)){
    
    #If a column has more than 2 non-NA entries then keep it in new cleaned dataframe
    if(length(DF[which(is.na(DF[,i])==FALSE),i])>length){
      DFnew<-cbind(DFnew,DF[,i])
    }
    
  }
  
  #Return cleaned dataframe
  DFnew<-DFnew[,2:ncol(DFnew)]
  
  return(DFnew)
}

#Function to clean DCMS Economic Estimates data

Clean_DCMSEconomic<-function(Data,sheetno = 2){
  
  Data<-paste0("./Data/DCMS",Data,".xlsx")
  
  Data.tibble<-readxl::read_excel(Data,sheet = sheetno)
  
  Years.string<-gsub("Years: ","",as.data.frame(Data.tibble[2,1]))
  pos = regexpr(" - ", Years.string)[1]
  Year_min<-as.numeric(substr(Years.string,1,pos-1))
  Year_max<-as.numeric(substr(Years.string,pos+3,nchar(Years.string)))
  
  tourism_rng<-grep("Tourism",Data.tibble[[1]],ignore.case = FALSE)
  UK_rng<-grep("UK",Data.tibble[[1]],ignore.case = FALSE)
  sector_rng<-grep("Sector",Data.tibble[[1]],ignore.case = FALSE)
  
  notes_row<-min(grep("Notes",Data.tibble[[1]],ignore.case = FALSE))
  sector_row<-min(sector_rng[which(sector_rng>3)])
  UK_row<-max(UK_rng[which(UK_rng>sector_row & UK_rng<notes_row)])
  tourism_row<-min(tourism_rng[which(tourism_rng>sector_row & tourism_rng<notes_row)])
  
  Min_column<-min(which(grepl(Year_min,Data.tibble[sector_row,])))
  Max_column<-min(which(grepl(Year_max,Data.tibble[sector_row,])))
  Year_rng<-seq(Min_column,Max_column,1)
  
  Tourism_Data<-as.data.frame(Data.tibble[tourism_row,Year_rng])
  UK_Data<-as.data.frame(Data.tibble[UK_row,Year_rng])
  Other_Data<-as.numeric(UK_Data)-as.numeric(Tourism_Data)
  
  Data.table<-rbind(Tourism_Data,Other_Data)
  colnames(Data.table)<-Year_min:Year_max
  rownames(Data.table)<-c("Tourism","Other Industries")
  
  return(Data.table)
}

#Function to clean any ONS Time Series csv file
#Updated Clean_ONSTimeSeries v1

Clean_ONSTimeSeriesv1<-function(Code, Mode = "Annual",start = 2010){
  
  if(!(Mode %in% c("Annual","Quarterly","Monthly"))){return(NA)}
  
  #Read in data based on Time Series 4 character code
  TS<-data.table::fread(paste0("./Data/",Code,".csv"))
  
  if(Mode == "Annual"){
    #Convert first and second columns to class numeric and suppress warnings
    TS[[2]]<-suppressWarnings(as.numeric(TS[[2]]))
    TS[[1]]<-suppressWarnings(as.numeric(TS[[1]]))
    
    #Get rid of all rows which contain an NA
    TS<-na.omit(TS)
    
    #Rename column names to generic 'Year' and 'Value'
    colnames(TS)<-c("Year","Value")
    
    #Return clean time series
    return(TS)} else if(Mode == "Quarterly"){
      
      #Convertsecond column to class numeric and suppress warnings
      TS[[2]]<-suppressWarnings(as.numeric(TS[[2]])) 
      
      #Get rid of all rows which contain an NA
      TS<-na.omit(TS) 
      YearRng<-seq(1980,as.integer(format(Sys.Date(), "%Y")))
      
      TS_QF_year<-0
      TS_QF_quarter<-0
      
      for(i in 1:4){
        
        Qi<-paste0(YearRng," Q",i)
        
        TS_Qi<-TS[which(TS[[1]] %in% Qi),]
        
        TS_QiF<-TS_Qi[[1]][nrow(TS_Qi)]
        
        TS_QiF_year<-as.integer(substr(TS_QiF,1,4))
        TS_QiF_quarter<-as.integer(substr(TS_QiF,nchar(TS_QiF),nchar(TS_QiF)))
        
        if(TS_QiF_year>TS_QF_year){TS_QF_quarter=TS_QiF_quarter
        TS_QF_year=TS_QiF_year} else 
          if(TS_QiF_quarter>TS_QF_quarter & TS_QF_year == TS_QiF_year){TS_QF_quarter=TS_QiF_quarter}
      }
      
      Q_rng_max<-which(TS[[1]] == paste0(max(YearRng)," Q",TS_QF_quarter))
      Q_rng_min<-Q_rng_max - (max(YearRng)-start)*4
      
      Q_rng<-Q_rng_min:Q_rng_max
      
      TS_Q<-TS[Q_rng,]
      #Rename column names to generic 'Year' and 'Value'
      colnames(TS_Q)<-c("Year","Value")
      #Return clean time series
      return(TS_Q)
    } else {

      YearRng<-seq(1980,as.integer(format(Sys.Date(), "%Y")))
      
      Month_rng<-(nrow(TS)-(max(YearRng)-start)*12):nrow(TS)
      
      TS_Month_Rng<-TS[Month_rng,]
      colnames(TS_Month_Rng)<-c("Year","Value")
      
      return(TS_Month_Rng)
      
    }
}

#Updated Clean_ONSTimeSeries

Clean_ONSTimeSeriesv2<-function(Code, Mode = "Annual",TP = NA){
  
  if(!(Mode %in% c("Annual","Quarterly","Monthly"))){return(NA)}
  
  #Read in data based on Time Series 4 character code
  TS<-data.table::fread(paste0("./Data/",Code,".csv"))
  
  if(Mode == "Annual"){
    #Convert first and second columns to class numeric and suppress warnings
    TS[[2]]<-suppressWarnings(as.numeric(TS[[2]]))
    TS[[1]]<-suppressWarnings(as.numeric(TS[[1]]))
    
    #Get rid of all rows which contain an NA
    TS<-na.omit(TS)
    
    #Rename column names to generic 'Year' and 'Value'
    colnames(TS)<-c("Year","Value")
    
    #Return clean time series
    return(TS)} else if(Mode == "Quarterly"){
      
      #Convertsecond column to class numeric and suppress warnings
      TS[[2]]<-suppressWarnings(as.numeric(TS[[2]])) 
      
      #Get rid of all rows which contain an NA
      TS<-na.omit(TS) 
      YearRng<-seq(1980,as.integer(format(Sys.Date(), "%Y")))
      
      TS_QF_year<-0
      TS_QF_quarter<-0
      
      for(i in 1:4){
        
        Qi<-paste0(YearRng," Q",i)
        
        TS_Qi<-TS[which(TS[[1]] %in% Qi),]
        
        TS_QiF<-TS_Qi[[1]][nrow(TS_Qi)]
        
        TS_QiF_year<-as.integer(substr(TS_QiF,1,4))
        TS_QiF_quarter<-as.integer(substr(TS_QiF,nchar(TS_QiF),nchar(TS_QiF)))
        
        if(TS_QiF_year>TS_QF_year){TS_QF_quarter=TS_QiF_quarter
        TS_QF_year=TS_QiF_year} else 
          if(TS_QiF_quarter>TS_QF_quarter & TS_QF_year == TS_QiF_year){TS_QF_quarter=TS_QiF_quarter}
      }
      
      if((is.na(TP)==FALSE)&(TP %in% c("Q1","Q2","Q3","Q4"))){
        
        Q<-paste0(YearRng," ",TP)
        
      }else{
        
      Q<-paste0(YearRng," Q",TS_QF_quarter)
      
      }
        
      Q_rng<-which(TS[[1]] %in% Q)
      
      TS_Q<-TS[Q_rng,]
      #Rename column names to generic 'Year' and 'Value'
      colnames(TS_Q)<-c("Year","Value")
      #Return clean time series
      return(TS_Q)
    } else {
      
      TS_lastrow<-nrow(TS) 
      TS_lastentry<-TS[TS_lastrow,]
      TS_lastmonth<-substr(TS_lastentry[1,1],6,8)
      
      YearRng<-seq(1980,as.integer(format(Sys.Date(), "%Y")))
      
      if((is.na(TP)==FALSE)&(TP %in% c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))){
      
        Month<-paste0(YearRng," ",TP)
        
      }else{
        
      Month<-paste0(YearRng," ",TS_lastmonth)
      
      }
      
      Month_rng<-which(TS[[1]] %in% Month)

      TS_Month_Rng<-TS[Month_rng,]
      colnames(TS_Month_Rng)<-c("Year","Value")
      
      return(TS_Month_Rng)
      
    }
}

#Check for ONS updates to DCMS data and adds to Clean_DCMSEconomic output

Clean_Economic<-function(Data, sheetno = 2){
  
  #Produces cleaned DCMS economic data
  DCMS<-Clean_DCMSEconomic(Data = Data, sheetno = sheetno)
  DCMSMaxYear<-max(as.integer(colnames(DCMS)))
  
  TSASheets0<-readxl::excel_sheets("./Data/TSA.xls")
  TSASheets<-TSASheets0[which(TSASheets0!="Contents" & TSASheets0!="Notes")]
  TSAYears<-as.integer(substr(TSASheets,nchar(TSASheets)-3,nchar(TSASheets)))
  TSAMaxYear<-max(TSAYears)
  
  if(TSAMaxYear<=DCMSMaxYear){return(DCMS)} else {
    
  TSAYearRng<-seq(DCMSMaxYear+1,TSAMaxYear)
  
  Rng<-data.frame(NA)
    
    for(year in TSAYearRng){
      
      if(Data == "GVA"){
      
        TDGVA<-paste0("TDGVA-",year)
      
        if(length(which(grepl(TDGVA,TSASheets)==TRUE))>0){
        
          TDGVASheetNo<-which(grepl(TDGVA,TSASheets0)==TRUE)
        
          TDGVAyear<-readxl::read_excel("./Data/TSA.xls",sheet = TDGVASheetNo)
          TDGVAyear<-as.data.frame(TDGVAyear)
          Clean_TDGVAyear<-CleanDF(TDGVAyear)
          col<-length(colnames(Clean_TDGVAyear))
          row<-length(Clean_TDGVAyear[which(!is.na(Clean_TDGVAyear[,col])),1])
          
          GVA<-as.numeric(Clean_TDGVAyear[row,col])
          GVA<-data.frame(GVA)
          colnames(GVA)<-year
        
        } else {
          
          TDGVAT5No<-which(grepl(paste0("T5-",year),TSASheets0)==TRUE)
          TDGVAT7No<-which(grepl(paste0("T7-",year),TSASheets0)==TRUE)
          
          TDGVAyear_T5<-as.data.frame(readxl::read_excel("./Data/TSA.xls",sheet = TDGVAT5No))
          TDGVAyear_T7<-as.data.frame(readxl::read_excel("./Data/TSA.xls",sheet = TDGVAT7No))
          
          Clean_T5<-CleanDF(TDGVAyear_T5)
          Clean_T7<-CleanDF(TDGVAyear_T7)
          
          Ratios<-suppressWarnings(as.numeric(Clean_T7[,which(grepl("Tourism ratio*",Clean_T7[2,]))]))
          Ratios<-Ratios[which(!is.na(Ratios))]
          Ratios<-Ratios[1:(length(Ratios)-1)]

          IndustryGVA<-na.omit(Clean_T5)
          IndustryGVA<-suppressWarnings(as.numeric(IndustryGVA[length(IndustryGVA[,1]),]))
          IndustryGVA<-IndustryGVA[which(!is.na(IndustryGVA))]
          IndustryRng<-c(1:(length(IndustryGVA)-3),(length(IndustryGVA)-1))
          IndustryGVA<-IndustryGVA[IndustryRng]
          
          GVA<-sum(IndustryGVA*Ratios)
          GVA<-data.frame(GVA)
          colnames(GVA)<-year
          
         }
        
        GVA_TS<-Clean_ONSTimeSeries("ABML")
        GVA_TS_year<-GVA_TS[which(GVA_TS[,1]==year),]
        UKGVA<-GVA_TS_year$Value
        GVA<-rbind(GVA,(UKGVA-GVA))
        Rng<-cbind(Rng,GVA)
        
      } else if(Data == "Employment" & length(which(grepl(paste0("T7-",year),TSASheets)==TRUE))>0){
        
        TDGVAT7No<-which(grepl(paste0("T7-",year),TSASheets0)==TRUE)
        TDGVAyear_T7<-as.data.frame(readxl::read_excel("./Data/TSA.xls",sheet = TDGVAT7No))
        Clean_T7<-CleanDF(TDGVAyear_T7)
        Employment<-na.omit(Clean_T7)
        Employment<-as.numeric(Employment[length(Employment[,1]),length(Employment[1,])-1])
        
        Employment<-data.frame(Employment)
        colnames(Employment)<-year
        
        Rng<-cbind(Rng,Employment)
        Rng[2,]<-rep(NA,length(rownames(Rng)))
        
      }
    }
  AppendONS<-cbind(DCMS,Rng[,2:length(Rng)])
  Append_Rng<-(length(DCMS[1,])+1):(length(DCMS[1,])+length(TSAYearRng))
  
  colnames(AppendONS)[Append_Rng]<-TSAYearRng
  return(AppendONS)
  }

}

#Returns 

Clean_ONSVisitSpend<-function(Mode = "Annual",start=2010,TP = NA,ver = "v2"){
  
  if(ver == "v1"){
    
    Visits<-Clean_ONSTimeSeriesv1("GMAA",Mode,start)
    Spend<-Clean_ONSTimeSeriesv1("GMAK",Mode,start)
    
  }else if(ver == "v2"){
    
    Visits<-Clean_ONSTimeSeriesv2("GMAA",Mode,TP)
    Spend<-Clean_ONSTimeSeriesv2("GMAK",Mode,TP)
    
  }
  
  VYear<-as.integer(substr(Visits$Year,1,4))
  SYear<-as.integer(substr(Spend$Year,1,4))
    
    VSYear_Rng<-pmin(VYear,SYear)
    Year_Rng<-start:max(VSYear_Rng)
    
    Visits_Rng<-Visits[which(VYear%in%Year_Rng),]
    Spend_Rng<-Spend[which(SYear%in%Year_Rng),]
  
  Visits_Rng_value<-as.numeric(t(Visits_Rng)[2,])
  Spend_Rng_value<-as.numeric(t(Spend_Rng)[2,])
  
  VisitSpend<-rbind(Visits_Rng_value,Spend_Rng_value)
  colnames(VisitSpend)<-Visits_Rng[[1]]
  rownames(VisitSpend)<-c("Visits","Spend")
  
  return(VisitSpend)
}
    
Clean_GBTSVisitSpend<-function(){
  
  GBTS<-read.csv("./Data/GBTS.csv") 
  if(colnames(GBTS)[1]=="X"){GBTS<-GBTS[,2:length(GBTS)]}
  colnames(GBTS)<-as.integer(gsub("X","",colnames(GBTS)))
  row.names(GBTS)<-c("Visits","Spend")
  
  return(GBTS)}

Clean_VB_PPTXData<-function(Date,Series){
  
  pptx_file<-paste0("./Data/",Date," ",Series,".pptx")  
  
  content<-officer::read_pptx(pptx_file)
  content<-officer::pptx_summary(content)
  
  table_cells <- content %>% filter(content_type %in% "table cell")
  
  slide_id_rng<-unique(table_cells$slide_id)
  
  TableCheck = FALSE
  slide = min(slide_id_rng)
  i = 1
  
  while(TableCheck == FALSE & slide<=max(slide_id_rng)){
    
    Filter<-table_cells %>% filter( slide_id == slide )
    
    id_rng<-as.numeric(unique(Filter$id))
    Table<-data.frame(NA)
    
    for(j in id_rng){
      
      Filter2<-Filter %>% filter(id == j) %>%
        select(row_id, cell_id, text) %>% 
        spread(cell_id, text)
      
      Filter2<-as.data.frame(Filter2)
      
      if(nrow(Filter2)<nrow(Table)){Filter2[6:(nrow(Filter2)+1),]<-Filter2[5:nrow(Filter2),]
      Filter2[5,]<-Filter2[1,]}
      
      if(which(id_rng == j) == 1){
        Table<-merge(Table,Filter2, all = TRUE)} else {
          Table<-cbind(Table,Filter2)
        }
      
    }
    
    if(nrow(Table)<7){i = i+1
    slide = slide_id_rng[i]
    TableCheck = FALSE} else {TableCheck = TRUE}
    
  }
  
  Table2<-suppressWarnings(apply(Table, 2, function(x) as.numeric(gsub("[*]","",gsub("[,]","",gsub("[£]","",gsub("^$|^ $", NA, x)))),fixed = TRUE)))
  
  colcheck<-sapply(1:ncol(Table2),function(x) length(Table2[which(is.na(Table2[,x])),x]))
  
  colcheck_mode<-as.integer(names(table(colcheck))[which(table(colcheck)==max(table(colcheck)))])
  
  colcheck_mode_min_max<-colcheck_mode[which(!(colcheck_mode %in% nrow(Table2)))]
  
  Table3<-na.omit(cbind(Table[,2],Table2[,which(colcheck %in% colcheck_mode_min_max)]))
  
  colnames(Table3)<-Table3[1,]
  
  Table3<-Table3[2:nrow(Table3),2:ncol(Table3)]
  
  if(Series == "GBTS"){
    
    row_rng<-which(!(1:nrow(Table3) %in% seq(3,nrow(Table3),by = 3)))
    Table3<-Table3[row_rng,]
    
    if(nrow(Table3)>4){rownames(Table3)[5:6]<-c("GB Expenditure","England Expenditure")}
    
    rownames(Table3)[1:4]<-c("GB Trips","England Trips","GB Bednights","England Bednights")
    
    years<-unique(as.numeric(colnames(Table3)))
    
    Latest_1<-Table3[,which(colnames(Table3)==years[1])]
    Latest<-Table3[,which(colnames(Table3)==years[2])]
    
    colnames(Latest_1)<-c(paste0(years[1],"-",1:12,"-",1),paste0("YTD ",gsub(years[2],years[1],Date)))
    colnames(Latest)<-c(paste0(years[2],"-",1:(ncol(Latest)-1),"-",1),paste0("YTD ",Date))
    
    GBTS<-list(Latest_1,Latest)
    names(GBTS)<-years
    return(GBTS)
    
  } else {
    
    rownames(Table3)<-c("GB 3 months","Eng 3 Months","GB YTD","Eng YTD")
    
    halfcol<-ncol(Table3)/2
    
    Visits<-Table3[,1:halfcol]
    
    Spend<-Table3[,(halfcol+1):ncol(Table3)]
    
    GBDV<-list(Visits,Spend)
    
    names(GBDV)<-c("Visits","Spend")
    
    return(GBDV)
    
  }
}

Clean_VB_LatestGBTS<-function(){
  
  Files<-list.files("./Data/")
  PPTX_Dates<-substr(Files[which(grepl("pptx",Files) & grepl("GBTS",Files))],1,10)
  PPTX_Dates<-ymd(PPTX_Dates)
  
  Latest_PPTX<-Clean_VB_PPTXData(PPTX_Dates[length(PPTX_Dates)],"GBTS")
  
  GBTSMonthly<-read.csv("./Data/GBTSMonthly.csv",na.strings=c(NA,""))[,2:4]
  M_CurrentDates<-dmy(GBTSMonthly$Month)
  
  MissingMonths<-PPTX_Dates[which(!(PPTX_Dates %in% M_CurrentDates))]
  
  if(length(MissingMonths)==0){return(GBTSMonthly)}else{
  
  Av_Months<-c(M_CurrentDates,MissingMonths)
  
  Add_Data1<-Latest_PPTX[[2]][,which(ymd(colnames(Latest_PPTX[[2]])) %in% M_CurrentDates)]
  Add_Data2<-Latest_PPTX[[2]][,which(ymd(colnames(Latest_PPTX[[2]])) %in% MissingMonths)]
  
  date_rng<-which(M_CurrentDates %in% ymd(colnames(Add_Data1)))
  
  if(nrow(Add_Data1)==6){
    
    Add_Data1<-Add_Data1[c(1,5),]
    
    Add_Data_Dates<-colnames(Add_Data2)
    Add_Data_Dates<-format(ymd(Add_Data_Dates[which(!grepl("YTD",Add_Data_Dates))]),"%d/%m/%Y")
    
    Add_Data2<-Add_Data2[c(1,5),which(!grepl("YTD",colnames(Add_Data2)))]
    
    GBTSMonthly[date_rng,2]<-Add_Data1[1,]
    GBTSMonthly[date_rng,3]<-Add_Data1[2,]
    
    if(is.null(nrow(Add_Data2))){
    GBTSExtra<-t(as.data.frame(c(Add_Data_Dates,Add_Data2)))
    }else{
      GBTSExtra<-t(rbind(Add_Data_Dates,Add_Data2))
    }
    
    rownames(GBTSExtra)<-NULL
    colnames(GBTSExtra)<-c("Month","Visits","Spend")
    
    GBTSMonthly<-rbind(GBTSMonthly,GBTSExtra)
    
  }else{
    
    Add_Data1<-Add_Data1[c(1),]
    
    Add_Data_Dates<-colnames(Add_Data2)
    Add_Data_Dates<-format(ymd(Add_Data_Dates[which(!grepl("YTD",Add_Data_Dates))]),"%d/%m/%Y")
    
    Add_Data2<-Add_Data2[c(1),which(!grepl("YTD",colnames(Add_Data2)))]
   
    GBTSMonthly[date_rng,2]<-Add_Data1
    
    if(is.null(nrow(Add_Data2))){
      GBTSExtra<-t(as.data.frame(c(Add_Data_Dates,Add_Data2)))
    }else{
      GBTSExtra<-t(rbind(Add_Data_Dates,Add_Data2))
    }
    
    GBTSExtra<-cbind(GBTSExtra,rep(NA,nrow(GBTSExtra)))
    rownames(GBTSExtra)<-NULL
    colnames(GBTSExtra)<-c("Month","Visits","Spend")
    
    GBTSMonthly<-rbind(GBTSMonthly,GBTSExtra)
    
  }
 
  write.csv(GBTSMonthly,file = "./Data/GBTSMonthly.csv")
  return(GBTSMonthly)}
  
}

Clean_VB_GBTSModev1<-function(Data,Mode){
  
  if(Mode =="Monthly"){
    
    Data$Spend<-Data$Spend/1000
    return(Data)
    
  }else if(Mode == "Quarterly"){
    
  quarters<-as.yearqtr(dmy(Data$Month)) 
  lastquarter<-quarters[length(quarters)]
  num_lastquarters<-length(which(quarters==lastquarter))
  
  QData_Visits<-tapply(Data$Visits,as.yearqtr(dmy(Data$Month)),sum)
  QData_Spend<-tapply(Data$Spend,as.yearqtr(dmy(Data$Month)),sum)/1000
  
  QData<-t(rbind(QData_Visits,QData_Spend))
  QData<-data.frame(rownames(QData),as.numeric(QData[,1]),as.numeric(QData[,2]))
  
  colnames(QData)<-c("Quarter","Visits","Spend")
  
  if(num_lastquarters<3){QData<-QData[1:(nrow(QData)-1),]}
  
  return(QData)
  
  }else if(Mode == "Annual"){
    
  AData<-Clean_GBTSVisitSpend()  
  AData<-t(AData)
  AData<-cbind(as.numeric(rownames(AData)),as.numeric(AData[,1]),as.numeric(AData[,2]))
  
  colnames(AData)<-c("Year","Visits","Spend")
  
  return(AData)
  
  }

}

Clean_VB_GBTSModev2<-function(Data,TP){
  
  if(TP %in% c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")){
    
    Data$Month<-format(dmy(Data$Month),"%b %Y")
    
    Data<-Data[which(grepl(TP,Data$Month)),]
    
    Data$Spend<-Data$Spend/1000
    
    return(Data)
    
  }else if(TP %in% c("Q1","Q2","Q3","Q4")){
    
    QData<-Clean_VB_GBTSModev1(Data,Mode = "Quarterly")
    
    QData<-QData[which(grepl(TP,as.character(QData$Quarter))),]
    
    return(QData)
    
  }else if(is.na(TP)){
    
    AData<-Clean_VB_GBTSModev1(Data,Mode = "Annual")
    
    return(AData)
    
  }
  
}

Clean_RegionalTables<-function(){
  
  ##Inbound Data
  
  InboundSheets<-readxl::excel_sheets("./Data/VB_IPSData.xls")
  
  LatestYear<-max(as.integer(InboundSheets))
  LatestYear_1<-LatestYear-1
  
  InboundShtNo<-which(InboundSheets==LatestYear)
  InboundShtNo_1<-which(InboundSheets==LatestYear-1)
  
  Inbound<-readxl::read_excel("./Data/VB_IPSData.xls",sheet = InboundShtNo)
  Inbound_1<-readxl::read_excel("./Data/VB_IPSData.xls",sheet = InboundShtNo_1)
  
  RegionList<-c("West Midlands","East of England","East Midlands",
                "London","North West","North East","South East",
                "South West","Yorkshire","Scotland","Wales","NORTHERN IRELAND")
 
  InboundList<-NULL
  
 for(x in 0:1){
  
  if(x==0){DF=Inbound}else{DF=Inbound_1} 
   
  rows<-NULL
  
  for(i in 1:nrow(DF)){if(length(DF[i,2:ncol(DF)][which(!is.na(DF[i,ncol(DF)]))])>0){
                            rows<-c(rows,i)}}
  
  DF<-DF[rows,]
  DF<-cbind(DF[[1]],DF[,which(!is.na(DF[1,]))])
  colnames(DF)<-c("NUTS1.Region",paste0("Inbound ",DF[1,2:ncol(DF)]))
  DF<-DF[match(RegionList, DF[,1]),]
  
  InboundList<-list(InboundList,DF)
 }
  InboundList<-list(InboundList[[1]][[2]],InboundList[[2]])
  names(InboundList)<-c(LatestYear,LatestYear_1)
  for(x in 1:2){rownames(InboundList[[x]])<-rep(NULL, nrow(InboundList[[x]]))}
  
  ##Domestic Overnight
  
  DORV<-read.csv("./Data/DORegionalVisits.csv")
  DORS<-read.csv("./Data/DORegionalSpend.csv")
  
  DORV<-DORV[,2:ncol(DORV)]
  DORS<-DORS[,2:ncol(DORS)]
  
  colnames(DORV)<-gsub("X","",colnames(DORV))
  colnames(DORS)<-gsub("X","",colnames(DORS))
  
  YearMax<-as.integer(max(colnames(DORV)[2:ncol(DORV)],colnames(DORS)[2:ncol(DORS)],LatestYear))
  YearRng<-c(YearMax-1,YearMax)
  
  DORV<-DORV[,c(1,which(colnames(DORV) %in% YearRng))]
  DORS<-DORS[,c(1,which(colnames(DORS) %in% YearRng))]
  
  if(!(YearMax %in% colnames(DORV))){DORV[,ncol(DORV)+1]<-rep(NA,nrow(DORV))
                                    colnames(DORV)[ncol(DORV)]<-YearMax}
  
  if(!(YearMax %in% colnames(DORS))){DORS[,ncol(DORS)+1]<-rep(NA,nrow(DORS))
  colnames(DORS)[ncol(DORS)]<-YearMax}
  
  if(!(YearMax-1 %in% colnames(DORV))){DORV[,ncol(DORV)+1]<-rep(NA,nrow(DORV))
  colnames(DORV)[ncol(DORV)]<-YearMax-1}
  
  if(!(YearMax-1 %in% colnames(DORS))){DORS[,ncol(DORS)+1]<-rep(NA,nrow(DORS))
  colnames(DORS)[ncol(DORS)]<-YearMax-1}
  
  DO<-cbind(as.vector(DORV[,1]),DORV[,as.character(YearMax)],DORS[,as.character(YearMax)])
  colnames(DO)<-c("NUTS1.Region","DO Visits","DO Spend")
  
  DO_1<-cbind(as.vector(DORV[,1]),DORV[,as.character(YearMax-1)],DORS[,as.character(YearMax-1)])
  colnames(DO_1)<-c("NUTS1.Region","DO Visits","DO Spend")
  
  if(YearMax %in% names(InboundList)){InboundList[[as.character(YearMax)]]=
                                                             cbind(InboundList[[as.character(YearMax)]],
                                                                   DO[,2:ncol(DO)])}
  
  if(YearMax - 1 %in% names(InboundList)){InboundList[[as.character(YearMax-1)]]=
    cbind(InboundList[[as.character(YearMax-1)]],
          DO_1[,2:ncol(DO_1)])}
  
  for(i in 1:2){
  FinalCol<-which(!(grepl("Nights",colnames(InboundList[[i]])) | grepl("Sample",colnames(InboundList[[i]]))))
  InboundList[[i]]<-InboundList[[i]][,FinalCol]
  InboundList[[i]][,1]=as.vector(InboundList[[i]][,1])
  InboundList[[i]][9,1]="Yorkshire and the Humber"
  InboundList[[i]][12,1]="Northern Ireland"
  }
  
  StatsWales<-read.csv("./Data/StatsWales.csv")
  colnames(StatsWales)<-as.integer(gsub("X","",colnames(StatsWales)))
  
  RegionalList<-InboundList
  
  int_rng<-intersect(names(RegionalList),colnames(StatsWales))
  
  for(year in int_rng){
    
  RegionalList[[year]][,4] = as.numeric(as.character(RegionalList[[year]][,4]))  
  RegionalList[[year]][,5] = as.numeric(as.character(RegionalList[[year]][,5])) 
  
  RegionalList[[year]][11,4] = StatsWales[1,year]
  RegionalList[[year]][11,5] = StatsWales[2,year]
  }
  
 return(RegionalList)
  
}
  
Clean_RegionalValue<-function(Data = "GVA"){
  
  filepath<-paste0("./Data/TourismRegional",Data,".xls")
  NUTS1Sheets<-readxl::excel_sheets(filepath)
  sheetno<-min(which(grepl("NUTS1",NUTS1Sheets)))
  
  NUTS1<-readxl::read_excel(filepath,sheet = sheetno)
  
  if(Data=="GVA"){
    
    RegionList<-c("West Midlands","East of England","East Midlands",
                                "London","North West","North East","South East",
                                "South West","Yorkshire and The Humber","Scotland","Wales","Northern Ireland")
  
    YearOpt<-na.omit(colnames(NUTS1))
    YearOpt<-gsub("X__","",YearOpt)
    YearOptRng<-suppressWarnings(which(is.na(as.numeric(YearOpt))))
    YearOpt<-substr(YearOpt[YearOptRng],nchar(YearOpt[YearOptRng])-3,nchar(YearOpt[YearOptRng]))
    Year<-min(as.numeric(YearOpt))
  
  } else {
    
    RegionList<-c("West Midlands","East of England","East Midlands",
                  "London","North West","North East","South East",
                  "South West","Yorkshire & Humberside","Scotland","Wales","Northern Ireland")
                                
  }

  
  NUTS1<-NUTS1[which(!is.na(NUTS1[,1])),]
  NUTS1<- as.data.frame(NUTS1[,colSums(is.na(NUTS1))<nrow(NUTS1)])
  
  RegionMatch<-match(RegionList,NUTS1[,2])
  col1<-2
  
  if(length(na.omit(RegionMatch))==0){RegionMatch<-match(RegionList,NUTS1[,1])
                                      col1<-1}
  
  NUTS1_Regions<-as.data.frame(NUTS1[RegionMatch,col1:ncol(NUTS1)])
  NUTS1_Regions[,1]<-gsub("T","t",NUTS1_Regions[,1])
  NUTS1_Regions[,1]<-gsub("& Humberside","and the Humber",NUTS1_Regions[,1])
  
  if(Data=="GVA"){
    
    GVAfilepath<-paste0("./Data/Regional",Data,".xls")
    GVASheets<-readxl::excel_sheets(GVAfilepath)
    sheetno<-min(which(grepl("Table",GVASheets)))
    
    GVA<-readxl::read_excel(GVAfilepath,sheet = sheetno)
    GVA<-GVA[which(!is.na(GVA[,1])),]
    GVA<- as.data.frame(GVA[,colSums(is.na(GVA))<nrow(GVA)])
    
    CurrentGVA<-GVA[,c(1,3,min(which(grepl(Year,GVA[1,]))))]
    Current_NUTS1GVA<-CurrentGVA[which(CurrentGVA[,1]=="NUTS1"),]
    
    Current_NUTS1GVA<-Current_NUTS1GVA[match(RegionList,Current_NUTS1GVA[,2]),2:3]
    
    NUTS1_Regions<-cbind(NUTS1_Regions[,c(1,3)],Current_NUTS1GVA[,2])
    NUTS1_Regions$Percentage<-as.numeric(NUTS1_Regions[,2])/as.numeric(NUTS1_Regions[,3])
    
    colnames(NUTS1_Regions)<-c("NUTS1","TDGVA","Region GVA","% of Regional GVA")
  
    } else {colnames(NUTS1_Regions)<-c("NUTS1","TDEmployment","Region Employment","% of all jobs")}
  
  return(NUTS1_Regions)
  
}

Clean_ReleaseCalendar<-function(){
  
  DCMS_RC<-na.omit(read.csv("./Data/ReleaseCalendar.csv",na.strings = c("",NA)))[,2:4]
  DCMS_RC$Release.Date<-format(as.Date(as.vector(DCMS_RC$Release.Date), "%d/%m/%Y"),"%d-%m-%Y")
  
  ONS_RC<-read.csv("./Data/ONSReleaseCalendar.csv")[,2:4]
  
  colnames(DCMS_RC)<-colnames(ONS_RC)
  
  RC<-rbind(ONS_RC,DCMS_RC)
  
  RC<-RC[order(as.Date(RC$Date, format="%d-%m-%Y")),]
  
  minDate<-min(as.Date(RC$Date, format="%d-%m-%Y"))
  
  maxDate<-minDate
  month(maxDate)<-month(maxDate)+2
  
  RC<-subset(RC, as.Date(RC$Date, format="%d-%m-%Y")< maxDate)
  
  Org_List<-c("DCMS","ONS","VisitEngland")
  
  RC<-RC[which(RC$Organisation %in% Org_List),]
  
  RC<-RC[which((RC$Organisation %in% Org_List[2:3])|((RC$Organisation=="DCMS")&(grepl("DCMS Sectors",RC$Publication)))),]
  
  return(RC)
}
