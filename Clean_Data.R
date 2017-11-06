library(lubridate)

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
  
  Min_column<-which(Data.tibble[sector_row,]==Year_min)
  Max_column<-which(Data.tibble[sector_row,]==Year_max)
  Year_rng<-seq(Min_column,Max_column,1)
  
  Tourism_Data<-as.data.frame(Data.tibble[tourism_row,Year_rng])
  UK_Data<-as.data.frame(Data.tibble[UK_row,Year_rng])
  Other_Data<-UK_Data-Tourism_Data
  
  Data.table<-rbind(Tourism_Data,Other_Data)
  colnames(Data.table)<-as.data.frame(Data.tibble[sector_row,Year_rng])
  rownames(Data.table)<-c("Tourism","Other Industries")
  
  return(Data.table)
}

#Function to clean any ONS Time Series csv file

Clean_ONSTimeSeries<-function(Code, Mode = "Annual"){
  
  if(!(Mode %in% c("Annual","Quarters"))){return(NA)}
  
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
  return(TS)} else {
  
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
  
  Q<-paste0(YearRng," Q",TS_QF_quarter)
  
  TS_Q<-TS[which(TS[[1]] %in% Q),]
  #Rename column names to generic 'Year' and 'Value'
  colnames(TS_Q)<-c("Year","Value")
  #Return clean time series
  return(TS_Q)
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

Clean_ONSVisitSpend<-function(Mode = "Annual",start=2010){
  
  Visits<-Clean_ONSTimeSeries("GMAA",Mode)
  Spend<-Clean_ONSTimeSeries("GMAK",Mode)
  
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
