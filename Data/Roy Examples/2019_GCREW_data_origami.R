#GCREW WARMING DATA ORIGAMI
#Roy Rich  orginate 8/13/2018

# The following codes are functions and procedures for parsing loggernet data streams into reseracher tables for warming projects
# functions are set up in order with annotation along with diretory structure needed to implement routinely.
## Libraries 

##plyr, reshape2, lubridate, data.table libraries

###version streamlines data flow to make as automated script.
###This script will run on PC. 

###Step 1- Move data to processing directories....
###Data can be on Loggernet server in two directories.. these should be mirrored and identical

##C:\Users\richr\Dropbox (Smithsonian)\DOE Warming Logger Swap\Loggernet Data\Datadump
##C:\Campbellsci\LoggerNet\GCREW_Rawdata_Archive

#Directories

###file copy statements scripts
# raw_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/raw_dir/raw_2016"
# raw_dir_copy<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/raw_dir/raw_2016_archive"
# 
# raw_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/raw_dir/raw_2017"
# raw_dir_copy<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/raw_dir/raw_2017_archive"

#For filer function
source_dir<-"C:/Users/richr/Dropbox (Smithsonian)/DOE Warming Logger Swap/Loggernet Data/Datadump"
raw_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/raw_dir/raw_2018"
raw_dir_copy<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/raw_dir/raw_2018_archive"
Filer(source_dir, raw_dir, raw_dir_copy) ## to run step 1
  

### STEP 1 function to copy files from loggernet directorieo runs
Filer<- function (source_dir, raw_dir, raw_dir_copy){

dir.create(raw_dir_copy)
dir.create(raw_dir)
dir.create(source_dir)

  ### LIST FILES (RECURSIVE) AND COPY TO NEW DIRECTORY FOR ARCHIVING UNPROCESSED DATA
i<-list.files(source_dir, pattern = NULL, all.files = FALSE,
              full.names = TRUE, recursive = TRUE,
              ignore.case = FALSE)

file.copy(i, raw_dir) ### archinve manually
file.copy(i, raw_dir_copy) ### archinve manually
file.copy(i, raw_dir)}

### STEP 2 cHECK FILES FOR DATA AND LOGGERNET FORMAT
### Function DATACHECK
###removes short files less that 5 rows header is 4 lines in TOA5, and 2 lines in TOACI1)
###raw_dir is location of files and should be a copy of raw files; no additional files should be included
### removes no dat/backup file extensions as well

DATACHECK<-function(raw_dir) {
    i<-list.files(raw_dir, pattern = NULL, all.files = FALSE, 
                full.names = TRUE, recursive = TRUE, 
                ignore.case = FALSE) 
 
    
    j<-file_ext(i[1:length(i)])
    
    p<-c(1:length(i)) 
    for (n in p) {
    
      if(j[n]!= "dat" && j[n]!= "backup") file.remove(i[n])}
    
    
    i<-list.files(raw_dir, pattern = NULL, all.files = FALSE, 
                  full.names = TRUE, recursive = TRUE, 
                  ignore.case = FALSE) 
    
    p<-c(1:length(i)) 
    for (n in p) {
    p2<-read.table(i[n], sep = ",", quote = "\"'", 
                   dec = ".",
                   na.strings = "NA", colClasses = "character", nrows = 6, 
                   skip = 0, check.names = FALSE,fill=TRUE, 
                   strip.white = FALSE,blank.lines.skip = TRUE) 
    type<-p2[1,1]	
    p3<-nrow(p2) 
    if(p3<3 & type =="TOACI1") file.remove(i[n])
    if(p3<5 & type =="TOA5") file.remove(i[n])
     ##NEW 2016
    
  } 
   }
###To run DATACHECK (change for each year or file system)
library(tools)
DATACHECK(raw_dir)

##raw_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/raw_dir/raw_2016"
##raw_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/raw_dir/raw_2017"
raw_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/raw_dir/raw_2018"
raw_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/raw_dir/raw_2019"

##########################################################################

###PROCESS
###Convert TOA5 or TOACI1 to .csv files
###header change and rename file

#Incoming directory= raw_dir

#Outgoing directory=  proc_dir

#meta_file = record of processed files


PROCESS<-function(proc_dir,raw_dir,meta_file) { 
  
  dir.create(proc_dir)
  
    i<-list.files(raw_dir, pattern = NULL, all.files = FALSE, 
                full.names = TRUE, recursive = TRUE, 
                ignore.case = FALSE) 
  p<-c(1:length(i)) 
  for (n in p) {
    currentfile<-i[n] 
    dat<-read.table(i[n],sep = ",", quote = "\"", 
                    dec = ".", 
                    na.strings = "NA", colClasses = "character", nrows = 6, 
                    skip = 0, check.names = FALSE,fill=TRUE, 
                    strip.white = FALSE,blank.lines.skip = TRUE) 
    
    type<-dat[1,1]           
    
    
    if(type =="TOACI1") dat3<-read.table(i[n],, sep = ",", quote = "\"'",
                                         dec = ".",,,,
                                         na.strings = "NA", colClasses = "character", nrows = -1,
                                         skip = 2,,fill=TRUE,
                                         strip.white = FALSE,blank.lines.skip = TRUE,
                                         ,,,,,)
    
    if(type =="TOA5") dat3<-read.table(i[n],, sep = ",", quote = "\"'",
                                       dec = ".",,,,
                                       na.strings = "NA", colClasses = "character", nrows = -1,
                                       skip = 4,,fill=TRUE,
                                       strip.white = FALSE,blank.lines.skip = TRUE,
                                       ,,,,,)
    d<-ncol(dat3) 
    dat1<-dat[1,]
    dat2<-dat[2,1:d] 
    datnames<-c(dat2,"Logger", "Program", "Table") 
    newnames<-datnames 
    rm(dat) 
    if(type =="TOA5")dat4<-cbind(dat3,dat1[2],dat1[6],dat1[8]) 
    if(type =="TOACI1")dat4<-cbind(dat3,dat1[2],type,dat1[3])
    colnames(dat4)<-newnames 
    
    dat5<-substr(dat4[1,1],1,10) 
    dat6<-substr(dat4[1,1],12,20) 
    dat7<-chartr(":","-",dat6)
    ###create unique file name "d" by pulling data from process file in this order
    ###table type, date, time, block, nrows in dataframe, type of orginal file 
    ###any file name that could be overwritten would be duplicate file
    if(type =="TOA5")
      d<-c(paste(dat1[1,8],dat5,dat7,dat1[1,2],nrow(dat4),type,".csv",sep="_")) 
    if(type =="TOACI1")d<-c(paste(dat1[1,3],dat5,dat7,dat1[1,2],nrow(dat4),type,".csv",sep="_")) 
    
    d<-c(paste(proc_dir,"/", d ,sep="")) 
    
    dat8<-c(d,datnames,i[n]) 
    write.table(dat8,meta_file, append = TRUE, quote = FALSE, sep = ",", 
                ,na = "NA", dec = ".", row.names = FALSE, 
                col.names = FALSE, qmethod = c("escape", "double")) 
    write.table(dat4,d,append = FALSE, quote = FALSE, sep = ",", 
                ,na = "NA", dec = ".", row.names = FALSE,
                col.names = TRUE, qmethod = c("escape", "double"))
    file.remove(i[n])
    
    rm(dat1)
    rm(dat2)
    rm(dat3)
    rm(datnames)
    rm(dat4)
    rm(d)
    rm(dat6)
    rm(dat7)
    rm(dat8)
  }
}

###to run process function
library(lubridate)
library(data.table)

# #dir.create(proc_dir) as needed
# raw_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/raw_dir/raw_2016"
# proc_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2016"
# meta_file<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2016/2016_meta.txt"
# #proc_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed" # as needed
# 
# raw_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/raw_dir/raw_2017"
# proc_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2017"
# meta_file<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2017/2017_meta.txt"

raw_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/raw_dir/raw_2019"
proc_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2019"
#proc_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2018-2"  ## second half ofyear 2-12-2019
meta_file<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2019/2019_meta.txt"

PROCESS(proc_dir,raw_dir,meta_file)


### this function collects R-variables (no need to every year)

HEADERCHECK<-function(proc_dir_copy,header_file) {

    i<-list.files(proc_dir_copy, pattern = NULL, all.files = FALSE,
                full.names = TRUE, recursive = TRUE,
                ignore.case = FALSE) 
  dat3<-read.table(i[1],header=TRUE, sep = ",", quote = "\"'",
                   dec = ".",
                   na.strings = "NA", colClasses = "character", nrows = 2,
                   skip = 0, check.names = FALSE,fill=TRUE, 
                   strip.white = FALSE,blank.lines.skip = TRUE)
  
  names5<-names(dat3)
  p<-c(2:length(i))
  for (n in p) {
    dat<-read.table(i[n],header=TRUE, sep = ",", quote = "\"'",
                    dec = ".",
                    na.strings = "NA", colClasses = "character", nrows = 2,
                    skip = 0, check.names = FALSE,fill=TRUE,
                    strip.white = FALSE,blank.lines.skip = TRUE)
    names1<-names(dat)
    names<-c(names1,names5)
    names5<-names
    datnames<-(unique(names5))
  }
  datnames2<-t(datnames)
  write.table(datnames2,header_file,append = FALSE, quote = FALSE, sep = ",",
              , na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"))
  rm(datnames2)
  rm(datnames)
}



header_file<-"C://Users/richr/Dropbox (Smithsonian)/R_processing//loggernetcol2.txt"
HEADERCHECK(proc_dir_copy,header_file)

HEADERCHECK(proc_dir_copy,header_file)

proc_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2017"
dir.create(proc_dir)

##STEP 4 directory creation for processed subdirectories

proc_dir_copy<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2019_copy"
dir.create(proc_dir_copy)

proc_dir_copy<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2019_copy/export"
dir.create(proc_dir_copy)

proc_dir_copy<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2019_copy/control"
dir.create(proc_dir_copy)

proc_dir_copy<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2019_copy/ch4"
dir.create(proc_dir_copy)

proc_dir_copy<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2019_copy/check"
dir.create(proc_dir_copy)

proc_dir_copy<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2019_copy/expdc"
dir.create(proc_dir_copy)


##### Re-Formats raw data files from loggernet processing, and stacks (melts) the data. 
##### Creates a logger file with all logger level data, and creates a plot level file with all plot level data
##### Creates renormalized files for data file
###The two key files needed for this are:
##1 var_role: a list of variables in the CR1000 processed data,their common variable name, and whether they need to be stacked.
##2 design_table: this connects the experimental design to the cr1000 variable name and the scale link for data. It has every variable used in datalogger exept raw t_temp data which are store separatetly, and 60 minute data.
## An excel file called R-processing_keys.xlsx has sheets needed to add or change the structure of the data. The original source for this is the configuration file and the Sauron Program. After the 2012 switchover of to water removal, the program structure became ver complex to accomdate the change in experimental design. Variables may be coded by original (standard coding or by PPT manipulation coding). Caution must be made in properly ading new varables to check code and determine experimental structure of channels.

#var_role = linkage between Loggernet varaibles and scale of data 
#design_table = experimental design and link
#source_dir = directory of processed files
#output_dir = directory of renormalized files
#store_dir= stored files moved after renormalizing

library(reshape2)
library(data.table)
library(lubridate)

mean_rm<-function(x){mean(x, na.rm=TRUE)} ### used for aggregate function, allows mean data to deal with NA within apply functions

NormGCREW<-function(source_dir,design_table,var_role,output_dir,store_dir, increment){
  
  var<-fread(var_role)
  design<-fread(design_table)
  plotname<-fread(plot_names)
  increment= 15
  
  key2=c("logger","link")
  setkeyv(plotname,key2)
  setkeyv(design,key2)
  
  #creat keytables
  plotname<-lapply(plotname,tolower)
  plotname<-as.data.table(plotname)
  
  design<-lapply(design,tolower)
  design<-as.data.table(design)
  
  design2<-merge(plotname,design,allow.cartesian=TRUE,by=key2)
  
  
  var1<-var[var_type=="key",]
  key<-as.vector(var1$cr1000_name)
  var1<-var[var_type=="n",]
  nnn<-as.vector(var1$cr1000_name)
  var1<-var[var_type=="y",]
  ddd<-as.vector(var1$cr1000_name)
  
  i<-list.files(source_dir, pattern = NULL, all.files = FALSE,
                full.names = TRUE, recursive = TRUE,
                ignore.case = FALSE)
  j<-list.files(source_dir, pattern = NULL, all.files = FALSE,
                full.names = FALSE, recursive = TRUE,
                ignore.case = FALSE)
  
  n=1 #for testing
  
  p<-c(1:length(i))
  for (n in p)
    
  {dt<-fread(i[n])
  
  ###condition names
  setnames(dt, tolower(names(dt)))
  newnames<-names(dt)
  newnames<-gsub("[(]","",newnames)
  newnames<-gsub("[)]","",newnames)
  setnames(dt, newnames)
  
  ### lowercase data
  dt<-lapply(dt,tolower)
  dt<-as.data.table(dt)		
  
  if (is.na(dt$logger) == TRUE) dt[,logger:=statname1]
  
  ## create time2 and row_id
  Sys.setenv(TZ="America/Cancun") ### set for EST all year long
  dt<-subset(dt,grepl("20..-..-.. ..:..:..", dt$timestamp))
  dt$timestamp<-as.POSIXct(dt$timestamp)
  dt2<-round(minute(dt$timestamp)/increment)*increment
  dt$time2<-update(dt$timestamp, min=dt2)
  dt$rowid<-paste(dt$timestamp,dt$logger,sep="_")
  dt[,rowid:=paste("gcrew", rowid, sep="_")] # create rowid
  
  plot_names<-c(key,ddd)		
  site_names<-c(key,nnn)
  
  dtbase<-subset(dt,select=names(dt)%in%site_names) ##site data for merge
  dts<-subset(dt,select=names(dt)%in%plot_names) ##plot level to denormalize
  
  ###normalize data
  rm(dt2)
  
  ###stack block data
  key5<-c("logger","rowid","time2", "timestamp")
  dtb2<-melt(dtbase, id.vars = key5, measure.vars = , variable.name="cr1000_name",, na.rm = FALSE,)
  dtb2<-apply(dtb2,2,tolower)
  dtb3<-as.data.table(dtb2)
  rm(dtb2)
  
  ###stack plotdata
  key<-c("logger","rowid","time2")
  dts2<-melt(dts, id.vars = key, measure.vars = , variable.name="cr1000_name",, na.rm = FALSE,)
  dts2<-apply(dts2,2,tolower)
  dts3<-as.data.table(dts2)
  rm(dts2)
  
  key2=c("logger","cr1000_name")
  setkeyv(dts3,key2)
  setkeyv(design2,key2)
  key3<-c("rowid","time2", "logger", "plot_id", "value", "research_name", "cr1000_name", "type")
  key4<-c("rowid","time2", "logger", "plot_id")
  dts4<-merge(dts3,design2,allow.cartesian=TRUE,by=key2)
  dts5<-subset(dts4, select=key3)
  
  ###merge and recast data
  dts5<-subset(dts4, select=key3)
  dts5A<-dts5[type=="number",]
  dts5B<-dts5[type=="character",]
  
  setkey(dts5A,NULL)
  dts5A<-unique(dts5A)
  dts5A$value<-as.double(dts5A$value)
  is.na(dts5A)<-is.na(dts5A)
  dts6A<-dcast(dts5A,plot_id+rowid+logger+time2~ research_name, fun.aggregate = mean_rm ,subset = NULL, drop = TRUE, value.var = "value") # note that fill should be allowed to default
  dts6<-as.data.table(dts6A)
  
  ### fix for text data
  # if(nrow(dts5B)> 0){
  #   setkey(dts5B,NULL)
  #   dts5B<-unique(dts5B)
  #   is.na(dts5B)<-is.na(dts5B)
  #   dts6B<-dcast(dts5B,plot_id+rowid+logger+time2~ research_name, fun.aggregate = NULL ,subset = NULL, drop = TRUE, value.var = "value") # note that fill should be allowed to default
  #   dts6B<-as.data.table(dts6B)
  #   dts6<-dts6B[dts6]}
  # 
  key5<-c("rowid","logger","time2")
  
  dtbase<-unique(dtbase)# unique function in data.table uses key value as guide
  dtbase<-data.table(dtbase)
  
  dts6$time2<-as.POSIXct(dts6$time2)
  dtbase$time2<-as.POSIXct(dtbase$time2)
  setkeyv(dts6,key5)
  setkeyv(dtbase,key5)
  ### merge non normalized variables with data
  dts7<-dtbase[dts6]
  
  key6<-c("plot_id", "logger")
  setkeyv(dts7,key6)
  add<- c("site", "plot_id","logger", "plot_name", "treatment_name", "transect","warming_level","co2_level","treatment")
  plotname2<-as.data.table(unique(subset(plotname, select=add)))
  setkeyv(plotname2,key6)
  dts8<-plotname2[dts7]
  ###
  
  d<-c(paste("norm_",j[n]))
  d<-c(paste(output_dir,d,sep="/"))
  
  write.table(dts8,d, append = FALSE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"))
  
  
  b<-c(paste(store_dir,j[n],sep="/"))
  file.copy(i[n],b)
  file.remove(i[n])
  }}

##run seperately for each table type
 
design_table<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/design_role_GCREW2017.csv" 
var_role<- "C://Users/richr/Dropbox (Smithsonian)/R_processing/variable_role_GCREW2017.csv" 
plot_names<- "C://Users/richr/Dropbox (Smithsonian)/R_processing/plot_names_GCREW2017.csv"

source_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/processed/processed_2019_copy/export"
output_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/normal/export_2019"
store_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/normal/STORE/export_2019"
increment= 15

dir.create(source_dir)
dir.create(output_dir)
dir.create(store_dir)

NormGCREW(source_dir,design_table,var_role, output_dir, store_dir, increment)

source_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/1min"
output_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/normal/1min"

NormalGCREW(source_dir,design_table,var_role, output_dir, store_dir)


###function cleans duplicates and parses data files by month. Uses rbind.fill to keep all headers

library(plyr)
MONTHLYMANAGE<-function(source_dir,output_dir,tab){

  
    i<-list.files(source_dir, pattern = NULL, all.files = FALSE,
                full.names = TRUE, recursive = TRUE,
                ignore.case = FALSE)
    j<-list.files(source_dir, pattern = NULL, all.files = FALSE,
                full.names = FALSE, recursive = TRUE,
                ignore.case = FALSE)
    
    p<-c(1:length(i))
    for (n in p)
      
    {dt<-fread(i[n])
    
    dt$time2<-as.POSIXct(dt$time2)
  
  MO= unique(month(dt$time2))   
  YR<-unique(year(dt$time2))
  BK<-unique(tolower(dt$logger))
  Y1<-merge(YR,MO)
  names(Y1)<-c("YR","MO")
  YB<-merge(Y1,BK)
  names(YB)<-c("YR","MO", "BK")
  data.frame(YB)
  
  p<-c(1:nrow(YB))
  for (m in p) {
   
     dtplot<- subset(dt, month(dt$time2)==YB$MO[m] & dt$logger== YB$BK[m])
    d<-c(paste(YB$BK[m],tab,YB$MO[m],YB$YR[m],sep="_"))
    d<-c(paste(d,"csv",sep="."))
    d<-c(paste(output_dir,d,sep="/"))
  
      if (file.exists(d)==TRUE) {
      
      dt2<-fread(d)
      dt2<-rbind.fill(dtplot,dt2)
      dt2<-unique(dt2)
      dt2<-as.data.table(dt2)
      
      write.table(dt2,d, append = FALSE, quote = FALSE, sep = ",",
                  , na = "NA", dec = ".", row.names = FALSE,
                  col.names = TRUE, qmethod = c("escape", "double"))
      rm(dt2)}
    
      if (file.exists(d)==FALSE) {
      
      write.table(dtplot,d, append = FALSE, quote = FALSE, sep = ",",
                  , na = "NA", dec = ".", row.names = FALSE,
                  col.names = TRUE, qmethod = c("escape", "double"))
      }
  }}}

source_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/normal/export_2019" # 
##source_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/normal/export_combined"
output_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export_combined"

tab= "export"
MONTHLYMANAGE(source_dir,output_dir,tab)

###for getting rid
library(plyr)
library (lubridate)

MONTHLYMANAGE2<-function(source_dir,output_dir,tab){
  
  
  i<-list.files(source_dir, pattern = NULL, all.files = FALSE,
                full.names = TRUE, recursive = TRUE,
                ignore.case = FALSE)
  j<-list.files(source_dir, pattern = NULL, all.files = FALSE,
                full.names = FALSE, recursive = TRUE,
                ignore.case = FALSE)
  
  p<-c(1:length(i))
  for (n in p)
    
  {dt<-fread(i[n])
  
  dt$time2<-as.POSIXct(dt$time2)
  
  dt[,yday:=yday(time2)]
  dt[,week:=week(time2)]
  dt[,minute:=minute(time2)]
  dt[,hour:=hour(time2)]
  dt[,year:=year(time2)]
  dt[,month:=month(time2)]
  
  MO= unique(month(dt$time2))   
  YR<-unique(year(dt$time2))
  BK<-unique(tolower(dt$logger))
  Y1<-merge(YR,MO)
  names(Y1)<-c("YR","MO")
  YB<-merge(Y1,BK)
  names(YB)<-c("YR","MO", "BK")
  data.frame(YB)
  
  p<-c(1:nrow(YB))
  for (m in p) {
    
    dtplot<- subset(dt, month(dt$time2)==YB$MO[m] & dt$logger== YB$BK[m])
    d<-c(paste(YB$BK[m],tab,YB$MO[m],YB$YR[m],sep="_"))
    d<-c(paste(d,"csv",sep="."))
    d<-c(paste(output_dir,d,sep="/"))
    
    if (file.exists(d)==TRUE) {
      
      dt2<-fread(d)
      dt2<-rbind.fill(dtplot,dt2)
      dt2<-unique(dt2)
      dt2<-as.data.table(dt2)
      
      write.table(dt2,d, append = FALSE, quote = FALSE, sep = ",",
                  , na = "NA", dec = ".", row.names = FALSE,
                  col.names = TRUE, qmethod = c("escape", "double"))
      rm(dt2)}
    
    if (file.exists(d)==FALSE) {
      
      write.table(dtplot,d, append = FALSE, quote = FALSE, sep = ",",
                  , na = "NA", dec = ".", row.names = FALSE,
                  col.names = TRUE, qmethod = c("escape", "double"))
    }
  }}}

##source_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export-nd_combined/warmingv1end7-2018.csv"
source_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export_combined_2019/"
output_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export-nd_combined/"

tab= "export_nd"
MONTHLYMANAGE2(source_dir,output_dir,tab)



 
####
library (data.table)
library (lubridate)
library (zoo)
library(gtools)
library(reshape2)
library(ggplot2)
library(plyr)
library(gridExtra)

### Assemble data
export_load<-function(path,newpath)
{
  i<-list.files(path, pattern = NULL, all.files = FALSE,
                full.names = TRUE, recursive = TRUE,
                ignore.case = FALSE)
  
  dt1<-fread(i[1])
  
  p<-c(2:length(i))
  
  for (n in p)
    
  {dt2<-fread(i[n])
  
  dt2<-rbind.fill(dt1,dt2)
  dt1<-dt2
  dt3<-unique(dt2)
  }
  
  dt3<-as.data.table(dt3)
  
  if (file.exists(newpath)==TRUE){write.table(dt3,newpath, append = TRUE, quote = FALSE, sep = ",",
                                              , na = "NA", dec = ".", row.names = FALSE,
                                              col.names = FALSE, qmethod = c("escape", "double"))}
  
  if (file.exists(newpath)==FALSE){write.table(dt3,newpath, append = FALSE, quote = FALSE, sep = ",",
                                               , na = "NA", dec = ".", row.names = FALSE,
                                               col.names = TRUE, qmethod = c("escape", "double"))}
}




export_load(path,newpath)

newpath<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export-nd_combined/warmingv12019_2020.csv"
path<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export_combined_2019"

dir.create(path)
dir.create(newpath)

YEARLYMANAGE<-function(source_dir,output_dir,tab){
  
 # i<-list.files(source_dir, pattern = NULL, all.files = FALSE,
 #              full.names = TRUE, recursive = TRUE,
 #                ignore.case = FALSE)
 #  j<-list.files(source_dir, pattern = NULL, all.files = FALSE,
 #                full.names = FALSE, recursive = TRUE,
 #                ignore.case = FALSE)
  
  # p<-c(1:length(i))
  # for (n in p)
    
  {dt<-fread(source_dir)
  
  dt$time2<-as.POSIXct(dt$time2)
  
  #MO= unique(month(dt$time2))   
  YR<-unique(year(dt$time2))
  BK<-unique(tolower(dt$logger))
  YB<-merge(YR,BK)
  names(YB)<-c("YR","BK")
  data.frame(YB)
  
  p<-c(1:nrow(YB))
  for (m in p) {
    
    dtplot<- subset(dt, year(dt$time2)==YB$YR[m] & dt$logger== YB$BK[m])
    d<-c(paste(YB$BK[m],tab,YB$YR[m],sep="_"))
    d<-c(paste(d,"csv",sep="."))
    d<-c(paste(output_dir,d,sep="/"))
    
    if (file.exists(d)==TRUE) {
      
      dt2<-fread(d)
      dt2<-rbind.fill(dtplot,dt2)
      dt2<-unique(dt2)
      dt2<-as.data.table(dt2)
      
      write.table(dt2,d, append = FALSE, quote = FALSE, sep = ",",
                  , na = "NA", dec = ".", row.names = FALSE,
                  col.names = TRUE, qmethod = c("escape", "double"))
      rm(dt2)}
    
    if (file.exists(d)==FALSE) {
      
      write.table(dtplot,d, append = FALSE, quote = FALSE, sep = ",",
                  , na = "NA", dec = ".", row.names = FALSE,
                  col.names = TRUE, qmethod = c("escape", "double"))
    }
  }}}


source_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export-nd_combined/warmingv1end7-2018.csv"
source_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export-nd_combined/warmingv12019_2020.csv"


source_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export_combined_2019"
output_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export-nd_combined/"
tab= "export-nd"

YEARLYMANAGE(source_dir,output_dir,tab)







###parsing into files
###Creates Storage File For Data to be Bound
##store block
##store stack



###duplicate hunting

dup_removal<-function(source_dir, output_dir){
  k<-list.files(source_dir, pattern = NULL, all.files = FALSE,
                full.names = TRUE, recursive = TRUE,
                ignore.case = FALSE)
  l<-list.files(source_dir, pattern = NULL, all.files = FALSE,
                full.names = FALSE, recursive = TRUE,
                ignore.case = FALSE)
  p<-c(1:length(k))
  for (n in p) {
    dt3<-fread(k[n])
    dt3<-unique(dt3)
    c<-c(paste("nd", l[n], sep="_"))
    c<-c(paste(output_dir,c, sep="/"))
    write.table(dt3,c, append = FALSE, quote = FALSE, sep = ",",
                , na = "NA", dec = ".", row.names = FALSE,
                col.names = TRUE, qmethod = c("escape", "double"))
  }
}

###15min
source_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export_2017"
output_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export-nd_2017"

source_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export-nd_combined/yearly_data"
output_dir<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export-nd_combined/yearly_data_final"

dup_removal(source_dir, output_dir)
dir.create(output_dir)

##60min
no_dup_dir<-"/Volumes/disk7/b4warmed3/export/2014_60_stacked_nd" ###files outputted
output_dir<-"/Volumes/disk7/b4warmed3/export/2014_60_stacked" ###files outputted


Final_db<- function(input_dir, duplicate_dir, db_dir, increment_sec){
  
  i<-list.files(path= input_dir, pattern = NULL, all.files = FALSE,
                full.names = TRUE, recursive = TRUE,
                ignore.case = FALSE)
  
  j<-list.files(path= input_dir, pattern = NULL, all.files = FALSE,
                full.names = FALSE, recursive = TRUE,
                ignore.case = FALSE)
  
  p<-c(1:length(i))
  for (n in p)
  {dat<-fread(i[n])
  Sys.setenv(TZ="America/Regina")
  #Sys.setenv(TZ="America/North_Dakota/Center")
  
  
  #ring<-dat$ring[1]
  dat$timestamp<-as.POSIXct(dat$timestamp)
  dat$time2<-as.POSIXct(dat$time2)
  
  # Creates gapless time series of time-related variables with start and end times matching the original data file
  time2 = seq.POSIXt(min(dat$time2), max(dat$time2), by = increment_sec)
  TIME2ladder = data.table(time2)
  setkey(dat, time2)
  setkey(TIME2ladder,time2)
  dat2 <- merge(TIME2ladder, dat, all.x=TRUE, allow.cartesian=TRUE)
  
  #dat2$ring<-ring
  dat2$rowid<-paste(dat2$time2,"_",dat2$ring,sep="")
  dat2$rowid<-gsub(" ","_", dat2$rowid, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE)
  dat<-as.data.table(apply(dat2,2,tolower))
  key<-names(dat)
  #dat<-dat[,(names(title2))]
  #dat4<-dat[!duplicated(dat[-4]),]
  #dat3<-!duplicated(dat, dat$program)
  
  #remove duplicates based on program column
  #dat2<-aggregate((dat[,1])~dat[,1], data=dat, FUN=length)
  setkey(dat,rowid)
  dat2<-dat[,id:=length(timestamp), by="rowid"]
  #makes table of names and frequency of each
  dat_db<-dat2
  #dat3<- dat2[id>1,]
  #dat4<- dat2[id==1,]
  #makes subset of duplicates and non duplicates
  #dat_dup<-subset(dat3,select=key)
  #dat_db<-subset(dat4,select=key)
  
  #subset dat based on duplicate and nonduplicate
  rm(dat,dat2,dat3,dat4)
  
  e<-c(paste("db",j[n],sep="_"))
  e<-c(paste(db_dir,e,sep="/"))
  
  #d<-c(paste("dup",j[n],sep="_"))
  #d<-c(paste(duplicate_dir,d,sep="/"))
  
  write.table(dat_db,e, append = FALSE, quote = FALSE, sep = ",",
              , na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"))
  
  #write.table(dat_dup,d, append =FALSE, quote = FALSE, sep = ",",
  #           , na = "NA", dec = ".", row.names = FALSE,
  #         col.names = TRUE, qmethod = c("escape", "double"))
  
  #rm(dat_db,dat_dup)
  }}

### ROWID AND TIME LADDER
## for export min files
input_dir <-"/Users/rich0475/Desktop/BioconWarming/2013_data/flat/control"
db_dir <-"/Users/rich0475/Desktop/BioconWarming/2013_data/db/control"
duplicate_dir <-"/Users/rich0475/Desktop/BioconWarming/2013_data/dup/control"
increment_sec<-60

input_dir <-"/Users/rich0475/Desktop/BioconWarming/2013_data/flat/tdr"
db_dir <-"/Users/rich0475/Desktop/BioconWarming/2013_data/db/tdr"
duplicate_dir <-"/Users/rich0475/Desktop/BioconWarming/2013_data/dup/tdr"
increment_sec<-3600

input_dir <-"/Users/rich0475/Desktop/BioconWarming/2013_data/flat/export"
db_dir <-"/Users/rich0475/Desktop/BioconWarming/2013_data/db/export"
duplicate_dir <-"/Users/rich0475/Desktop/BioconWarming/2013_data/dup/export"
increment_sec<-900



