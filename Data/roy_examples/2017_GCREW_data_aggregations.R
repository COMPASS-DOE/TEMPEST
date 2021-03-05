
### GCREW AGGREGATIONS
### aggregation data

####
library (data.table)
library (lubridate)
library (zoo)
library(gtools)
library(reshape2)
library(ggplot2)
library(plyr)
library(gridExtra)

newpath<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export-nd/draftwarm2016v1.csv"
newpath<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export-nd_2017/warmingv1end9-2017.csv"

###split data by year
dt<-fread(newpath)


mean_rm<-function(x){mean(x, na.rm=TRUE)} ### used for aggregate function, allows mean data to deal with NA within apply functions
stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
merm<-function(x){ mean(x, na.rm=TRUE)}##create meantrim average



###Add columns
##Dates
dt$time2<-as.POSIXct(dt$time2)

## assign dates columns
dt[,yday:=yday(time2)]
dt[,week:=week(time2)]
dt[,minute:=minute(time2)]
dt[,hour:=hour(time2)]
dt[,year:=year(time2)]
dt[,month:=month(time2)]
names(dt)
# [1] "site"               "plot_id"            "logger"             "plot_name"          "treatment_name"    
# [6] "transect"           "warming_level"      "co2_level"          "treatment"          "timestamp"         
# [11] "record"             "tref_avg1"          "tref_avg2"          "tref_avg3"          "flag1"             
# [16] "flag2"              "flag3"              "flag4"              "flag5"              "flag6"             
# [21] "flag7"              "flag8"              "flag9"              "flag10"             "flag11"            
# [26] "flag12"             "flag13"             "flag14"             "flag15"             "flag16"            
# [31] "flag17"             "flag18"             "flag19"             "flag20"             "airtemp_avg"       
# [36] "rh_avg"             "vp_avg"             "vpd_avg"            "svp_avg"            "wind_speed_avg"    
# [41] "wind_direction_avg" "amb_aa_avg"         "amb_ext_avg"        "amb_aa_std"         "amb_ext_std"       
# [46] "ambx_avg"           "ambxb_avg1"         "ambxb_avg2"         "sbtemp"             "tmv"               
# [51] "targettemp_avg"     "amb_ba_avg"         "batt_check"         "program"            "table"             
# [56] "time2"              "rowid"              "a_ard_duty"         "a_duty"             "amb_b2"            
# [61] "amb_bet0"           "amb_bt0"            "amb_r"              "atarget"            "atemp_avg"         
# [66] "b_ard_duty"         "b_duty"             "btarget"            "dev_a_avg"          "dif_a_avg"         
# [71] "dif_b_avg"          "dif_p_avg"          "heat_a_avg"         "heat_a_std"         "lag_b_avg"         
# [76] "lag_r"              "lagtemp_avg"        "pin_b_avg"          "pin_bdev_avg"       "pin_r"             
# [81] "pintemp_avg"        "powerpro_above"     "powerpro_below"     "soil_check_avg"     "soil_eveness"      
# [86] "soil_message"       "amb_a_avg"          "amb_a_std"          "amb_b"              "b_dc"              
# [91] "b_targetdev"        "b_targetdif"        "dev_a"              "dev_a_error"        "dev_a_proj2"       
# [96] "dif_a"              "dif_b"              "dif_p"              "lag_b"              "pin_b"             
# [101] "proj_amb30"         "proj_difb"          "proj_lag30"         "proj_pin30"         "soil_check"        

#dt<-fread(newpath)

col_keep<- c("site","treatment_name","transect","warming_level","co2_level","treatment","flag1","year", "month")
col_agg<- c("dif_b_avg","pintemp_avg")

g<-unique(dt$treatment)
d<-c(col_keep,col_agg)

dg<-subset(dt, select=d)
dg<-dg[flag1==-1]
##for fewer months
dg<-dg[month>5]
dg<-dg[month<9]

#key1= c("amb_x_amb", "1.7_x_amb",  "3.4_x_amb", "5.1_x_amb")
key1= c("amb_x_elev", "amb_x_amb" , "1.7_x_amb" , "3.4_x_amb" , "5.1_x_amb",   "5.1_x_elev")


stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
merm<-function(x){ mean(x, na.rm=TRUE)}##create meantrim average

sub1<- c("site","treatment_name","warming_level","treatment", "year")

dg5<-dg[,lapply(.SD,mean_rm), by=sub1]
dg4<-dg[,lapply(.SD,stderr), by=sub1]

setnames(dg4,"dif_b_avg", "SE_Tbelowground_Delta")
setnames(dg4,"pintemp_avg", "SE_Tbelowground")
setnames(dg5,"pintemp_avg", "Tbelowground")
setnames(dg5,"dif_b_avg", "Tbelowground_Delta")

sub4<- c("site","treatment_name","warming_level","treatment")
setkeyv(dg5, sub4)
setkeyv(dg4, sub4)

dg6<-dg5[dg4]
dg6[,TAerrorU:=Tbelowground_Delta+1.96*SE_Tbelowground_Delta]
dg6[,TAerrorL:=Tbelowground_Delta-1.96*SE_Tbelowground_Delta]
dg6[,TBerrorU:=Tbelowground+1.96*SE_Tbelowground]
dg6[,TBerrorL:=Tbelowground-1.96*SE_Tbelowground]

fp1<-c("darkorchid2", "dodgerblue","seagreen3", "orange1", "firebrick1", "deeppink3")

# dg6$site_canopy<-gsub("_"," ",dg6$site_canopy)
# dg6$site_canopy<-gsub("cfc","CFC",dg6$site_canopy)
# dg6$site_canopy<-gsub("hwrc","HWRC",dg6$site_canopy)

dg6$warming_level <- gsub("amb","Ambient",dg6$warming_level)

dg6$warming_level<-factor(dg6$warming_level, levels = c("Ambient", "1.7", "3.4", "5.1"), ordered=TRUE)
dg6$treatment<-factor(dg6$treatment, levels = c("amb_x_elev", "amb_x_amb" , "1.7_x_amb" , "3.4_x_amb" , "5.1_x_amb",   "5.1_x_elev"), ordered=TRUE)


#dg6<-dg6[treatment!="amb_x_elev"]
#dg6<-dg6[treatment!="5.1_x_elev"]

dg7<-subset(dg6, dg6$year==2016)
#dg7<-subset(dg6)

##figure 2017 edited         
fig_bg<-ggplot(dg7, aes(site,Tbelowground, fill=treatment, colour=treatment, group=treatment,ymin= TBerrorL, ymax=TBerrorU,width=.5))+ ylab("20cm Temp(C)") + xlab ("Site")+geom_bar(stat="identity",position="dodge",alpha=0.50,lwd=.2)+ scale_fill_manual(values=fp1)+facet_wrap(~ year)+ scale_colour_manual(values=fp1)+ theme_bw()+geom_errorbar(stat="identity", position= "dodge", colour="black", lwd=.3) + theme(strip.background=element_blank())+ theme(axis.text.x = element_text(angle = 0, size = 8))+ theme(axis.text.y = element_text(angle = 0, size = 8))

png(file="alldata_20cm.png",width=3000, height=1600, res=600)
plot(fig_bg)
dev.off()

##Goodfigure Summer 20cm
fig_bg<-ggplot(dg7, aes(site,Tbelowground, fill=treatment, colour=treatment, group=treatment,ymin= TBerrorL, ymax=TBerrorU,width=.5))+ ylab("20cm Temp(C)") + xlab ("Site")+geom_bar(stat="identity",position="dodge",alpha=0.50,lwd=.2)+ scale_fill_manual(values=fp1)+facet_wrap(~ year)+ scale_colour_manual(values=fp1)+ theme_bw()+geom_errorbar(stat="identity", position= "dodge", colour="black", lwd=.3) + theme(strip.background=element_blank())+ theme(axis.text.x = element_text(angle = 0, size = 8))+ theme(axis.text.y = element_text(angle = 0, size = 8))
##+ theme(legend.position="none")

png(file="summerdata_20cm_.png",width=4000, height=1600, res=600)
plot(fig_bg)
dev.off()

##figure         
fig_bg<-ggplot(dg7, aes(site,Tbelowground, fill=warming_level, colour=warming_level, group=warming_level,ymin= TBerrorL, ymax=TBerrorU,width=.5))+ ylab("20cm Temp(C)") + xlab ("Site")+geom_bar(stat="identity",position="dodge",alpha=0.50,lwd=.2)+ scale_fill_manual(values=fp1)+ scale_colour_manual(values=fp1)+ theme_bw()+geom_errorbar(stat="identity", position= "dodge", colour="black", lwd=.3) + theme(strip.background=element_blank())+ theme(axis.text.x = element_text(angle = 0, size = 8))+ theme(axis.text.y = element_text(angle = 0, size = 8))

png(file="fig1.png",width=3000, height=1600, res=600)
plot(fig_bg)
dev.off()


dg8<-subset(dg6, year=2017)
##figure 5 aboveground         
fig_bg<-ggplot(dg8, aes(site,Tbelowground, fill=warming_level, colour=warming_level, group=warming_level,ymin= TBerrorL, ymax=TBerrorU,width=.5))+ ylab("20cm Temp(C)") + xlab ("Site")+geom_bar(stat="identity",position="dodge",alpha=0.50,lwd=.2)+ scale_fill_manual(values=fp1)+ scale_colour_manual(values=fp1)+ theme_bw()+geom_errorbar(stat="identity", position= "dodge", colour="black", lwd=.3) + theme(strip.background=element_blank())+ theme(axis.text.x = element_text(angle = 0, size = 8))+ theme(axis.text.y = element_text(angle = 0, size = 8))

png(file="fig3.png",width=3000, height=1600, res=600)
plot(fig_bg)
dev.off()

##Good figure summer delta
fig_bgd<-ggplot(dg7, aes(site,Tbelowground_Delta, fill=treatment, colour=treatment, group=treatment,ymin= TAerrorL, ymax=TAerrorU,width=.5))+ ylab("20cm Delta (C)") + xlab ("Site")+geom_bar(stat="identity",position="dodge",alpha=0.5,lwd=.2)+ scale_fill_manual(values=fp1)+facet_wrap(~ year)+scale_colour_manual(values=fp1)+ theme_bw()+geom_errorbar(stat="identity", position= "dodge", colour="black", lwd=.3) + theme(strip.background=element_blank())+ theme(axis.text.x = element_text(angle = 0, size = 8))+theme(axis.text.y = element_text(angle = 0, size = 8))+ theme(legend.position="none")


png(file="summer_20cm_delta-nolegen.png",width=2400, height=1000, res=600)
plot(fig_bgd)
dev.off()


##density plots 2017
dt2<-dt
#dt2<-subset(dt, logger=="c4log")
dt2<-subset(dt2, flag1<0)

dt2$warming_level <- gsub("amb","Ambient",dt2$warming_level)
dt2$warming_level<-factor(dt2$warming_level, levels = c("Ambient", "1.7", "3.4", "5.1"), ordered=TRUE)


den<-ggplot(dt2, aes(x=pintemp_avg, fill=warming_level)) + geom_density(alpha=.3) + scale_fill_manual(values=fp1)+ scale_colour_manual(values=fp1)+ ylab("Density") + xlab ("Temperature") +facet_wrap(~year+site)
png(file="densitytest.png",width=4800, height=4800, res=600)+ theme(legend.position="none")
plot(den)
dev.off()

###real
dt2<-dt
#dt2<-subset(dt, logger=="c4log")
dt2<-subset(dt2, flag1<0)
dt2<-subset(dt2, month<9)
dt2<-subset(dt2, month>6)

#dt2$warming_level <- gsub("amb","Ambient",dt2$warming_level)
#dt2$warming_level<-factor(dt2$warming_level, levels = c("Ambient", "1.7", "3.4", "5.1"), ordered=TRUE)

dt2$treatment<-factor(dt2$treatment, levels = c("amb_x_elev", "amb_x_amb" , "1.7_x_amb" , "3.4_x_amb" , "5.1_x_amb",   "5.1_x_elev"), ordered=TRUE)
dt2$year<-factor(dt2$year, levels = c("2016", "2017"), ordered=TRUE)
dt2$site<-factor(dt2$site, levels = c("c3", "c4"), ordered=TRUE)



den<-ggplot(dt2, aes(x=pintemp_avg, fill=treatment)) + geom_density(alpha=.3) + scale_fill_manual(values=fp1)+ scale_colour_manual(values=fp1)+ ylab("Density") + xlab ("Temperature") +facet_wrap(~year+site)
png(file="den_summer_20cm.png",width=4800, height=4800, res=600)+ theme(legend.position="none")
plot(den)
dev.off()

den<-ggplot(dt2, aes(x=atemp_avg, fill=treatment)) + geom_density(alpha=.3) + scale_fill_manual(values=fp1)+ scale_colour_manual(values=fp1)+ ylab("Density") + xlab ("Temperature") +facet_wrap(~year+site)
png(file="den_summer_above.png",width=4800, height=4800, res=600)+ theme(legend.position="none")
plot(den)
dev.off()

##air_temp
den<-ggplot(dt2, aes(x=airtemp_avg, fill=year)) + geom_density(alpha=.3) + scale_fill_manual(values=fp1)+ scale_colour_manual(values=fp1)+ ylab("Density") + xlab ("Temperature") +facet_wrap(~ site, nrow=2)
png(file="den_airtemp.png",width=4800, height=4800, res=600)+ theme(legend.position="none")
plot(den)
dev.off()



# Interleaved histograms
fig2<-ggplot(dg, aes(x=pintemp_avg, fill=treatment)) +
  geom_histogram(binwidth=.5, position="dodge")
png(file="f2.png",width=3600, height=2400, res=600)
plot(fig2)
dev.off()

##2017 line plots for trace

#dt2<-dt[,hour:=hour(time2)]

dt2<-dt[,doy:=yday(time2)]
dt2<-dt2[site=="c4"]

dt2<-data.table(dt)
merm<-function(x){ mean(x, na.rm=TRUE)}##create meantrim average
sub1<-c("treatment", "yday", "site", "year")

dt2<-dt2[,lapply(.SD,merm), by=sub1]
sub3<-c("treatment","time2", "yday", "year", "site")
#sub4<-c("Temp")


sub2<-c("pintemp_avg","atemp_avg", "dif_b_avg", "dif_a_avg")
dt4<-melt(dt2,id.vars=sub3,measure.vars=sub2, value.name="value", na.rm=TRUE)
dt4<-dt4[yday>153]
dt4<-dt4[yday<243]

#dt4$warming_level <- gsub("amb","Ambient",dt4$warming_level)
#dt4$warming_level<-factor(dt4$warming_level, levels = c("Ambient", "1.7", "3.4", "5.1"), ordered=TRUE)
dt4$treatment<-factor(dt4$treatment, levels = c("amb_x_elev", "amb_x_amb" , "1.7_x_amb" , "3.4_x_amb" , "5.1_x_amb",   "5.1_x_elev"), ordered=TRUE)
dt4$year<-factor(dt4$year, levels = c("2016", "2017"), ordered=TRUE)
dt4$site<-factor(dt4$site, levels = c("c3", "c4"), ordered=TRUE)


p<-ggplot(dt4, aes(x=yday,y=value, group=treatment,colour=treatment))+facet_wrap(~ site + year,nrow = 2, ncol = 2, scales = "fixed")
figline<-p +ylab ("Temperature (C)") + xlab("yday") + geom_line(data= subset(dt4, variable =="pintemp_avg"), stat="identity",lwd=.3,lty=2)  +scale_fill_manual(values=fp1)+ scale_colour_manual(values=fp1)+ theme_bw()+ 
  geom_line(data= subset(dt4, variable =="dif_b_avg"), stat="identity",lwd=.3,lty=2)+
  geom_line(data= subset(dt4, variable =="atemp_avg"), stat="identity",lwd=.3,lty=1)+
  geom_line(data= subset(dt4, variable =="dif_a_avg"), stat="identity",lwd=.3,lty=1)

theme(legend.position="none")+theme(strip.background=element_blank())

png(file="facetted_trace_summer.png",width=6000, height=4800, res=600)
plot(figline)
dev.off()


library(lubridate)

#dt2<-dt[,hour:=hour(time2)]
dt2<-dt[,doy:=yday(time2)]
dt2<-dt2[site=="c4"]

dt2<-data.table(dt2)
merm<-function(x){ mean(x, na.rm=TRUE)}##create meantrim average
sub1<-c("warming_level", "doy")

dt2<-dt2[,lapply(.SD,merm), by=sub1]
sub3<-c("warming_level","time2", "doy")
#sub4<-c("Temp")


sub2<-c("pintemp_avg","atemp_avg", "dif_b_avg", "dif_a_avg")
dt4<-melt(dt2,id.vars=sub3,measure.vars=sub2, value.name="value", na.rm=TRUE)
dt4<-dt4[doy>200]
dt4<-dt4[doy<280]

dt4$warming_level <- gsub("amb","Ambient",dt4$warming_level)
dt4$warming_level<-factor(dt4$warming_level, levels = c("Ambient", "1.7", "3.4", "5.1"), ordered=TRUE)


#dg5$Treatment <- factor(dg5$Treatment,
                        levels = c("amb", "+1.7", "+3.4"))
#dg5<-subset(dg5, year<2012)
p<-ggplot(dt4, aes(x=doy,y=value, group=warming_level,colour=warming_level))

figline<-p +ylab ("Temperature (C)") + xlab("doy") + geom_line(data= subset(dt4, variable =="pintemp_avg"), stat="identity",lwd=.3,lty=2)  +scale_fill_manual(values=fp1)+ scale_colour_manual(values=fp1)+ theme_bw()+ 
  geom_line(data= subset(dt4, variable =="dif_b_avg"), stat="identity",lwd=.3,lty=2)+
  geom_line(data= subset(dt4, variable =="atemp_avg"), stat="identity",lwd=.3,lty=1)+
  geom_line(data= subset(dt4, variable =="dif_a_avg"), stat="identity",lwd=.3,lty=1)

  theme(legend.position="none")+theme(strip.background=element_blank())

png(file="L1.png",width=4800, height=1200, res=600)
plot(figline)
dev.off()



col_keep<- c("site","flag1","year", "month")
col_agg<- c("pintemp_avg")

g<-unique(dt$treatment)
d<-c(col_keep,col_agg)

dg<-subset(dt, select=d)
dg<-dg[flag1==-1]
##for fewer months
dg<-dg[month>5]
dg<-dg[month<9]

#key1= c("amb_x_amb", "1.7_x_amb",  "3.4_x_amb", "5.1_x_amb")
key1= c("amb_x_elev", "amb_x_amb" , "1.7_x_amb" , "3.4_x_amb" , "5.1_x_amb",   "5.1_x_elev")


stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
mean_rm<-function(x){ mean(x, na.rm=TRUE)}##create meantrim average

sub1<- c("site", "year")

dg5<-dg[,lapply(.SD,mean_rm), by=sub1]
dg4<-dg[,lapply(.SD,stderr), by=sub1]

setnames(dg4,"dif_b_avg", "SE_Tbelowground_Delta")
setnames(dg4,"pintemp_avg", "SE_Tbelowground")
setnames(dg5,"pintemp_avg", "Tbelowground")
setnames(dg5,"dif_b_avg", "Tbelowground_Delta")

sub4<- c("site", "year" )
setkeyv(dg5, sub4)
setkeyv(dg4, sub4)

dg6<-dg5[dg4]
dg6[,TAerrorU:=Tbelowground_Delta+1.96*SE_Tbelowground_Delta]
dg6[,TAerrorL:=Tbelowground_Delta-1.96*SE_Tbelowground_Delta]
dg6[,TBerrorU:=Tbelowground+1.96*SE_Tbelowground]
dg6[,TBerrorL:=Tbelowground-1.96*SE_Tbelowground]


####Air temperature adjustment
names(dt)
dt[,revised_voltage_avg:=((airtemp_avg+20)/0.0833333)] 
dt[,revised_airtemp_avg:= ((revised_voltage_avg*.12)-40)]

dt$revised_airtemp_avg


col_keep<- c("site","treatment_name","transect","warming_level","co2_level","treatment","flag1","year", "month")
col_agg<- c("dif_b_avg", "dif_a_avg", "atemp_avg","pintemp_avg", "revised_airtemp_avg")

g<-unique(dt$treatment)
d<-c(col_keep,col_agg)

dg<-subset(dt, select=d)
dg<-dg[flag1==-1]
##for fewer months
dg<-dg[month>4]
dg<-dg[month<11]

#key1= c("amb_x_amb", "1.7_x_amb",  "3.4_x_amb", "5.1_x_amb")
key1= c("amb_x_elev", "amb_x_amb" , "1.7_x_amb" , "3.4_x_amb" , "5.1_x_amb",   "5.1_x_elev")


stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
mean_rm<-function(x){ mean(x, na.rm=TRUE)}##create meantrim average

sub1<- c("site","treatment_name","warming_level","treatment", "year", "month")


dg5<-dg[,lapply(.SD,mean_rm), by=sub1]
dg4<-dg[,lapply(.SD,stderr), by=sub1]

setnames(dg4,"dif_b_avg", "SE_Tbelowground_Delta")
setnames(dg4,"pintemp_avg", "SE_Tbelowground")
setnames(dg5,"pintemp_avg", "Tbelowground")
setnames(dg5,"dif_b_avg", "Tbelowground_Delta")

setnames(dg4,"dif_a_avg", "SE_Taboveground_Delta")
setnames(dg4,"atemp_avg", "SE_Taboveground")
setnames(dg5,"atemp_avg", "Taboveground")
setnames(dg5,"dif_a_avg", "Taboveground_Delta")

setnames(dg4,"revised_airtemp_avg", "SE_airtemp")
setnames(dg5,"revised_airtemp_avg", "Revised_airtemp")

setkeyv(dg4,sub1)
setkeyv(dg5,sub1)

dg2<-dg4[dg5]

d<-"C://Users/richr/Dropbox (Smithsonian)/R_processing/monthly/export-nd_2017/CNbymonth.csv"

write.table(dg2,d,append = FALSE, quote = FALSE, sep = ",", 
            ,na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))




##2018 line plots for trace DOE, smartx2 proposal

#dt2<-dt[,hour:=hour(time2)]

dt2<-dt[,doy:=yday(time2)]
dt2<-dt2[site=="c4"]

dt2<-data.table(dt)
merm<-function(x){ mean(x, na.rm=TRUE)}##create meantrim average
sub1<-c("treatment", "yday", "site", "year")

dt2<-dt2[,lapply(.SD,merm), by=sub1]
sub3<-c("treatment","time2", "yday", "year", "site")
#sub4<-c("Temp")


sub2<-c("pintemp_avg","atemp_avg", "dif_b_avg", "dif_a_avg")
dt4<-melt(dt2,id.vars=sub3,measure.vars=sub2, value.name="value", na.rm=TRUE)
dt4<-dt4[yday>153]
dt4<-dt4[yday<243]

dt4<-dt4[yday>100]
dt4<-dt4[yday<250]


#dt4$warming_level <- gsub("amb","Ambient",dt4$warming_level)
#dt4$warming_level<-factor(dt4$warming_level, levels = c("Ambient", "1.7", "3.4", "5.1"), ordered=TRUE)
dt4$treatment<-factor(dt4$treatment, levels = c("amb_x_elev", "amb_x_amb" , "1.7_x_amb" , "3.4_x_amb" , "5.1_x_amb",   "5.1_x_elev"), ordered=TRUE)
dt4$year<-factor(dt4$year, levels = c("2016", "2017"), ordered=TRUE)
dt4$site<-factor(dt4$site, levels = c("c3", "c4"), ordered=TRUE)


p<-ggplot(dt4, aes(x=yday,y=value, group=treatment,colour=treatment))+facet_wrap(~ site + year,nrow = 2, ncol = 2, scales = "fixed")+scale_y_continuous(limits = c(15, 40))
figline<-p +ylab ("Temperature (C)") + xlab("yday") + geom_line(data= subset(dt4, variable =="pintemp_avg"), stat="identity",lwd=.3,lty=2)  +scale_fill_manual(values=fp1)+ scale_colour_manual(values=fp1)+ theme_bw()+ 
  geom_line(data= subset(dt4, variable =="dif_b_avg"), stat="identity",lwd=.3,lty=2)+
  geom_line(data= subset(dt4, variable =="atemp_avg"), stat="identity",lwd=.3,lty=1)+
  geom_line(data= subset(dt4, variable =="dif_a_avg"), stat="identity",lwd=.3,lty=1)

theme(legend.position="none")+theme(strip.background=element_blank())

png(file="facetted_trace_summer_SMARTX.png",width=6000, height=4800, res=600)
plot(figline)
dev.off()

###alternate single picture

dt3<-dt4[site=="c3"]
dt3<-dt3[year=="2017"]
dt3<-dt3[treatment!="5.1_x_elev"]
dt3<-dt3[treatment!="amb_x_elev"]

fp1<-c( "dodgerblue","seagreen3", "orange1", "firebrick1")


g<-ggplot(dt3, aes(x=yday,y=value, group=treatment,colour=treatment))+(scale_y_continuous(limits = c(16, 36)))
figline<-g +ylab ("Temperature (C)") + xlab("yday") + geom_line(data= subset(dt3, variable =="pintemp_avg"), stat="identity",lwd=.7,lty=2)  +scale_fill_manual(values=fp1)+ scale_colour_manual(values=fp1)+ theme_bw()+ 
  geom_line(data= subset(dt3, variable =="dif_b_avg"), stat="identity",lwd=.7,lty=2)+
  geom_line(data= subset(dt3, variable =="atemp_avg"), stat="identity",lwd=.7,lty=1)+
  geom_line(data= subset(dt3, variable =="dif_a_avg"), stat="identity",lwd=.7,lty=1)

theme(legend.position="none")+theme(strip.background=element_blank())

png(file="c3_trace_SMARTX.png",width=6000, height=3600, res=600)
plot(figline)
dev.off()


g<-ggplot(dt3, aes(x=yday,y=value, group=treatment,colour=treatment))+(scale_y_continuous(limits = c(10, 36)))
figline<-g +ylab ("Temperature (C)") + xlab("Day of Year") + geom_line(data= subset(dt3, variable =="pintemp_avg"), stat="identity",lwd=.7,lty=2)  +scale_fill_manual(values=fp1)+ scale_colour_manual(values=fp1)+ theme_bw()+
  geom_line(data= subset(dt3, variable =="dif_b_avg"), stat="identity",lwd=.7,lty=2)+
  geom_line(data= subset(dt3, variable =="atemp_avg"), stat="identity",lwd=.7,lty=1)+
  geom_line(data= subset(dt3, variable =="dif_a_avg"), stat="identity",lwd=.7,lty=1)+
  theme(legend.position="none")+theme(strip.background=element_blank())+
  theme(text = element_text(size = 22), axis.title = element_text(size =16))


png(file="c3_trace_SMARTXv3.png",width=5000, height=1600, res=600)
plot(figline)
dev.off()

