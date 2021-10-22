library(doParallel)
registerDoParallel(cores=2)
library(caret)
library(glmnet)
library(tseries)
library(tibble)
library(lattice)
library(RColorBrewer)
library(rgdal)
library(cruts)
library(raster)
library(plyr)
library(dplyr)
library(ggpubr)
library(ncf)
setwd("~/data")

#all_delays<-read.csv("all_regions_delays_all_inf.csv", stringsAsFactors = F)
  
# read in all region delay data, calculate summary measures
all_delays<-read.csv("all_regions_delays_all_inf.csv", stringsAsFactors = F)%>%
  mutate(DateSmearEXAMINED=as.Date(DateSmearEXAMINED, format="%m/%d/%y"))%>%
  mutate(DateSmearEXAMINED=as.POSIXct(strptime(DateSmearEXAMINED,format='%Y-%m-%d')))%>%
  mutate(YrRecorded=format(as.Date(DateSmearEXAMINED,format='%Y-%m-%d'),'%Y'))%>%
  filter(YrRecorded!=2020)
  
  
# count # of cases with implausible date ranges
all_NA<-all_delays[all_delays$Delays=="#NUM!",]
num_implausible<-nrow(all_NA)
percent_implausible<-(num_implausible/nrow(all_delays))*100

# compute median delays
## convert delays to numeric var
all_delays$Delays<-as.numeric(all_delays$Delays)
all_median_delays<-median(as.numeric(all_delays$Delays),na.rm=TRUE)
all_sd_delays<-sd(as.numeric(all_delays$Delays),na.rm=TRUE)
all_delays$Region<-as.factor(all_delays$Region)
all_delays$ID<-rep(1,nrow(all_delays))

# Figure 1a
munic <- getData("GADM", country='GUY',level=1)
munic$NAME_1 <- as.factor(as.character(munic$NAME_1)) 
munic$CC_1 <- as.factor(as.character(munic$CC_1)) 
# spplot(munic,"NAME_1",col.regions = c("#F8766D","#00BF7D","#A3A500",
#                                       "darkgray","darkgray",
#                                       "darkgray","darkgray","#00B0F6",
#                                       "darkgray","#E76BF3"))

spplot(munic,"CC_1",col.regions = c("#F8766D","darkgray","darkgray","darkgray",
                                    "#A3A500","darkgray","darkgray",
                                    "#00BF7D","#00B0F6","#E76BF3"))

# Figure 1b
all_reg_within_one_month <- all_delays %>% 
  subset(Delays<=30) %>%
  mutate(DateSmearEXAMINED=as.Date(DateSmearEXAMINED, format="%m/%d/%y"))%>%
  mutate(IDKEY1=as.Date(IDKEY1, format="%m/%d/%y"))%>%
  mutate(DateSmearEXAMINED=as.POSIXct(strptime(DateSmearEXAMINED,format='%Y-%m-%d')))%>%
  mutate(IDKEY1=as.POSIXct(strptime(IDKEY1,format='%Y-%m-%d')))%>%
  mutate(YrKnown=format(as.Date(IDKEY1,format='%Y-%m-%d'),'%Y'))%>%
  mutate(YrRecorded=format(as.Date(DateSmearEXAMINED,format='%Y-%m-%d'),'%Y'))%>%
  dplyr::select(Region,YrRecorded,ID)%>%
  dplyr::group_by(Region,YrRecorded)%>%
  dplyr::summarise(n=sum(ID))%>%
  filter(Region%in%c(1,4,7,8,9))

all_reg_all_yrs <- all_delays %>% 
  mutate(DateSmearEXAMINED=as.Date(DateSmearEXAMINED, format="%m/%d/%y"))%>%
  mutate(IDKEY1=as.Date(IDKEY1, format="%m/%d/%y"))%>%
  mutate(DateSmearEXAMINED=as.POSIXct(strptime(DateSmearEXAMINED,format='%Y-%m-%d')))%>%
  mutate(IDKEY1=as.POSIXct(strptime(IDKEY1,format='%Y-%m-%d')))%>%
  mutate(YrKnown=format(as.Date(IDKEY1,format='%Y-%m-%d'),'%Y'))%>%
  mutate(YrRecorded=format(as.Date(DateSmearEXAMINED,format='%Y-%m-%d'),'%Y'))%>%
  dplyr::select(Region,YrRecorded,ID)%>%
  dplyr::group_by(Region,YrRecorded)%>%
  dplyr::summarise(all_n=sum(ID))%>%
  filter(Region%in%c(1,4,7,8,9))

all_reg_both <- merge(all_reg_within_one_month,all_reg_all_yrs,
                      by=c("Region","YrRecorded"))  #cbind(all_reg_within_one_month,all_reg_all_yrs)
all_reg_both$prop <- all_reg_both$n/all_reg_both$all_n
all_reg_both$Region<-as.factor(all_reg_both$Region)

ggplot(all_reg_both,aes(x=Region,y=prop,fill=Region))+geom_boxplot()+
  xlab("Region")+
  ylab("Proportion of cases reported by the end of the month")+
  #ggtitle("Proportion of cases reported within a month, by region")+
  theme(axis.text.x = element_text(size = 10),
        panel.background = element_blank(),axis.line=element_line(color="black"))

ggsave("~/DescriptiveOutput/Figure1b.png")
## summary statistics
quantile(all_reg_both$prop[which(all_reg_both$Region=='1')])
quantile(all_reg_both$prop[which(all_reg_both$Region=='4')])
quantile(all_reg_both$prop[which(all_reg_both$Region=='7')])
quantile(all_reg_both$prop[which(all_reg_both$Region=='8')])
quantile(all_reg_both$prop[which(all_reg_both$Region=='9')])

medians<-aggregate(prop~Region,all_reg_both,median)

# extract annual cases for each region
annual_cases=read.csv("all_regions_delays_all_inf.csv", stringsAsFactors = F)%>%
  mutate(DateSmearEXAMINED=as.Date(DateSmearEXAMINED, format="%m/%d/%y"))%>%
  mutate(IDKEY1=as.Date(IDKEY1, format="%m/%d/%y"))%>%
  mutate(DateSmearEXAMINED=as.POSIXct(strptime(DateSmearEXAMINED,format='%Y-%m-%d')))%>%
  mutate(IDKEY1=as.POSIXct(strptime(IDKEY1,format='%Y-%m-%d')))%>%
  mutate(YrKnown=format(as.Date(IDKEY1,format='%Y-%m-%d'),'%Y'))%>%
  mutate(YrRecorded=format(as.Date(DateSmearEXAMINED,format='%Y-%m-%d'),'%Y'))%>%
  dplyr::select(Region,YrRecorded,ID)%>%
  dplyr::group_by(Region,YrRecorded)%>%
  dplyr::summarise(n=n())%>%
  filter(Region%in%c(1,4,7,8,9))%>%
  filter(YrRecorded!=2020)%>%
  mutate(Region=as.factor(Region))

annual_cases_plot<-ggplot(annual_cases,aes(YrRecorded,n,fill=Region,group=Region))+geom_col(col='white')+
  ylab("Number of reported cases")+
  geom_text(
    aes(label=n, vjust=-0.5),size=1.5)+theme_classic()+theme(axis.text.x=element_text(angle=45,size=6))

annual_cases_plot+facet_wrap(~Region)

ggsave("~/DescriptiveOutput/annual_cases.png")
  
annual_cases_no_NA=read.csv("all_regions_delays_all_inf.csv", stringsAsFactors = F)%>%
  filter(Delays!='#NUM!')%>%
  mutate(DateSmearEXAMINED=as.Date(DateSmearEXAMINED, format="%m/%d/%y"))%>%
  mutate(IDKEY1=as.Date(IDKEY1, format="%m/%d/%y"))%>%
  mutate(DateSmearEXAMINED=as.POSIXct(strptime(DateSmearEXAMINED,format='%Y-%m-%d')))%>%
  mutate(IDKEY1=as.POSIXct(strptime(IDKEY1,format='%Y-%m-%d')))%>%
  mutate(YrKnown=format(as.Date(IDKEY1,format='%Y-%m-%d'),'%Y'))%>%
  mutate(YrRecorded=format(as.Date(DateSmearEXAMINED,format='%Y-%m-%d'),'%Y'))%>%
  dplyr::select(Region,YrRecorded,ID)%>%
  dplyr::group_by(Region,YrRecorded)%>%
  dplyr::summarise(n=n())%>%
  filter(Region%in%c(1,4,7,8,9))%>%
  filter(YrRecorded!=2020)%>%
  mutate(Region=as.factor(Region))

annual_cases_no_NA_plot<-ggplot(annual_cases_no_NA,
                                aes(YrRecorded,n,fill=Region,group=Region))+geom_col(col='white')+
  ylab("Number of reported cases")+
  geom_text(
    aes(label=n, vjust=-0.5),size=1.5
  )+theme_classic()+theme(axis.text.x=element_text(angle=45,size=6))

annual_cases_no_NA_plot+facet_wrap(~Region)

ggsave("~/DescriptiveOutput/annual_cases_no_na.png")

# extract median delays for each region, over the study period
median.delays_reg<-all_delays %>% 
  group_by(Region)%>%
  filter(Region%in%c(1,4,7,8,9))%>%
  filter(YrRecorded!=2020)%>%
  summarise(median_delays=median(as.numeric(Delays),na.rm=TRUE))
  

# Fig S1:  density plot of delays by region

reg1_hist <- all_delays %>%
  filter(YrRecorded!=2020)%>%
  filter(Region==1)%>%
  ggplot(aes(x=as.numeric(Delays),y=..density..))+
  geom_histogram(colour="darkgreen", fill="lightyellow")+xlab("Delays (days)")+xlim(0,300)+
  theme_classic()

reg4_hist <- all_delays %>%
  filter(YrRecorded!=2020)%>%
  filter(Region==4)%>%
  ggplot(aes(x=as.numeric(Delays),y=..density..))+
  geom_histogram(colour="darkgreen", fill="lightyellow")+xlab("Delays (days)")+xlim(0,300)+
  theme_classic()

reg7_hist <- all_delays %>%
  filter(YrRecorded!=2020)%>%
  filter(Region==7)%>%
  ggplot(aes(x=as.numeric(Delays),y=..density..))+
  geom_histogram(colour="darkgreen", fill="lightyellow")+xlab("Delays (days)")+xlim(0,300)+
  theme_classic()

reg8_hist <- all_delays %>%
  filter(YrRecorded!=2020)%>%
  subset(Region==8)%>%
  ggplot(aes(x=as.numeric(Delays),y=..density..))+
  geom_histogram(colour="darkgreen", fill="lightyellow")+xlab("Delays (days)")+xlim(0,300)+
  theme_classic()

reg9_hist <- all_delays %>%
  filter(YrRecorded!=2020)%>%
  subset(Region==9)%>%
  ggplot(aes(x=as.numeric(Delays),y=..density..))+
  geom_histogram(colour="darkgreen", fill="lightyellow")+xlab("Delays (days)")+xlim(0,300)+
  theme_classic()

ggarrange(reg1_hist,reg4_hist,reg7_hist,reg8_hist,reg9_hist,
          labels = c("Region 1", "Region 4", "Region 7", "Region 8", "Region 9"),
          ncol = 2, nrow = 3, label.x=0.4, font.label = list(size = 9))+
  theme_transparent(base_size = 12, base_family = "")

ggsave("~/DescriptiveOutput/all_delays_histogram.png")

# extract delay data by region at the monthly level 
reg1_month <- all_delays%>%
  subset(Region==1) %>%
  mutate(mo_yr_examined=format(as.Date(DateSmearEXAMINED,format='%m/%d/%y'),format='%m/%y'))%>%
  filter(YrRecorded!=2020)%>%
  filter(Delays!='#NUM!')%>%
  dplyr::mutate(delays=as.numeric(Delays))%>%
  mutate(delays=as.numeric(Delays))%>%
  dplyr::group_by(mo_yr_examined)%>%
  dplyr::summarise(total_delays=median(delays))

reg4_month <- all_delays%>%
  subset(Region==4) %>%
  mutate(mo_yr_examined=format(as.Date(DateSmearEXAMINED,format='%m/%d/%y'),format='%m/%y'))%>%
  filter(YrRecorded!=2020)%>%
  filter(Delays!='#NUM!')%>%
  dplyr::mutate(delays=as.numeric(Delays))%>%
  mutate(delays=as.numeric(Delays))%>%
  dplyr::group_by(mo_yr_examined)%>%
  dplyr::summarise(total_delays=median(delays))

reg7_month <- all_delays%>%
  subset(Region==7) %>%
  mutate(mo_yr_examined=format(as.Date(DateSmearEXAMINED,format='%m/%d/%y'),format='%m/%y'))%>%
  filter(YrRecorded!=2020)%>%
  filter(Delays!='#NUM!')%>%
  dplyr::mutate(delays=as.numeric(Delays))%>%
  mutate(delays=as.numeric(Delays))%>%
  dplyr::group_by(mo_yr_examined)%>%
  dplyr::summarise(total_delays=median(delays))

reg8_month <- all_delays%>%
  subset(Region==8) %>%
  mutate(mo_yr_examined=format(as.Date(DateSmearEXAMINED,format='%m/%d/%y'),format='%m/%y'))%>%
  filter(YrRecorded!=2020)%>%
  filter(Delays!='#NUM!')%>%
  dplyr::mutate(delays=as.numeric(Delays))%>%
  mutate(delays=as.numeric(Delays))%>%
  dplyr::group_by(mo_yr_examined)%>%
  dplyr::summarise(total_delays=median(delays))

reg9_month <- all_delays%>%
  subset(Region==9) %>%
  mutate(mo_yr_examined=format(as.Date(DateSmearEXAMINED,format='%m/%d/%y'),format='%m/%y'))%>%
  filter(YrRecorded!=2020)%>%
  filter(Delays!='#NUM!')%>%
  dplyr::mutate(delays=as.numeric(Delays))%>%
  mutate(delays=as.numeric(Delays))%>%
  dplyr::group_by(mo_yr_examined)%>%
  dplyr::summarise(total_delays=median(delays))

colnames(reg1_month)[2]<-"total_delays_reg_1"
colnames(reg4_month)[2]<-"total_delays_reg_4"
colnames(reg7_month)[2]<-"total_delays_reg_7"
colnames(reg8_month)[2]<-"total_delays_reg_8"
colnames(reg9_month)[2]<-"total_delays_reg_9"

## assess correlations between rainfall and median delays (2006-2019)
# read in rainfall data - adapted from code provided by Ayesha Mahmud 
range= c( "2006-01-01","2019-12-31")
munic <- getData("GADM", country="GUY", level=1)
ncfile = paste("cru_ts4.04.1901.2019.pre",".dat.nc", sep = "") 
pre <- cruts2poly(ncfile, munic, timeRange = range , na.rm = TRUE)
pre@data$code_long = as.character(unique(munic$GID_1))
pre <- as.data.frame(pre@data)

# subset to region1
pre_reg1<-t(pre[1,-length(pre)])

# combine with monthly delays data
pre_reg1<-cbind.data.frame(seq(as.Date("2006/1/1"), as.Date("2019/12/31"), by = "month"),pre_reg1)
colnames(pre_reg1)<-c("mo_yr_examined","pre")
pre_reg1$mo_yr_examined<-format(pre_reg1$mo_yr_examined,'%m/%y')
reg1_month_pre<-merge(pre_reg1,reg1_month,by="mo_yr_examined")

# compute correlation + significance
cor.test(reg1_month_pre$pre,reg1_month_pre$total_delays_reg_1)
ccf_reg1=ccf(reg1_month_pre$pre,reg1_month$total_delays_reg_1)

# subset to region4
pre_reg4<-t(pre[4,-length(pre)])
# combine with monthly delays data
pre_reg4<-cbind.data.frame(seq(as.Date("2006/1/1"), as.Date("2019/12/31"), by = "month"),pre_reg4)
colnames(pre_reg4)<-c("mo_yr_examined","pre")
pre_reg4$mo_yr_examined<-format(pre_reg4$mo_yr_examined,'%m/%y')
reg4_month_pre<-merge(pre_reg4,reg4_month,by="mo_yr_examined")

# compute correlation + significance
cor.test(reg4_month_pre$pre,reg4_month_pre$total_delays_reg_4)
ccf_reg4=ccf(reg4_month_pre$pre,reg4_month$total_delays_reg_4)

# subset to region7
pre_reg7<-t(pre[7,-length(pre)])
# combine with monthly delays data
pre_reg7<-cbind.data.frame(seq(as.Date("2006/1/1"), as.Date("2019/12/31"), by = "month"),pre_reg7)
colnames(pre_reg7)<-c("mo_yr_examined","pre")
pre_reg7$mo_yr_examined<-format(pre_reg7$mo_yr_examined,'%m/%y')
reg7_month_pre<-merge(pre_reg7,reg7_month,by="mo_yr_examined")

# compute correlation + significance
cor.test(reg7_month_pre$pre,reg7_month_pre$total_delays_reg_7)
ccf_reg7=ccf(reg7_month_pre$pre,reg7_month$total_delays_reg_7)

# subset to region8
pre_reg8<-t(pre[8,-length(pre)])
# combine with monthly delays data
pre_reg8<-cbind.data.frame(seq(as.Date("2006/1/1"), as.Date("2019/12/31"), by = "month"),pre_reg8)
colnames(pre_reg8)<-c("mo_yr_examined","pre")
pre_reg8$mo_yr_examined<-format(pre_reg8$mo_yr_examined,'%m/%y')
reg8_month_pre<-merge(pre_reg8,reg8_month,by="mo_yr_examined")

# compute correlation + significance
cor.test(reg8_month_pre$pre,reg8_month_pre$total_delays)
ccf_reg8=ccf(reg8_month_pre$pre,reg8_month$total_delays)

# subset to region9
pre_reg9<-t(pre[9,-length(pre)])
# combine with monthly delays data
pre_reg9<-cbind.data.frame(seq(as.Date("2006/1/1"), as.Date("2019/12/31"), by = "month"),pre_reg9)
colnames(pre_reg9)<-c("mo_yr_examined","pre")
pre_reg9$mo_yr_examined<-format(pre_reg9$mo_yr_examined,'%m/%y')
reg9_month_pre<-merge(pre_reg9,reg9_month,by="mo_yr_examined")

# compute correlation + significance
cor.test(reg9_month_pre$pre,reg9_month_pre$total_delays_reg_9)
ccf_reg9=ccf(reg9_month_pre$pre,reg9_month$total_delays_reg_9)

## assess correlations between connectivity and median delays
# subset to annually aggregated delays by region in 2015
all_reg_2015 <- read.csv("all_regions_delays_all_inf.csv", stringsAsFactors = F) %>%
  mutate(IDKEY1 = as.Date(IDKEY1, format="%m/%d/%y")) %>%
  mutate(DateSmearEXAMINED = as.Date(DateSmearEXAMINED, format="%m/%d/%y"))%>%
  mutate(yr_examined=format(as.Date(DateSmearEXAMINED,format='%m/%d/%y'),format='%Y'))%>%
  filter(Delays!='#NUM!')%>%
  mutate(delays=as.numeric(Delays))%>%
  dplyr::group_by(yr_examined,Region)%>%
  dplyr::summarise(total_delays=median(delays))%>%
  filter(yr_examined==2015)

# read in connectivity data and combine w/ delay data
connectivity_data=read.csv("accessibility_data.csv")
connectivity_delays=data.frame(cbind(all_reg_2015$Region, 
                                     connectivity_data$AccesibilityIndex[c(1,4,7,8,9)],
                                     all_reg_2015$total_delays))
colnames(connectivity_delays)<-c("Region","Connectivity","Delays")

# assess the correlation between connectivity & delays
cor.test(connectivity_delays$Connectivity,connectivity_delays$Delays)

# plot delays vs connectivity
#connectivity_delays$Region<-as.factor(connectivity_delays$Region)
#ggplot(connectivity_delays,aes(x=Connectivity,y=Delays,fill=Region,group=1))+
#geom_point(size=5,shape=23)+
#geom_smooth(method="lm")+xlab("Accessibility Index")+ylab("Median Delays")

# conduct an ADF test to evaluate the stationarity distributions of monthly delays by region
# from 2006-2017

adf_reg1=adf.test(reg1_month$total_delays_reg_1)
adf_reg4=adf.test(reg4_month$total_delays_reg_4)
adf_reg7=adf.test(reg7_month$total_delays_reg_7)
adf_reg8=adf.test(reg8_month$total_delays_reg_8)
adf_reg9=adf.test(reg9_month$total_delays_reg_9)

# assess synchronicity between delay distributions
merged_dist<-merge(reg1_month,reg4_month,by="mo_yr_examined")
merged_dist_2<-merge(merged_dist,reg7_month,by="mo_yr_examined")
merged_dist_3<-merge(merged_dist_2,reg8_month,by="mo_yr_examined")
merged_dist_4<-merge(merged_dist_3,reg9_month,by="mo_yr_examined")
merged_dist_all<-rbind(merged_dist_4[,2],merged_dist_4[,3],merged_dist_4[,4],merged_dist_4[,5],merged_dist_4[,6])

synchr<-mSynch(merged_dist_all,resamp=1000)
print(synchr)
merged_dist_all_2<-rbind(merged_dist_3[,2],merged_dist_3[,2],merged_dist_3[,4],merged_dist_3[,5])
synchr_2<-mSynch(merged_dist_all_2,resamp=1000)
print(synchr_2)


########

# quantify the proportion of cases reported within a month using estimaes from th e best performing models for each region
data_and_predictions_reg1_NM2$Region<-rep(1,nrow(data_and_predictions_reg1_NM)) 
data_and_predictions_reg4_DIM$Region<-rep(4,nrow(data_and_predictions_reg4_DIM))
data_and_predictions_reg7_NM2$Region<-rep(7,nrow(data_and_predictions_reg7_NM))
data_and_predictions_reg8_NM2$Region<-rep(8,nrow(data_and_predictions_reg8_NM2))

data_and_predictions_best_all = rbind(data_and_predictions_reg1_NM2,data_and_predictions_reg4_DIM,
                                      data_and_predictions_reg7_NM2,data_and_predictions_reg8_NM2)


data_and_predictions_best_all_annual<-data_and_predictions_best_all%>%
  mutate(YrRecorded=format(as.Date(date,format='%Y-%m-%d'),'%Y'))%>%
  group_by(YrRecorded,Region)%>%
  summarise(annual_predicted=sum(point_estimate))

merged_model_data=merge(data_and_predictions_best_all_annual,
                        all_reg_both,by=c("YrRecorded","Region"),all=TRUE)
merged_model_data_2<-merged_model_data%>%
  group_by(YrRecorded,Region)%>%
  mutate(known_predicted=ifelse(Region%in%c(1,4,7,8),annual_predicted,n))%>%
  mutate(known_predicted_nona=ifelse(is.na(known_predicted)==TRUE,n,known_predicted))%>%
  mutate(prop_new=known_predicted_nona/all_n)%>%
  mutate(prop_new_corrected=ifelse(prop_new>1,1,prop_new))

medians_prop_new_corrected<-aggregate(prop_new_corrected~Region,merged_model_data_2,median)

# 
# ggplot()+geom_boxplot(data=all_reg_both,aes(x=Region,y=prop,fill=Region),alpha=0.1)+
#   xlab("Region")+
#   ylab("Proportion of cases reported by the end of the month")+
#   #ggtitle("Proportion of cases reported within a month, by region")+
#   geom_boxplot(data=merged_model_data_2,aes(x=Region,y=prop_new_corrected,fill=Region))+
#   theme(axis.text.x = element_text(size = 10),
#         panel.background = element_blank(),axis.line=element_line(color="black"))
# 
# ggplot()+geom_boxplot(data=merged_model_data_2,aes(x=Region,y=prop_new_corrected,fill=Region),
#                       alpha=0.1)+
#   xlab("Region")+
#   ylab("Proportion of cases reported by the end of the month")+
#   #ggtitle("Proportion of cases reported within a month, by region")+
#   theme(axis.text.x = element_text(size = 10),
#         panel.background = element_blank(),axis.line=element_line(color="black"))


# counting prop vivax, malariae and mixed infections
species_case=read.csv("species_case.csv")%>%
  filter(Case=='RECHECK')%>%
  group_by(InfParas)%>%
  summarise(n=n())%>%
  mutate(prop=n/sum(n))%>%
  filter(InfParas=='MIX INF - FAL. + MAL. + VIV.' | InfParas=='MIX INF - FAL. + VIV.'
         | InfParas=='MIX INF - GAM. + MAL. + VIV.' | InfParas=='MIX INF - GAM. + VIV.' 
         | InfParas=='MIX INF - MAL. + VIV.' | InfParas=='Vivax' | InfParas=='VIVAX')%>%
  summarise(sum_prop=sum(prop))



