library(tidyverse)
library(DataCombine)
setwd("~/Desktop/Guyana Research/")

# ref for mondf function: https://rdrr.io/github/keithabailey/PRMTools/src/R/mondf.R
mondf <- function(d1, d2) {
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate needed for this function to work. Please install it.",
         call. = FALSE)
  }
  library(lubridate)
  
  monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon }
  monnb(d2) - monnb(d1)
}

prep_data=function(all_region_data,region){
  regi_data_prep<-read.csv(all_region_data,stringsAsFactors = F) %>%
    subset(Region==region) %>%
    mutate(IDKEY1 = as.Date(IDKEY1, format="%m/%d/%y")) %>%
    mutate(DateSmearEXAMINED = as.Date(DateSmearEXAMINED, format="%m/%d/%y"))
  
  regi_data_prep_1 <-regi_data_prep %>%
    mutate(val = 1) %>%
    {aggregate(.$val, by=list(.$DateSmearEXAMINED, .$IDKEY1), FUN=function(x) sum(x,na.rm=T))} %>%
    set_names(c("DateSmearEXAMINED","IDKEY1","Count"))  
  
  # convert dates to as.POSIXct of the Y-m-d format
  regi_data_prep_1$DateSmearEXAMINED<-as.POSIXct(strptime(regi_data_prep_1$DateSmearEXAMINED,format='%Y-%m-%d'))
  regi_data_prep_1$IDKEY1<-as.POSIXct(strptime(regi_data_prep_1$IDKEY1,format='%Y-%m-%d'))
  
  # create data frame of known cases from t-n 
  regi_data_prep_2 <- regi_data_prep_1%>% 
    mutate(MoYrKnown=format(as.Date(IDKEY1,format='%Y-%m-%d'),'%Y-%m'))%>%
    mutate(MoYrRecorded=format(as.Date(DateSmearEXAMINED,format='%Y-%m-%d'),'%Y-%m'))%>%
    mutate(difference_dates = mondf(DateSmearEXAMINED,IDKEY1)) %>%
    subset(difference_dates>=0) %>%
    #subset(difference_dates<13)%>%
    {aggregate(.$Count,by=list(.$MoYrKnown,.$MoYrRecorded,.$difference_dates), FUN=function(x) sum(x,na.rm=T))} %>% 
    set_names(c("date_known","date_recorded","time","count")) %>% 
    mutate(diff3 = paste0("t-",time)) %>%
    dplyr::select(date_known,diff3,count) %>%
    {aggregate(.$count, by =list(.$date_known, .$diff3), FUN=sum)} %>%
    set_names(c("tdate","diff","count")) %>%
    spread(diff, count) %>%
    dplyr::select("tdate",paste0("t-",0:12)) %>%
    replace(is.na(.),0) 
  
  regi_data_prep_3 <- seq(as.Date("2006/1/1"),as.Date("2019/12/31"),"month") %>%
    as.data.frame() %>%
    set_names(("tdate")) %>%
    mutate(tdate = format(tdate,"%Y-%m")) %>%
    merge(regi_data_prep_2, by = "tdate", all.x = T) %>%
    replace(is.na(.),0)
  
  # accumulate case counts
  regi_data_prep_3_final <- data.frame(regi_data_prep_3)
  for (i in 2:nrow(regi_data_prep_3_final)) {
    for (j in 3:length(regi_data_prep_3_final)) {
      if (regi_data_prep_3[i,j]==0) {
        regi_data_prep_3_final[i,j]=regi_data_prep_3_final[i-1,j-1]
      }
      else {
        regi_data_prep_3_final[i,j]=regi_data_prep_3_final[i,j]+regi_data_prep_3_final[i-1,j-1]
      }
    }
  }
  
  regi_converged <- regi_data_prep %>%
    mutate(MoYrKnown=format(as.Date(IDKEY1,format='%Y-%m-%d'),'%Y-%m'))%>%
    mutate(MoYrRecorded=format(as.Date(DateSmearEXAMINED,format='%Y-%m-%d'),'%Y-%m'))%>%
    mutate(YrRecorded=format(as.Date(DateSmearEXAMINED,format='%Y-%m-%d'),'%Y'))%>%
    subset(YrRecorded!=2020)%>%
    mutate(difference_dates = mondf(DateSmearEXAMINED,IDKEY1)) %>%
    subset(difference_dates>=0) %>%
    #subset(difference_dates<13) %>%
    group_by(DateSmearEXAMINED)%>%
    group_by(MoYrRecorded)%>%
    dplyr::summarise(n=n())
  
  if(nrow(regi_converged)!=168) {
    all_dates=data.frame(MoYrRecorded=format(seq(as.Date("2006/1/1"),as.Date("2019/12/31"),"month"),"%Y-%m"))
    regi_full_converged=full_join(all_dates,regi_converged)%>%
      mutate(n=ifelse(is.na(n)==TRUE,0,n))
    regi_data_prep_final <- data.frame(cbind(regi_data_prep_3_final,regi_full_converged[,2]))
  }
  
  else if(nrow(regi_converged)==168) {
    regi_data_prep_final <- data.frame(cbind(regi_data_prep_3_final,regi_converged[,2]))
  }
  
  colnames(regi_data_prep_final)[length(regi_data_prep_final)] <- "n"
  
  regi_data_prep_final_2 <- regi_data_prep_final[,c("tdate","t.12","t.11","t.10","t.9","t.8","t.7",
                                                    "t.6","t.5","t.4","t.3","t.2","t.1","t.0","n")]
  colnames(regi_data_prep_final_2)<-c("date", "t-12", "t-11","t-10","t-9","t-8","t-7",
                                      "t-6","t-5","t-4","t-3","t-2","t-1","t","t.final")
  
  return(regi_data_prep_final_2)
}

setwd("~/Desktop/Guyana Research")

reg1_data=prep_data("all_regions_delays_all_inf.csv",1)
reg4_data=prep_data("all_regions_delays_all_inf.csv",4)
reg7_data=prep_data("all_regions_delays_all_inf.csv",7)
reg8_data=prep_data("all_regions_delays_all_inf.csv",8)
reg9_data=prep_data("all_regions_delays_all_inf.csv",9)

setwd("~/Desktop/Guyana Research/Nowcasting/Input Data")
write.csv(reg1_data,"reg1_data_all_inf_all_yrs_jul22-21.csv",row.names=FALSE)
write.csv(reg4_data,"reg4_data_all_inf_all_yrs_jul22-21.csv",row.names=FALSE)
write.csv(reg7_data,"reg7_data_all_inf_all_yrs_jul22-21.csv",row.names=FALSE)
write.csv(reg8_data,"reg8_data_all_inf_all_yrs_jul22-21.csv",row.names=FALSE)
write.csv(reg9_data,"reg9_data_all_inf_all_yrs_jul22-21.csv",row.names=FALSE)


## prepare NM datasets 

### region 1
reg1_data_NM=cbind.data.frame(reg4_data[,seq(2,13)],reg7_data[,seq(2,13)],reg8_data[,seq(2,13)],
                        reg1_data[,seq(2,15)])
for (i in 1:36){
  if (i %in% seq(1,12)){
    colnames(reg1_data_NM)[i]<-paste0(colnames(reg1_data_NM[i]),'.reg4')
  }
  else if (i %in% seq(13,24)){
    colnames(reg1_data_NM)[i]<-paste0(colnames(reg1_data_NM[i]),'.reg7')
  }
  else if (i %in% seq(25,36)){
    colnames(reg1_data_NM)[i]<-paste0(colnames(reg1_data_NM[i]),'.reg8')
  }
}

write.csv(reg1_data_NM,"reg1_data_all_inf_all_yrs_NM_jul22-21.csv",row.names=FALSE)

### region 7
reg7_data_NM=cbind.data.frame(reg1_data[,seq(2,13)],reg4_data[,seq(2,13)],reg8_data[,seq(2,13)],
                              reg7_data[,seq(2,15)])

for (i in 1:36){
  if (i %in% seq(1,12)){
    colnames(reg7_data_NM)[i]<-paste0(colnames(reg7_data_NM[i]),'.reg1')
  }
  else if (i %in% seq(13,24)){
    colnames(reg7_data_NM)[i]<-paste0(colnames(reg7_data_NM[i]),'.reg4')
  }
  else if (i %in% seq(25,36)){
    colnames(reg7_data_NM)[i]<-paste0(colnames(reg7_data_NM[i]),'.reg8')
  }
}

write.csv(reg7_data_NM,"reg7_data_all_inf_all_yrs_NM_jul22-21.csv",row.names=FALSE)

### region 8
reg8_data_NM=cbind.data.frame(reg1_data[,seq(2,13)],reg4_data[,seq(2,13)],reg7_data[,seq(2,13)],
                              reg8_data[,seq(2,15)])

for (i in 1:36){
  if (i %in% seq(1,12)){
    colnames(reg8_data_NM)[i]<-paste0(colnames(reg8_data_NM[i]),'.reg1')
  }
  else if (i %in% seq(13,24)){
    colnames(reg8_data_NM)[i]<-paste0(colnames(reg8_data_NM[i]),'.reg4')
  }
  else if (i %in% seq(25,36)){
    colnames(reg8_data_NM)[i]<-paste0(colnames(reg8_data_NM[i]),'.reg7')
  }
}

write.csv(reg8_data_NM,"reg8_data_all_inf_all_yrs_NM_jul22-21.csv",row.names=FALSE)

### region 9
reg9_data_NM=cbind.data.frame(reg1_data[,seq(2,13)],reg4_data[,seq(2,13)],reg8_data[,seq(2,13)],
                              reg9_data[,seq(2,15)])

for (i in 1:36){
  if (i %in% seq(1,12)){
    colnames(reg9_data_NM)[i]<-paste0(colnames(reg9_data_NM[i]),'.reg1')
  }
  else if (i %in% seq(13,24)){
    colnames(reg9_data_NM)[i]<-paste0(colnames(reg9_data_NM[i]),'.reg4')
  }
  else if (i %in% seq(25,36)){
    colnames(reg9_data_NM)[i]<-paste0(colnames(reg9_data_NM[i]),'.reg8')
  }
}

write.csv(reg9_data_NM,"reg9_data_all_inf_all_yrs_NM_jul22-21.csv",row.names=FALSE)
