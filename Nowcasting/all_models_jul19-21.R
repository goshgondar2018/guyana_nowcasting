# load in libraries
#init()
#library(doParallel)
#registerDoParallel(cores=2)
#library(caret)
library(dplyr)
library(glmnetUtils)
library(tseries)
library(tibble)
library(lattice)
#library(RColorBrewer)
library(rgdal)
library(cruts)
library(raster)
library(ggplot2)
library(Metrics)

train_1year <- 12

# define a relative rmse function
rrmse<-function(predicted,true){
  sqrt(mean((predicted-true)^2))/(diff(range(true)))
}

# specify function for defining rolling confidence intervals
generate_CIs<-function(model_output){
  error_data_input=data.frame(matrix(nrow=132,ncol=6))
  colnames(error_data_input)<-c("sd","rmse","diff_sd_rmse","point_estimate","lower_CI","upper_CI")
  ## determine the RMSE of the past 24 observations for each date, starting with month 25
  for (i in 1:132){
    error_data_input_i=model_output[seq(i,i+23),]
    ### extract the residuals of the predictions for the past 24 months
    residuals_i=error_data_input_i$predicted-error_data_input_i$true
    ### calculate the SD of the residuals
    sd_i=sd(residuals_i)
    ### calculate the RMSE of the predictions for the past 24 months
    rmse_i=rmse(error_data_input_i$true,error_data_input_i$predicted)
    error_data_input[i,1]=sd_i
    error_data_input[i,2]=rmse_i
    error_data_input[i,3]=sd_i-rmse_i
    ### define CIs as predicted point estimates +/- the rmse of the predictions for the past 24 observations
    error_data_input[i,4]=model_output$predicted[i+24]
    error_data_input[i,5]=model_output$predicted[i+24]-rmse_i
    error_data_input[i,6]=model_output$predicted[i+24]+rmse_i
  }
  error_data_input$date=model_output$date[25:156]
  all_regi_errors=inner_join(error_data_input[,c("date","point_estimate","lower_CI","upper_CI")],
                             model_output[,c("date","known","true")],by='date')
  
  model_output_2008=model_output[,c("date","predicted","known","true")]%>%
    mutate(year=format(date,"%Y"))%>%
    subset(year==2007|year==2008)%>%
    dplyr::select(-c(year))
  
  model_output_2008$lower_CI=NA
  model_output_2008$upper_CI=NA
  colnames(model_output_2008)=c("date","point_estimate","known","true","lower_CI","upper_CI")
  
  all_regi_errors_final=rbind.data.frame(model_output_2008[,c("date","point_estimate","lower_CI",
                                                              "upper_CI","known","true")],all_regi_errors)
  return(all_regi_errors_final)
}

DIM<-function(input_data){
  all_data=read.csv(input_data)
  all_data$date <- seq(as.Date("2006/1/1"),by="month",length.out=168) 
  all_data_roles<-define_roles(all_data, dates = "date", response = "t.final")
  
  # run the enet moving time window function
  enet_test <- enet_predict_all_yrs(data = all_data,roles = all_data_roles,
                                    train_period = train_1year,choice = "min",DIM_or_NM="DIM")
  
  df_enet_test <- enet_test$res
  
  data_and_predictions <- cbind.data.frame(df_enet_test$month,
                                           all_data$t.final[13:168],
                                           df_enet_test$pred,
                                           all_data$t[13:168])
  colnames(data_and_predictions)<-c("date","true","predicted","known")
  
  # compute the relative root mean square error
  r_rmse<-rrmse(data_and_predictions$predicted,data_and_predictions$true)
  #sqrt(mean((data_and_predictions$predicted-data_and_predictions$true)^2))/(diff(range(data_and_predictions$true)))
  data_and_predictions_final<-generate_CIs(data_and_predictions)
  return(list(results=data_and_predictions_final,error=r_rmse))
}


# function to run first network model
NM_1<-function(input_data,pre_reg1,pre_reg7,pre_reg8,pre_reg9){
  all_data=read.csv(input_data) 
  all_data$date <- seq(as.Date("2006/1/1"),by="month",length.out=168) 
  if(input_data=="reg1_data_all_inf_all_yrs_NM_jul22-21.csv"){
    all_data<-add_column(all_data,pre_reg1[1:168],.after='t')
  }
  else if(input_data=="reg7_data_all_inf_all_yrs_NM_jul22-21.csv"){
    all_data<-add_column(all_data,pre_reg7[1:168],.after='t')
  }
  else if(input_data=="reg8_data_all_inf_all_yrs_NM_jul22-21.csv"){
    all_data<-add_column(all_data,pre_reg8[1:168],.after='t')
  }
  else {
    all_data<-add_column(all_data,pre_reg9[1:168],.after='t')
  }
  all_data_date<-seq(as.Date("2006/1/1"),by="month",length.out=168) 
  all_data<-all_data[,-ncol(all_data)]
  all_data<-cbind(all_data_date,all_data)
  colnames(all_data)[1]<-c("date")
  
  all_data_roles <- define_roles(all_data, dates = "date", response = "t.final")
  
  enet_test <- enet_predict_all_yrs(data = all_data,roles = all_data_roles,
                                    train_period = train_1year,choice = "min",DIM_or_NM="NM1")
  df_enet_test<- enet_test$res
  
  data_and_predictions<- cbind.data.frame(df_enet_test$month,
                                          all_data$t.final[13:168],
                                          df_enet_test$pred,
                                          all_data$t[13:168])
  
  colnames(data_and_predictions)<-c("date","true","predicted","known")
  
  # compute the relative root mean square error
  
  r_rmse<-rrmse(data_and_predictions$predicted,data_and_predictions$true)
  
  data_and_predictions_final<-generate_CIs(data_and_predictions)
  
  return(list(results=data_and_predictions_final,error=r_rmse))
}

# function to run second network model
NM_2<-function(input_data,data_and_predictions_reg4_DIM,data_and_predictions_reg7_DIM,
               data_and_predictions_reg8_DIM,pre_reg1,pre_reg7,pre_reg8,pre_reg9){
  
  all_data=read.csv(input_data) 
  all_data$date <- seq(as.Date("2006/1/1"),by="month",length.out=168) 
  new<-all_data
  if(input_data=="reg1_data_all_inf_all_yrs_NM_jul22-21.csv"){
    new<-add_column(new,
                    c(data_and_predictions_reg4_DIM$true[1:12],data_and_predictions_reg4_DIM$point_estimate),
                    .after='t.1.reg4')
    new<-add_column(new,
                    c(data_and_predictions_reg7_DIM$true[1:12],data_and_predictions_reg7_DIM$point_estimate),
                    .after='t.1.reg7')
    new<-add_column(new,
                    c(data_and_predictions_reg8_DIM$true[1:12],data_and_predictions_reg8_DIM$point_estimate),
                    .after='t.1.reg8')
    new<-add_column(new,pre_reg1[1:168],.after='t')
  }
  else if (input_data=="reg7_data_all_inf_all_yrs_NM_jul22-21.csv"){
    new<-add_column(new,
                    c(data_and_predictions_reg1_DIM$true[1:12],data_and_predictions_reg1_DIM$point_estimate),
                    .after='t.1.reg1')
    new<-add_column(new,
                    c(data_and_predictions_reg4_DIM$true[1:12],data_and_predictions_reg4_DIM$point_estimate),
                    .after='t.1.reg4')
    new<-add_column(new,
                    c(data_and_predictions_reg8_DIM$true[1:12],data_and_predictions_reg8_DIM$point_estimate),
                    .after='t.1.reg8')
    new<-add_column(new,pre_reg7[1:168],.after='t')
  }
  else if (input_data=="reg8_data_all_inf_all_yrs_NM_jul22-21.csv"){
    new<-all_data
    new<-add_column(new,
                    c(data_and_predictions_reg1_DIM$true[1:12],data_and_predictions_reg1_DIM$point_estimate),
                    .after='t.1.reg1')
    new<-add_column(new,
                    c(data_and_predictions_reg4_DIM$true[1:12],data_and_predictions_reg4_DIM$point_estimate),
                    .after='t.1.reg4')
    new<-add_column(new,
                    c(data_and_predictions_reg7_DIM$true[1:12],data_and_predictions_reg7_DIM$point_estimate),
                    .after='t.1.reg7')
    new<-add_column(new,pre_reg8[1:168],.after='t')
  }
  else {
    new<-all_data
    new<-add_column(new,
                    c(data_and_predictions_reg1_DIM$true[1:12],data_and_predictions_reg1_DIM$point_estimate),
                    .after='t.1.reg1')
    new<-add_column(new,
                    c(data_and_predictions_reg4_DIM$true[1:12],data_and_predictions_reg4_DIM$point_estimate),
                    .after='t.1.reg4')
    new<-add_column(new,
                    c(data_and_predictions_reg7_DIM$true[1:12],data_and_predictions_reg7_DIM$point_estimate),
                    .after='t.1.reg7')
    new<-add_column(new,
                    c(data_and_predictions_reg8_DIM$true[1:12],data_and_predictions_reg8_DIM$point_estimate),
                    .after='t.1.reg8')
    new<-add_column(new,pre_reg9[1:168],.after='t')
  }
  all_data<-new
  all_data_date<-seq(as.Date("2006/1/1"),by="month",length.out=168) 
  all_data<-all_data[,-ncol(all_data)]
  all_data<-cbind(all_data_date,all_data)
  colnames(all_data)[1]<-c("date")
  
  all_data_roles <- define_roles(all_data, dates = "date", response = "t.final")
  
  enet_test <- enet_predict_all_yrs(data = all_data,roles = all_data_roles,
                                    train_period = train_1year,choice = "min",DIM_or_NM="NM2")
  
  df_enet_test<- enet_test$res
  
  data_and_predictions <- cbind.data.frame(df_enet_test$month,
                                           all_data$t.final[13:168],
                                           df_enet_test$pred,
                                           all_data$t[13:168])
  
  colnames(data_and_predictions)<-c("date","true","predicted","known")
  
  # compute the relative root mean square error
  r_rmse<-rrmse(data_and_predictions$predicted,data_and_predictions$true)
  
  data_and_predictions_final<-generate_CIs(data_and_predictions)

  return(list(results=data_and_predictions_final,error=r_rmse))
  
}

ensemble_model <- function(data_and_predictions_regi_DIM,data_and_predictions_regi_NM,
                           data_and_predictions_regi_NM2){
  # collate all model predictions (DIM, NM1, and NM2) for region 1
  all_regi_output=data_and_predictions_regi_DIM%>%
    inner_join(data_and_predictions_regi_NM[,c("date","point_estimate")],by='date')%>%
    inner_join(data_and_predictions_regi_NM2[,c("date","point_estimate")],by='date')
  
  colnames(all_regi_output)<-c("date","DIM_predicted","lower_CI","upper_CI",
                               "known","true","NM1_predicted","NM2_predicted")
  all_regi_output$mean=rowMeans(subset(all_regi_output, select = c('DIM_predicted','NM1_predicted','NM2_predicted')), na.rm = TRUE)
  
  # identify the rRMSE of the prior three observations for each date, starting with the fourth observation
  all_RMSEs=data.frame(matrix(nrow=153,ncol=3))
  colnames(all_RMSEs)<-c("DIM_rmse","NM1_rmse","NM2_rmse")
  for (i in 1:153){
    last_three_observations=all_regi_output[seq(i,i+2),]
    DIM_rmse=rrmse(last_three_observations$DIM_predicted,last_three_observations$true)
    NM1_rmse=rrmse(last_three_observations$NM1_predicted,last_three_observations$true)
    NM2_rmse=rrmse(last_three_observations$NM2_predicted,last_three_observations$true)
    all_RMSEs[i,1]=DIM_rmse
    all_RMSEs[i,2]=NM1_rmse
    all_RMSEs[i,3]=NM2_rmse
  }
  # extract the indices of the model yielding the lowest rRMSE of the prior three observations for each date
  all_RMSEs_indices_min=apply(all_RMSEs,1,which.min)
  
  # collate all predictions (from the fourth date-) and use indices to identify the model which yields the 
  # lowest prior-3 rRMSE for each date
  all_regi_output_selected=cbind.data.frame(all_regi_output$DIM_predicted[seq(4,156)],
                                            all_regi_output$NM1_predicted[seq(4,156)],
                                            all_regi_output$NM2_predicted[seq(4,156)])
  colnames(all_regi_output_selected)<-c("DIM","NM1","NM2")
  
  #REF: https://stackoverflow.com/questions/56099936/r-how-can-i-select-a-value-from-each-row-based-on-an-index-vector
  final_point_estimates=all_regi_output_selected[cbind(seq_len(nrow(all_regi_output_selected)), all_RMSEs_indices_min)]
  
  ## add the mean estimates for the first three observations and combine all model estimates, known and true case counts
  ## for each date
  final_point_estimates_ALL=c(all_regi_output$mean[1:3],final_point_estimates)
  data_and_predictions_regi_ensemble=cbind.data.frame(all_regi_output$date,final_point_estimates_ALL,
                                                      all_regi_output$true,all_regi_output$known)
  colnames(data_and_predictions_regi_ensemble)<-c("date","predicted","true","known")
  
  data_and_predictions_regi_ensemble_final<-generate_CIs(data_and_predictions_regi_ensemble)
  return(list(results=data_and_predictions_regi_ensemble_final))
}

plot_DIM_results<-function(data_and_predictions){
  colors=c("True #"="darkblue","DIM Estimated #"="red", "Known #"="red")
  linetypes=c("True #"="solid","DIM Estimated #"="solid","Known #"="dashed")
  plot<-ggplot(data=data_and_predictions,aes(date))+
    geom_line(aes(y=point_estimate,color="DIM Estimated #",lty="DIM Estimated #",group=1))+
    geom_ribbon(aes(ymin=lower_CI,ymax=upper_CI),alpha=0.2)+
    #geom_line(aes(y=known),color='red')+
    #geom_line(aes(y=true),color='green')+
    geom_line(aes(y=known,color="Known #",lty="Known #",group=1))+
    geom_line(aes(y=true,color="True #",lty="True #",group=1))+
    scale_colour_manual(values = colors,name="Key")+
    scale_linetype_manual(values=linetypes,name="Key")+
    theme(legend.text = element_text(size=17),legend.title=element_text(size=17),
          legend.position="bottom")+
    xlab("Date")+ylab("Number of cases")+ 
    scale_x_date(limits=as.Date(c("2007-01-01", "2019-12-31")),date_breaks="2 years")+
    #ggtitle("DIM estimated, known and true case counts (P. falc), Region 1")+
    theme(axis.text.x = element_text(size = 20,angle = 45, vjust = 1, hjust=1),axis.text.y = element_text(size = 20),
          axis.title.x=element_text(size=20),
          axis.title.y=element_text(size=20))+
    theme(panel.background = element_blank(),axis.line=element_line(color="black"))
  return(plot)
}
# function to plot network model results
plot_NM_1_results<-function(data_and_predictions){
  colors=c("True #"="darkblue","NM Estimated #"="red","Known #"="red")
  linetypes=c("True #"="solid","NM Estimated #"="solid","Known #"="dashed")
  plot<-ggplot(data=data_and_predictions,aes(date))+
    geom_line(aes(y=point_estimate,color="NM Estimated #",lty="NM Estimated #",group=1))+
    geom_ribbon(aes(ymin=lower_CI,ymax=upper_CI),alpha=0.2)+
    geom_line(aes(y=true,color="True #",lty="True #",group=1))+
    geom_line(aes(y=known,color="Known #",lty="Known #",group=1))+
    scale_colour_manual(values = colors,name="Key")+
    scale_linetype_manual(values=linetypes,name="Key")+
    theme(legend.text = element_text(size=17),legend.title=element_text(size=17),
          legend.position="bottom")+
    xlab("Date")+ylab("Number of cases")+ 
    ggtitle("NM 1")+
    scale_x_date(limits=as.Date(c("2007-01-01", "2019-12-31")),date_breaks="2 years")+
    theme(axis.text.x = element_text(size = 20,angle = 45, vjust = 1, hjust=1),
          axis.text.y = element_text(size = 20),
          axis.title.x=element_text(size=20),
          axis.title.y=element_text(size=20),
          plot.title = element_text(size=22,hjust = 0.5))+
    theme(panel.background = element_blank(),axis.line=element_line(color="black"))
  return(plot)
}

plot_NM_2_results<-function(data_and_predictions){
  colors=c("True #"="darkblue","NM Estimated #"="red","Known #"="red")
  linetypes=c("True #"="solid","NM Estimated #"="solid","Known #"="dashed")
  plot<-ggplot(data=data_and_predictions,aes(date))+
    geom_line(aes(y=point_estimate,color="NM Estimated #",lty="NM Estimated #",group=1))+
    geom_ribbon(aes(ymin=lower_CI,ymax=upper_CI),alpha=0.2)+
    geom_line(aes(y=true,color="True #",lty="True #",group=1))+
    geom_line(aes(y=known,color="Known #",lty="Known #",group=1))+
    scale_colour_manual(values = colors,name="Key")+
    scale_linetype_manual(values=linetypes,name="Key")+
    theme(legend.text = element_text(size=17),legend.title=element_text(size=17),
          legend.position="bottom")+
    xlab("Date")+ylab("Number of cases")+ 
    ggtitle("NM 2")+
    scale_x_date(limits=as.Date(c("2007-01-01", "2019-12-31")),date_breaks="2 years")+
    theme(axis.text.x = element_text(size = 20,angle = 45, vjust = 1, hjust=1),
          axis.text.y = element_text(size = 20),
          axis.title.x=element_text(size=20),
          axis.title.y=element_text(size=20),
          plot.title = element_text(size=22,hjust = 0.5))+
    theme(panel.background = element_blank(),axis.line=element_line(color="black"))
  return(plot)
}

plot_ensemble_results<-function(data_and_predictions){
  colors=c("True #"="darkblue","Ensemble Estimated #"="red","Known #"="red")
  linetypes=c("True #"="solid","Ensemble Estimated #"="solid","Known #"="dashed")
  plot<-ggplot(data=data_and_predictions,aes(date))+
    geom_line(aes(y=point_estimate,color="Ensemble Estimated #",lty="Ensemble Estimated #",group=1))+
    geom_ribbon(aes(ymin=lower_CI,ymax=upper_CI),alpha=0.2)+
    geom_line(aes(y=true,color="True #",lty="True #",group=1))+
    geom_line(aes(y=known,color="Known #",lty="Known #",group=1))+
    scale_colour_manual(values = colors,name="Key")+
    scale_linetype_manual(values=linetypes,name="Key")+
    theme(legend.text = element_text(size=17),legend.title=element_text(size=17),
          legend.position="bottom")+
    xlab("Date")+ylab("Number of cases")+ 
    scale_x_date(limits=as.Date(c("2007-01-01", "2019-12-31")),date_breaks="2 years")+
    theme(axis.text.x = element_text(size = 20,angle = 45, vjust = 1, hjust=1),
          axis.text.y = element_text(size = 20),
          axis.title.x=element_text(size=20),
          axis.title.y=element_text(size=20))+
    theme(panel.background = element_blank(),axis.line=element_line(color="black"))
  return(plot)
}

plot_all_results<-function(data_and_predictions_DIM,data_and_predictions_NM,
                           data_and_predictions_NM2,data_and_predictions_ensemble){
  all_predictions=data_and_predictions_DIM%>%
    inner_join(data_and_predictions_NM[,c('date',"point_estimate","lower_CI","upper_CI")],by='date')%>%
    inner_join(data_and_predictions_NM2[,c('date',"point_estimate","lower_CI","upper_CI")],by='date')%>%
    inner_join(data_and_predictions_ensemble[,c('date',"point_estimate","lower_CI","upper_CI")],by='date')
  
  colnames(all_predictions)<-c("date","DIM_point_estimate","lower_CI_DIM","upper_CI_DIM",
                               "known","true","NM_point_estimate","lower_CI_NM","upper_CI_NM",
                               "NM2_point_estimate","lower_CI_NM2","upper_CI_NM2",
                               "ensemble_point_estimate","lower_CI_ensemble","upper_CI_ensemble")
  
  colors=c("True #"="darkblue","DIM Estimated #"="red","NM Estimated #"="magenta","NM2 Estimated #"='cyan',
           "Known #"="red","Ensemble Estimated #"="green")
  linetypes=c("True #"="solid","DIM Estimated #"="solid","NM Estimated #"="solid","NM2 Estimated #"="solid",
              "Known #"="dashed","Ensemble Estimated #"="solid")
  plot<-ggplot(data=all_predictions,aes(date))+
    geom_line(aes(y=DIM_point_estimate,color="DIM Estimated #",lty="DIM Estimated #",group=1))+
    geom_ribbon(aes(ymin=lower_CI_DIM,ymax=upper_CI_DIM),alpha=0.2)+
    geom_line(aes(y=NM_point_estimate,color="NM Estimated #",lty="NM Estimated #",group=1))+
    geom_ribbon(aes(ymin=lower_CI_NM,ymax=upper_CI_NM),alpha=0.2,fill='yellow')+
    geom_line(aes(y=NM2_point_estimate,color="NM2 Estimated #",lty="NM2 Estimated #",group=1))+
    geom_ribbon(aes(ymin=lower_CI_NM2,ymax=upper_CI_NM2),alpha=0.2,fill='magenta')+
    geom_line(aes(y=ensemble_point_estimate,color="Ensemble Estimated #",lty="Ensemble Estimated #",group=1))+
    geom_ribbon(aes(ymin=lower_CI_NM2,ymax=upper_CI_NM2),alpha=0.2,fill='yellow')+
    geom_line(aes(y=true,color="True #",lty="True #",group=1))+
    geom_line(aes(y=known,color="Known #",lty="Known #",group=1))+
    scale_colour_manual(values = colors,name="Key")+
    scale_linetype_manual(values=linetypes,name="Key")+
    theme(legend.text = element_text(size=12),legend.title=element_text(size=12),
          legend.position="bottom")+
    xlab("Date")+ylab("Number of cases")+ 
    scale_x_date(limits=as.Date(c("2007-01-01", "2019-12-31")),date_breaks="2 years")+
    theme(axis.text.x = element_text(size = 20,angle = 45, vjust = 1, hjust=1),
          axis.text.y = element_text(size = 20),
          axis.title.x=element_text(size=20),
          axis.title.y=element_text(size=20))+
    theme(panel.background = element_blank(),axis.line=element_line(color="black"))
  return(plot)
}

