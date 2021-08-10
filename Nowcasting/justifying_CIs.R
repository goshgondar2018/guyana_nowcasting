
library(ggpubr)

# define a function to  return the residuals, the rmse of point estimates, and sd of residuals
compare_errors <- function(data_and_predictions_regi) {
  data_and_predictions_regi_errors=data.frame(data_and_predictions_regi$point_estimate-data_and_predictions_regi$true)
  colnames(data_and_predictions_regi_errors)<-'residuals'
  
  ggplot(data_and_predictions_regi_errors,aes(residuals))+
    geom_density()
  
  sd=sd(data_and_predictions_regi_errors$residuals)
  rmse=rmse(data_and_predictions_regi$point_estimate,data_and_predictions_regi$true)
  return(list(table_errors=data_and_predictions_regi_errors,sd=sd,rmse=rmse))
  
}

# DIM model

## plot a histogram of residuals for region 1, and comparison of rmse and sd of residuals 
compare_errors_df_reg1_DIM=compare_errors(data_and_predictions_reg1_DIM)
data_and_predictions_reg1_DIM_errors=compare_errors_df_reg1_DIM$table_errors
colnames(data_and_predictions_reg1_DIM_errors)<-'residuals'

# REF to aid in constructing plot: https://stackoverflow.com/questions/6967664/ggplot2-histogram-with-normal-curve
residuals_reg1_DIM<-ggplot(data_and_predictions_reg1_DIM_errors,aes(x=residuals))+
  geom_histogram(aes(y=..density..),fill=' dark green',col='yellow')+theme_classic()+
  stat_function(fun = dnorm, args = list(mean = mean(data_and_predictions_reg1_DIM_errors$residuals), 
                                         sd = sd(data_and_predictions_reg1_DIM_errors$residuals)),col='magenta',
                size=1)+
  ggtitle(paste0("rmse = ",round(compare_errors_df_reg1_DIM$rmse,2), '\n sd=',
                 round(compare_errors_df_reg1_DIM$sd,2)))+
  theme(plot.title = element_text(size =  10, face = "bold",hjust = 0.5))

## extract the rmse and sd of residuals for region 4 
compare_errors_df_reg4_DIM=compare_errors(data_and_predictions_reg4_DIM)
data_and_predictions_reg4_DIM_sd=compare_errors_df_reg4_DIM$sd
data_and_predictions_reg4_DIM_rmse=compare_errors_df_reg4_DIM$rmse

## plot a histogram of residuals for region 7, and comparison of rmse and sd of residuals 
compare_errors_df_reg7_DIM=compare_errors(data_and_predictions_reg7_DIM)
data_and_predictions_reg7_DIM_errors=compare_errors_df_reg7_DIM$table_errors
colnames(data_and_predictions_reg7_DIM_errors)<-'residuals'
residuals_reg7_DIM<-ggplot(data_and_predictions_reg7_DIM_errors,aes(x=residuals))+
  geom_histogram(aes(y=..density..),fill=' dark green',col='yellow')+theme_classic()+
  stat_function(fun = dnorm, args = list(mean = mean(data_and_predictions_reg7_DIM_errors$residuals), 
                                         sd = sd(data_and_predictions_reg7_DIM_errors$residuals)),col='magenta',
                size=1)+
  ggtitle(paste0("rmse = ",round(compare_errors_df_reg7_DIM$rmse,2), '\n sd=',
                 round(compare_errors_df_reg7_DIM$sd,2)))+
  theme(plot.title = element_text(size =  10, face = "bold",hjust = 0.5))

## plot a histogram of residuals for region 8, and comparison of rmse and sd of residuals
compare_errors_df_reg8_DIM=compare_errors(data_and_predictions_reg8_DIM)
data_and_predictions_reg8_DIM_errors=compare_errors_df_reg8_DIM$table_errors
colnames(data_and_predictions_reg8_DIM_errors)<-'residuals'
residuals_reg8_DIM<-ggplot(data_and_predictions_reg8_DIM_errors,aes(x=residuals))+
  geom_histogram(aes(y=..density..),fill=' dark green',col='yellow')+theme_classic()+
  stat_function(fun = dnorm, args = list(mean = mean(data_and_predictions_reg8_DIM_errors$residuals), 
                                         sd = sd(data_and_predictions_reg8_DIM_errors$residuals)),col='magenta',
                size=1)+
  ggtitle(paste0("rmse = ",round(compare_errors_df_reg8_DIM$rmse,2), '\n sd=',
                 round(compare_errors_df_reg8_DIM$sd,2)))+
  theme(plot.title = element_text(size =  10, face = "bold",hjust = 0.5))

# NM1 model

compare_errors_df_reg1_NM=compare_errors(data_and_predictions_reg1_NM)
data_and_predictions_reg1_NM_errors=compare_errors_df_reg1_NM$table_errors
colnames(data_and_predictions_reg1_NM_errors)<-'residuals'
residuals_reg1_NM<-ggplot(data_and_predictions_reg1_NM_errors,aes(x=residuals))+
  geom_histogram(aes(y=..density..),fill=' dark green',col='yellow')+theme_classic()+
  stat_function(fun = dnorm, args = list(mean = mean(data_and_predictions_reg1_NM_errors$residuals), 
                                         sd = sd(data_and_predictions_reg1_NM_errors$residuals)),col='magenta',
                size=1)+
  ggtitle(paste0("rmse = ",round(compare_errors_df_reg1_NM$rmse,2), '\n sd=',
                 round(compare_errors_df_reg1_NM$sd,2)))+
  theme(plot.title = element_text(size =  10, face = "bold",hjust = 0.5))

compare_errors_df_reg7_NM=compare_errors(data_and_predictions_reg7_NM)
data_and_predictions_reg7_NM_errors=compare_errors_df_reg7_NM$table_errors
colnames(data_and_predictions_reg7_NM_errors)<-'residuals'
residuals_reg7_NM<-ggplot(data_and_predictions_reg7_NM_errors,aes(x=residuals))+
  geom_histogram(aes(y=..density..),fill=' dark green',col='yellow')+theme_classic()+
  stat_function(fun = dnorm, args = list(mean = mean(data_and_predictions_reg7_NM_errors$residuals), 
                                         sd = sd(data_and_predictions_reg7_NM_errors$residuals)),col='magenta',
                size=1)+
  ggtitle(paste0("rmse = ",round(compare_errors_df_reg7_NM$rmse,2), '\n sd=',
                 round(compare_errors_df_reg7_NM$sd,2)))+
  theme(plot.title = element_text(size =  10, face = "bold",hjust = 0.5))

compare_errors_df_reg8_NM=compare_errors(data_and_predictions_reg8_NM)
data_and_predictions_reg8_NM_errors=compare_errors_df_reg8_NM$table_errors
colnames(data_and_predictions_reg8_NM_errors)<-'residuals'
residuals_reg8_NM<-ggplot(data_and_predictions_reg8_NM_errors,aes(x=residuals))+
  geom_histogram(aes(y=..density..),fill=' dark green',col='yellow')+theme_classic()+
  stat_function(fun = dnorm, args = list(mean = mean(data_and_predictions_reg8_NM_errors$residuals), 
                                         sd = sd(data_and_predictions_reg8_NM_errors$residuals)),col='magenta',
                size=1)+
  ggtitle(paste0("rmse = ",round(compare_errors_df_reg8_NM$rmse,2), '\n sd=',
                 round(compare_errors_df_reg8_NM$sd,2)))+
  theme(plot.title = element_text(size =  10, face = "bold",hjust = 0.5))

# NM2 model
compare_errors_df_reg1_NM2=compare_errors(data_and_predictions_reg1_NM2)
data_and_predictions_reg1_NM2_errors=compare_errors_df_reg1_NM2$table_errors
colnames(data_and_predictions_reg1_NM2_errors)<-'residuals'

residuals_reg1_NM2<-ggplot(data_and_predictions_reg1_NM2_errors,aes(x=residuals))+
  geom_histogram(aes(y=..density..),fill=' dark green',col='yellow')+theme_classic()+
  stat_function(fun = dnorm, args = list(mean = mean(data_and_predictions_reg1_NM2_errors$residuals), 
                                         sd = sd(data_and_predictions_reg1_NM2_errors$residuals)),col='magenta',
                size=1)+
  ggtitle(paste0("rmse = ",round(compare_errors_df_reg1_NM2$rmse,2), '\n sd=',
                 round(compare_errors_df_reg1_NM2$sd,2)))+
  theme(plot.title = element_text(size =  10, face = "bold",hjust = 0.5))

compare_errors_df_reg7_NM2=compare_errors(data_and_predictions_reg7_NM2)
data_and_predictions_reg7_NM2_errors=compare_errors_df_reg7_NM2$table_errors
colnames(data_and_predictions_reg7_NM2_errors)<-'residuals'
residuals_reg7_NM2<-ggplot(data_and_predictions_reg7_NM2_errors,aes(x=residuals))+
  geom_histogram(aes(y=..density..),fill=' dark green',col='yellow')+theme_classic()+
  stat_function(fun = dnorm, args = list(mean = mean(data_and_predictions_reg7_NM2_errors$residuals), 
                                         sd = sd(data_and_predictions_reg7_NM2_errors$residuals)),col='magenta',
                size=1)+
  ggtitle(paste0("rmse = ",round(compare_errors_df_reg7_NM2$rmse,2), '\n sd=',
                 round(compare_errors_df_reg7_NM2$sd,2)))+
  theme(plot.title = element_text(size =  10, face = "bold",hjust = 0.5))

compare_errors_df_reg8_NM2=compare_errors(data_and_predictions_reg8_NM2)
data_and_predictions_reg8_NM2_errors=compare_errors_df_reg8_NM2$table_errors
colnames(data_and_predictions_reg8_NM2_errors)<-'residuals'
residuals_reg8_NM2<-ggplot(data_and_predictions_reg8_NM2_errors,aes(x=residuals))+
  geom_histogram(aes(y=..density..),fill=' dark green',col='yellow')+theme_classic()+
  stat_function(fun = dnorm, args = list(mean = mean(data_and_predictions_reg8_NM2_errors$residuals), 
                                         sd = sd(data_and_predictions_reg8_NM2_errors$residuals)),col='magenta',
                size=1)+
  ggtitle(paste0("rmse = ",round(compare_errors_df_reg8_NM2$rmse,2), '\n sd=',
                 round(compare_errors_df_reg8_NM2$sd,2)))+
  theme(plot.title = element_text(size =  10, face = "bold",hjust = 0.5))

# ensemble model
compare_errors_df_reg1_ensemble=compare_errors(data_and_predictions_reg1_ensemble)
data_and_predictions_reg1_ensemble_errors=compare_errors_df_reg1_ensemble$table_errors
colnames(data_and_predictions_reg1_ensemble_errors)<-'residuals'

residuals_reg1_ensemble<-ggplot(data_and_predictions_reg1_ensemble_errors,aes(x=residuals))+
  geom_histogram(aes(y=..density..),fill=' dark green',col='yellow')+theme_classic()+
  stat_function(fun = dnorm, args = list(mean = mean(data_and_predictions_reg1_ensemble_errors$residuals), 
                                         sd = sd(data_and_predictions_reg1_ensemble_errors$residuals)),col='magenta',
                size=1)+
  ggtitle(paste0("rmse = ",round(compare_errors_df_reg1_ensemble$rmse,2), '\n sd=',
                 round(compare_errors_df_reg1_ensemble$sd,2)))+
  theme(plot.title = element_text(size =  10, face = "bold",hjust = 0.5))

compare_errors_df_reg7_ensemble=compare_errors(data_and_predictions_reg7_ensemble)
data_and_predictions_reg7_ensemble_errors=compare_errors_df_reg7_ensemble$table_errors
colnames(data_and_predictions_reg7_ensemble_errors)<-'residuals'
residuals_reg7_ensemble<-ggplot(data_and_predictions_reg7_ensemble_errors,aes(x=residuals))+
  geom_histogram(aes(y=..density..),fill=' dark green',col='yellow')+theme_classic()+
  stat_function(fun = dnorm, args = list(mean = mean(data_and_predictions_reg7_ensemble_errors$residuals), 
                                         sd = sd(data_and_predictions_reg7_ensemble_errors$residuals)),col='magenta',
                size=1)+
  ggtitle(paste0("rmse = ",round(compare_errors_df_reg7_ensemble$rmse,2), '\n sd=',
                 round(compare_errors_df_reg7_ensemble$sd,2)))+
  theme(plot.title = element_text(size =  10, face = "bold",hjust = 0.5))

compare_errors_df_reg8_ensemble=compare_errors(data_and_predictions_reg8_ensemble)
data_and_predictions_reg8_ensemble_errors=compare_errors_df_reg8_ensemble$table_errors
colnames(data_and_predictions_reg8_ensemble_errors)<-'residuals'
residuals_reg8_ensemble<-ggplot(data_and_predictions_reg8_ensemble_errors,aes(x=residuals))+
  geom_histogram(aes(y=..density..),fill=' dark green',col='yellow')+theme_classic()+
  stat_function(fun = dnorm, args = list(mean = mean(data_and_predictions_reg8_ensemble_errors$residuals), 
                                         sd = sd(data_and_predictions_reg8_ensemble_errors$residuals)),col='magenta',
                size=1)+
  ggtitle(paste0("rmse = ",round(compare_errors_df_reg8_ensemble$rmse,2), '\n sd=',
                 round(compare_errors_df_reg8_ensemble$sd,2)))+
  theme(plot.title = element_text(size =  10, face = "bold",hjust = 0.5))

ggarrange(residuals_reg1_DIM,residuals_reg7_DIM,residuals_reg8_DIM,
          residuals_reg1_NM,residuals_reg7_NM,residuals_reg8_NM,
          residuals_reg1_NM2,residuals_reg7_NM2,residuals_reg8_NM2,
          residuals_reg1_ensemble,residuals_reg7_ensemble,residuals_reg8_ensemble,
          ncol=3,nrow=4)

ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/residuals_distribution.png")


