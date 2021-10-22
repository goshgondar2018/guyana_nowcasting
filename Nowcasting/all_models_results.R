
# read in rainfall data for network models 
# adapted from code provided by Ayesha Mahmud 
setwd("~/Desktop/Guyana Research")

#range= c( "2006-01-01","2019-12-31")
#munic <- getData("GADM", country="GUY", level=1)
#ncfile = paste("cru_ts4.04.1901.2019.pre",".dat.nc", sep = "") 
#pre <- cruts2poly(ncfile, munic, timeRange = range , na.rm = TRUE)

#pre@data$code_long = as.character(unique(munic$GID_1))
#pre <- as.data.frame(pre@data)
#pre_reg1<-t(pre[1,-length(pre)])
#pre_reg7<-t(pre[7,-length(pre)])
#pre_reg8<-t(pre[8,-length(pre)])
#pre_reg9<-t(pre[9,-length(pre)])

pre_reg1 <- read.csv("pre_reg1.csv")[,2]
pre_reg7 <- read.csv("pre_reg7.csv")[,2]
pre_reg8 <- read.csv("pre_reg8.csv")[,2]
pre_reg9 <- read.csv("pre_reg9.csv")[,2]

setwd("~/Desktop/Guyana Research/Nowcasting/Input Data")

# extract and plot DIM results

## region 1
DIM_output_reg1=DIM("reg1_data_all_inf_all_yrs_jul22-21.csv")  
data_and_predictions_reg1_DIM=DIM_output_reg1$results
r_rmse_reg1_DIM=DIM_output_reg1$error
plot_DIM_results(data_and_predictions_reg1_DIM)
ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg1_all_inf_DIM_all_yrs.png")

## region 4
DIM_output_reg4=DIM("reg4_data_all_inf_all_yrs_jul22-21.csv")  
data_and_predictions_reg4_DIM=DIM_output_reg4$results
r_rmse_reg4_DIM=DIM_output_reg4$error
plot_DIM_results(data_and_predictions_reg4_DIM)
ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg4_all_inf_DIM_all_yrs.png")

## region 7
DIM_output_reg7=DIM("reg7_data_all_inf_all_yrs_jul22-21.csv") 
data_and_predictions_reg7_DIM=DIM_output_reg7$results
r_rmse_reg7_DIM=DIM_output_reg7$error
plot_DIM_results(data_and_predictions_reg7_DIM)
ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg7_all_inf_DIM_all_yrs.png")

## region 8
DIM_output_reg8=DIM("reg8_data_all_inf_all_yrs_jul22-21.csv")  
data_and_predictions_reg8_DIM=DIM_output_reg8$results
r_rmse_reg8_DIM=DIM_output_reg8$error
plot_DIM_results(data_and_predictions_reg8_DIM)
ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg8_all_inf_DIM_all_yrs.png")

## region 9 - failed to converge
DIM_output_reg9=DIM("reg9_data_all_inf_all_yrs_jul22-21.csv")  
data_and_predictions_reg9_DIM=DIM_output_reg9$results
r_rmse_reg9=DIM_output_reg9$error
plot_DIM_results(data_and_predictions_reg9_DIM)

# extract and plot NM 1 results

## region 1

NM_output_reg1=NM_1("reg1_data_all_inf_all_yrs_NM_jul22-21.csv",pre_reg1,pre_reg7,pre_reg8,pre_reg9)  
data_and_predictions_reg1_NM=NM_output_reg1$results
r_rmse_reg1_NM1=NM_output_reg1$error
plot_NM_1_results(data_and_predictions_reg1_NM)
ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg1_all_inf_NM1_all_yrs.png")

## region 7
NM_output_reg7=NM_1("reg7_data_all_inf_all_yrs_NM_jul22-21.csv",pre_reg1,pre_reg7,pre_reg8,pre_reg9) 
data_and_predictions_reg7_NM=NM_output_reg7$results
r_rmse_reg7_NM1=NM_output_reg7$error
plot_NM_1_results(data_and_predictions_reg7_NM)
ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg7_all_inf_NM1_all_yrs.png")

## region 8
NM_output_reg8=NM_1("reg8_data_all_inf_all_yrs_NM_jul22-21.csv",pre_reg1,pre_reg7,pre_reg8,pre_reg9)  
data_and_predictions_reg8_NM=NM_output_reg8$results
r_rmse_reg8_NM1=NM_output_reg8$error
plot_NM_1_results(data_and_predictions_reg8_NM)
ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg8_all_inf_NM1_all_yrs.png")

## region 9 - failed to converge
NM_output_reg9=NM_1("reg9_data_all_inf_all_yrs_NM_jul22-21.csv",pre_reg1,pre_reg7,pre_reg8,pre_reg9)  
data_and_predictions_reg9_NM=NM_output_reg9$results
r_rmse_reg9_NM1=NM_output_reg9$error
plot_NM_1_results(data_and_predictions_reg9_NM)

# extract and plot  NM 2 results (must do this after running DIMs)

## region 1
NM2_output_reg1=NM_2(input_data="reg1_data_all_inf_all_yrs_NM_jul22-21.csv",
                     data_and_predictions_reg4_DIM,
                     data_and_predictions_reg7_DIM,
                     data_and_predictions_reg8_DIM,
                     pre_reg1,pre_reg7,pre_reg8,pre_reg9)  
data_and_predictions_reg1_NM2=NM2_output_reg1$results
r_rmse_reg1_NM2=NM2_output_reg1$error
plot_NM_2_results(data_and_predictions_reg1_NM2)
ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg1_all_inf_NM2_all_yrs.png")

## region 7
NM2_output_reg7=NM_2(input_data="reg7_data_all_inf_all_yrs_NM_jul22-21.csv",
                     data_and_predictions_reg4_DIM,
                     data_and_predictions_reg7_DIM,
                     data_and_predictions_reg8_DIM,
                     pre_reg1,pre_reg7,pre_reg8,pre_reg9)  
data_and_predictions_reg7_NM2=NM2_output_reg7$results
r_rmse_reg7_NM2=NM2_output_reg7$error
plot_NM_2_results(data_and_predictions_reg7_NM2)
ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg7_all_inf_NM2_all_yrs.png")

## region 8
NM2_output_reg8=NM_2(input_data="reg8_data_all_inf_all_yrs_NM_jul22-21.csv",
                     data_and_predictions_reg4_DIM,
                     data_and_predictions_reg7_DIM,
                     data_and_predictions_reg8_DIM,
                     pre_reg1,pre_reg7,pre_reg8,pre_reg9)    
data_and_predictions_reg8_NM2=NM2_output_reg8$results
r_rmse_reg8_NM2=NM2_output_reg8$error
plot_NM_2_results(data_and_predictions_reg8_NM2)
ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg8_all_inf_NM2_all_yrs.png")

## region 9 - failed to converge
NM2_output_reg9=NM_2(input_data="reg9_data_all_inf_all_yrs_NM_jul22-21.csv",
                     data_and_predictions_reg4_DIM,
                     data_and_predictions_reg7_DIM,
                     data_and_predictions_reg8_DIM,
                     pre_reg1,pre_reg7,pre_reg8,pre_reg9)    
data_and_predictions_reg9_NM2=NM2_output_reg9$results
r_rmse_reg9_NM2=NM2_output_reg9$error

# extract and plot ensemble model results (must do this after running DIMs)

## region1
data_and_predictions_reg1_ensemble_df=ensemble_model(data_and_predictions_reg1_DIM,data_and_predictions_reg1_NM,
                                                  data_and_predictions_reg1_NM2)
data_and_predictions_reg1_ensemble=data_and_predictions_reg1_ensemble_df$results
plot_ensemble_results(data_and_predictions_reg1_ensemble)
ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg1_all_inf_ensemble_all_yrs.png")

r_rmse_reg1_ensemble=rrmse(data_and_predictions_reg1_ensemble$point_estimate,data_and_predictions_reg1_ensemble$true)

## region 7
data_and_predictions_reg7_ensemble_df=ensemble_model(data_and_predictions_reg7_DIM,data_and_predictions_reg7_NM,
                                                     data_and_predictions_reg7_NM2)
data_and_predictions_reg7_ensemble=data_and_predictions_reg7_ensemble_df$results
plot_ensemble_results(data_and_predictions_reg7_ensemble)
ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg7_all_inf_ensemble_all_yrs.png")

r_rmse_reg7_ensemble=rrmse(data_and_predictions_reg7_ensemble$point_estimate,data_and_predictions_reg7_ensemble$true)

## region 8
data_and_predictions_reg8_ensemble_df=ensemble_model(data_and_predictions_reg8_DIM,data_and_predictions_reg8_NM,
                                                     data_and_predictions_reg8_NM2)
data_and_predictions_reg8_ensemble=data_and_predictions_reg8_ensemble_df$results
plot_ensemble_results(data_and_predictions_reg8_ensemble)
ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg8_all_inf_ensemble_all_yrs.png")

r_rmse_reg8_ensemble=rrmse(data_and_predictions_reg8_ensemble$point_estimate,data_and_predictions_reg8_ensemble$true)

# plot ALL predictions

## region 1
plot_all_results(data_and_predictions_reg1_DIM,data_and_predictions_reg1_NM,data_and_predictions_reg1_NM2,
                 data_and_predictions_reg1_ensemble)

ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg1_all_inf_all_models_all_yrs.png")

## region 7
plot_all_results(data_and_predictions_reg7_DIM,data_and_predictions_reg7_NM,data_and_predictions_reg7_NM2,
                 data_and_predictions_reg7_ensemble)

ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg7_all_inf_all_models_all_yrs.png")

## region 8
plot_all_results(data_and_predictions_reg8_DIM,data_and_predictions_reg8_NM,data_and_predictions_reg8_NM2,
                 data_and_predictions_reg8_ensemble)

ggsave("~/Desktop/Guyana Research/Nowcasting/Figures_aug03-21/reg8_all_inf_all_models_all_yrs.png")


# compute error reduction for best performing NM
## first compute the error rate associated with relying on known cases
r_rmse_reg1b<-sqrt(mean((data_and_predictions_reg1_DIM$known-data_and_predictions_reg1_DIM$true)^2))/(diff(range(data_and_predictions_reg1_DIM$true)))
percent_error_reduction_reg1=((r_rmse_reg1b-r_rmse_reg1_NM2)/r_rmse_reg1b)*100
error_reduction_factor_reg1=r_rmse_reg1b/r_rmse_reg1_NM2

r_rmse_reg4b<-sqrt(mean((data_and_predictions_reg4_DIM$known-data_and_predictions_reg4_DIM$true)^2))/(diff(range(data_and_predictions_reg4_DIM$true)))
percent_error_reduction_reg4=((r_rmse_reg4b-r_rmse_reg4_DIM)/r_rmse_reg4b)*100
error_reduction_factor_reg4=r_rmse_reg4b/r_rmse_reg4_DIM

r_rmse_reg7b<-sqrt(mean((data_and_predictions_reg7_DIM$known-data_and_predictions_reg7_DIM$true)^2))/(diff(range(data_and_predictions_reg7_DIM$true)))
percent_error_reduction_reg7=((r_rmse_reg7b-r_rmse_reg7_NM2)/r_rmse_reg7b)*100
error_reduction_factor_reg7=r_rmse_reg7b/r_rmse_reg7_NM2

r_rmse_reg8b<-sqrt(mean((data_and_predictions_reg8_DIM$known-data_and_predictions_reg8_DIM$true)^2))/(diff(range(data_and_predictions_reg8_DIM$true)))
percent_error_reduction_reg8=((r_rmse_reg8b-r_rmse_reg8_NM2)/r_rmse_reg8b)*100
error_reduction_factor_reg8=r_rmse_reg8b/r_rmse_reg8_NM2

# compare rmse and SD of residuals

