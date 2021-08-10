# Built on/Adapted from code provided by Canelle Poirier

# specify define_roles function
define_roles <- function(df, dates, response, predictors = NULL) {
  
  vn <- colnames(df)
  
  if (!dates %in% vn) {
    stop(dates, " not found in data frame")
  }
  
  if (!response %in% vn) {
    stop(response, " not found in data frame")
  }
  
  if (is.null(predictors)) {
    predictors <- colnames(df)[!colnames(df) %in% c(dates, response)]
  }
  
  list(dates = dates, response = response, predictors = predictors)
}


# specify enet function
enet_predict_all_yrs <- function(data, roles, train_period,
                                 na_action = c("omit", "fill"),
                                 choice = c("se", "min"),DIM_or_NM=c("DIM","NM","NM2"),
                                 model="glmnet",
                                 ...) {
  
  # Define the range for monthly predictions
  pred_seq <- seq.Date(as.Date("2007-01-01"), as.Date("2019-12-20"), by="month")
  
  # Define the method for dealing with missing values
  na_action <- match.arg(na_action)
  
  # Ensure that the dates are ordered chronologically 
  data <- data %>% arrange(roles$dates)
  
  # Initialize a progress bar
  progress <- ProgressBar(length(pred_seq))
  
  # Loop through each prediction month
  res <- lapply(pred_seq, function(date_to_pred) {
    
    # Recalculate the progression time
    progress$getProgression()
    
    # Recover the date corresponding to the prediction month 
    target_date <- which(data[[roles$dates]] == date_to_pred)
    
    # Define the bounds of the training period 
    
    start_train <- ifelse(target_date < train_period | train_period == "all",
                          1, target_date - train_period)
    end_train <- ifelse(target_date <= 1, 1, target_date - 1)
    
    # Subset the data from the start of the training period to the target month of prediction 
    data <- data[start_train:target_date, ]
    
    # Either remove NA observations or replace w/ a 0 value
    if (na_action == "omit") {
      to_rm <- colSums(sapply(data, is.na)) > 0
      data <-  data[!to_rm]
      predictors <- roles$predictors[roles$predictors %in% colnames(data)]
    }
    
    if (na_action == "fill") {
      data[is.na(data)] <- 0
      predictors <- roles$predictors
    }
    
    data_scale <- scale(data[,roles$predictors])
    
    data_scale <- data.frame(data$date,data_scale,data[,roles$response])
    
    colnames(data_scale) <- colnames(data)
    
    #data <- data_scale
    
    # Extract the training observations
    train_data <- data_scale[1:(nrow(data_scale)-1), ]
    
    # Extrat the test observations 
    test_data <- data_scale[nrow(data_scale), roles$predictors, drop = FALSE]
    
    # Extract the unscaled test observations
    test_data_unscaled= data[nrow(data), roles$predictors, drop = FALSE]
    
    if (DIM_or_NM=='DIM'){
      train_data$t.final=as.vector(t(test_data_unscaled[1:12])) # replace the converged case counts with case counts known by the target month
    }
    
    else if (DIM_or_NM=='NM1'){
      train_data$t.final=as.vector(t(test_data_unscaled[37:48])) # replace the converged case counts with case counts known by the target month
    }
    else{
      train_data$t.final=as.vector(t(test_data_unscaled[40:51])) # replace the converged case counts with case counts known by the target monnth
    }
    #formula <- paste0(roles$response, "~", paste0(predictors, collapse = "+"))
    
    set.seed(12345)
    
    enet.fit <- cva.glmnet(y=train_data[,roles$response],
                           x=as.matrix(train_data[,predictors]), 
                           nfolds = 3, 
                           grouped = FALSE,
                           standardize = F)
    
    
    #Code below adapted from:
    #https://stackoverflow.com/questions/54803990/extract-the-best-parameters-from-cva-glmnet-object
    
    alpha<-enet.fit$alpha
    error<-sapply(enet.fit$modlist,function(mod) {min(mod$cvm)})
    opt_alpha<-alpha[which.min(error)]
    
    lambdaSE<-sapply(enet.fit$modlist, `[[`, "lambda.1se")
    error<-sapply(enet.fit$modlist, function(mod) {min(mod$cvm)})
    best <- which.min(error)
    lambdaSE=lambdaSE[best]
    
    lambdamin<-sapply(enet.fit$modlist, `[[`, "lambda.min")
    lambdamin=lambdamin[best]
    
    if(choice == "se"){ 
      
      enet.fit.bis <- glmnet(x = as.matrix(train_data[,predictors]),
                             y = train_data[,roles$response], 
                             lambda=lambdaSE,
                             alpha=opt_alpha,
                             standardize = F)
      enet.coef <- as.matrix(coef(enet.fit.bis, lambda = lambdaSE))}
    
    else {enet.fit.bis <- glmnet(x = as.matrix(train_data[,predictors]), 
                                 y = train_data[,roles$response], 
                                 lambda=lambdamin,
                                 alpha = opt_alpha,
                                 standardize = F)
    enet.coef <- as.matrix(coef(enet.fit.bis, lambda=lambdamin))}
    
    res <- train_data[,roles$response] - predict(enet.fit.bis, newx = as.matrix(train_data[,predictors]))
    
    pred <- predict(enet.fit.bis, newx = as.matrix(test_data))
    
    list(month = data_scale[nrow(data_scale), roles$dates],
         obs = data_scale[nrow(data_scale), roles$response],
         pred = pred,
         coef = enet.coef)
  })
  #%>% bind_rows()
  coef <- lapply(res, FUN = function(x){x[[4]]})
  pred_res <- lapply(res, FUN = function(x){x[[3]]}) %>% as.numeric()
  month_res <- lapply(res, FUN = function(x){as.character(x[[1]])}) %>% unlist() %>% as.Date()
  obs_res <- lapply(res, FUN = function(x){x[[2]]}) %>% as.numeric()
  df_res <- data.frame(month_res, obs_res, pred_res)
  colnames(df_res) <- c("month", "obs", "pred")
  return(list(res = df_res,coef=coef))
}

# specify progress bar function

ProgressBar <- setRefClass(
  "ProgressBar",
  fields = list(
    i = "integer",
    total = "integer",
    last_time = "POSIXct",
    duration = "numeric"
  ),
  methods = list(
    initialize = function(n_loop) {
      i <<- 1L
      total <<- as.integer(n_loop)
      last_time <<- Sys.time()
      duration <<- integer()
    },
    getProgression = function() {
      
      duration[i] <<- Sys.time() - last_time
      time_remaining <- round(mean(duration) * (total - i), 1)
      done <- round(i / total * 20)
      # if (i > 1) {
      cat(
        "\r[", rep("#", done), rep("-", 20 - done), "] ",
        done * 5, " % - ",
        "Estimated time remaining: ", time_remaining, " sec\r", sep = "")
      # }
      i <<- i + 1L
      last_time <<- Sys.time()
      
    }
  )
)



# specify progress bar function

ProgressBar <- setRefClass(
  "ProgressBar",
  fields = list(
    i = "integer",
    total = "integer",
    last_time = "POSIXct",
    duration = "numeric"
  ),
  methods = list(
    initialize = function(n_loop) {
      i <<- 1L
      total <<- as.integer(n_loop)
      last_time <<- Sys.time()
      duration <<- integer()
    },
    getProgression = function() {
      
      duration[i] <<- Sys.time() - last_time
      time_remaining <- round(mean(duration) * (total - i), 1)
      done <- round(i / total * 20)
      # if (i > 1) {
      cat(
        "\r[", rep("#", done), rep("-", 20 - done), "] ",
        done * 5, " % - ",
        "Estimated time remaining: ", time_remaining, " sec\r", sep = "")
      # }
      i <<- i + 1L
      last_time <<- Sys.time()
      
    }
  )
)

