#---------------------------------------------------------------------------------
# A function for creating the codebook using the archive chromebook TV MRF dataset
#---------------------------------------------------------------------------------
# create an html codebook
CreateQuestBook <- function(raw_sav_data) {
  # initiate an empty vector to gather the questions from the survey
  questions <- character()
  values_desc <- character()
  
  # use for loop to gather all the questions and statement
  attach(raw_sav_data)
  for (i in 1:length(names(raw_sav_data))) {
    # gathering the labels
    questions_label <- attr(get(names(raw_sav_data)[i]), 'label')
    if (is.null(questions_label)) {
      questions[i] <- "NONE"
    } 
    else {
      questions[i] <- questions_label
    }
    
    values_label <- attr(get(names(raw_sav_data)[i]), 'labels')
    
    # inner loop to concantenate the values coded desc into a single string
    valout <- character() # initiate value output as valout
    for (j in 1:length(values_label)) {
      valout <- paste(valout, paste0(names(values_label)[j], ": ", values_label[[j]])) %>%
        trimws("left")
    }
    # saved the value output for each iteration of i into values_desc
    values_desc[i] <- valout
    
  }
  detach(raw_sav_data)
  
  # create a dataframe to hold the questions and statements
  questbook <- data.frame(questions, values_desc, stringsAsFactors = FALSE) %>%
    mutate(QNumber = names(raw_sav_data)) %>%
    select(QNumber, questions, values_desc)
  
  names(questbook) <- c('QNumber', 'Statement', 'Coded Value')
  
  # return the dataframe
  questbook
}

#------------------------------------------------------
## MRF Regression function
#-----------------------------------------------------
MRF_Regression_Calculation <- function(x_data, y_data, accuracy = 17) {
  
  ## Residual sum function
  residual_sum_function <- function(parameterset, x_data, y_data){
    
    ym = parameterset[1]
    a = parameterset[2]
    b = parameterset[3]
    
    return(sum ((y_data-( ym/(1+exp(a-b*x_data))))^2))
  }
  
  ## Start point function
  start_points <- function(goal){ 
    
    start_point_matrix <- expand.grid (ym = seq (0,100, goal),
                                       a = seq (-10,10, goal),
                                       b = seq (0,20, goal))
    return (start_point_matrix)
  }
  
  
  start_point <- start_points(accuracy)
  
  
  cat(nrow(start_point)," Runs --- estimated duration: ", round((nrow(start_point)*0.8),0), " seconds \n" )
  
  bestframe <- data.frame() 
  for (i2 in 1:nrow(start_point)){
    
    pari <- c(start_point$ym[i2],
              start_point$a[i2],
              start_point$b[i2])
    
    optimx_output <- optimx(par=pari, fn = residual_sum_function,
                            lower = c(0, -Inf, 0), upper = c(100, Inf, Inf),
                            control = list(all.methods=TRUE, trace=FALSE),
                            x_data=x_data, y_data=y_data)
    
    names(optimx_output)[1:4] <- c("lm","a","b","sumofsquares")
    
    best_parameter <- (optimx_output[order(optimx_output$sumofsquares),][1:4][1,])
    
    bestframe <- rbind (bestframe,best_parameter)
    
    cat("Run ", i2, " of " ,nrow(start_point)) 
  }
  
  cat ("\n \n done \n") 
  
  return(bestframe[order(bestframe$sumofsquares),][1,])
}  

#---------------------------------------------------
# MRF Performance Function
#----------------------------------------------------
MRF_KPI_Performance_Function <- function(kpi_data) {
  
  results=list()
  for(i in 1:length(kpi_data)){
    cat("Model ", i, " of ",length(kpi_data),":      ") 
    
    x=kpi_data[[i]][,1] 
    y=kpi_data[[i]][,2]
    
    results[[i]] <- MRF_Regression_Calculation(x_data=x, y_data=y, accuracy = 17) #set "accuracy" to 21 for quick testing, set to 17 for final analysis
  }
  return(results)
}

#---------------------------------------------------
# R-square function
#----------------------------------------------------
r_squared_function <- function  (parameters = t(results[[i]])[1:3,1] , data = kpi_list[[i]] ){
  
  ym <- parameters[1]
  a <- parameters[2]
  b <- parameters[3]
  x <- data[,1]
  
  y_estimate <- ym/(1+exp(a-b*x))
  y_actual <- data[,2] 
  
  residual_variation <- (y_actual-y_estimate)^2
  variation_y <- (y_actual-mean(y_actual))^2
  
  r_squared<- 1-(sum(residual_variation)/sum(variation_y))
  
  return (r_squared)
}