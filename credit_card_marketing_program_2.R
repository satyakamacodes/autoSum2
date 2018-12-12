closeAllConnections()
rm(list = ls())
cat("\f")

setwd("/home/satyakama/Documents/business/credit_card_marketing")

master_data <- read.csv("creditcardmarketing-bbm.csv",
                        header = T,
                        sep = ",",
                        stringsAsFactors = T,
                        na.strings = c("NA", 
                                       ""))

str(master_data)

master_data <- subset(master_data,
                      select = -Customer.Number)

prop.table(table(master_data$Offer.Accepted))

set.seed(100)

print(require(caret))

splitSample <- sample(1:3,
                      size=nrow(master_data),
                      prob=c(0.7,0.15,0.15),
                      replace = T)

training_data <- master_data[splitSample==1,]

cat("The number of rows and columns in training_data are", 
    dim(training_data)[1],
    "and",
    dim(training_data)[2],
    "respectively.",
    "\n")

validation_data <- master_data[splitSample==2,]

cat("The number of rows and columns in validation_data are", 
    dim(validation_data)[1],
    "and",
    dim(validation_data)[2],
    "respectively.",
    "\n")

testing_data <- master_data[splitSample==3,]

cat("The number of rows and columns in testing_data are", 
    dim(testing_data)[1],
    "and",
    dim(testing_data)[2],
    "respectively.",
    "\n")

print(require(h2o))

h2o.init(nthreads = -1,
         min_mem_size = "8g",
         max_mem_size = "10g")

options("h2o.use.data.table"=TRUE)

training_h2o_df <- as.h2o(training_data)

h2o.dim(training_h2o_df)

validation_h2o_df <- as.h2o(validation_data)

h2o.dim(validation_h2o_df)

testing_h2o_df <- as.h2o(testing_data)

h2o.dim(testing_h2o_df)

target <- "Offer.Accepted"

predictors <- setdiff(names(training_h2o_df), 
                      target)

predictors

start.time <- print(Sys.time())

automl_h2o_models <- h2o.automl(x = predictors, 
                                y = target,
                                training_frame    = training_h2o_df,
                                leaderboard_frame = validation_h2o_df,
                                nfolds = 5, 
                                max_runtime_secs = 60*15,
                                max_models = 500,
                                stopping_metric = "RMSE",
                                exclude_algos = c("StackedEnsemble", "GLM", "GBM", "DRF"),
                                seed = 123,
                                sort_metric = "RMSE")

end.time <- print(Sys.time())

automl_h2o_models 

automl_h2o_all_models <- as.data.frame(automl_h2o_models@leaderboard)

automl_h2o_all_models

automl_best_model <- automl_h2o_models@leader

automl_best_model

#automl performance on test

perf_automl_test <- h2o.predict(automl_best_model, 
                                testing_h2o_df)

options("datatable.verbose"= FALSE)
options("h2o.use.data.table"=TRUE)

perf_automl_test <- as.data.frame(perf_automl_test)

perf_automl_test

# colnames(perf_automl_test) <- c("predicted_y_n")
# 
# perf_automl_test$predicted_y_n <- as.numeric(perf_automl_test$predicted_y_n)
# 
# predicted_y <- (perf_automl_test$predicted_y_n*(max(training_data$higgs_pz) - min(training_data$higgs_pz)))+min(training_data$higgs_pz)
# 
# comp_df <- data.frame(cbind(testing_data$higgs_pz,
#                             predicted_y))
# 
# colnames(comp_df) <- c("actual_y",
#                        "predicted_y")
# 
# #Sorting the values of actual_y by ascending order
# 
# comp_df <- comp_df[order(comp_df$actual_y),]
# 
# serial_no <- seq(1, dim(comp_df)[1], 1)
# 
# comp_df <- cbind(comp_df, serial_no)