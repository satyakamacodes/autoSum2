closeAllConnections()
rm(list = ls())

cat("\f")

setwd("C:/Users/Paul B/Documents/R_projects/insurance_fraud")

m.data <- read.csv("claims.csv",
                   header = T,
                   sep = ",",
                   stringsAsFactors = T,
                   na.strings = c("", "NA"))

dim(m.data)

sum(is.na(m.data))

str(m.data)

m.data <- subset(m.data,
                 select = -c(PolicyNumber,
                             RepNumber))

m.data$WeekOfMonthClaimed <- as.factor(m.data$WeekOfMonthClaimed)
m.data$FraudFound_P <- as.factor(m.data$FraudFound_P)

print(require(h2o))

h2o.init(nthreads = -1,
         min_mem_size = "4g",
         max_mem_size = "6g")

final.df <- as.h2o(m.data)

split_h2o <- h2o.splitFrame(final.df,
                            c(0.7,
                              0.1),
                            seed = 1234 )


train_h2o <- h2o.assign(split_h2o[[1]],
                        "train" )
h2o.dim(train_h2o)

valid_h2o <- h2o.assign(split_h2o[[2]],
                        "valid" )
h2o.dim(valid_h2o)

test_h2o  <- h2o.assign(split_h2o[[3]],
                        "test" )
h2o.dim(test_h2o)

target <- "FraudFound_P"

predictors <- setdiff(names(train_h2o),
                      target)

predictors

start <- print(Sys.time())

automl_h2o_models <- h2o.automl(x = predictors,
                                y = target,
                                training_frame    = train_h2o,
                                leaderboard_frame = valid_h2o,
                                nfolds = 5,
                                balance_classes = T,
                                max_runtime_secs = 60*15,
                                project_name = "seven",
                                max_after_balance_size = 10,
                                sort_metric = "AUC",
                                stopping_metric = "AUC")
end.time <- Sys.time()

time.taken <- print(end.time - start)

beepr::beep(3)



leadership_board <- automl_h2o_models@leaderboard

print(leadership_board)

automl_leader <- automl_h2o_models@leader

automl_leader

saved.model <- h2o.saveModel(object = automl_leader,
                             path = getwd(),
                             force = T)

fetched.model <- h2o.loadModel(saved.model)

h2o.varimp_plot(automl_leader)


prediction_on_test <- h2o.predict(object = fetched.model,
                                  newdata = test_h2o)

model_performance_on_test <- h2o.performance(automl_leader,
                                             test_h2o)

print(model_performance_on_test)
