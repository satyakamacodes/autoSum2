cat("\f")

modeling_data <- read.csv("churn.csv",
                          header = T,
                          sep = ",",
                          na.strings = c("", "NA"))

dim(modeling_data)

modeling_data <- modeling_data[complete.cases(modeling_data),]

str(modeling_data)

modeling_data$churn <- ifelse(modeling_data$churn =="Yes", "leaving", "not_leaving")

modeling_data$churn <- as.factor(modeling_data$churn)

modeling_data <- modeling_data[complete.cases(modeling_data),]



# (prop.table(table(modeling_data$churn))*100)[1]

print(require(ggplot2))

# print(require(scales))

ggplot(modeling_data,
       aes(x = churn,
           fill = churn)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", 
            vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Percentage of customers leaving and not leaving", 
       y = "Percent", 
       x = "Churn")


set.seed(100)

splitSample <- sample(1:2,
                      size=nrow(modeling_data),
                      prob=c(0.7,0.3),
                      replace = T)

training_data <- modeling_data[splitSample==1,]

cat("The number of rows and columns in training_data are", 
    dim(training_data)[1],
    "and",
    dim(training_data)[2],
    "respectively.",
    "\n")

# validation_data <- modeling_data[splitSample==2,]
# 
# cat("The number of rows and columns in validation_data are", 
#     dim(validation_data)[1],
#     "and",
#     dim(validation_data)[2],
#     "respectively.",
#     "\n")

testing_data <- modeling_data[splitSample==2,]

cat("The number of rows and columns in testing_data are", 
    dim(testing_data)[1],
    "and",
    dim(testing_data)[2],
    "respectively.",
    "\n")

print(require(caret))

ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, 
                     repeats = 3, 
                     verboseIter = FALSE,
                     sampling = "smote")

set.seed(42)

model_rf_smote <- caret::train(churn ~ .,
                               data = training_data,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)


final_smote <- data.frame(actual = testing_data$churn,
                          predict(model_rf_smote,
                                  newdata = testing_data, 
                                  type = "prob"))


saveRDS(final_smote, "final_smote.rds")

fetched_model <- readRDS("final_smote.rds")


# final_smote$predict <- ifelse(final_smote$benign > 0.5, "benign", "malignant")

final_smote$predict <- ifelse(fetched_model$leaving > 0.5, 
                              "leaving", 
                              "not_leaving")


cm_smote <- confusionMatrix(as.factor(fetched_model$predict), 
                            as.factor(testing_data$churn))

cm_smote



# final_under <- data.frame(actual = testing_data$churn,
#                           predict(model_rf_under, 
#                                   newdata = testing_data, 
#                                   type = "prob"))
# 
# final_under$predict <- ifelse(final_under$leaving > 0.5, "leaving", "not_leaving")
# cm_under <- confusionMatrix(final_under$predict, testing_data$churn)

# print(require(h2o))
# 
# h2o.init(nthreads = -1,
#          min_mem_size = "8g",
#          max_mem_size = "10g")
# 
# options("h2o.use.data.table"=TRUE)
# 
# # modeling_data <- as.h2o(modeling_data)
# #
# # split_h2o <- h2o.splitFrame(modeling_data,
# #                             c(0.7,
# #                               0.1),
# #                             seed = 1234 )
# 
# training_h2o_df <- as.h2o(training_data)
# 
# 
# h2o.dim(training_h2o_df)
# 
# # validation_h2o_df <- as.h2o(validation_data)
# # 
# # h2o.dim(validation_h2o_df)
# 
# testing_h2o_df <- as.h2o(testing_data)
# 
# h2o.dim(testing_h2o_df)
# 
# 
# target <- "churn"
# 
# predictors <- setdiff(names(training_h2o_df),
#                       target)
# 
# predictors
# 
# start.time <- print(Sys.time())
# 
# minutes <- 15
# 
# automl_higgs_px <- h2o.automl(x = predictors,
#                               y = target,
#                               training_frame    = training_h2o_df,
#                               leaderboard_frame = testing_h2o_df,
#                               nfolds = 5,
#                               balance_classes = T,
#                               max_after_balance_size = 1,
#                               max_runtime_secs = 60*minutes,
#                               max_models = 500,
#                               stopping_metric = "AUTO",
#                               seed = 123,
#                               sort_metric = "AUTO")
# 
# end.time <- print(Sys.time())
# 
# beepr::beep(3)
# # 
# # 
# # # g <- ggplot(modeling_data, aes(churn))
# # # 
# # # g + geom_bar()
# # 
# # # ggplot(data = modeling_data) +
# # #   geom_bar(mapping = aes(x = churn, y = ..prop..), stat = "count")
# # # 
# # # ggplot(modeling_data, 
# # #        aes(x= churn, y = churn)) +
# # #   geom_bar(stat="identity") +
# # #   scale_y_continuous(labels = scales::percent)
# # # 
# # # # ggplot(modeling_data, 
# # # #        aes(x=total_intl_charge,
# # # #            fill=churn)) +
# # # #   geom_density(alpha = 0.15)
# # # # 
# # # # plot(modeling_data$total_intl_charge)
# # # # 
# # # # 
# # # # barplot(prop.table(table(modeling_data$churn)))
# # # # 
# # # 
# # # 
# # # # barplot(modeling_data$churn)
# # # 
# # # # print(require(Boruta))
# # # # 
# # # # set.seed(1234)
# # # # 
# # # # boruta.train <- Boruta(churn ~.,
# # # #                        data = master_data,
# # # #                        doTrace = 2,
# # # #                        maxRuns = 500)
# # # # 
# # # # final.boruta <- TentativeRoughFix(boruta.train)
# # # # 
# # # # final.selected.features <- getSelectedAttributes(final.boruta)
# # # 
# # # 
# # # # print(require(dplyr))
# # # # 
# # # # # print(require(reshape2))
# # # # 
# # # # 
# # # # 
# # # # # master_data_id <- mutate(master_data_num,
# # # # #                          id=as.numeric(rownames(master_data_num)))
# # # # # 
# # # # # master_data_stack <- melt(master_data_id,
# # # # #                           id="id")
# # # # # 
# # # # # print(require(dplyr))
# # # # 
# # # # master_data_num <- select_if(master_data, 
# # # #                              is.numeric)
# # # # 
# # # # str(modeling_data)
# # # # 
# # # # 
# # # # print(require(ggplot2))
# # # # 
# # # # 
# # # # ggdensity(modeling_data, 
# # # #           x = colnames(master_data_num), 
# # # #           y = "..density..",
# # # #           fill = "churn",
# # # #           add = "mean", 
# # # #           rug = TRUE,
# # # #           combine = F)
# # # # 
# # # # p <- ggplot(modeling_data, 
# # # #             aes(x= c(total_intl_calls),
# # # #                 fill = churn )) + 
# # # #   geom_density(alpha = 0.3)
# # # # p
# # # # 
# 
# h2o.shutdown()

h2o.shutdown()
