cat("\f")
closeAllConnections()
rm(list = ls())


setwd("C:/Users/satyakama.paul/Documents/R_projects/organics")

master_data <- read.csv("organics.csv",
                        sep = ",",
                        header = T,
                        na.strings = c("", "NA"),
                        stringsAsFactors = T)

str(master_data)

#Renaming variables

colnames(master_data) <- c("sl_no.",
                           "cust_id.",
                           "gender",
                           "dob",
                           "date_of_record_extraction",
                           "age",
                           "age_group_1",
                           "age_group_2",
                           "tv_reg.",
                           "n_gr.",
                           "n_ty.",
                           "localty_card_app._date",
                           "no._org_purc.",
                           "amt._spent",
                           "geog_reg.",
                           "loy._class",
                           "whe._org_purc.",
                           "infl._gr.",
                           "time_loy._mem.")

master_data$whe._org_purc. <- as.factor(master_data$whe._org_purc.)




#First iteration of removing variables that are not important
master_data <- subset(master_data,
                      select = -c(sl_no.,
                                  cust_id.,
                                  dob,
                                  date_of_record_extraction,
                                  age_group_1,
                                  age_group_2,
                                  localty_card_app._date,
                                  no._org_purc.))


#master_data <- master_data[complete.cases(master_data),]

print(require(dplyr))

View(master_data)

# master_data <- sample_n(master_data, 
#                         5000)

print(require(h2o))
h2o.init(nthreads = 4,
         min_mem_size = "8g",
         max_mem_size = "10g")

master_data <- as.h2o(master_data)


split_h2o <- h2o.splitFrame(master_data, c(0.6, 
                                           0.2), 
                            seed = 1234 )


train_h2o <- h2o.assign(split_h2o[[1]], "train" ) # 60%
dim(train_h2o)
valid_h2o <- h2o.assign(split_h2o[[2]], "valid" ) # 20%
dim(valid_h2o)
test_h2o  <- h2o.assign(split_h2o[[3]], "test" )  # 20%
dim(test_h2o)

target <- "whe._org_purc."
predictors <- setdiff(names(train_h2o), 
                      target)


# Run the automated machine learning 
automl_h2o_models <- h2o.automl(
     x = predictors, 
     y = target,
     training_frame    = train_h2o,
     leaderboard_frame = valid_h2o,
     balance_classes = T,
     stopping_metric = c("AUC"),
     max_runtime_secs = 60*3
)
print(require(beepr))

beep("mario")


automl_leader <- automl_h2o_models@leader

automl_leader



pred_conversion <- h2o.predict(object = automl_leader, 
                               newdata = test_h2o)




h2o.table(pred_conversion$predict, 
          test_h2o$whe._org_purc.)


perf <- h2o.performance(automl_leader,test_h2o)

plot(perf)

h2o.confusionMatrix(perf)
h2o.accuracy(perf)
h2o.tpr(perf)

h2o.shutdown(prompt = T)

# print(require(doParallel))
# print(require(missForest))
# 
# registerDoParallel(cores=4)
# m.imp <- missForest(master_data,
#                     parallelize = "variables",
#                     verbose = T)



# print(require(Amelia))

# missmap(master_data)
# 
# print(require(mice))

# print(require(VIM))
# 
# m_plot <- aggr(master_data, col=c('navyblue','yellow'),
#                numbers=TRUE, sortVars=TRUE,
#                labels=names(master_data), cex.axis=.7,
#                gap=3, ylab=c("Missing data","Pattern"))
# 
# print(require(missForest))
# 
# iris.imp <- missForest(master_data)

