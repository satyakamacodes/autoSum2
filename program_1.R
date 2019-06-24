closeAllConnections()
rm(list = ls())

setwd("C:/Users/satyakama.paul/Documents/R_projects/Autism/ASD-Adult-data-Version-2/ASD Adult data Version 2")

master_data <- read.csv("Autism_Data_Adult_Version 2.csv",
                        header = T,
                        sep = ",",
                        stringsAsFactors = T,
                        na.strings = c("NA", ""))

str(master_data)

master_data <- subset(master_data,
                      select = -c(Case.No,
                                  Ethnicity,
                                  Residence,
                                  Used_App_Before,
                                  Why.taken.the.screening,
                                  Score,
                                  Screening.Type,
                                  User))

str(master_data)

# A1 = I often notice small sounds when others do not
# A2 = I usually concentrate more on the whole picture rather than the small details
# A3 = I find it easy to do more than one thing at once
# A4 = If there is an interruption, I can switch back to what I was doing very quickly
# A5 = I find it easy to 'read between the lines' when someone is talking to me
# A6 = I know how to tell if someone listening to me is getting bored
# A7 = When I'm reading a story I find it difficult to work out the characters' intentions
# A8 = I like to collect information about categories of things (e.g. types of car, types of bird, types of train, types of plant, etc)
# A9 = I find it easy to work out what someone is thinking or feeling just by looking at their face
# A10 = I find it difficult to work out people's intentions


#Changing the nature of certain variables

cols.factor <- paste("A", 
                     seq(1,10,1), 
                     sep ="")

master_data[cols.factor] <- lapply(master_data[cols.factor],
                                   as.factor)

#Romila shorten code

master_data$A1 <- ifelse(master_data$A1 == "1", 
                           "YES",
                           "NO")
master_data$A2 <- ifelse(master_data$A2 == "1", 
                         "YES",
                         "NO")
master_data$A3 <- ifelse(master_data$A3 == "1", 
                         "YES",
                         "NO")
master_data$A4 <- ifelse(master_data$A4 == "1", 
                         "YES",
                         "NO")
master_data$A5 <- ifelse(master_data$A5 == "1", 
                         "YES",
                         "NO")
master_data$A6 <- ifelse(master_data$A6 == "1", 
                         "YES",
                         "NO")
master_data$A7 <- ifelse(master_data$A7 == "1", 
                         "YES",
                         "NO")
master_data$A8 <- ifelse(master_data$A8 == "1", 
                         "YES",
                         "NO")
master_data$A9 <- ifelse(master_data$A9 == "1", 
                         "YES",
                         "NO")
master_data$A10 <- ifelse(master_data$A10 == "1", 
                         "YES",
                         "NO")

#Changing column values from lower to upper case

master_data <- data.frame(lapply(master_data, 
                                 function(v) {
                              if (is.factor(v)) 
                                   return(toupper(v))
                              else return(v)
                         }))

master_data$Sex <- ifelse(master_data$Sex == "F",
                          "FEMALE",
                          "MALE")


View(master_data)



sum(is.na(master_data))

master_data <- master_data[complete.cases(master_data),]



colnames(master_data)

#Model building
library(h2o)

h2o.init(nthreads = -1,
         min_mem_size = "10g",
         max_mem_size = "12g")

final.df <- as.h2o(master_data)

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

target <- "Class"

predictors <- setdiff(names(train_h2o),
                      target)

predictors

start <- print(Sys.time())

automl_h2o_models <- h2o.automl(x = predictors,
                                y = target,
                                training_frame    = train_h2o,
                                validation_frame = valid_h2o,
                                leaderboard_frame = valid_h2o,
                                nfolds = 5,
                                balance_classes = T,
                                max_runtime_secs = 60*15,
                                max_models = 500,
                                seed = 1234,
                                project_name = as.character(Sys.time()),
                                sort_metric = "mean_per_class_error",
                                stopping_metric = "mean_per_class_error")
end.time <- Sys.time()

time.taken <- print(end.time - start)


leadership_board <- automl_h2o_models@leaderboard

print(leadership_board)

automl_leader <- automl_h2o_models@leader

automl_leader

saved.model <- h2o.saveModel(object = automl_leader,
                             path = getwd(),
                             force = T)

fetched.model <- h2o.loadModel(saved.model)

h2o.varimp_plot(automl_leader,
                num_of_features = 16)


prediction_on_test <- h2o.predict(object = fetched.model,
                                  newdata = test_h2o)

model_performance_on_test <- h2o.performance(automl_leader,
                                             test_h2o)

print(model_performance_on_test)


#Test on new data for ex: ASD patient

new.data_1 <- data.frame(A1 = as.factor("YES"),
                       A2 = as.factor("YES"),
                       A3 = as.factor("YES"),
                       A4 = as.factor("YES"),
                       A5 = as.factor("YES"),
                       A6 = as.factor("YES"),
                       A7 = as.factor("YES"),
                       A8 = as.factor("YES"),
                       A9 = as.factor("YES"),
                       A10 = as.factor("YES"),
                       Age = as.integer(43),
                       Sex = as.factor("MALE"),
                       Jaundice = as.factor("YES"),
                       Family_ASD = as.factor("YES"),
                       Language = as.factor("ENGLISH"))
                       
                       
new.data_1 <- as.h2o(new.data_1)

new.pred_1 <- h2o.predict(object = fetched.model,
                        newdata = new.data_1)

new.pred_1

#Test on new data for ex: control

new.data_2 <- data.frame(A1 = as.factor("NO"),
                         A2 = as.factor("NO"),
                         A3 = as.factor("NO"),
                         A4 = as.factor("NO"),
                         A5 = as.factor("NO"),
                         A6 = as.factor("NO"),
                         A7 = as.factor("NO"),
                         A8 = as.factor("NO"),
                         A9 = as.factor("NO"),
                         A10 = as.factor("NO"),
                         Age = as.integer(43),
                         Sex = as.factor("MALE"),
                         Jaundice = as.factor("YES"),
                         Family_ASD = as.factor("NO"),
                         Language = as.factor("ENGLISH"))


new.data_2 <- as.h2o(new.data_2)

new.pred_2 <- h2o.predict(object = fetched.model,
                          newdata = new.data_2)

new.pred_2

#=========================================================
# Explanable AI
explainer <- lime::lime(x = as.data.frame(train_h2o),
                        model = fetched.model)

explanations <- lime::explain(x = as.data.frame(new.data_2),
                              explainer = explainer,
                              n_permutations = 500,
                              feature_select = "auto",
                              n_labels = 1,
                              n_features = length(predictors))

lime::plot_features(explanations, ncol = 2)
