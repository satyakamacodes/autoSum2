closeAllConnections()
rm(list=ls())

setwd("C:\\Users\\u264800\\Downloads")

m.data <- read.csv("claims.csv",
                   header = T,
                   sep = ",",
                   stringsAsFactors = T)

dim(m.data)

print(require(Amelia))
missmap(m.data)

names(m.data)

prop.table(table(m.data$FraudFound_P))*100

#Removing unimportant variable(s)

m.data <- subset(m.data,
                 select = -c(PolicyNumber))
dim(m.data)

#Sending the response column to the extreme bottom
print(require(dplyr))
m.data <- m.data %>% select(-FraudFound_P, everything())

names(m.data)
str(m.data)


m.data$FraudFound_P <- as.factor(m.data$FraudFound_P)

#Feature selection done on a random sample of 25% of m.data

set.seed(100)
sample.f.fs <- m.data[sample(1:nrow(m.data), 
                             (nrow(m.data)*0.25),
                             replace=FALSE),
                      ]

dim(sample.f.fs)

print(require(Boruta))

set.seed(100)

start.time <- Sys.time()

tent.imp.features <- Boruta(FraudFound_P~.,
                            data = sample.f.fs,
                            maxRuns = 100, 
                            doTrace = 2)

print(tent.imp.features)

end.time <- Sys.time()

fs.time <- print(end.time - start.time)

final.imp.features <- TentativeRoughFix(tent.imp.features)
print(final.imp.features)

final.selected.features <- print(getSelectedAttributes(final.imp.features),
                                 withTenative = T)

imp.xf = NULL

for (i in 1:length(final.selected.features)){
     
     imp.xf[i] = paste0(final.selected.features[i])
     
}

imp.f <- c(imp.xf,
           "FraudFound_P")

imp.xy <- subset(m.data,
                 select = imp.f)

modeling.data <- data.frame(imp.xy)

#Balanced sampling

print(require(ROSE))

data.balanced.both <- ovun.sample(FraudFound_P  ~ ., 
                                  data = modeling.data, 
                                  method = "both", 
                                  p=0.5,      
                                  N=10000, seed = 1)$data


print(require(caret))


set.seed(100)

inTrain <- createDataPartition(y = data.balanced.both$FraudFound_P,
                               p = 0.7,
                               list = F)

training.data <- data.balanced.both[inTrain,]

training.x <- subset(training.data,
                     select = -c(FraudFound_P))

training.y <- training.data$FraudFound_P

testing.data <- data.balanced.both[-inTrain,]

testing.x <- subset(testing.data,
                    select = -c(FraudFound_P))

testing.y <- testing.data$FraudFound_P


print(require(randomForest))

set.seed(100)

print(require(randomForest))

rf1 <- randomForest(FraudFound_P ~.,
                   data = training.data)

print(rf1)




#Confusion matrix for the test data with default values
test.pred <- predict(rf1,
                     testing.data)

confusionMatrix(test.pred,
                testing.y)

plot(rf1)


tuning <- tuneRF(training.x,
                 training.y,
                 stepFactor = 1,
                 plot = T,
                 ntreeTry = 300,
                 trace = T,
                 improve = 0.1)


rf2 <- randomForest(FraudFound_P ~.,
                    data = training.data,
                    mtry = 8,
                    ntree = 300,
                    importance = T,
                    proximity = T)



#Confusion matrix for the test data with tuned values
test.pred2 <- predict(rf2,
                     testing.data)

cm2 <- confusionMatrix(test.pred,
                       testing.y)

print(cm2)

t2 <- cm2$table

overall.misclassification <- ((t2[1,2]+t2[2,1])/length(testing.y))*100

print(overall.misclassification)

actual.fraud.but.not.deteched.as.fraud.per1000 <- ((t2[1,2])/length(testing.y))*1000

print(actual.fraud.but.not.deteched.as.fraud.per1000)


not.fraud.but.deteched.as.fraud.per1000 <- ((t2[2,1])/length(testing.y))*1000
print(not.fraud.but.deteched.as.fraud.per1000)



