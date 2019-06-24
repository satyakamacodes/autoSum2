closeAllConnections()
rm(list = ls())
cat("\f")

setwd("/media/satyakama/Data/projects/r/autism")

modeling_data <- read.csv("master_data.csv",
                          header = T,
                          sep = ",",
                          stringsAsFactors = T)

#======================================================
# EDA_1
#======================================================

library(waffle)

plot_1 <- waffle(c(round(prop.table(table(modeling_data$class))*100, 1)[2], 
                round(prop.table(table(modeling_data$class))*100, 1)[1]),
                rows = 5, 
                keep = T,
                colors = c("red", "black"),
                title = 'Proportion of individuals with ASD (in red) in the population', 
                legend_pos="bottom")

plot_1

ggsave("plot_1.png",
       width = 20,
       height = 8)

#======================================================


class_renamed <- paste(as.character("ASD"),
                        as.character(modeling_data$class),
                        sep = " - ")

modeling_data_plot_2 <- cbind(modeling_data,
                              class_renamed)

library(ggplot2)

library(ggpubr)

plot_2.1 <- ggplot(modeling_data_plot_2,
                 aes(notice_small_sounds,
                     group = class_renamed,
                     fill = class_renamed)) +
            geom_bar(aes(y = ..prop..,
                         fill = factor(..x..)),
                     stat="count") +
            scale_y_continuous(labels=scales::percent) +
            ylab("Frequency in %") +
            xlab("notice small sounds")+
            facet_grid(~class_renamed)+
            theme(legend.title=element_blank())+ 
            theme(legend.position="none")+ 
  scale_fill_manual(values=c("black", "red"))


plot_2.2 <- ggplot(modeling_data_plot_2,
                 aes(holistic_concentration,
                     group = class_renamed,
                     fill = class_renamed)) +
  geom_bar(aes(y = ..prop..,
               fill = factor(..x..)),
           stat="count") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Frequency in %") +
  xlab("holistic concentration")+
  facet_grid(~class_renamed)+
  theme(legend.title=element_blank())+ 
  theme(legend.position="none")+ 
  scale_fill_manual(values=c("black", "red"))


plot_2.3 <- ggplot(modeling_data_plot_2,
                   aes(multi_tasking,
                       group = class_renamed,
                       fill = class_renamed)) +
  geom_bar(aes(y = ..prop..,
               fill = factor(..x..)),
           stat="count") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Frequency in %") +
  xlab("multi tasking")+
  facet_grid(~class_renamed)+
  theme(legend.title=element_blank())+ 
  theme(legend.position="none")+ 
  scale_fill_manual(values=c("black", "red"))

plot_2.4 <- ggplot(modeling_data_plot_2,
                   aes(interruption_switch_back,
                       group = class_renamed,
                       fill = class_renamed)) +
  geom_bar(aes(y = ..prop..,
               fill = factor(..x..)),
           stat="count") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Frequency in %") +
  xlab("interruption switch back")+
  facet_grid(~class_renamed)+
  theme(legend.title=element_blank())+ 
  theme(legend.position="none")+ 
  scale_fill_manual(values=c("black", "red"))

plot_2.5 <- ggplot(modeling_data_plot_2,
                   aes(read_between_lines,
                       group = class_renamed,
                       fill = class_renamed)) +
  geom_bar(aes(y = ..prop..,
               fill = factor(..x..)),
           stat="count") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Frequency in %") +
  xlab("read between lines")+
  facet_grid(~class_renamed)+
  theme(legend.title=element_blank())+ 
  theme(legend.position="none")+ 
  scale_fill_manual(values=c("black", "red"))

plot_2.6 <- ggplot(modeling_data_plot_2,
                   aes(identify_bored,
                       group = class_renamed,
                       fill = class_renamed)) +
  geom_bar(aes(y = ..prop..,
               fill = factor(..x..)),
           stat="count") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Frequency in %") +
  xlab("identify bored")+
  facet_grid(~class_renamed)+
  theme(legend.title=element_blank())+ 
  theme(legend.position="none")+ 
  scale_fill_manual(values=c("black", "red"))


plot_2.7 <- ggplot(modeling_data_plot_2,
                   aes(recognise_character_intensions,
                       group = class_renamed,
                       fill = class_renamed)) +
  geom_bar(aes(y = ..prop..,
               fill = factor(..x..)),
           stat="count") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Frequency in %") +
  xlab("recognise character intensions")+
  facet_grid(~class_renamed)+
  theme(legend.title=element_blank())+ 
  theme(legend.position="none")+ 
  scale_fill_manual(values=c("black", "red"))


plot_2.8 <- ggplot(modeling_data_plot_2,
                   aes(collect_info,
                       group = class_renamed,
                       fill = class_renamed)) +
  geom_bar(aes(y = ..prop..,
               fill = factor(..x..)),
           stat="count") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Frequency in %") +
  xlab("collect info")+
  facet_grid(~class_renamed)+
  theme(legend.title=element_blank())+ 
  theme(legend.position="none")+ 
  scale_fill_manual(values=c("black", "red"))


plot_2.9 <- ggplot(modeling_data_plot_2,
                   aes(face_reading,
                       group = class_renamed,
                       fill = class_renamed)) +
  geom_bar(aes(y = ..prop..,
               fill = factor(..x..)),
           stat="count") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Frequency in %") +
  xlab("face reading")+
  facet_grid(~class_renamed)+
  theme(legend.title=element_blank())+ 
  theme(legend.position="none")+ 
  scale_fill_manual(values=c("black", "red"))


plot_2.10 <- ggplot(modeling_data_plot_2,
                   aes(recognise_people_intensions,
                       group = class_renamed,
                       fill = class_renamed)) +
  geom_bar(aes(y = ..prop..,
               fill = factor(..x..)),
           stat="count") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Frequency in %") +
  xlab("recognise people intensions")+
  facet_grid(~class_renamed)+
  theme(legend.title=element_blank())+ 
  theme(legend.position="none")+ 
  scale_fill_manual(values=c("black", "red"))

plot_2.11 <- ggplot(modeling_data_plot_2,
                    aes(sex,
                        group = class_renamed,
                        fill = class_renamed)) +
  geom_bar(aes(y = ..prop..,
               fill = factor(..x..)),
           stat="count") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Frequency in %") +
  xlab("sex")+
  facet_grid(~class_renamed)+
  theme(legend.title=element_blank())+ 
  theme(legend.position="none")+ 
  scale_fill_manual(values=c("black", "red"))

plot_2.12 <- ggplot(modeling_data_plot_2,
                    aes(jaundice,
                        group = class_renamed,
                        fill = class_renamed)) +
  geom_bar(aes(y = ..prop..,
               fill = factor(..x..)),
           stat="count") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Frequency in %") +
  xlab("jaundice")+
  facet_grid(~class_renamed)+
  theme(legend.title=element_blank())+ 
  theme(legend.position="none")+ 
  scale_fill_manual(values=c("black", "red"))

plot_2.13 <- ggplot(modeling_data_plot_2, 
                    aes(x = age,
                        group = class_renamed,
                        fill = class_renamed)) + 
  geom_density()+
  scale_y_continuous(labels=scales::percent)+
  ylab("Probability density") +
  xlab("age")+
  facet_grid(~class_renamed)+
  theme(legend.title=element_blank())+ 
  theme(legend.position="none")+ 
  scale_fill_manual(values=c("black", "red"))


plot_2_11 <- ggarrange(plot_2.1, 
                        plot_2.2, 
                        plot_2.3,
                        plot_2.4,
                        plot_2.5,
                        plot_2.6,
                        plot_2.7,
                        ncol = 7,
                        nrow = 1)

plot_2_12 <- ggarrange(plot_2.8, 
                       plot_2.9,
                       plot_2.10,
                       plot_2.11,
                       plot_2.12,
                       plot_2.13,
                       ncol = 6,
                       nrow = 1)

plot_2 <- ggarrange(plot_2_11,
                    plot_2_12,
                    nrow =2)


plot_2


ggsave("plot_2.png",
       width = 20,
       height = 8)

#========================================================
library(dplyr)

data_par_coord <- modeling_data

age_modified <- cut(data_par_coord$age, 3)

levels(age_modified) <- c("17-38",
                          "38.1-59",
                          "59.1-80")

data_par_coord <- cbind(data_par_coord,
                        age_modified)

data_par_coord <- subset(data_par_coord,
                         select = -c(age))

data_par_coord_grouped <- data_par_coord %>% group_by(notice_small_sounds, 
                                                      holistic_concentration, 
                                                      multi_tasking, 
                                                      interruption_switch_back,
                                                      read_between_lines,
                                                      identify_bored,
                                                      recognise_character_intensions,
                                                      collect_info,
                                                      face_reading,
                                                      recognise_people_intensions,
                                                      sex,
                                                      jaundice,
                                                      age_modified,
                                                      class) %>% count()


library(alluvial)

data_par_coord_grouped <- as.data.frame(data_par_coord_grouped)

# 4d
plot_3 <- alluvial( data_par_coord_grouped[,1:14],
                    freq = data_par_coord_grouped$n, 
                    alpha = 0.7,
                    border = NA,
                    xw = 0.1,
                    col=ifelse(data_par_coord_grouped$class == "NO", "grey2", "red"))


plot_3


library(ggalluvial)

ggplot(as.data.frame(data_par_coord_grouped),
       aes(y = n, 
           axis1 = notice_small_sounds, 
           axis2 = holistic_concentration,
           axis3 = multi_tasking,
           axis4 = interruption_switch_back,
           axis5 = read_between_lines,
           axis6 = identify_bored,
           axis7 = recognise_character_intensions,
           axis8 = collect_info,
           axis9 = face_reading,
           axis10 = recognise_people_intensions,
           axis11 = sex,
           axis12 = jaundice,
           axis13 = age_modified,
           axis14 = class)) +
  geom_alluvium(aes(fill = class), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("notice_small_sounds", 
                              "holistic_concentration",
                              "multi_tasking",
                              "interruption_switch_back",
                              "read_between_lines",
                              "identify_bored")) +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1))


# +
#   scale_fill_brewer(type = "qual", palette = "Set1") +
#   ggtitle("UC Berkeley admissions and rejections, by sex and department")



library(Rtsne)

tsne <- Rtsne(modeling_data[,-14],
              dims = 2,
              perplexity=50,
              verbose=TRUE,
              max_iter = 15000,
              check_duplicates = F)

library(ggplot2)

tsne_plot <- data.frame(x = tsne$Y[,1], 
                        y = tsne$Y[,2], 
                        class = modeling_data$class)

ggplot(tsne_plot) + geom_point(aes(x=x,
                                   y=y, 
                                   color=class))












modeling_data_unique <- unique(modeling_data) # Remove duplicates

modeling_data_unique <- as.matrix(modeling_data_unique[,1:13])

set.seed(123) # Set a seed if you want reproducible results

tsne_out <- Rtsne(modeling_data_unique) # Run TSNE















Labels <- modeling_data$class
  
colors = rainbow(length(unique(Labels)))


names(colors) = unique(Labels)

## Executing the algorithm on curated data


# Plotting
plot(tsne$Y, t='n',
     main="Cluster visualization using tsne - Red - {0}, Cyan - {1}",
     xlab = "Dimension 1",
     ylab = "Dimension 2")

text(tsne$Y,
     labels = Labels,
     col=colors[Labels])


#Model building
library(h2o)

h2o.init(nthreads = -1,
         min_mem_size = "11g",
         max_mem_size = "12g")

final.df <- as.h2o(modeling_data)

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

target <- "class"

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
                                balance_classes = F,
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
