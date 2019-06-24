closeAllConnections()
rm(list = ls())
cat("\f")

setwd("/media/satyakama/Data/projects/r/autism")

master_data <- read.csv("Autism_Data_Adult_Version.csv",
                        header = T,
                        sep = ",",
                        stringsAsFactors = T,
                        na.strings = c("NA", ""))

master_data <- subset(master_data,
                      select = -c(Ethnicity,
                                  Family_ASD,
                                  Residence,
                                  Used_App_Before,
                                  Why.taken.the.screening,
                                  Score,
                                  Screening.Type,
                                  Language,
                                  User))

master_data_A <- master_data[, 1:11]

master_data_A_new = reshape2::dcast( dplyr::mutate(
  reshape2::melt(master_data_A,
                 id.var="Case.No"),
      value=plyr::mapvalues(value, 
                            c("0","1"),
                            c("YES","NO"))
   ),Case.No~variable)


master_data <- cbind(master_data_A_new,
                     master_data[,12:15])

master_data$Sex <- ifelse(master_data$Sex == "f", 
                          "FEMALE",
                          "MALE")

master_data$Jaundice <- toupper(master_data$Jaundice)

master_data <- subset(master_data,
                      select = -c(Case.No))

master_data <- master_data[complete.cases(master_data),]

master_data[sapply(master_data, is.character)] <- lapply(master_data[sapply(master_data, 
                                                                            is.character)], as.factor)

# A1 = I often notice small sounds when others do not
# A2 = I usually concentrate more on the whole picture rather than the small details
# A3 = I find it easy to do more than one thing at once
# A4 = If there is an interruption, I can switch back to what I was doing very quickly
# A5 = I find it easy to ‘read between the lines’ when someone is talking to me
# A6 = I know how to tell if someone listening to me is getting bored
# A7 = When I’m reading a story I find it difficult to work out the characters’ intentions
# A8 = I like to collect information about categories of things (e.g. types of car, types of bird, types of train, types of plant, etc)
# A9 = I find it easy to work out what someone is thinking or feeling just by looking at their face
# A10 = I find it difficult to work out people’s intentions


colnames(master_data) <- c("notice_small_sounds",
                           "holistic_concentration",
                           "multi_tasking",
                           "interruption_switch_back",
                           "read_between_lines",
                           "identify_bored",
                           "recognise_character_intensions",
                           "collect_info",
                           "face_reading",
                           "recognise_people_intensions",
                           "age",
                           "sex",
                           "jaundice",
                           "class")
write.table(master_data,
            file = "master_data.csv",
            append = F,
            quote = F,
            row.names = F,
            sep = ",",
            dec = ".")


