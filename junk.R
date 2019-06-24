library(dplyr)


df <- modeling_data

df$age <- cut2(df$age, g = 3)


df <- df[, c(1:3,11, 14)]

# group by combinations and count
df_grouped <- df %>% group_by(small_sounds, 
                              holistic_concentration, 
                              multi_tasking, 
                              switch_back,
                              read_between_lines,
                              listening_bored,
                              recognise_character_intensions,
                              collect_info,
                              recognise_feeling,
                              recognise_people_intensions,
                              sex,
                              jaundice,
                              age,
                              class) %>% count()

# install.packages(c("alluvial"), dependencies = TRUE)
require(alluvial)

# Titanic data
tit <- as.data.frame(df_grouped)

# 4d
alluvial( tit[,1:13], freq=tit$n, border=NA,
          hide = tit$n < quantile(tit$n, .50))



library(Hmisc)

age2 <- cut()



library(ggalluvial)

titanic_wide <- data.frame(tit)

ggplot(data = titanic_wide,
       aes(axis1 = small_sounds, 
           axis2 = holistic_concentration,
           axis3 = multi_tasking,
           axis4 = switch_back,
           axis5 = read_between_lines,
           axis6 = listening_bored,
           axis7 = recognise_character_intensions,
           axis8 = collect_info,
           axis9 = recognise_feeling,
           axis10 = recognise_people_intensions,
           axis11 = sex,
           axis12 = jaundice,
           axis13 = age,
           y = n)) +
  scale_x_discrete(limits = c("small_sounds",
                              "holistic_concentration", 
                              "multi_tasking",
                              "switch_back",
                              "read_between_lines",
                              "listening_bored",
                              "recognise_character_intensions",
                              "sex", "age"), expand = c(.1, .05)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill = class)) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  ggtitle("passengers on the maiden voyage of the Titanic",
          "stratified by demographics and survival") +
  theme(legend.position = 'bottom')

