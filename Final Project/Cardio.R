library(ggplot2)
library(funModeling)
library(Hmisc)
library(dplyr)

cardio <- read.csv("CardioGoodFitness.csv")

attach(cardio)

str(cardio)

#-----------------------------------------------------------------------------------------
#UNIVARIATE

#CATEGORICAL

ggplot(data=cardio)+geom_bar(mapping = aes(x=Gender, fill=Gender))

#-----------------------------------------------------------------------------------------

#NUMERICAL

summary(cardio$Fitness)

ggplot(data = cardio, mapping = aes(x = Fitness)) + geom_histogram(bins = 5, fill = "grey") +geom_freqpoly(binwidth= 1, color = "red") + theme_light()
ggplot(data = cardio, mapping = aes(x = Fitness)) + geom_boxplot()

hist(cardio$Fitness)

#-----------------------------------------------------------------------------------------

#BIVARIATE

#CATEGORICAL & NUMERICAL

ggplot(cardio, aes(x = Product, y = Usage)) + 
  geom_bar(stat = "identity") +
  labs(x = "Product", y = "Usage")

#-----------------------------------------------------------------------------------------

#NUMERICAL & NUMERICAL

g <- ggplot(cardio) + geom_jitter(aes(y = Miles, x = Fitness))
plot(g)

#-----------------------------------------------------------------------------------------

#PROFILING

cardio_data <- df_status(cardio)
cardio_data

#-----------------------------------------------------------------------------------------

ggplot(cardio, aes(x=Usage, fill=Product)) + 
  geom_bar() +
  labs( x="Product", y="Number of User", title="Product User by Gender")

#VISUALIZATION 1

ggplot(cardio, aes(x=Usage, fill=MaritalStatus))+
  geom_histogram(bins=5, colour="#1380A1", position = "dodge") +
  labs(title="Marital Status Rate by Gender", y="Number of User", subtitle = "Distribution by usage, gender and product")+
  theme_bw() +
  facet_grid(Gender~Product, scales="free")

#-----------------------------------------------------------------------------------------

#VISUALIZATION 2

ggplot(cardio, aes(Fitness, Miles)) + 
  geom_jitter(aes(colour = Product)) + 
  geom_smooth(method="lm", se=FALSE) + 
  labs(title="Fitness VS Miles", 
       y="Miles", 
       subtitle = "Distribution by fitness, miles and product") +
  theme(plot.title = element_text(size=20,
                                  face="bold",
                                  family="American Typewriter",
                                  color="tomato",
                                  hjust=0.5,
                                  lineheight = 1.2),
        plot.subtitle=element_text(size=15,
                                   family="American Typewriter",
                                   face="bold",
                                   hjust=0.5))

#-----------------------------------------------------------------------------------------

#PROFILING
sleep <- read.csv("Sleep_Efficiency.csv")

sleep_data <- df_status(sleep)

arrange(sleep_data, -p_zeros) %>% select(variable, q_zeros, p_zeros)

vars_to_remove=filter(sleep_data, p_zeros > 50) %>% .$variable
vars_to_remove

sleep_data_2=select(sleep, - one_of(vars_to_remove))
