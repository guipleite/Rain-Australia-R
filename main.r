#install.packages("gmodels")
#install.packages("ggthemes")

library(tidyverse)
library(caTools)
library(gmodels)
library(ggthemes)


dataset = tibble(read.csv('weatherAUS.csv'))
dataset

locations = count(dataset, Location,sort = TRUE)

rainToday = count(dataset, RainToday, sort = TRUE)
rainTomorrow = count(dataset, RainTomorrow, sort = TRUE)


rainTomorrow

dataset$RainTomorrow

# Heat Map

MTemp <- dataset[!is.na(dataset$MaxTemp), ] %>% 
  select(Location, MaxTemp)


aggregate(MTemp$MaxTemp, by=list(Category=MTemp$Location), FUN=mean)

# Cumulative Rainfall Map

RF <- dataset[!is.na(dataset$Rainfall), ] %>% 
  select(Location, Rainfall)


aggregate(RF$Rainfall, by=list(Category=RF$Location), FUN=sum)

# Rain by day

RD <- dataset[!is.na(dataset$Rainfall), ] %>% 
  select(Date, Rainfall)

RD$Date <= substring(RD$Date,6,7) # Month only


RainMonth = aggregate(RD$Rainfall, by=list(Category=substring(RD$Date,6,7)), FUN=mean) # Mean Rainfall by month


ggplot(data=RainMonth, aes(x=Category, y=x, group=1)) +
  geom_line()+
  geom_point()







# Encoding categorical data
dataset$Country = factor(dataset$Country,
                         levels = c('France', 'Spain', 'Germany'),
                         labels = c(1, 2, 3))
dataset$Purchased = factor(dataset$Purchased,
                           levels = c('No', 'Yes'),
                           labels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)