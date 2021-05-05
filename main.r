# install.packages("gmodels")
# install.packages("ggthemes")
# install.packages("ggmap")
# install.packages("sf")
# install.packages("mapview")
# install.packages("ggrepel")
# install.packages("lobstr")
# install.packages("mapproj")
# install.packages("dummies")
# install.packages('caTools')
# install.packages('randomForest')
# library(mapproj)

library(tidyverse)
library(caTools)
library(gmodels)
library(ggthemes)
library(ggrepel)
library(sf)
library(mapview)
library(ggmap)
library(dummies)
library(caTools)

dataset = tibble(read.csv('weatherAUS.csv'))
dataset

locations = count(dataset, Location,sort = TRUE)

rainToday = count(dataset, RainToday, sort = TRUE)
rainTomorrow = count(dataset, RainTomorrow, sort = TRUE)

rainTomorrow

dataset$RainTomorrow

# Heat Map
# https://cran.r-project.org/web/packages/ggmap/readme/README.html
# register_google(key = "APIkey", write = TRUE)

MTemp <- dataset[!is.na(dataset$MaxTemp), ] %>% 
  select(Location, MaxTemp)


TempLocation <- aggregate(MTemp$MaxTemp, by=list(Category=MTemp$Location), FUN=mean)
TempLocation$Category <- paste(TempLocation$Category, ", Australia")# Adding Australia to the end of location name since there are many cities with the same names in other countries

cities_df <- as.data.frame(TempLocation)
locations_df <- mutate_geocode(cities_df, Category)
locations <- as_tibble(locations_df)

locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)
# mapview(locations_sf) # Google Maps interactive window



aus_coord <- geocode("Australia")

map.in <- get_googlemap(center = c(aus_coord$lon, aus_coord$lat), 
                        zoom = 4,
                        color = "bw",
                        style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:off")



ggmap(map) +
  geom_point(data = locations,
             aes(x = lon, y = lat, size = x),
             color = "red", alpha = 0.5)




# Rainfall

RF <- dataset[!is.na(dataset$Rainfall), ] %>% 
  select(Location, Rainfall)

RainLocation <- aggregate(RF$Rainfall, by=list(Category=RF$Location), FUN=sum)
RainLocation$Category <- paste(RainLocation$Category, ", Australia")# Adding Australia to the end of location name since there are many cities with the same names in other countries

Rcities_df <- as.data.frame(RainLocation)
Rlocations_df <- mutate_geocode(Rcities_df, Category)
Rlocations <- as_tibble(Rlocations_df)

Rlocations_sf <- st_as_sf(Rlocations, coords = c("lon", "lat"), crs = 4326)

aus_coord <- geocode("Australia")

map <- get_googlemap(center = c(aus_coord$lon, aus_coord$lat), zoom = 4)
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")

ggmap(map) +
  geom_point(data = Rlocations,
             aes(x = lon, y = lat, size = x),
             color = "red", alpha = 0.5)

ggmap(map) +
  stat_density_2d(
    data = Rlocations,
    aes(x = lon, y = lat, fill = stat(x)),
    alpha = .1,
    bins = 30,
    geom = "polygon"
  ) +
  scale_fill_gradientn(colors = YlOrBr)


# Rain by Month

RD <- dataset[!is.na(dataset$Rainfall), ] %>% 
  select(Date, Rainfall)

RD$Date <= substring(RD$Date,6,7) # Month only
RainMonth = aggregate(RD$Rainfall, by=list(Category=substring(RD$Date,6,7)), FUN=mean) # Mean Rainfall by month


ggplot(data=RainMonth, aes(x=Category, y=x, group=1)) +
  geom_line()+
  geom_point()


############################################################################

print(colMeans(is.na(dataset))) # Percentage of missing data in each column

datasetClean <- subset(dataset, select=-c(Evaporation, Sunshine, Cloud9am, Cloud3pm, Date)) # Removing features with high NA precentage
datasetClean <- datasetClean[complete.cases(datasetClean), ] # Removing NAs


# One-Hot Encoding categorical features

WD3<-dummy(datasetClean$WindDir3pm)
WD9<-dummy(datasetClean$WindDir9am)
WGD<-dummy(datasetClean$WindGustDir
           )
datasetD <- cbind(datasetClean, WD3)
datasetD <- cbind(datasetD, WD9)
datasetD <- cbind(datasetD, WGD)

datasetD$RainToday <- factor(datasetD$RainToday,
             levels = c('No', 'Yes'),
             labels = c(0, 1))

datasetD$RainTomorrow <- factor(datasetD$RainTomorrow,
                   levels = c('No', 'Yes'),
                   labels = c(0, 1))

df<-tibble(datasetD)


# Splitting the dataset into the Training set and Test set
set.seed(1337)

split = sample.split(df$RainTomorrow, SplitRatio = 0.8)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)


library(randomForest)
y <- training_set$RainTomorrow
X <- subset(training_set, select=-c(RainTomorrow))

classifier = randomForest(x = X,
                          y = y,
                          ntree = 200)

y_pred = predict(classifier, newdata = subset(test_set, select=-c(RainTomorrow)))

# Confusion Matrix
cm = table(test_set$RainTomorrow, y_pred)
print(cm)

# Accuracy Score
accuracy = sum(y_pred == test_set$RainTomorrow) / nrow(test_set)
print(accuracy)


