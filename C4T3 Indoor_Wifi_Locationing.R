# C4T3: Model Indoor Wifi locationing using data from UCI website (UJIIndoorLoc_Data_Set) 

library(dplyr)


####### Load Data
data <-read.csv("C:/Users/dufff/R/C4T3/data.csv")


####### Explore Data
summary(data)
str(data)
hist(data$BUILDINGID) # Building 2 has about 2x observations as Bldg 0 & 1 accounting ~ 50% of the observations
hist(data$FLOOR)      # Floors 0-3 about the same at 4500 observations, floor 4 low at 1000
hist(data$SPACEID)    # Not normally distributed
hist(data$RELATIVEPOSITION)  #80% of observations taken outside in front of the door
hist(data$USERID)     # Two of eighteen users account for ~ 2/3rds of the observations
hist(data$PHONEID)    # phones 13 & 14 account for ~ 40 of observations.

Bldsize <- data %>% count(BUILDINGID, SPACEID)   # Table with # of SpiceID for each building
hist(Bldsize$BUILDINGID)                         # Building 2 has the most rooms but not by much,
Bldsize %>% count(BUILDINGID)                    #  78, 86, 97 respectfully




########  Remove Columns with no data (Wifi spots that have no pings/data).

data <- data %>% select(where(~n_distinct(.) >1))  # Removes 55 Columns/Features down to 472 variables
str(data)


########  Concatenate Building/Floor/Space to simplify dependent variable.  - Recode Building and Floors to get rid of "0"
data <- data %>% mutate(BUILDINGID=recode(BUILDINGID,  # Recode building # to get rid of zero
                                          '0'='1',
                                          '1'='2',
                                          '2'='3'))
data <- data %>% mutate(FLOOR=recode(FLOOR,     # Recode floor # to get rid of zero
                                      '0'='1',
                                      '1'='2',
                                      '2'='3',
                                      '3'='4',
                                      '4'='5'))
data$BUILDINGID <- as.integer(data$BUILDINGID)  # Reconvert datatype of Building to Integer
data$FLOOR <- as.integer(data$FLOOR)            # Reconvert datatype of floor to Integer


data <-cbind(data,paste(data$BUILDINGID,data$FLOOR,data$SPACEID), stringsAsFactors=FALSE)   # Concatenate the Columns
colnames(data)[473] <-"LOCATION"                                                            # Name new Column = LOCATION
data <- data %>% relocate(LOCATION, .before = BUILDINGID)                                   # Move LOCATION column to front   
data$LOCATION <- as.factor(data$LOCATION)                                                   # Set LOCATION as "factor" data type
str(data)

########  DATA SET CHALLENGES  ###########
# Many factor levels of dependent variable LOCATION (735)
# Wide and sparce data
#Model by Building....reduces variables, Dependent Factors, and observations


#######  Split by Building and check Wifi Density for each Building to see if consistent:
Bld1 <- filter(data, BUILDINGID=='1')                # Each Building has similar Wifi density.
Bld2 <- filter(data, BUILDINGID=='2')                #    Bld1 => 200 Wifis
Bld3 <- filter(data, BUILDINGID=='3')                #    Bld2 => 207 Wifis.
Bld1 <- Bld1 %>% select(where(~n_distinct(.) >1))    #    Bld3 => 203 Wifis
Bld2 <- Bld2 %>% select(where(~n_distinct(.) >1))    #
Bld3 <- Bld3 %>% select(where(~n_distinct(.) >1))    #

######### Check how many room #'s do not have ping
v1 <- -104:0
index1 <- !!rowSums(Reduce('|', lapply(v1, '==', data)), na.rm=TRUE)   # 76 out of 19861 rows do not have pings....leaving as is.
table(index1)




#########  Prepare Building Data Sets (dfx) for modeling
df1 = Bld1
df2 = Bld2
df3 = Bld3
# Remove unused columns (wifis not pinged)
df1 <- df1 %>% select(where(~n_distinct(.) >1))  # 207 Columns/Features down from 472 variables
df2 <- df2 %>% select(where(~n_distinct(.) >1))  # 214 Columns/Features down from 472 variables
df3 <- df3 %>% select(where(~n_distinct(.) >1))  # 210 Columns/Features down from 472 variables
# Remove unneeded columns "Floor, SpaceID, RelativePosition, UserID, PhoneID, Timestamp)
df1 <- df1[-c(2:7)]
df2 <- df2[-c(2:7)]
df3 <- df3[-c(2:7)]


######### Start Modeling Process
library(caret)
set.seed(998)


# Set up Parallel Processing
library(doParallel)
cl <- makeCluster(6)
registerDoParallel(cl)
getDoParWorkers()

clusterEvalQ(cl, {
  library(caret)})

# Partition Data into Training and Testing

df1inTraining <- createDataPartition(df1$LOCATION, p = .75, list = FALSE)
df1train <- df1[df1inTraining,]
df1test <- df1[-df1inTraining,]
##
df2inTraining <- createDataPartition(df2$LOCATION, p = .75, list = FALSE)
df2train <- df2[df2inTraining,]
df2test <- df2[-df2inTraining,]
##
df3inTraining <- createDataPartition(df3$LOCATION, p = .75, list = FALSE)
df3train <- df3[df3inTraining,]
df3test <- df3[-df3inTraining,]

str(df1train)
str(df2train)
str(df3train)


#########   RANDOM FOREST    ###########


fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)  # 10 fold cross validation

rfFitdf1 <- train(LOCATION~., 
               data = df1train,
               method = "rf",
               trControl=fitControl,
               tuneLength = 5,
               metric = "Accuracy")

rfFitdf1   # repeat = 1, tunelength = 5, Accuracy = 0.764train & 0.774actuals)
rfFitdf2   # repeat = 1, tunelength = 5, Accuracy = 0.866train & 0.872actuals)
rfFitdf3   # repeat = 1, tunelength = 5, Accuracy = 0.828train & 0.834actuals)

preddf1rf <- predict(rfFitdf1, newdata = df1test)
preddf2rf <- predict(rfFitdf2, newdata = df2test)
preddf3rf <- predict(rfFitdf3, newdata = df3test)

df1test$LOCATION <- as.factor(df1test$LOCATION)
df2test$LOCATION <- as.factor(df2test$LOCATION)
df3test$LOCATION <- as.factor(df3test$LOCATION)

postResample(pred = preddf1rf, obs = df1test$LOCATION)
postResample(pred = preddf2rf, obs = df2test$LOCATION)
postResample(pred = preddf3rf, obs = df3test$LOCATION)


#########      C5.0     ####################

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)  # 10 fold cross validation
#grid <- expand.grid(.winnow = c(TRUE,FALSE), .trials = c(40,50,60), .model = c("rules","tree"))

c5fitdf3 <- train(LOCATION~., 
                  data = df3train,
                  method = "C5.0",
                  trControl=fitControl,
                  tuneLength = 5,
                  metric = "Accuracy")

c5fitdf1  # tuneLength = 5, Accuracy = 0.719train & 0.708actual)
c5fitdf2  # tuneLength = 5, Accuracy = 0.812train & 0.820actual)
c5fitdf3  # tuneLength = 5, Accuracy = 0.727train & 0.741actual)

preddf1c <- predict(c5fitdf1, newdata = df1test)
preddf2c <- predict(c5fitdf2, newdata = df2test)
preddf3c <- predict(c5fitdf3, newdata = df3test)

df1test$LOCATION <- as.factor(df1test$LOCATION)
df2test$LOCATION <- as.factor(df2test$LOCATION)
df3test$LOCATION <- as.factor(df3test$LOCATION)

postResample(pred = preddf1c, obs = df1test$LOCATION)
postResample(pred = preddf2c, obs = df2test$LOCATION)
postResample(pred = preddf3c, obs = df3test$LOCATION)



#########      gbm     ####################  Did not use.......machine crashed twice.  

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)  # 10 fold cross validation

gbfitdf1 <- train(LOCATION~., 
                 data = df1train,
                 method = "gbm",
                 trControl=fitControl,
                 tuneLength = 5,
                 metric = "Accuracy")



#########     k-Nearest Neighbors     ####################

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)  # 10 fold cross validation

knnfitdf3 <- train(LOCATION~., 
                  data = df3train,
                  method = "knn",
                  trControl=fitControl,
                  tuneLength = 5,
                  metric = "Accuracy")

knnfitdf1   # tune length = 5, accuracy = 0.552test & 0.532accuracy
knnfitdf2   # tunelength = 5, accuracy = 0.679test & 0.705accuracy
knnfitdf3   # tunelength = 5, accuracy = 0.629test & 0.647accuracy

preddf1knn <- predict(knnfitdf1, newdata = df1test)
preddf2knn <- predict(knnfitdf2, newdata = df2test)
preddf3knn <- predict(knnfitdf3, newdata = df3test)

df1test$LOCATION <- as.factor(df1test$LOCATION)
df2test$LOCATION <- as.factor(df2test$LOCATION)
df3test$LOCATION <- as.factor(df3test$LOCATION)

postResample(pred = preddf1knn, obs = df1test$LOCATION)
postResample(pred = preddf2knn, obs = df2test$LOCATION)
postResample(pred = preddf3knn, obs = df3test$LOCATION)



#########     CART     ####################  Did not use as low accuracy turning out similar to knn

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)  # 10 fold cross validation

crtfitdf1 <- train(LOCATION~., 
                   data = df1train,
                   method = "rpart",
                   trControl=fitControl,
                   tuneLength = 5,
                   metric = "Accuracy")

crtfitdf1   # tunelength = 5, accuracy = 0.568test


#############  STOP PARALLELL PROCESSING  ############
stopCluster(cl) 
#############  STOP PARALLELL PROCESSING  ############



############## Performance Evaluation Comparison Chart
Results <- resamples(list(rnfBld0 = rfFitdf1, rnfBld1 = rfFitdf2, rnfBld2 = rfFitdf3,
                          c50Bld0 = c5fitdf1, c50Bld1 = c5fitdf2, c50Bld2 = c5fitdf3,
                          knnBld0 = knnfitdf1, knnBld1 = knnfitdf2, knnBld2 = knnfitdf3))

summary(Results)


