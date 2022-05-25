# Import Data and Rename to Data and DataI for convenience

data <-read.csv("C:/Users/dufff/R/C3T2/completeresponses.csv")   # Complete dataset to be used for training and testing             
dataI <-read.csv("C:/Users/dufff/R/C3T2/surveyincomplete.csv")   # Incomplete dataset to have values predicted
data$brand <- as.factor(data$brand) # Convert Target variable to two factor classification
dataI$brand <- as.factor(dataI$brand) # Convert Target variable to two factor classification
#str(data)
#str(dataI)

#load Caret library and set seed
library(caret)
set.seed(998)

# define an 75%/25% train/test split of data
inTraining <- createDataPartition(data$brand, p = .75, list = FALSE)
training <- data[inTraining,]
testing <- data[-inTraining,]
str(training)


# STOCHASTIC GRADIENT BOOSTING (gbm) CLASSIFICATION MODEL ALGORYTHM (tuneLengths 1 and 5)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)  #10 fold cross validation

gbmFit5 <- train(brand~., 
                data = training,
                method = "gbm",
                trControl=fitControl,
                tuneLength = 5,
                metric = "Accuracy")
gbmFit5
#plot(gbmFit5)
#gbmFit   #  tunelength = 1


# Running Predictions on Testing data
predsgbm5 <- predict(gbmFit5, newdata = testing)  # Testing predictions gbmFit - Array
actuals <- testing$brand # Actual results Array
#gbmtable <- cbind(actuals, predsgbm)  # Combine Predictions and Actuals into a DataTable => might use for performance evauluation
#str(predsgbm)
#str(actuals)

# Evaluation of Predictions vs Actuals (groundtruth)
confusionMatrix(data = predsgbm5, actuals)
#postResample(predsgbm5, actuals)

# Tune Length = 1
#     Testing Accuracy = 0.728; 
#     Training Accuracy = 0.731

# Tune Length = 5
#     Testing Accuracy = 0.927; Kappa = 0.846 (n.trees = 50, shrinkage = o.1, n.minobsinnode = 10)
#     Training Accuracy = 0.915


# Features Selection / Reduction process:  
library(gbm)  # for some reason needed to do this for varImp() to work
importancegbm5 = varImp(gbmFit5)  # => 1 tunelength, Salary has score of 100 all others 0
#plot(importancegbm5)
importancegbm5



# RANDOM FOREST CLASSIFICATION MODEL ALGORYTHM (tuneLenghts 1 and 5)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)  #10 fold cross validation

rfFit5 <- train(brand~., 
                data = training,
                method = "rf",
                
                trControl=fitControl,
                tuneLength = 5,
                metric = "Accuracy")
rfFit5
#rfFit1
#plot(rfFit5)

# tuneLength 1
#   Training Accuracy = 0.924; kappa = 839 
# tuneLength 5
#   Testing Accuracy = 0.913 (mtry = 5)
#   Training Accuracy = 0.925 (Mtry = 5)


# Review of Importance of X Variables
importancerf5 = varImp(rfFit5)  # Both tuneLengths have salary at 100 then followed by age around 55 than others < 13
importancerf5
           
# Running Predictions on Testing data
predsrf5 <- predict(rfFit5, newdata = testing)  # Testing predictions rfFit5 - Array
actuals <- testing$brand # Actual results Array
confusionMatrix(data = predsrf5, actuals)
#postResample(predsrf5, actuals)


# Feature Reductions via the Random Forrest Model rfe

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
rfFitrfe <- rfe(training[1:6], training[,7], 
                sizes=c(1:6),
                rfeControl=control)
rfFitrfe

predsrfe <- predict(rfFitrfe, newdata = testing)
predsrfe <- predsrfe$pred
actuals <- testing$brand
confusionMatrix(data = predsrfe, actuals)


# training = as.data.frame(training)  # convert training data file from tibble to dataframe....forgot why.


  # Training Accuracy = 0.925: Kappa = 0.842: Feature = Salary/Age/Credit/elev/zipcode with 5 features....drop "car"
  # Testing Accuracy = 0920: Kappa = 0.831




# C5.0 DECISION TREES AND RULE-BASED ALGORYTHM (tuneLenghts 1 and 5)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)  #10 fold cross validation
c5Fit5 <- train(brand~., 
                data = training,
                method = "C5.0",
                trControl=fitControl,
                tuneLength = 5,
                metric = "Accuracy")
c5Fit5
#c5Fit1

# tuneLength 1
#   Training Accuracy = 0.886; Kappa = 0.756

# tuneLength 5
#   Testing Accuracy = 0.918; Kappy = 0.827 (Model = Tree, winnow = TRUE, trials = 10)
#   Training Accuracy = 0.924; Kappa = 0.839


# Review of Importance of X Variables
importancec5 = varImp(c5Fit5)  # Both tuneLengths have salary at 100 then followed by age around 55 than others < 13
plot(importancec5, cex.axis = 10, cex.lab = 3)


# Running Predictions on Testing data
predsc55 <- predict(c5Fit5, newdata = testing)  # Testing predictions c5fit5 - Array
actuals <- testing$brand # Actual results Array
#str(predsc55)
#str(actuals)

# Evaluation of Predictions vs Actuals (groundtruth)
confusionMatrix(data = predsc55, actuals)
#postResample(predsc55, actuals)




# IMPLEMENT MODEL TO INCOMPLETE DATA USING gbmFit5 modele as it had highest accuracy (0.927)

IncData <- dataI[1:6]
#IncData
predsIncSrvy <- predict(gbmFit5, newdata = IncData)

# Compare prediction in Incomplete Survey with Complete Survey
table(predsIncSrvy)  # 38.7% are "0 - Acer brand"  predicted outcomes within the 3% error range of expected.
table(data$brand)    # 37.8% are "0 - Acer brand"  (3.02% error range)

# Combine Predicted Brands and Brands from Complete Survey into a Dataframe
a = predsIncSrvy
b = data$brand
a = as.data.frame(a)
b = as.data.frame(b)
colnames(a) <- c('brand')
colnames(b) <- c('brand')
Brands <- rbind(a,b)  # combines the two tables
# summary(Brands)


# rename brand values in dfc: 0 = Acer, 1 = Sony....doing this for plotting purposes
Brands <- with(Brands, factor(brand, levels = c("0", "1"), labels = (c("Acer", "Sony"))))
Brandp <- with(a, factor(brand, levels = c("0", "1"), labels = (c("Acer", "Sony"))))
Branda <- with(b, factor(brand, levels = c("0", "1"), labels = (c("Acer", "Sony"))))

summary(Branda)
summary(Brandp)
summary(Brands)


plot(Brands, main = "Combined Survey Data")
plot(Brandp, main = "Incomplete Survey Predictions")
plot(Branda, main = "Complete Survey Data")

