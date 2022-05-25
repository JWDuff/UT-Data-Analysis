# Practice R programming in R-Studio Environment


install.packages(readr)
library(readr)
iris = read.csv("C:/Users/dufff/OneDrive/Documents/iris.csv")
iris
summary(iris)  
str(iris)
names(iris)
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
plot.default(iris$Sepal.Length, iris$Sepal.Width, type = "p", main = "Sepal Scatter Plot", xlab = "Length", ylab = "Width")
qqnorm(iris$Petal.Width)
hist(iris$Petal.Width)
qqnorm(iris$Petal.Length)
hist(iris$Petal.Length)

# rename columns:
names(iris)
names(iris)<-c("X","slen", "swid", "plen", "pwid","species")
iris

# Encode Categorical data column => Species
rv <- c(iris$species) # Create character vector of the column
rv
rf <- factor(rv) # factor the vector column
rf
iris$species<- as.numeric(rf) # convert column in table to numbers
iris

# Set up for linear Regression

set.seed(123)

# Set size of training data tabel
trainSize <- round(nrow(iris) * 0.8)
testSize <- nrow(iris) - trainSize
trainSize

# Split data table into training and testing tables
training_indices<- sample(seq_len(nrow(iris)), size=trainSize)
training_indices
trainSet <- iris[training_indices,]
testSet <- iris[-training_indices,]
trainSet

# Train the Model and review results

lm1 <- lm(species ~ slen + swid + plen + pwid, trainSet)
summary(lm1)

# Test the model performance against test data
preds<-predict(lm1,testSet)
gndtrth<-testSet[c(6)]
preds
gndtrth
gtf<-as.numeric(gndtrth$species)
prd<-as.numeric(preds)
str(gtf)
str(prd)

#Plotting Predicted Values:
prddf<- as.data.frame(prd) # Putting PRD vaues in data table format
prddf <- cbind(ID = 1:nrow(prddf), prddf) # Adding ID columen for as x-axis for the plot
plot(prddf$ID,prddf$prd, main = "Plot of Predicted Values of Test Data", xlab = "ID", ylab = "Predicted Value", pch = 19)

# Scatter plot of Predicted vs. Ground Truth.
plot.default(preds,gtf, main = "Scatter Plot of Predicted vs. Ground Truth", xlab ="Predicted", ylab = "Ground Truth", pch = "o", col = "black", cex = 3)


# Decode Predicted Values to 1,2,3, by rounding and compare to Actual Results
Predicted = prd
Predicted <- round(prd1, 0)
GroundTruth = gtf

Predicted
GroundTruth

save.image()

