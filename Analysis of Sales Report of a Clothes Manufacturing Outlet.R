# import necessary libraries

library(readxl)
library(dplyr)
library(caTools)

library(e1071)
library(caret)
library(randomForest)

# Load Data

Attribute_DataSet <- read_excel("F:/SYMPLI LEARN/R programming/Project/Clothes Manufacturing Outlet/Attribute DataSet.xlsx")
View(Attribute_DataSet)
str(Attribute_DataSet)
summary(Attribute_DataSet)

Dress_Sales <- read_excel("F:/SYMPLI LEARN/R programming/Project/Clothes Manufacturing Outlet/Dress Sales.xlsx")
View(Dress_Sales)
str(Dress_Sales)
summary(Dress_Sales)

# remove Dress_ID column

Attribute <- Attribute_DataSet[, -1]
View(Attribute)
str(Attribute)
summary(Attribute)

Dress <- Dress_Sales[, -1]
str(Dress)
View(Dress)


# check the unique values for each columns


# style 

unique(Attribute$Style)
Attribute$Style[Attribute$Style == "sexy"] = "Sexy"


# Price

Attribute$Price[Attribute$Price == "low"] = "Low"
Attribute$Price[Attribute$Price == "high"] = "High"
unique(Attribute$Price)

# Size

Attribute$Size[Attribute$Size == "small"] = "S"
Attribute$Size[Attribute$Size == "s"] = "S"
unique(Attribute$Size)

# Season

Attribute$Season[Attribute$Season == "summer"] = "Summer"
Attribute$Season[Attribute$Season == "Autumn"] = "Automn"
Attribute$Season[Attribute$Season == "winter"] = "Winter"
Attribute$Season[Attribute$Season == "spring"] = "Spring"
unique(Attribute$Season)

# NeckLine

Attribute$NeckLine[Attribute$NeckLine == "sweetheart"] = "Sweetheart"
unique(Attribute$NeckLine)

# SleeveLength

Attribute$SleeveLength[Attribute$SleeveLength == "sleevless"] = "sleeveless"
Attribute$SleeveLength[Attribute$SleeveLength == "sleeevless"] = "sleeveless"
Attribute$SleeveLength[Attribute$SleeveLength == "sleveless"] = "sleeveless"
Attribute$SleeveLength[Attribute$SleeveLength == "thressqatar"] = "threequarter"
Attribute$SleeveLength[Attribute$SleeveLength == "threequater"] = "threequarter"
Attribute$SleeveLength[Attribute$SleeveLength == "urndowncollor"] = "turndowncollor"
Attribute$SleeveLength[Attribute$SleeveLength == "cap-sleeves"] = "capsleeves"
unique(Attribute$SleeveLength)


unique(Attribute$waiseline)
unique(Attribute$Material)

# FabricType

Attribute$FabricType[Attribute$FabricType == "shiffon"] = "chiffon"
Attribute$FabricType[Attribute$FabricType == "sattin"] = "satin"
Attribute$FabricType[Attribute$FabricType == "wollen"] = "woolen"
Attribute$FabricType[Attribute$FabricType == "flannael"] = "flannel"
Attribute$FabricType[Attribute$FabricType == "knitting"] = "knitted"
unique(Attribute$FabricType)

# Decoration

Attribute$Decoration[Attribute$Decoration == "embroidary"] = "embroidery"
Attribute$Decoration[Attribute$Decoration == "sequined"] = "sequins"
Attribute$Decoration[Attribute$Decoration == "sequined"] = "sequins"
Attribute$Decoration[Attribute$Decoration == "ruched"] = "ruche"
Attribute$Decoration[Attribute$Decoration == "none"] = "null"
unique(Attribute$Decoration)

# Pattern Type

Attribute$`Pattern Type`[Attribute$`Pattern Type`== "leapord"] =  "leopard"
Attribute$`Pattern Type`[Attribute$`Pattern Type`== "none" ] = "null"
unique(Attribute$`Pattern Type`)


# factoring 



Attribute$Style <- as.factor(Attribute$Style)
Attribute$Price <- as.factor(Attribute$Price)
Attribute$Size <- as.factor(Attribute$Size)
Attribute$Season <- as.factor(Attribute$Season)
Attribute$NeckLine <- as.factor(Attribute$NeckLine)
Attribute$SleeveLength <- as.factor(Attribute$SleeveLength)
Attribute$waiseline <- as.factor(Attribute$waiseline)
Attribute$Material <- as.factor(Attribute$Material)
Attribute$FabricType <- as.factor(Attribute$FabricType)
Attribute$Decoration <- as.factor(Attribute$Decoration)
Attribute$`Pattern Type` <- as.factor(Attribute$`Pattern Type`)
Attribute$Recommendation <- as.factor(Attribute$Recommendation)

str(Attribute)
View(Attribute)

# count of missing values in attribset dataset

colSums(is.na(Attribute))

# Create the function.

get_mode <- function(a) {
  unique_value <- unique(a)
  unique_value[which.max(tabulate(match(a , unique_value)))]
}

# fill missing Value with mode

Attribute$Price[is.na(Attribute$Price) == TRUE] <- get_mode(Attribute$Price)
Attribute$Season[is.na(Attribute$Season) == TRUE] <- get_mode(Attribute$Season)
Attribute$NeckLine[is.na(Attribute$NeckLine) == TRUE] <- get_mode(Attribute$NeckLine)
Attribute$waiseline[is.na(Attribute$waiseline) == TRUE] <- get_mode(Attribute$waiseline)
Attribute$Material[is.na(Attribute$Material) == TRUE] <- get_mode(Attribute$Material)
Attribute$FabricType[is.na(Attribute$FabricType) == TRUE] <- get_mode(Attribute$FabricType)
Attribute$Decoration[is.na(Attribute$Decoration) == TRUE] <- get_mode(Attribute$Decoration)
Attribute$`Pattern Type`[is.na(Attribute$`Pattern Type`) == TRUE] <- get_mode(Attribute$`Pattern Type`)


Attribute_DataSet <- data.frame(Attribute)
str(Attribute_DataSet)



str(Dress)

# Convert all variable types to numeric

Dress <- as.data.frame(apply(Dress , 2 , as.numeric))

# Rename columns name in dresssale_ dataset

colnames(Dress)[colnames(Dress) == "41314"] <- "2/9/2013"
colnames(Dress)[colnames(Dress) == "41373"] <- "4/9/2013"
colnames(Dress)[colnames(Dress) == "41434"] <- "6/9/2013"
colnames(Dress)[colnames(Dress) == "41495"] <- "8/9/2013"
colnames(Dress)[colnames(Dress) == "41556"] <- "10/9/2013"
colnames(Dress)[colnames(Dress) == "41617"] <- "12/9/2013"
colnames(Dress)[colnames(Dress) == "41315"] <- "2/10/2013"
colnames(Dress)[colnames(Dress) == "41374"] <- "4/10/2013"
colnames(Dress)[colnames(Dress) == "41435"] <- "6/10/2013"
colnames(Dress)[colnames(Dress) == "40400"] <- "8/10/2013"
colnames(Dress)[colnames(Dress) == "41557"] <- "10/10/2013"
colnames(Dress)[colnames(Dress) == "41618"] <- "12/10/2013"

# sum all values on row on (total sales)

Dress$Total_Sales <- rowSums(Dress)
View(Dress)

Dress_Sales <- as.data.frame(Dress)

str(Dress_Sales)

# Marged data

Merge_Data <- data.frame(Attribute_DataSet , Dress_Sales)
str(Merge_Data)


# spliting dataset 

set.seed(123)

split_Data <- sample.split(Merge_Data$Recommendation , SplitRatio = .7)
train_Data <- subset(Merge_Data , split_Data == TRUE)
test_Data <- subset(Merge_Data , split_Data == FALSE)

dim(train_Data)
dim(test_Data)


# naive bayes model

naive_model <- naiveBayes(Recommendation ~ . , data = train_Data) # build model
pred <- predict(naive_model , train_Data)
table(pred)
table(train_Data$Recommendation , pred)
confusionMatrix(table(train_Data$Recommendation , pred))

naive_predict <- predict(naive_model , test_Data) # predict test set
table(naive_predict , test_Data$Recommendation) # create table


# Support vector machine

svm_model <- svm(Recommendation ~ . , train_Data) # build model
table(predict(svm_model))
confusionMatrix(train_Data$Recommendation , predict(svm_model) , positive = '1') # create confusion Matrix

svm_predict <- predict(svm_model , test_Data) # predict test set
table(svm_predict , test_Data$Recommendation) # create table


# Random Forest

randomForest_model <- randomForest(x = train_Data , y = train_Data$Recommendation , ntree = 800) # build model
confusionMatrix(train_Data$Recommendation , predict(randomForest_model) , positive = '1') # create confusion Matrix

randomForest_predict <- predict(randomForest_model , test_Data) # predict test set
table(randomForest_predict , test_Data$Recommendation) # create table

# regression (Total Sales and (Style+Season+Material+Price))

regressor_Sales <- lm(formula = Total_Sales ~ Style+Season+Material+Price , data = train_Data) # build model
summary(regressor_Sales) # print model summary
plot(regressor_Sales , pch = 16 , col = 'blue') # Plot the results
abline(regressor_Sales) # Add regression line

# regression (Total Sales and Rating)

regressor_Rating <- lm(formula = Total_Sales ~ Rating , data = train_Data ) # build model
summary(regressor_Rating)
plot(regressor_Rating , pch = 16 , col = 'blue')
abline(regressor_Rating)

# evaluation

original <- test_Data$Total_Sales
pred1 <- predict(regressor_Rating , test_Data)  
predicted = pred1
difference <- original - predicted

mse <- mean((difference)^2) # MSE
mae <- mean(abs(difference)) # MAE
rmse <- sqrt(mse) # RMSE
R2 <- 1 - (sum((difference)^2 / sum((original - mean(original))^2))) # R^2

cat(" MAE:", mae, "\n", "MSE:", mse, "\n", "RMSE:", rmse, "\n", "R-squared:", R2) 



