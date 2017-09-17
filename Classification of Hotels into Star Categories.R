
### Classification of Hotels in Singapore into their Star Categories based on review scores###


#Installing libraries
install.packages("ggplot2")
install.packages("e1071")
install.packages("rpart")
install.packages("randomForest")
install.packages("caret")
install.packages("Hmisc")
install.packages("mice")

#Importing Libraries
library(ggplot2)
library(e1071)
library(rpart)
library(randomForest)
library(caret)
library(Hmisc)
library(mice)

#Importing Hotel List .CSV file 
ListOfHotels <- read.csv("C:/Users/Janika Kotak/Documents/MTC TechTest/Hotels/HOTELS.csv", header=TRUE)  

#Getting the basic summary of data
summary(ListOfHotels)

#Selecting the necessary columns
ListOfHotels <- subset(ListOfHotels, select=c(1:3))

#Renaming a column for merging with another file later
colnames(ListOfHotels)[3] <- ("HotelName")

#Importing Hotel details CSV File
HotelReviews <- read.csv("C:/Users/Janika Kotak/Documents/MTC TechTest/Hotels/Hotel Reviews.csv", header=TRUE)  

#Getting the basic summary of data
summary(HotelReviews)

#Renaming a column for merging with another file later
colnames(HotelReviews)[1] <- ("HotelName")

#FuzzyLookup to match approximate strings

#Separating the string variable from each dataset
ListOfHotels.name = data.frame(ListOfHotels$HotelName)
names(ListOfHotels.name)[names(ListOfHotels.name)=="ListOfHotels.HotelName"] = "NAME"
ListOfHotels.name$NAME = as.character(ListOfHotels.name$NAME)
# Removing duplicates
ListOfHotels.name = unique(ListOfHotels.name) 
head(ListOfHotels.name)

HotelReviews.name = data.frame(HotelReviews$HotelName)
names(HotelReviews.name)[names(HotelReviews.name)=="HotelReviews.HotelName"] = "NAME2"
HotelReviews.name$NAME2 = as.character(HotelReviews.name$NAME2)
# Removing duplicates
HotelReviews.name = unique(HotelReviews.name) 
head(HotelReviews.name)

#Matching Hotel Name strings from ListOfHotels with Hotel Name from HotelReviews
HotelReviews.name$NAME <- "" #Creating an empty column in the HotelReviews file

for(i in 1:dim(HotelReviews.name)[1]) {
  x <- agrep(HotelReviews.name$NAME2[i], ListOfHotels.name$NAME,
             ignore.case=TRUE, value=TRUE,
             max.distance = 0.05, useBytes = TRUE)
  x <- paste0(x,"")
  HotelReviews.name$NAME[i] <- x
}

#Merging the FuzzyLookup file with the HotelReviews File
HotelReviews = merge(HotelReviews,HotelReviews.name, by.x=c("HotelName"), by.y=c("NAME2"),all=TRUE)

#Merging the new HotelReviews File with the ListOfHotels File to obtain the Address XY coordinates by keeping only perfect matches
HotelReviews = merge(HotelReviews,ListOfHotels, by.x=c("NAME"), by.y=c("HotelName"))

#Creating a MasterRecord by eliminating repetitive columns
MasterRecord = subset(HotelReviews, select=c(2:65))

#Looking for missing values
summary(MasterRecord)  #Has no missing records



#########################Changing Data Types##################################

sapply(MasterRecord, class)

#Changing Data Types of variables
MasterRecord$HotelName <- as.character(MasterRecord$HotelName)
MasterRecord$City_Address <- as.character(MasterRecord$City_Address)

#########################Train-Test Split##################################

##Splitting data into training and testing##
Sample_size <- floor(0.80 * nrow(MasterRecord))

## setting the seed to make the partition reproducible
set.seed(777)
Training_sample <- sample(seq_len(nrow(MasterRecord)), size = Sample_size)

##Training dataset to be used
Training_set<- MasterRecord[Training_sample,]
Training_set <- subset(Training_set, select = c(2,4:64))

##Testing dataset to be used
Test_set<- MasterRecord[-Training_sample,]
Test_set <- subset(Test_set, select = c(2,4:64))


##############################Data Modeling###############################

####################################SVM###################################

set.seed(777)

result <- Test_set
result$actual<- Test_set$Star_Category

# Fitting model
SVM <-svm(Star_Category ~ ., data = Training_set, type="C-classification")
summary(SVM)



#Predict Output 
predictedSVM = predict(SVM,Test_set)
result$predicted<- predictedSVM

#Generating Confusion Matrix
table(actual=result$actual,prediction=result$predicted)

##prediction
##actual 3 4 5
##     3 5 0 0
##     4 8 0 0
##     5 2 0 0

## Model Evaluation
result.correct = nrow(result[result$predicted==result$Star_Category,])
cat("Correct predictions = ", result.correct ,"\n")
cat("Accuracy = ", result.correct / nrow(Test_set) * 100 ,"\n")

## Accuracy 33%



############################Decision Tree###############################

# Grow tree 
DecisionTreeModel <- rpart(Star_Category ~ ., data = Training_set,method="class")
summary(DecisionTreeModel)

#Predict Output- Winner Class
predicted <- predict(DecisionTreeModel, Test_set, type="class")
#Predict Output- Probabilities for each class
predicted <- predict(DecisionTreeModel, Test_set, type="class")

#Plot tree
plot(fit, uniform=TRUE,main="Classification Tree for Hotels")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#Evaluation
#Check model performance
confusionMatrix(Test_set$Star_Category, predicted)


## Results ##

##Confusion Matrix and Statistics

##Reference
##Prediction 3 4 5
##         3 4 1 0
##         4 0 7 1
##         5 0 1 1

##Overall Statistics

##Accuracy : 0.8   ------> states 80% model accuracy          
##95% CI : (0.5191, 0.9567)



###### Decision Tree is opted over SVM ######


