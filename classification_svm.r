library(kernlab)
library(dplyr)
library(caret)
library(ggplot2)

##TEXTBOOK DATASET
#Import the dataset
letters_recognition <- read.csv("letter-recognition.csv",stringsAsFactors = TRUE)

#check if there are any missing values
sum(is.na(letters_recognition))
#therefore the dataset does not have 
#any missing values

#data preview
str(letters_recognition)
#the dataset has 20000 observations and 17 variables

#number of each letters in the dataset
table(letters_recognition$letter)

#visualize the distribution of dataset
barplot(table(letters_recognition$letter))

#proportion of each letter in the dataset
prop.table(table(letters_recognition$letter))
#we can observe that the proportion of each letter is
#similar, therefore there is not much discrepency between 
#any of the letter classes

#splitting the dataset in train and test sets
set.seed(123)
#we split the dataset 70%-30%
sample_data <- sample(1:nrow(letters_recognition),size = round(0.7*nrow(letters_recognition)))
letter_train <- letters_recognition[sample_data,]
letter_test <- letters_recognition[-sample_data,]

#proportion of letters in the train and test datasets
print("Train Dataset Letters")
prop.table(table(letter_train$letter))

print("Test Dataset Letters")
prop.table(table(letter_test$letter))

#Hence, the proportion of dataset in the train and test
#dataset is similar.

#SVM model training 
#first we will try the vanilladot kernel (linear)
model_linear <- ksvm(letter ~ ., data = letter_train,kernel = "vanilladot")
model_linear_predict <- predict(model_linear,letter_test,type = "response")
confusionMatrix(model_linear_predict,letter_test$letter)
print(paste("Accuracy:",mean(model_linear_predict == letter_test$letter)))
#The accuracy of the model is 85.2166.[Quite Powerful]

#Changing the kernels
#rbfdot kernel
set.seed(123)
model_2 <- ksvm(letter ~ ., data = letter_train,kernel = "rbfdot")
model_2_predict <- predict(model_2,letter_test,type = "response")
confusionMatrix(model_2_predict,letter_test$letter)
print(paste("Accuracy:",mean(model_2_predict == letter_test$letter)))
#the accuracy of the model jumped to 92.83% by changing the
#the kernel to rbfdot.

#polydot kernel
model_3 <- ksvm(letter ~ ., data = letter_train,kernel = "polydot")
model_3_predict <- predict(model_3,letter_test,type = "response")
confusionMatrix(model_3_predict,letter_test$letter)
print(paste("Accuracy:",mean(model_3_predict == letter_test$letter)))
#the accuracy of the model is 85.2166 (same as the linear (vanilladot kernel))

#Let us try hyperparameter tuning to see
#whether the model_2 can be improved further
set.seed(123)
model_tune <- ksvm(letter ~ ., data = letter_train,kernel = "rbfdot",C = 2)
model_tune_predict <- predict(model_tune,letter_test,type = "response")
confusionMatrix(model_tune_predict,letter_test$letter)
prop.table(table(model_tune_predict == letter_test$letter))
print(paste("Accuracy:",mean(model_tune_predict == letter_test$letter)))
#Hence,the accuracy is 94.55.

#adding cross validation
set.seed(123)
model_tune2 <- ksvm(letter ~ ., data = letter_train,kernel = "rbfdot",C = 1,cross = 5)
model_tune_predict2 <- predict(model_tune2,letter_test,type = "response")
confusionMatrix(model_tune_predict2,letter_test$letter)
prop.table(table(model_tune_predict2 == letter_test$letter))
print(paste("Accuracy:",mean(model_tune_predict2 == letter_test$letter)))
#The is accuracy is 92.833
#Therefore rbfdot and C = 2 has the best accuracy


#################################################################################

#Hotel Reservations Dataset
#I. IMPORTING THE DATASET
#import the dataset
hotel_reservation <- read.csv('Hotel Reservations.csv')

#see the data
str(hotel_reservation)
head(hotel_reservation)

#check for missing values
colSums(is.na(hotel_reservation))
#therefore there are no missing values

#II. DATA CLEANING
#remove the id [it does not provide additional information]
hotel_reservation <- subset(hotel_reservation,select = - Booking_ID)
str(hotel_reservation)

#convert the character columns to factor
#type of meal_plan
hotel_reservation$type_of_meal_plan <- factor(hotel_reservation$type_of_meal_plan)
hotel_reservation$room_type_reserved <- factor(hotel_reservation$room_type_reserved)
hotel_reservation$market_segment_type <- factor(hotel_reservation$market_segment_type)
str(hotel_reservation)

#convert the target variable "booking_status" to binary 2 if Not Canceled and 1 if Canceled
hotel_reservation$booking_status <- factor(hotel_reservation$booking_status)
str(hotel_reservation)
head(hotel_reservation)

#III. DATA VISUALIZATION
#Booking Status
#barplot(table(hotel_reservation$booking_status),col = c("red","green"))

ggplot(data = hotel_reservation,aes(x=booking_status,fill = booking_status)) + geom_bar() + labs(title = "Barplot of Booking Status")
table(hotel_reservation$booking_status)
prop.table(table(hotel_reservation$booking_status))

#we have Canceled = 11885 and Not_Canceled = 24390
#Proportion of Canceled reservation = 0.3276 
#Proportion of Not-Canceled reservation = 0.6723

#Type of Meal Plan
ggplot(data = hotel_reservation,aes(x=type_of_meal_plan,fill = booking_status)) + geom_bar() + labs(title = "Barplot of Type of Meal Plan")
table(hotel_reservation$type_of_meal_plan)
prop.table(table(hotel_reservation$type_of_meal_plan))
#Meal Plan 1 is the most booked plan

#Room Type Reserved
ggplot(data = hotel_reservation,aes(x=room_type_reserved,fill = booking_status)) + geom_bar() + labs(title = "Barplot of Room Type Reserved")
table(hotel_reservation$room_type_reserved)
prop.table(table(hotel_reservation$room_type_reserved))
#Room type 1 is the most booked room

#Market Segment Type
ggplot(data = hotel_reservation,aes(x=market_segment_type,fill = booking_status)) + geom_bar() + labs(title = "Barplot ofMarket Segment Type")
table(hotel_reservation$market_segment_type)
prop.table(table(hotel_reservation$market_segment_type))
#Most bookings are done online
hotel_reservation[hotel_reservation$booking_status == "Canceled"||hotel_reservation$market_segment_type=="Complementary",]
#Interesting the booking which were complimentary do not have cancellations

#lead_time 
ggplot(data = hotel_reservation,aes(x=lead_time,fill = booking_status)) + geom_histogram() + labs(title = "Lead Time")
#increase in the lead time 
#leads to a decrease in the number of reservations

#avg_price_per_room
ggplot(data = hotel_reservation,aes(x=avg_price_per_room,fill = booking_status)) + geom_histogram() + labs(title = "Previous Cancelations")

#Number of Adults
ggplot(data = hotel_reservation,aes(x=no_of_adults,fill = booking_status)) + geom_bar() + labs(title = "Number of Adults")
#Maximum number of bookings are for two adults

#Number of Children
ggplot(data = hotel_reservation,aes(x=no_of_children,fill = booking_status)) + geom_bar() + labs(title = "Number of children")
table(hotel_reservation$no_of_children)
#Maximum number of bookings have zero children

#Number of weekend nights
ggplot(data = hotel_reservation,aes(x=no_of_weekend_nights,fill = booking_status)) + geom_bar() + labs(title = "Number of Weekend Nights")

#Number of week nights
ggplot(data = hotel_reservation,aes(x=no_of_week_nights,fill = booking_status)) + geom_bar() + labs(title = "Number of Week Nights")

#repeated guest
ggplot(data = hotel_reservation,aes(x=repeated_guest,fill = booking_status)) + geom_bar() + labs(title = "Repeated Guest")
hotel_reservation[hotel_reservation$booking_status == "Canceled"||hotel_reservation$repeated_guest==1,]
#Maximum number of bookings are new guests.
#No repeated guest did cancel the booking

#no_of_previous_cancellations 
ggplot(data = hotel_reservation,aes(x=no_of_previous_cancellations,fill = booking_status)) + geom_bar() + labs(title = "Previous Cancelations")

#no_of_special_requests
ggplot(data = hotel_reservation,aes(x=no_of_special_requests,fill = booking_status)) + geom_bar() + labs(title = "Special Requests")
#Maximum bookings have zero number of special requests 

#no_of_previous_bookings_not_canceled
ggplot(data = hotel_reservation,aes(x=no_of_previous_bookings_not_canceled,fill = booking_status)) + geom_bar() + labs(title = "Previous Bookings Not Canceled")

#arrival_year
ggplot(data = hotel_reservation,aes(x=arrival_year,fill = booking_status)) + geom_bar() + labs(title = "Arrival Year")
#more reservations in 2018

#required_car_parking_space  
ggplot(data = hotel_reservation,aes(x=required_car_parking_space,fill = booking_status)) + geom_bar() + labs(title = "Parking Space")
#Most reservations do not require parking spaces

#IV. PRE-TRAINING 
#factor variables to numeric 
#One-Hot Encoding With dummy variables
levels(hotel_reservation$room_type_reserved)
levels(hotel_reservation$type_of_meal_plan)
levels(hotel_reservation$market_segment_type)

#categorical variables
categorical_vars <- c("room_type_reserved", "type_of_meal_plan", "market_segment_type")
#set.seed(123)
#create the encoded matrix
encoded_matrix <- predict(dummyVars(~., data = hotel_reservation[categorical_vars]), newdata = hotel_reservation)
encoded_data <- as.data.frame(encoded_matrix)
str(encoded_data)
encoded_hotel_data <- hotel_reservation[, !(names(hotel_reservation) %in% c("type_of_meal_plan","room_type_reserved","market_segment_type"))]
encoded_hotel_data <- cbind(encoded_hotel_data,encoded_data)
str(encoded_hotel_data)

#Split the dataset into training and testing

#70-30 split [70-Training, 30-Testing]
set.seed(123)
split_data_e <- sample(1:nrow(encoded_hotel_data),size = round(0.70*nrow(encoded_hotel_data)))
train_data_e <- encoded_hotel_data[split_data_e, ]
test_data_e <- encoded_hotel_data[-split_data_e, ]
#Train and Test dataset
str(train_data_e)
str(test_data_e)

#proportion of booking status in train data
print("Train Data")
prop.table(table(train_data_e$booking_status))

#proportion of booking status in test data
print("Test Data")
prop.table(table(test_data_e$booking_status))

#V. SUPPORT VECTOR MACHINE (SVM) MODEL
#Base Models
#Linear kernel Model
linear_e <- ksvm(booking_status ~ ., data = train_data_e, kernel = "vanilladot")
print(linear_e)
#Make predictions on the test data
pred_e <- predict(linear_e, newdata = test_data_e,type="response")
confusionMatrix(pred_e,test_data_e$booking_status)
#Accuracy : 0.8028
#Precision : 0.7524
#Sensitivity is 0.6003

#Radial kernel Model
radial_e <- ksvm(booking_status ~ ., data = train_data_e, kernel = "rbfdot")
print(radial_e)
#Make predictions on the test data
pred_e2 <- predict(radial_e, newdata = test_data_e,type="response")
confusionMatrix(pred_e2,test_data_e$booking_status)
#Accuracy : 0.8355
#Precision : 0.8055
#Sensitivity is 0.6616


#Poly kernel Model
poly_e <- ksvm(booking_status ~ ., data = train_data_e, kernel = "polydot")
print(poly_e)
#Make predictions on the test data
pred_e3 <- predict(poly_e, newdata = test_data_e,type="response")
confusionMatrix(pred_e3,test_data_e$booking_status)
#Accuracy : 0.8029
#Precision : 0.7524
#Sensitivity is 0.6006


#tune
gamma_val <- 0.1
kpar = list(sigma = 1 / sqrt(2 * gamma_val))
tune_e <- ksvm(booking_status ~ ., data = train_data_e, kernel = "rbfdot",C = 1, kpar = kpar)
print(tune_e)
#Make predictions on the test data
predtune_e <- predict(tune_e, newdata = test_data_e,type="response")
confusionMatrix(predtune_e,test_data_e$booking_status)
#Accuracy : 0.834
#Precision : 0.8777
#Sensitivity is 0.5775

gamma_val <- 1
kpar = list(sigma = 1 / sqrt(2 * gamma_val))
tune_e2 <- ksvm(booking_status ~ ., data = train_data_e, kernel = "rbfdot",C = 1, kpar = kpar)
print(tune_e2)
#Make predictions on the test data
predtune_e2 <- predict(tune_e2, newdata = test_data_e,type="response")
confusionMatrix(predtune_e2,test_data_e$booking_status)
#Accuracy : 0.8533
#Precision : 0.8483
#Sensitivity is 0.6769


tune_e3 <- ksvm(booking_status ~ ., data = train_data_e, kernel = "rbfdot",C = 2)
print(tune_e3)
#Make predictions on the test data
predtune_e3 <- predict(tune_e3, newdata = test_data_e,type="response")
confusionMatrix(predtune_e3,test_data_e$booking_status)
#Accuracy : 0.8384
#Precision : 0.8069
#Sensitivity is 0.6710

tune_e4 <- ksvm(booking_status ~ ., data = train_data_e, kernel = "rbfdot",cross = 5)
print(tune_e4)
#Make predictions on the test data
predtune_e4 <- predict(tune_e4, newdata = test_data_e,type="response")
confusionMatrix(predtune_e4,test_data_e$booking_status)
#Accuracy : 0.8354
#Precision : 0.8054
#Sensitivity is 0.6613

##ADDITIONAL
#Factor To Numeric
hotel <- hotel_reservation
hotel$type_of_meal_plan <- as.numeric(hotel$type_of_meal_plan)
hotel$room_type_reserved <- as.numeric(hotel$room_type_reserved)
hotel$market_segment_type <- as.numeric(hotel$market_segment_type)
str(hotel_reservation)
str(hotel)
set.seed(123)
split_data <- sample(1:nrow(hotel),size = round(0.70*nrow(hotel)))
train_data <- hotel[split_data,]
test_data <- hotel[-split_data,]

#Train and Test dataset
str(train_data)
str(test_data)

#proportion of booking status in train data
print("Train Data")
prop.table(table(train_data$booking_status))

#proportion of booking status in test data
print("Test Data")
prop.table(table(test_data$booking_status))

#hence, the proportion of booking_status in the train and test data are similar


##Linear Model 
##C = 1
svm_linear <- ksvm(booking_status ~ ., data = train_data, kernel = "vanilladot")
print(svm_linear)
#Predict
pred_linear <- predict(svm_linear,newdata = test_data,type="response")
confusionMatrix(pred_linear,test_data$booking_status)
#Accuracy is 80.06%
#Precision is 75.68%
#Sensitivity is 58.36


##Radial
##C = 1
svm_radial <- ksvm(booking_status ~ ., data = train_data, kernel = "rbfdot")
print(svm_radial)
#Predict
pred_radial <- predict(svm_radial,newdata = test_data,type="response")
confusionMatrix(pred_radial,test_data$booking_status)
#Accuracy is 83.49%
#Precision is 80.59%
#Sensitivity is 65.85

##Poly
svm_poly <- ksvm(booking_status ~ ., data = train_data, kernel = "polydot")
print(svm_poly)
#Predict
pred_poly <- predict(svm_poly,newdata = test_data,type="response")
confusionMatrix(pred_poly,test_data$booking_status)
#Accuracy is 80.06
#Precision is 75.68
#Sensitivity is 58.36
#Out of the three models "rbfdot" has the highest accuracy.

#Hyperparameter Tuning
#Tuning the models using the following:

#C=2
svm_tune <- ksvm(booking_status ~ ., data = train_data, kernel = "rbfdot",C=2)
print(svm_tune)

#Predict
pred_tune <- predict(svm_tune,newdata = test_data,type="response")
confusionMatrix(pred_tune,test_data$booking_status)
#Accuracy is 84.2
#Precision is 81.49
#Sensitivity is 67.49

#cross = 5
svm_tune2 <- ksvm(booking_status ~ ., data = train_data, kernel = "rbfdot",cross = 5)
print(svm_tune2)

#Predict
pred_tune2 <- predict(svm_tune2,newdata = test_data,type="response")
confusionMatrix(pred_tune2,test_data$booking_status)
#Accuracy is 83.51
#Precision is 80.62
#Sensitivity is 65.88


#C=1, gamma = 0.1
gamma_val <- 0.1
kpar = list(sigma = 1 / sqrt(2 * gamma_val))
svm_tune3 <- ksvm(booking_status ~ ., data = train_data, kernel = "rbfdot",C=1,kpar = kpar)
print(svm_tune3)

#Predict
pred_tune3 <- predict(svm_tune3,newdata = test_data,type="response")
confusionMatrix(pred_tune3,test_data$booking_status)
#Accuracy is 83.52
#Precision is 88.00
#Sensitivity is 57.97

#C=1, gamma = 1
gamma_val <- 1
kpar = list(sigma = 1 / sqrt(2 * gamma_val))
svm_tune4 <- ksvm(booking_status ~ ., data = train_data, kernel = "rbfdot",C=1,kpar = kpar)
print(svm_tune4)

#Predict
pred_tune4 <- predict(svm_tune4,newdata = test_data,type="response")
confusionMatrix(pred_tune4,test_data$booking_status)
#Accuracy is 85.52
#Precision is 85.15
#Sensitivity is 67.99

