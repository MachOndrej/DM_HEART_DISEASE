# Loads the neuralnet package into the system.
library(neuralnet)
# DATA PREP
# Opens the CSV file
heart_dis <- read.csv("HEART2.csv", head=TRUE, sep="\t")

# Set target to binary
#heart_dis$Indikace.srdecni.choroby <- ifelse(heart_dis$Indikace.srdecni.choroby == 2, 1, 0)

# Previews the heart disease dataset.
#View(heart_dis)

# Displays the structure of the data
#str(heart_dis)

# Displays the descriptive statistics for all variables in the dataset.  This
# shows whether all variables are numeric and if there are any missing values.
#summary(heart_dis)

# Sets the input variables to the same scale, such that they have the same
# mean and standard deviation.
heart_dis[1:12] <- scale(heart_dis[1:12])

# Verifies that the variables are set to the same scale.  All variables
# except for "fstat" should have a mean equal to 0.
#print(summary(heart_dis))

# Generates a random seed to allow us to reproduce the results.
set.seed(12345)

# The following code splits the dataset into a training set consisting of
# 70% of the data and a test set containing 30% of the data.
ind <- sample(2, nrow(heart_dis), replace = TRUE, prob = c(0.7, 0.3))
train_data <- heart_dis[ind == 1, ]
test_data <- heart_dis[ind == 2, ]

# Neural Net
# https://github.com/DaanishAhmed/R-Neural-Network
nn <- neuralnet(formula = Indikace.srdecni.choroby ~ .,
                data = train_data, hidden = 5,
                err.fct = "sse", linear.output = FALSE)

# Shows the summary of the model.
#summary(nn)

# We will examine the following properties: the response values, result
# matrix, and net result probabilities.

# Shows the values of the dependent variable for the first 20 observations.
nn$response[1:20]
# Shows the probability for the first 20 records.
nn$net.result[[1]][1:20]
# Shows the network result matrix, which includes information on the number
# of training steps, the error, and the weights.
nn$result.matrix

# Creates a visualization of the neural network.
#plot(nn)

# Computes the predicted values for the training set and rounds them to the
# nearest integer (either 0 or 1).
mypredict <- compute(nn, nn$covariate)$net.result
mypredict <- apply(mypredict, c(1), round)

# Shows the first 20 predicted values.
mypredict[1:20]

# Creates the confusion matrix on the training data.
table(mypredict, train_data$Indikace.srdecni.choroby, dnn =c("Predicted", "Actual"))

# Computes the predicted values for the test set and rounds them to the
# nearest integer (either 0 or 1).
testPred <- compute(nn, test_data[, 0:12])$net.result
testPred <- apply(testPred, c(1), round)

#  (Matice záměn = Confusion matrix)
#		  	          PREDIKCE
#			       FALSE     TRUE
# _________________________________
# R | FALSE     |  TN    |    FP   |
# E |       	|	     |  	   |
# A |___________|________|_________|
# L |		    |	     |  	   |
# I | TRUE      |  FN    |    TP   |
# T |       	|	     |	       |
# A |___________|________|_________|

# Accuracy = 100 - 100*((TP + TN)/(TP + TN + FP + FN))

# Creates the confusion matrix on the test data.
tab1 <- table(testPred, test_data$Indikace.srdecni.choroby, dnn = c("Predicted", "Actual"))
#print(tab1)
# Computes Missclassification error
mc_error <- (1 - sum(diag(tab1)) / sum(tab1))
mc_error <- mc_error * 100 # v procentech
print(paste("Neural Net - Error: ", mc_error, " %"))
nn_accuracy <- 100 - mc_error
print(paste("Neural Net - Accuracy: ", nn_accuracy, " %"))

# C&R Tree
# Load the 'rpart' package
library(rpart)

# Build a C&R tree for classification
car_tree <- rpart(Indikace.srdecni.choroby ~ .,
    data = train_data,
    method = "class")
#print(mytree)

# predikce (model, data, type)
predict_tree <- predict(car_tree, test_data, type = 'class')
# Confusion matrix
tab2 <- table(test_data$Indikace.srdecni.choroby, predict_tree)
tab2
# Accuracy
car_accuracy <- 100 * sum(diag(tab2)) / sum(tab2)
car_error <- 100 - car_accuracy
print(paste("C&R Tree - Error: ", car_error, " %"))
print(paste("C&R Tree - Accuracy: ", car_accuracy, " %"))

# CHAID
