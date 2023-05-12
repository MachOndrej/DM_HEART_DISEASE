# libraries
library(CHAID)
library(caret)
library(partykit)
set.seed(12345)
heart_dis <- read.csv("HEART2.csv", head = TRUE, sep="\t")
#View(heart_dis)
df <- sample(2, nrow(heart_dis), replace = TRUE, prob = c(0.7, 0.3))
train_data <- heart_dis[df == 1, ]
test_data <- heart_dis[df == 2, ]

# Ctree (použito místo CHAID)
# Train a ctree model using the training data
ctree_model <- ctree(Indikace.srdecni.choroby ~ ., data = train_data)

# Predikce
predict_ctree <- predict(ctree_model, newdata = test_data, type = "response")
# Confusion matrix
tab3 <- table(predict_ctree, test_data$Indikace.srdecni.choroby)
View(tab3)
# Dostaneme více ... Důvod (asi):
#  Stále získáváme více předpokládaných tříd na pozorování
#  s type = "response", může to být proto, že rozhodovací strom
#  má více než jeden list, který splňuje kritéria pro toto pozorování
sum_fn <- sum(tab3[2:8, 1]) # suma False Negative bez prvního řádku
sum_tp <- sum(tab3[2:8, 2]) # suma True Positive bez prvního řádku

print(sum_fn)
print(sum_fp)

# create a new data frame with two columns
tab3[2, 1] <- sum_fn
tab3[2, 2] <- sum_tp
tab3 <- tab3[-c(3:nrow(tab3)), ]
rownames(tab3) <- c("False", "True")
colnames(tab3) <- c("False", "True")
View(tab3)
print(tab3)
#rownames(new_tab3) <- c("False", "True")
#View(new_tab3)

# Accuracy
ctree_accuracy <- 100 * (sum(diag(tab3)) / sum(tab3))
ctree_error <- 100 - car_accuracy
print(paste("CTree - Error: ", ctree_error, " %"))
print(paste("CTree - Accuracy: ", ctree_accuracy, " %"))