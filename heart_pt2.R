# Import knihove
library(neuralnet)
library(rpart)
#install.packages("partykit", repos = 'http://cran.us.r-project.org', dependencies = TRUE)
library(partykit)
library(rattle)
library(caret)
# Příprava dat
# Načtení souboru upraveného pomocí Feature selection
heart_dis <- read.csv("HEART2.csv", head = TRUE, sep = "\t")

# Zobrazení datasetu
#View(heart_dis)

# Zobrazení struktury datasetu
#str(heart_dis)

# Zobrazení statistických údajů o datasetu
#summary(heart_dis)

# Nastavení stejného scale datasetu (kvůli pruměru a standatní odchylce)
heart_dis[1:13] <- scale(heart_dis[1:13])

# Seed
set.seed(12345)

# Rozdělení na testovací a trénovací (70:30)
ind <- sample(2, nrow(heart_dis), replace = TRUE, prob = c(0.7, 0.3))
train_data <- heart_dis[ind == 1, ]
test_data <- heart_dis[ind == 2, ]

# Neural Net:
# Tvorba modelu
nn <- neuralnet(formula = Indikace.srdecni.choroby ~ .,
                data = train_data, hidden = 5,
                err.fct = "sse", linear.output = FALSE)
# Vizualizace neuronové sítě
#plot(nn)

# Predikce na trénovacích datech
#mypredict <- compute(nn, nn$covariate)$net.result
#mypredict <- apply(mypredict, c(1), round)

# Prvních 20 testovacích predkcí
#mypredict[1:20]

# Creates the confusion matrix on the training data.
#table(mypredict, train_data$Indikace.srdecni.choroby, dnn = c("Predicted", "Actual"))

# Predikce na testovacích datech
pred_nn <- compute(nn, test_data[, 0:12])$net.result
pred_nn <- apply(pred_nn, c(1), round)

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

# Accuracy [%] = 100*((TP + TN)/(TP + TN + FP + FN))
# Error [%] = 100 - Accuracy

# Tvorba matice záměn
tab1 <- table(pred_nn, test_data$Indikace.srdecni.choroby)
rownames(tab1) <- c("False", "True")
colnames(tab1) <- c("False", "True")
# Zobrazení
View(tab1)

# Výpočet chyby a přesnosti pro Neural net
nn_accuracy <- 100 * (sum(diag(tab1)) / sum(tab1)) # v procentech
nn_error <- 100 - nn_accuracy
print(paste("Neural Net - Error: ", nn_error, " %"))
print(paste("Neural Net - Accuracy: ", nn_accuracy, " %"))

# C&R Tree:
# Tvorba modelu C&R tree
car_tree <- rpart(Indikace.srdecni.choroby ~ .,
    data = train_data,
    method = "class")

# Predikce
predict_tree <- predict(car_tree, test_data, type = "class")

# Confusion matrix
tab2 <- table(predict_tree, test_data$Indikace.srdecni.choroby)
rownames(tab2) <- c("False", "True")
colnames(tab2) <- c("False", "True")
View(tab2)

# Přestnost a chyba stromu
car_accuracy <- 100 * (sum(diag(tab2)) / sum(tab2))
car_error <- 100 - car_accuracy
print(paste("C&R Tree - Error: ", car_error, " %"))
print(paste("C&R Tree - Accuracy: ", car_accuracy, " %"))

# Ctree (použito místo CHAID):
# Trénování modelu
ctree_model <- ctree(Indikace.srdecni.choroby ~ ., data = train_data)

# Predikce
predict_ctree <- predict(ctree_model, newdata = test_data, type = "response")
plot(predict_ctree)
# Confusion matrix
tab3 <- table(predict_ctree, test_data$Indikace.srdecni.choroby)
#View(tab3)
# Dostaneme více ... Důvod (asi):
#  Stále získáváme více předpokládaných tříd na pozorování
#  s type = "response", může to být proto, že rozhodovací strom
#  má více než jeden list, který splňuje kritéria pro toto pozorování
sum_fn <- sum(tab3[2:8, 1]) # suma False Negative bez prvního řádku
sum_tp <- sum(tab3[2:8, 2]) # suma True Positive bez prvního řádku

#print(sum_fn)
#print(sum_tp)

# Práce s tabulkou
tab3[2, 1] <- sum_fn
tab3[2, 2] <- sum_tp
tab3 <- tab3[-c(3:nrow(tab3)), ]
rownames(tab3) <- c("False", "True")
colnames(tab3) <- c("False", "True")
#View(tab3)

# Přesnost a chyba  ==> VÝSLEDNÉ HODNOTY NEDÁVAJÍ SMYSL
ctree_accuracy <- 100 * (sum(diag(tab3)) / sum(tab3))
ctree_error <- 100 - ctree_accuracy
print(paste("CTree - Error: ", ctree_error, " %"))
print(paste("CTree - Accuracy: ", ctree_accuracy, " %"))