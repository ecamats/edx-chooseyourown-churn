##########################################################
# Loading of useful libraries and source file on github repo
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(treemapify)) install.packages("treemapify", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(fastAdaboost)) install.packages("fastAdaboost", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(DiagrammeR)) install.packages("DiagrammeR", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")

# Added some extra useful libraries that can come handy

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(GGally)
library(treemapify)
library(naivebayes)
library(kernlab)
library(gam)
library(Rborist)
library(fastAdaboost)
library(randomForest)
library(DiagrammeR)
library(pROC)

#import Telco Churn dataset:
#https://raw.githubusercontent.com/ecamats/edx-chooseyourown-churn/main/data/WA_Fn-UseC_-Telco-Customer-Churn.csv

raw_churn <- read.csv("https://raw.githubusercontent.com/ecamats/edx-chooseyourown-churn/main/data/WA_Fn-UseC_-Telco-Customer-Churn.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

str(raw_churn,strict.width="cut")

#simple functions to turn some Yes/No variables into 1's
#and zero's for better treatment or visualization later on
YesToOneFunction <- function(x){
  ifelse(x=="Yes", 1,0)
}

NoServiceToNo <- function(x){
  ifelse(x=="Yes", "Yes","No")
}


#general cleaning and transformation of certain variables aiming to an easier data exploration
#drop of records with NAs on Total charges since they are a minority
#adding new derivated variables such as contract completion rate...

transformed_churn <- raw_churn %>% 
  drop_na() %>%
  mutate(across(c(MultipleLines,OnlineSecurity:StreamingMovies), NoServiceToNo)) %>%
  mutate(tenure_years = tenure/12,
         Contract_length = ifelse(Contract == "Month-to-month",1,
                                  ifelse(Contract == "One year",12,24)),
         Contract_cycles = tenure/Contract_length,
         Contract_remainig = Contract_length*(1-Contract_cycles%%1),
         Contract_completion = round(Contract_cycles%%1*10)/10) %>%
  select(Churn, gender:TotalCharges,tenure_years:Contract_completion)


##########################################################
# Defining data partitions indexes for validation, training and test
##########################################################

set.seed(2021, sample.kind="Rounding")
#split of validation set out of the initial set

val_index <- createDataPartition(transformed_churn$Churn, times = 1, p = 0.2, list = FALSE)
clean_churn = transformed_churn %>% dplyr::slice(-val_index)

#clean_churn set will be kept for exploration purposes keeping original labels

test_index <- createDataPartition(clean_churn$Churn, times = 1, p = 0.2, list = FALSE)

#re-split of the non-validation part into training and test

##########################################################
# Creating dummy variables for all partitions for better model handling
##########################################################

y <- as.factor(transformed_churn$Churn)
y <- relevel(y, ref = "Yes")
dummies_model <- dummyVars(Churn ~ ., data=transformed_churn, fullRank = TRUE)
transformed_churn_mat <- predict(dummies_model, newdata = transformed_churn)

transformed_churn <- data.frame(transformed_churn_mat)
transformed_churn$Churn <- y

str(transformed_churn,strict.width="cut")

val_churn = transformed_churn %>% dplyr::slice(val_index)
model_churn = transformed_churn %>% dplyr::slice(-val_index)

test_set = model_churn %>% dplyr::slice(test_index)
train_set = model_churn %>% dplyr::slice(-test_index)

##########################################################
# Event definition and deep exploratory analysis
##########################################################

#definition of the event and key exploration metric --> churn_rate

clean_churn %>% group_by(Churn) %>% summarise(n = n()) %>% spread(Churn,n,fill=0) %>%
  mutate(CustomerCount=No+Yes, churn_rate=Yes/CustomerCount) 

avg_churn <- clean_churn %>% group_by(Churn) %>% summarise(n = n()) %>% spread(Churn,n,fill=0) %>%
  mutate(CustomerCount=No+Yes, churn_rate=Yes/CustomerCount) %>% pull(churn_rate)
clean_churn %>% group_by(Churn, gender) %>% summarise(n = n()) %>% spread(Churn,n, fill=0) %>%
  mutate(CustomerCount=No+Yes, churn_rate=Yes/CustomerCount) %>% knitr::kable()


#explore the demographics in absolute numbers and also their churn rates

clean_churn %>% mutate(SeniorCitizen = ifelse(SeniorCitizen == 1,"Senior","Youngster"),
                       Partner = ifelse(Partner == "Yes","with partner","alone"),
                       Dependents = ifelse(Dependents == "Yes","with dependents","")) %>%
  group_by(SeniorCitizen, Partner,Dependents,Churn) %>% summarize(n = n()) %>% 
  spread(Churn,n, fill=0) %>% mutate(CustomerCount=No+Yes, churn_rate=Yes/CustomerCount) %>%
  ggplot(aes(area=CustomerCount, label = paste(SeniorCitizen,Partner,Dependents,sep="\n",
                                               paste("(",as.character(CustomerCount),")",sep="")),
             subgroup=SeniorCitizen, subgroup2=Partner, subgroup3=Dependents, fill = churn_rate)) +
  geom_treemap() +
  geom_treemap_subgroup3_border(colour = "white", size = 2) +
  geom_treemap_text( colour = "white", place = "centre", grow = FALSE) +
  scale_fill_viridis_c() +
  ggtitle("Churn rate and number of customers by demographic group") +
  xlab("Area = number of subscribers") + labs(fill = "Churn rate")

#demographics play a significant role in churn with older alone population driving churn
#inversely younger population with partner and dependents have the lowest churn

#exploring the churn rate levels accross key variables

clean_churn %>% ggplot(aes(x=tenure, y=..count.., fill=Contract)) + geom_density(position = "stack")

#the customer based is composed predominantly of monthly customers with low tenure

clean_churn %>% group_by(Churn, Contract,tenure) %>% summarise(n = n()) %>% spread(Churn,n,fill=0) %>%
  mutate(CustomerCount=No+Yes, churn_rate=Yes/CustomerCount) %>%
  ggplot(aes(x=tenure, y=churn_rate)) + geom_smooth() + geom_point() +
  geom_hline(yintercept=avg_churn, linetype="dashed", color = "red") +
  facet_wrap(Contract ~ ., scales = "free_x")

#for monthly contracts the churn rate is higher for low tenure indicating early churn phenomena
#yearly or 2-year contracts have lower churn and seem to increase churn a later period

clean_churn %>% 
  mutate(across(c(PhoneService:MultipleLines,OnlineSecurity:StreamingMovies), YesToOneFunction)) %>%
  mutate(InternetService = ifelse(InternetService == "No",0,1)) %>%
  summarize(across(PhoneService:StreamingMovies,mean)) %>%
  gather(key = Service, value = Penetration) %>%
  ggplot(aes(x = reorder(Service,Penetration), y = Penetration)) + geom_col() + coord_flip() +
  ylab("Service Penetration")

#Phone and Internet services have high penetration across the customer base
#There is a series of additional services or add-ons that can be combined

clean_churn %>% 
  mutate(MainPlay = paste("Internet: ",InternetService,"\nPhone: ",PhoneService)) %>%
  mutate(across(c(PhoneService:MultipleLines,OnlineSecurity:StreamingMovies), YesToOneFunction)) %>%
  mutate(AddOnsCount = rowSums(across(c(MultipleLines,OnlineSecurity:StreamingMovies)))) %>%
  group_by(MainPlay,Churn,AddOnsCount) %>% summarize(n = n()) %>% 
  spread(Churn,n, fill=0) %>% mutate(CustomerCount=No+Yes, churn_rate=Yes/CustomerCount) %>%
  ggplot(aes(x=AddOnsCount,y=churn_rate)) + geom_col()  +
  geom_hline(yintercept=avg_churn, linetype="dashed", color = "red") +
  facet_grid(. ~ MainPlay)

clean_churn %>% 
  mutate(MainPlay = paste("Internet:",InternetService,"\nPhone:",PhoneService)) %>%
  mutate(across(c(PhoneService:MultipleLines,OnlineSecurity:StreamingMovies), YesToOneFunction)) %>%
  mutate(AddOnsCount = rowSums(across(c(MultipleLines,OnlineSecurity:StreamingMovies)))) %>%
  group_by(MainPlay,Churn,AddOnsCount) %>% summarize(n = n()) %>% 
  spread(Churn,n, fill=0) %>% mutate(CustomerCount=No+Yes, churn_rate=Yes/CustomerCount) %>%
  ggplot(aes(area=CustomerCount, label = paste(AddOnsCount,"Add-ons"),
             subgroup=MainPlay, subgroup2=AddOnsCount, fill = churn_rate)) +
  geom_treemap() +
  geom_treemap_subgroup2_border(colour = "white", size = 2) +
  geom_treemap_subgroup_border(colour = "black", size = 5) +
  geom_treemap_text( colour = "white", place = "bottomright", grow = FALSE, size = 12) +
  geom_treemap_subgroup_text( colour = "black", place = "topleft", grow = FALSE, size = 15) +
  scale_fill_viridis_c() +
  ggtitle("Churn rate and number of customers by product type and number of add-ons") +
  xlab("Area = number of subscribers") + labs(fill = "Churn rate")

#There is a very strong relation between the number of additional services or add-ons and the churn rate
#Much more than the main service itself (for example dsl + phone has higher churn than phone alone)

clean_churn %>%
  group_by(PaymentMethod,PaperlessBilling,Churn) %>% summarize(n = n()) %>% 
  spread(Churn,n, fill=0) %>% mutate(CustomerCount=No+Yes, churn_rate=Yes/CustomerCount) %>%
  mutate(PaperlessBilling = ifelse(PaperlessBilling=="Yes","Paperless","Physical Paper")) %>%
  ggplot(aes(x=PaymentMethod, y=churn_rate)) + geom_col() +
  geom_hline(yintercept=avg_churn, linetype="dashed", color = "red") +
  facet_grid(PaperlessBilling~.) + coord_flip()

#among different paying methods, electronic check seems to have higher churn rates
#when it comes to paperless billing, it would look like is in fact not driving lower churn

#exploring the churn rate levels accross monthly charges

clean_churn %>% 
  mutate(MainPlay = paste("Internet:",InternetService,"\nPhone:",PhoneService)) %>%
  mutate(across(c(PhoneService:MultipleLines,OnlineSecurity:StreamingMovies), YesToOneFunction)) %>%
  mutate(AddOnsCount = rowSums(across(c(MultipleLines,OnlineSecurity:StreamingMovies)))) %>%
  ggplot(aes(x=MonthlyCharges, y=..count..,fill=MainPlay)) + geom_histogram(position = "stack") 

#monthly charges seem to be well associated to different product configurations
#with less variability for the phone only lines and more spread for the different internet options

clean_churn %>% mutate(Strat_MonthlyCharges = round(MonthlyCharges/5,0)*5) %>%
  mutate(MainPlay = paste("Internet:",InternetService,"\nPhone:",PhoneService)) %>%
  group_by(MainPlay,Churn, Strat_MonthlyCharges) %>% summarise(n = n()) %>% spread(Churn,n) %>%
  mutate(churn_rate=Yes/(Yes+No)) %>% ggplot(aes(x=Strat_MonthlyCharges, y=churn_rate)) +
  geom_col() + facet_grid(. ~ MainPlay, scales = "free_x") +
  geom_hline(yintercept=avg_churn, linetype="dashed", color = "red")

#overall different products have different churn rates (particularly high in fiber optics)
#higher monthly charges seem to contribute positively to retain customers accross different products

##########################################################
# First simple model and results collection methodology
##########################################################

#naive model just guessing randomnly the churn status
prediction <- as.factor(sample(c("Yes","No"),nrow(test_set),replace = TRUE))
probability <- sample(c(1,0),nrow(test_set),replace = TRUE)

model_results <- data_frame(method = "random guessing",
        Accuracy = confusionMatrix(prediction, test_set$Churn)$overall["Accuracy"],
        Precision = confusionMatrix(prediction, test_set$Churn)$byClass["Precision"],
        Recall = confusionMatrix(prediction, test_set$Churn)$byClass["Recall"])

prediction_results <- data_frame(guess = prediction)

probability_results <- data_frame(guess = probability)

model_results %>% knitr::kable()

##########################################################
# Loop test of different algorithms with default settings with caret
##########################################################
models <- c("glm","lda","naive_bayes","rpart","knn","gamLoess")

fits <- lapply(models, function(model){ 
  train(Churn ~ ., method = model,data = train_set)
})
names(fits) <- models

predictions <- sapply(fits,function(object)
  predict(object, newdata = test_set))

probabilities <- sapply(fits,function(object)
  predict(object, newdata = test_set,type = "prob")$Yes)

predictions <- as.data.frame(predictions)
probabilities <- as.data.frame(probabilities)

accuracies <- sapply(predictions,function(x) 
  confusionMatrix(x,test_set$Churn)$overall["Accuracy"])
precisions <- sapply(predictions,function(x) 
  confusionMatrix(x,test_set$Churn)$byClass["Precision"])
recalls <- sapply(predictions,function(x) 
  confusionMatrix(x,test_set$Churn)$byClass["Recall"])

model_results <- bind_rows(model_results,data_frame(method = models, 
                                                    Accuracy = accuracies, 
                                                    Precision = precisions,
                                                    Recall = recalls))

prediction_results <- bind_cols(prediction_results, predictions)

probability_results <- bind_cols(probability_results, probabilities)
model_results %>% knitr::kable()

##########################################################
# test glm with less variables (5 top vars)
##########################################################

model_name <- "glm (5 top vars)"

fit <- train(Churn ~ ., method = "glm",data = train_set)
plot(varImp(fit))

fit <- train(Churn ~ Contract_cycles + PaperlessBillingYes +
               PaymentMethodElectronic.check + SeniorCitizen +
               tenure + ContractTwo.year + DependentsYes,
             method = "glm",data = train_set)

prediction <- predict(fit, test_set)
probability <- predict(fit, newdata = test_set,type = "prob")$Yes

model_results <- bind_rows(model_results,
                           data_frame(method = model_name,
                                      Accuracy = confusionMatrix(prediction, test_set$Churn)$overall["Accuracy"],
                                      Precision = confusionMatrix(prediction, test_set$Churn)$byClass["Precision"],
                                      Recall = confusionMatrix(prediction, test_set$Churn)$byClass["Recall"]))

prediction <- as.data.frame(prediction)
probability <- as.data.frame(probability)

names(prediction) <- model_name
names(probability) <- model_name

model_results %>% filter (method == "glm" | method == "glm (5 top vars)") %>% knitr::kable()

prediction_results <- bind_cols(prediction_results, prediction)
probability_results <- bind_cols(probability_results, probability)

##########################################################
# Random Forest - rf
##########################################################

model_name <- "rf (ntree = 64)"
fit <- train(Churn ~ ., method = "rf",data = train_set, ntree = 64)
fit$bestTune
fit$results
plot(varImp(fit))
plot(fit$finalModel)

prediction <- predict(fit, test_set)
probability <- predict(fit, newdata = test_set,type = "prob")$Yes

model_results <- bind_rows(model_results,
                           data_frame(method = model_name,
                                      Accuracy = confusionMatrix(prediction, test_set$Churn)$overall["Accuracy"],
                                      Precision = confusionMatrix(prediction, test_set$Churn)$byClass["Precision"],
                                      Recall = confusionMatrix(prediction, test_set$Churn)$byClass["Recall"]))

prediction <- as.data.frame(prediction)
probability <- as.data.frame(probability)

names(prediction) <- model_name
names(probability) <- model_name

model_results %>% knitr::kable()

prediction_results <- bind_cols(prediction_results, prediction)
probability_results <- bind_cols(probability_results, probability)

##########################################################
# Support Vector Machine - svmLinear2
##########################################################

#svmLinear2 has the possibility to generate probabilities

model_name <- "svmLinear2"
fit <- train(Churn ~ ., method = "svmLinear2",data = train_set, probability=TRUE)
prediction <- predict(fit, test_set)
probability <- predict(fit, newdata = test_set,type = "prob")$Yes

model_results <- bind_rows(model_results,
                           data_frame(method = model_name,
                                      Accuracy = confusionMatrix(prediction, test_set$Churn)$overall["Accuracy"],
                                      Precision = confusionMatrix(prediction, test_set$Churn)$byClass["Precision"],
                                      Recall = confusionMatrix(prediction, test_set$Churn)$byClass["Recall"]))

prediction <- as.data.frame(prediction)
probability <- as.data.frame(probability)

names(prediction) <- model_name
names(probability) <- model_name

model_results %>% knitr::kable()

prediction_results <- bind_cols(prediction_results, prediction)
probability_results <- bind_cols(probability_results, probability)

###################################
# Adaptative Boosting - adaboost
###################################

model_name <- "adaboost (nIter = 100)"
fit <- adaboost(Churn~., train_set, nIter = 100)
prediction <- predict(fit, test_set)$class
probability <- predict(fit, test_set)$prob[,1]

model_results <- bind_rows(model_results,
                           data_frame(method = model_name,
                                      Accuracy = confusionMatrix(prediction, test_set$Churn)$overall["Accuracy"],
                                      Precision = confusionMatrix(prediction, test_set$Churn)$byClass["Precision"],
                                      Recall = confusionMatrix(prediction, test_set$Churn)$byClass["Recall"]))

prediction <- as.data.frame(prediction)
probability <- as.data.frame(probability)

names(prediction) <- model_name
names(probability) <- model_name
model_results %>% knitr::kable()
prediction_results <- bind_cols(prediction_results, prediction)
probability_results <- bind_cols(probability_results, probability)

###################################
# Ensemble - voting system
###################################

model_name <- "ensemble (voting)"
votes <- data_frame(votes = prediction_results %>% select(-guess) %>%
  mutate(across(.cols = everything(), YesToOneFunction)) %>% rowMeans)

prediction <- votes %>% mutate(prediction = as.factor(ifelse(votes > 0.5,"Yes","No"))) %>% pull(prediction)
model_results <- bind_rows(model_results,
                           data_frame(method = model_name,
                                      Accuracy = confusionMatrix(prediction, test_set$Churn)$overall["Accuracy"],
                                      Precision = confusionMatrix(prediction, test_set$Churn)$byClass["Precision"],
                                      Recall = confusionMatrix(prediction, test_set$Churn)$byClass["Recall"]))

prediction <- as.data.frame(prediction)
probability <- as.data.frame(votes)

names(prediction) <- model_name
names(probability) <- model_name

model_results %>% knitr::kable()
names(prediction) <- model_name

prediction_results <- bind_cols(prediction_results, prediction)
probability_results <- bind_cols(probability_results, probability)

#######################################################################
# Accuracy plots
#######################################################################

model_results %>% ggplot(aes(x=reorder(method,-Accuracy), y=Accuracy)) + 
  geom_col() + coord_flip() + xlab("Method (by ascending Accuracy)")

#######################################################################
# ROC and AUC plots
#######################################################################

#Initiatie first plot with random model

par(pty = "s")

steps <- 0.04

#Overlay the rest of the models

roc(test_set$Churn, probability_results$guess, plot=TRUE, legacy.axes=TRUE,
    xlab="False Positive Percentage", ylab="True Positive Percentage",
    col="thistle", lwd=1, print.auc=TRUE, print.auc.x=0.7)

plot.roc(test_set$Churn, probability_results$glm,col="blue",lwd=1,
           print.auc=TRUE, add=TRUE, print.auc.y=0.5-1*steps, print.auc.x=0.7)

plot.roc(test_set$Churn, probability_results$lda,col="red",lwd=1,
         print.auc=TRUE, add=TRUE, print.auc.y=0.5-2*steps, print.auc.x=0.7)

plot.roc(test_set$Churn, probability_results$naive_bayes,col="green",lwd=1,
         print.auc=TRUE, add=TRUE, print.auc.y=0.5-3*steps, print.auc.x=0.7)

plot.roc(test_set$Churn, probability_results$rpart,col="grey",lwd=1,
         print.auc=TRUE, add=TRUE, print.auc.y=0.5-4*steps, print.auc.x=0.7)

plot.roc(test_set$Churn, probability_results$knn,col="darkorange",lwd=1,
         print.auc=TRUE, add=TRUE, print.auc.y=0.5-5*steps, print.auc.x=0.7)

plot.roc(test_set$Churn, probability_results$gamLoess,col="darkgreen",lwd=1,
         print.auc=TRUE, add=TRUE, print.auc.y=0.5-6*steps, print.auc.x=0.7)

plot.roc(test_set$Churn, probability_results$`rf (ntree = 64)`,col="brown",lwd=1,
         print.auc=TRUE, add=TRUE, print.auc.y=0.5-7*steps, print.auc.x=0.7)

plot.roc(test_set$Churn, probability_results$svmLinear2,col="purple",lwd=1,
         print.auc=TRUE, add=TRUE, print.auc.y=0.5-8*steps, print.auc.x=0.7)

plot.roc(test_set$Churn, probability_results$`adaboost (nIter = 100)`,col="darkblue",lwd=1,
         print.auc=TRUE, add=TRUE, print.auc.y=0.5-9*steps, print.auc.x=0.7)

plot.roc(test_set$Churn, probability_results$`ensemble (voting)`,col="black",lwd=1,
         print.auc=TRUE, add=TRUE, print.auc.y=0.5-10*steps, print.auc.x=0.7)

legend("bottomright",
       legend=c("guess",models,"rf","svmLinear2","adaboost","ensemble (voting)"),
       col=c("thistle","blue","red","green","grey","darkorange","darkgreen",
             "brown","purple","darkblue","black"),lwd=2, cex =0.6)

par(pty = "m")

#######################################################################
# Tuning of gamLoess for better results
#######################################################################
modelLookup("gamLoess")
#stick to degree of 1 like in rafa's textbook but play with the span values
#default parameters gave a best tune with span = 0.5
fits$gamLoess$bestTune

grid <- expand.grid(span = seq(0.25, 0.85, len = 10), degree = 1)
#we will not test degree grid because there is an identified bug on gamLoess package :(
#more info in https://stackoverflow.com/questions/32043010/r-crashes-when-training-using-caret-and-method-gamloess
fit_tune <- train(Churn ~ ., method = "gamLoess", data = train_set,tuneGrid=grid)

ggplot(fit_tune, highlight = TRUE)
fit_tune$bestTune

#######################################################################
# Generating meaningful outputs of selected model
#######################################################################

fit <- fits$gamLoess

prediction <- predict(fit, test_set)
probability <- predict(fit, newdata = test_set,type = "prob")$Yes

#confusion matrix

final_results <- data_frame(method = "gamLoess (final)",
                            Accuracy = confusionMatrix(prediction, test_set$Churn)$overall["Accuracy"],
                            Precision = confusionMatrix(prediction, test_set$Churn)$byClass["Precision"],
                            Recall = confusionMatrix(prediction, test_set$Churn)$byClass["Recall"])

final_results %>% knitr::kable()
confusionMatrix(prediction, test_set$Churn)$table %>% knitr::kable()
plot(confusionMatrix(prediction, test_set$Churn)$table)

#gain and lift analysis preparation

output <- data_frame(prediction = prediction_results$gamLoess,
                      probabilities = probability_results$gamLoess,
                      actuals = test_set$Churn,
                      MonthlyCharges = test_set$MonthlyCharges)

totals <- output %>% summarize(Decile = "Total",
                                Subscribers = n(),
                                Churners = sum(actuals == "Yes"),
                                Churn_Rate = round(Churners / Subscribers,2),
                                Revenues = round(sum(MonthlyCharges)),
                                MonthlyCharges = round(mean(MonthlyCharges))) %>%
  mutate(Cum_Churners = Churners, Cum_Subscribers = Subscribers)

deciles <- output %>% mutate(decile_rank = -ntile(probabilities,10)) %>%
  arrange(decile_rank) %>%  mutate(Decile = as.character(11+decile_rank)) %>%
  mutate(Decile = fct_reorder(Decile, decile_rank)) %>%
  group_by(decile_rank) %>% summarize(Decile = first(Decile),
                                  Subscribers = n(),
                                  Churners = sum(actuals == "Yes"),
                                  Churn_Rate = round(Churners / Subscribers,2),
                                  Revenues = round(sum(MonthlyCharges)),
                                  MonthlyCharges = round(mean(MonthlyCharges))) %>% 
  select(-decile_rank) %>% mutate(Cum_Churners = cumsum(Churners),
                                  Cum_Subscribers = cumsum(Subscribers))


deciles <- bind_rows(deciles,totals) %>% 
  mutate(Lift = round(Churn_Rate/totals$Churn_Rate,1),
         Gain_Score = round(Cum_Churners/totals$Churners,4)*100,
         Cum_Lift = round(Gain_Score/Cum_Subscribers*totals$Subscribers/100,2))

#gain and lift analysis result table

deciles %>% select(Decile,Subscribers,Churners,Churn_Rate,Cum_Churners,Gain_Score,Cum_Lift) %>% knitr::kable()

deciles %>% filter(Decile != "Total") %>% ggplot() +
  geom_line(aes(x=reorder(Decile,as.numeric(Decile)),y=Gain_Score,group=1)) + 
  geom_point(aes(x=reorder(Decile,as.numeric(Decile)),y=Gain_Score,group=1)) + 
  xlab("Deciles (churn prediction probability)") +
  ylab("Gain Score (cumulative coverage)")

deciles %>% filter(Decile != "Total") %>% ggplot() +
  geom_line(aes(x=reorder(Decile,as.numeric(Decile)),y=Cum_Lift,group=1)) + 
  geom_point(aes(x=reorder(Decile,as.numeric(Decile)),y=Cum_Lift,group=1)) + 
  xlab("Deciles (churn prediction probability)") +
  ylab("Cumulative Lift")

#simulation of real discount campaign

discount <- 0.3 #30% discount on current bill

opt_in = data_frame(churner = 0.5, non_churner = 0.8)

revenues <- deciles %>% select(Decile,Subscribers,Churn_Rate, MonthlyCharges, Gain_Score) %>% filter(Decile != "Total") %>%
  mutate(
    RevLoss_DoNothing        = round(-Subscribers*Churn_Rate                                      *MonthlyCharges),
    RevLoss_Churners         = round(-Subscribers*Churn_Rate     *(1-opt_in$churner)              *MonthlyCharges),
    RevLoss_Saved            = round(-Subscribers*Churn_Rate     *(opt_in$churner)      *discount *MonthlyCharges),
    RevLoss_Cannibalized     = round(-Subscribers*(1-Churn_Rate) *(opt_in$non_churner)  *discount *MonthlyCharges),
    RevLoss_DoSomething      = RevLoss_Churners + RevLoss_Saved + RevLoss_Cannibalized,
    NetImpact                = RevLoss_DoSomething - RevLoss_DoNothing,
    Cum_NetImpact            = cumsum(NetImpact)
         )

revenues %>% select(Decile,RevLoss_DoNothing,RevLoss_DoSomething, NetImpact, Gain_Score,Cum_NetImpact) %>% knitr::kable()
  
revenues %>% ggplot() +
  geom_bar(aes(x=reorder(Decile,as.numeric(Decile)),y=NetImpact),stat='identity') +
  geom_line(aes(x=reorder(Decile,as.numeric(Decile)),y=Cum_NetImpact,group=1,linetype = "")) +
  geom_point(aes(x=reorder(Decile,as.numeric(Decile)),y=Cum_NetImpact,group=1,linetype = "")) +
  labs(linetype = "Cumulative") + xlab("Deciles (churn prediction probability)") +
  ylab("Net Revenue Impacts")

#revenue impact on business

rev_base <- sum(revenues$MonthlyCharges * revenues$Subscribers)
loss_donothing <- sum(revenues$RevLoss_DoNothing)
loss_top3 <- sum(revenues$RevLoss_DoSomething[1:3])
loss_bottom7 <- sum(revenues$RevLoss_DoNothing[4:7])

old_case <- rev_base+loss_donothing
new_case <- rev_base+loss_top3+loss_bottom7

(new_case-old_case)/rev_base*100


#######################################################################
# Final hold-out test on validation set
#######################################################################

#build the final model on the largest training available set and test on validation
fit <- train(Churn ~ ., method = "gamLoess",data = model_churn)
prediction <- predict(fit, val_churn)
probability <- predict(fit, newdata = val_churn,type = "prob")$Yes

final_results <- data_frame(method = "gamLoess (holdout)",
                            Accuracy = confusionMatrix(prediction, val_churn$Churn)$overall["Accuracy"],
                            Precision = confusionMatrix(prediction, val_churn$Churn)$byClass["Precision"],
                            Recall = confusionMatrix(prediction, val_churn$Churn)$byClass["Recall"])

final_results %>% filter (method == "gamLoess (final)" | method == "gamLoess (holdout)") %>% knitr::kable()

#gain and lift analysis results

output <- data_frame(prediction = prediction,
                     probabilities = probability,
                     actuals = val_churn$Churn,
                     MonthlyCharges = val_churn$MonthlyCharges)

totals <- output %>% summarize(Decile = "Total",
                               Subscribers = n(),
                               Churners = sum(actuals == "Yes"),
                               Churn_Rate = round(Churners / Subscribers,2),
                               Revenues = round(sum(MonthlyCharges)),
                               MonthlyCharges = round(mean(MonthlyCharges))) %>%
  mutate(Cum_Churners = Churners, Cum_Subscribers = Subscribers)

deciles <- output %>% mutate(decile_rank = -ntile(probabilities,10)) %>%
  arrange(decile_rank) %>%  mutate(Decile = as.character(11+decile_rank)) %>%
  mutate(Decile = fct_reorder(Decile, decile_rank)) %>%
  group_by(decile_rank) %>% summarize(Decile = first(Decile),
                                      Subscribers = n(),
                                      Churners = sum(actuals == "Yes"),
                                      Churn_Rate = round(Churners / Subscribers,2),
                                      Revenues = round(sum(MonthlyCharges)),
                                      MonthlyCharges = round(mean(MonthlyCharges))) %>% 
  select(-decile_rank) %>% mutate(Cum_Churners = cumsum(Churners),
                                  Cum_Subscribers = cumsum(Subscribers))


deciles <- bind_rows(deciles,totals) %>% 
  mutate(Lift = round(Churn_Rate/totals$Churn_Rate,1),
         Gain_Score = round(Cum_Churners/totals$Churners,4)*100,
         Cum_Lift = round(Gain_Score/Cum_Subscribers*totals$Subscribers/100,2))

#gain and lift analysis result table

deciles %>% select(Decile,Subscribers,Churners,Churn_Rate,Cum_Churners,Gain_Score,Cum_Lift) %>% knitr::kable()

deciles %>% filter(Decile != "Total") %>% ggplot() +
  geom_line(aes(x=reorder(Decile,as.numeric(Decile)),y=Gain_Score,group=1)) + 
  geom_point(aes(x=reorder(Decile,as.numeric(Decile)),y=Gain_Score,group=1)) + 
  xlab("Deciles (churn prediction probability)") +
  ylab("Gain Score (cumulative coverage)")

deciles %>% filter(Decile != "Total") %>% ggplot() +
  geom_line(aes(x=reorder(Decile,as.numeric(Decile)),y=Cum_Lift,group=1)) + 
  geom_point(aes(x=reorder(Decile,as.numeric(Decile)),y=Cum_Lift,group=1)) + 
  xlab("Deciles (churn prediction probability)") +
  ylab("Cumulative Lift")

#simulation of real discount campaign

revenues <- deciles %>% select(Decile,Subscribers,Churn_Rate, MonthlyCharges, Gain_Score) %>% filter(Decile != "Total") %>%
  mutate(
    RevLoss_DoNothing        = round(-Subscribers*Churn_Rate                                      *MonthlyCharges),
    RevLoss_Churners         = round(-Subscribers*Churn_Rate     *(1-opt_in$churner)              *MonthlyCharges),
    RevLoss_Saved            = round(-Subscribers*Churn_Rate     *(opt_in$churner)      *discount *MonthlyCharges),
    RevLoss_Cannibalized     = round(-Subscribers*(1-Churn_Rate) *(opt_in$non_churner)  *discount *MonthlyCharges),
    RevLoss_DoSomething      = RevLoss_Churners + RevLoss_Saved + RevLoss_Cannibalized,
    NetImpact                = RevLoss_DoSomething - RevLoss_DoNothing,
    Cum_NetImpact            = cumsum(NetImpact)
  )

revenues %>% select(Decile,RevLoss_DoNothing,RevLoss_DoSomething, NetImpact, Gain_Score,Cum_NetImpact) %>% knitr::kable()

revenues %>% ggplot() +
  geom_bar(aes(x=reorder(Decile,as.numeric(Decile)),y=NetImpact),stat='identity') +
  geom_line(aes(x=reorder(Decile,as.numeric(Decile)),y=Cum_NetImpact,group=1,linetype = "")) +
  geom_point(aes(x=reorder(Decile,as.numeric(Decile)),y=Cum_NetImpact,group=1,linetype = "")) +
  labs(linetype = "Cumulative") + xlab("Deciles (churn prediction probability)") +
  ylab("Net Revenue Impacts")

#revenue impact on business

rev_base <- sum(revenues$MonthlyCharges * revenues$Subscribers)
loss_donothing <- sum(revenues$RevLoss_DoNothing)
loss_top3 <- sum(revenues$RevLoss_DoSomething[1:3])
loss_bottom7 <- sum(revenues$RevLoss_DoNothing[4:7])

old_case <- rev_base+loss_donothing
new_case <- rev_base+loss_top3+loss_bottom7

(new_case-old_case)/rev_base*100

#######################################################################
# End of script
#######################################################################
