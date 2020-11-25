###IDV LEARNERS PROJECT###

####loading libraries####
library("readxl")
library("zoo")
library("forecast")
library("tidyverse")
library("dplyr")
library("TTR")
library("GGally")
library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(xts)
library(TSstudio)
library(tidyverse)
library(rstatix)
library(ggpubr)


#####loading the dataset####
dat <- read_excel("ratesmx.xlsx", sheet = 1, col_names = FALSE)
dat <- dat[-c(1,2,4,5,6),] #we remove useless rows
colnames(dat) <- dat[1,] #assign names to columns
colnames(dat)[1] <- "date" 
names <- colnames(dat) #store names on a separate vector
dat <- dat[-c(1),] #remove names
dat <- as.matrix(dat) #transform to matrix
num_col <- ncol(dat) #extract the number of columns 
num_row <- nrow(dat) # extract the number of rows
dat <- mapply(dat, FUN=as.numeric, nrow = length(dat)) #transform to numeric 
#using extracted number of rows to preserve dimension
dat <- matrix(dat, ncol=num_col, nrow=num_row) #create a matrix again
#using extracted number of columns and rows to preserve dimension
dat <- data.frame(dat) # as data frame
dat[,1] <- as.Date(dat[,1], origin = "1899-12-30") #transform column one to dates
colnames(dat) <- names #restore names
###dat <- xts(x=dat[,-1],order.by= (dat$date)) #set as time series
#now we can work with the data


#now lest examine the data

# a summary of the data
summary(dat)

#boxplot of irs curve
boxplot((dat[,2:11]))


#visually examine the series#graph of the historical values of the IRS curve
dat %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = overnight), colour="#000000") +
  geom_line(aes(y = irs3m), colour="#2F4F4F") +
  geom_line(aes(y = irs6m), colour="#708090") +
  geom_line(aes(y = irs9m), colour="#778899") +
  geom_line(aes(y = irs1yr), colour="#696969") +
  geom_line(aes(y = irs2yr), colour="#808080") +
  geom_line(aes(y = irs3yr), colour="#A9A9A9") +
  geom_line(aes(y = irs4yr), colour="#C0C0C0") +
  geom_line(aes(y = irs5yr), colour="#0000FF") +
  geom_line(aes(y = irs7yr), colour="#6495ED") +
  geom_line(aes(y = irs10yr), colour="#00BFFF") + 
  xlab("years") + ylab("rate (%)") +
  ggtitle("Interest Rate Swaps - Mexico")


#10 year rates and base rates
#graph of the historical values of the overnight rate and the 10yr swap
dat %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = overnight), colour="#000000") +
  geom_line(aes(y = irs10yr), colour="#2F4F4F") +
  xlab("years") + ylab("rate (%)") +
  ggtitle("Interest Rates (10yr and overnight)  - Mexico")

dat %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = irs10yr), colour="#2F4F4F") +
  geom_line(aes(y = us10yr), colour="#778899") +
  xlab("years") + ylab("rate (%)") +
  ggtitle("Interest Rates (10yr)  - Mexico and US")
  

dat %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = irs10yr), colour="#2F4F4F") +
  geom_line(aes(y = mxn), colour="#778899") +
  xlab("years") +
  ggtitle("Interest Rates and FX  - Mexico") + 
  scale_y_continuous(
    name = "rate (%)",
    sec.axis = sec_axis(trans=~., name ="mxn")
  )

  
######split#####
#we will create a data partition in training and testing sets
set.seed(31416, sample.kind = "Rounding")
test_index <- createDataPartition(dat$irs3m, times = 1, p = 0.2, list = FALSE) # create a 20% test set
test_set <- dat[test_index,]
train_set <- dat[-test_index,]

#correlation between all IRS 
cor(train_set[,2:11])



#####base model#####
set.seed(1, sample.kind = "Rounding")
train_lm_1 <- train(irs10yr ~ overnight
                    , method = "lm", data = train_set)

#we observe the coefficients of the first model
train_lm_1[["finalModel"]][["coefficients"]]

#get training set RMSE
train_lm_1[["results"]][["RMSE"]]



#####linear model number 2####
set.seed(1, sample.kind = "Rounding")
train_lm_2 <- train(irs10yr ~ overnight + fed + us10yr + mxn
                    , method = "lm", data = train_set,
                    tuneGrid  = expand.grid(intercept = FALSE))


#displays the coefficients of the second model
train_lm_2[["finalModel"]][["coefficients"]]

#get RMSE
train_lm_2[["results"]][["RMSE"]]


####linear 3 model####
#liner model using all the variables and without intercept
set.seed(1, sample.kind = "Rounding")
train_lm_3 <- train(irs10yr ~ .
                    ,method = "lm", data = train_set,
                    tuneGrid  = expand.grid(intercept = FALSE))


#this will display the coefficients of all the variables
train_lm_3[["finalModel"]][["coefficients"]]

#RMSE for training set
train_lm_3[["results"]][["RMSE"]]
  
#####pca####
#correlation between all IRS 
cor(train_set[,2:11])

#pca
x <- train_set[,2:11] %>% as.matrix() #we set the IRS curve as an x matrix
pca <- prcomp(x) 
summary(pca)

#compute first pca vs overnigth rate
plot(pca$x[,1], train_set$overnight)
cor(pca$x[,1], train_set$overnight)


#compute second pca vs pendiente
plot(pca$x[,2], train_set$irs10yr - train_set$irs2yr)
cor(pca$x[,2], train_set$irs10yr - train_set$irs2y)


#compute second pca vs convexity
plot(pca$x[,3], train_set$irs2yr - train_set$irs5yr + train_set$irs10yr)
cor(pca$x[,3], train_set$irs2yr - train_set$irs5yr + train_set$irs10yr)

#computing residuals
# we will extract the pcas and the rotation matrix
pc1to3 <- pca$x[,1:3] 
pc_rot <- pca$rotation[,1:3]


#create a function to compute the recostructed curve with the pca
curvapca <- function(i) {
  rowSums(pc1to3[i,]*(pc_rot))+pca$center
}


# create a matrix of the curve
curva_con_pca <-  data.frame(t(sapply(seq(1:nrow(train_set)), curvapca)))

#then we can compute the squared residuals
pca_rmse <- sqrt(colSums((x - curva_con_pca)^2)/nrow(train_set))
pca_rmse

#now we compute the dispersion of the data
dispersion <- (x - curva_con_pca)

#this show us the residuals
boxplot(dispersion)

#residual of the 10yr pca
pca_rmse[10]

#####knn######
# we will extract the pcas and the rotation matrix
set.seed(1, sample.kind = "Rounding")
train_knn <- train(irs10yr ~ ., method = "knn", data = train_set,
                   tuneGrid = data.frame(k = seq(1, 11 , 1)))
train_knn$bestTune

plot(train_knn)

#we minimise the RMSE
min(train_knn[["results"]][["RMSE"]])


####random forest######
set.seed(1, sample.kind = "Rounding")
train_rf <- train(irs10yr ~ ., method = "rf", data = train_set,
                  tuneGrid = data.frame(mtry = seq(1, 15, 1)), ntree = 100)
train_rf$bestTune

ggplot(train_rf)

#and get the best rmse
min(train_rf[["results"]][["RMSE"]])



######RESULTS

#MODEL_1

#correlation between all IRS 
y_hat_lm_1 <- predict(train_lm_1, test_set, type = "raw") 
#this would be for the validation, we get the RMSE usiing the test set
base_model_rmse <- sqrt(sum((test_set$irs10yr - y_hat_lm_1)^2)/nrow(test_set))
base_model_rmse
plot(y_hat_lm_1, test_set$irs10yr)

#we proceed to make the same process for the six models
#model2
y_hat_lm_2 <- predict(train_lm_2, test_set, type = "raw")
second_model_rmse <- sqrt(sum((test_set$irs10yr - y_hat_lm_2)^2)/nrow(test_set))
second_model_rmse
plot(y_hat_lm_2, test_set$irs10yr)

#model3
y_hat_lm_3 <- predict(train_lm_3, test_set, type = "raw")
third_model_rmse <- sqrt(sum((test_set$irs10yr - y_hat_lm_3)^2)/nrow(test_set))
third_model_rmse
plot(y_hat_lm_3, test_set$irs10yr)

#mode_4

#compute pca in test set
x <- test_set[,2:11] %>% as.matrix()
pca <- prcomp(x)

#computing residuals
# we will extract the pcas and the rotation matrix
pc1to3 <- pca$x[,1:3] 
pc_rot <- pca$rotation[,1:3]

# create a matrix of the curve
curva_con_pca <-  data.frame(t(sapply(seq(1:nrow(test_set)), curvapca)))

#then we can compute the squared residuals
pca_rmse <- sqrt(colSums((x - curva_con_pca)^2)/nrow(test_set))
fourth_model_rmse <- pca_rmse[10]
fourth_model_rmse

y_hat_pca <- as.numeric(unlist(curva_con_pca[10]))
plot(y_hat_pca, test_set$irs10yr)

#model5
y_hat_knn <- predict(train_knn, test_set, type = "raw")
fifth_model_rmse <- sqrt(sum((test_set$irs10yr - y_hat_knn)^2)/nrow(test_set))
fifth_model_rmse
plot(y_hat_knn, test_set$irs10yr)


#model 6
y_hat_rf <- predict(train_rf, test_set, type = "raw")
sixth_model_rmse <- sqrt(sum((test_set$irs10yr - y_hat_knn)^2)/nrow(test_set))
sixth_model_rmse
plot(y_hat_rf, test_set$irs10yr)
