# useful libraries
library(readxl)   # to import data from Excel to R
library(corrplot) # to display the correlation plot
library(glmnet)
library(leaps)    # for regsubsets

# Upload datasets
data_salary <- read_excel("./data/NBA_players_salaries_HH.xlsx")
data_traditional_per48 <- read.csv("./data/RS_traditional_per48.csv")
data_traditional_tot <- read.csv("./data/RS_traditional_TOTALS.csv")
data_advanced <- read.csv("./data/RS_advanced_per48.csv")
data_miscellaneous <- read.csv("./data/RS_miscellaneous_per48.csv")
data_vorp <- read_excel("./data/vorp.xlsx")

# creating a new column in data_traditional_tot: MIN_G (minutes played per game)
# min/gp
MIN_G <- data_traditional_tot$MIN/data_traditional_tot$GP
data_traditional_tot <- cbind(data_traditional_tot, MIN_G)
rm(MIN_G) # remove the variable MIN_G, we won't use it anymore

# Select the features (columns) of interest
data_salary <- data_salary[, c(2, 3)]
data_traditional_per48 <- data_traditional_per48[, c(3, 7, 8, 15, 18, 21:32)]
data_traditional_tot <- data_traditional_tot[, c(3, 12, 68)]
data_advanced <- data_advanced[, c(3, 14, 17, 20, 23, 31, 32, 38)]
data_miscellaneous <- data_miscellaneous[, c(3, 24)]
data_vorp <- data_vorp[, c(2, 3, 23, 31, 32)]

# Rename a column to prepare the dataset to do a merge
names(data_salary)[names(data_salary) == "Player"] <- "PLAYER_NAME"
data_traditional_tot <- data_traditional_tot[data_traditional_tot$MIN > 480, ]  # considering players with at least 480 minutes played along the season (better measure compared to games played)

# Merge the datasets applying a full join
final_dataset <- merge(data_salary, data_traditional_per48, by = "PLAYER_NAME", all = TRUE)
final_dataset <- merge(final_dataset, data_advanced, by = "PLAYER_NAME", all = TRUE)
final_dataset <- merge(final_dataset, data_miscellaneous, by = "PLAYER_NAME", all = TRUE)
final_dataset <- merge(final_dataset, data_traditional_tot, by = "PLAYER_NAME", all = TRUE)
final_dataset <- merge(final_dataset, data_vorp, by = "PLAYER_NAME", all = TRUE)

# Facing NA values problem
final_dataset <- final_dataset[!is.na(final_dataset$AGE), ] # NA's we need to remove because no stats on NBA.com
final_dataset <- final_dataset[!is.na(final_dataset$MIN), ] # removing NA's for MIN (players with less than 480 minutes played)
final_dataset <- final_dataset[!is.na(final_dataset$VORP), ]

# Ensure that there aren't NA values
colSums(is.na(final_dataset))

# removing PFD.x and renaming PFD.y as PFD
final_dataset <- final_dataset[, -17]
colnames(final_dataset)[colnames(final_dataset) == 'PFD.y'] <- 'PFD'

# renaming the column 2023/2024 in "Salary", removing the "$" sign and changing the data class in numeric
colnames(final_dataset)[colnames(final_dataset) == '2023/24'] <- 'Salary'
final_dataset$Salary <- as.numeric(gsub("[\\$\\,]", "", final_dataset$Salary))
class(final_dataset$Salary)

# finally, we set the players' name as the row names
# thus, we can eliminate the column PLAYER_NAME
rownames(final_dataset) <- final_dataset$PLAYER_NAME
final_dataset <- final_dataset[, -1]


rm(data_salary, data_advanced, data_miscellaneous, data_traditional_per48, data_traditional_tot, data_vorp)


###########
### EDA ###
###########
attach(final_dataset)

# only numeric columns for EDA (removes the position)
numeric_cols <- sapply(final_dataset, is.numeric)
fd_numeric <- final_dataset[, numeric_cols]
summary(fd_numeric)

# variable Salary (dependent)
summary(Salary)
boxplot(Salary, main="Salary")
hist(Salary, main="Salary")

# log transformation of Salary
boxplot(log(Salary), main="Logarithmic salary")
hist(log(Salary), main="Logarithmic salary") 

# sqrt transformation of Salary
boxplot(sqrt(Salary), main="Square rooted Salary")
hist(sqrt(Salary), main="Square rooted Salary")  # forma più regolare, ulteriore motivo per usare sqrt

par(mfrow = c(2, 2))
boxplot(Salary, main="Salary")
hist(Salary, main="Salary")
boxplot(sqrt(Salary), main="Square rooted Salary")
hist(sqrt(Salary), main="Square rooted Salary")
par(mfrow = c(1, 1))


# Independent variables
boxplot(AGE, main="AGE", names=c("AGE"), show.names=TRUE, ylab="years")
boxplot(GP, main="Games played", names=c("GP"), show.names=TRUE, ylab="number of GP")
boxplot(MIN, main="MiN played per season", names=c("MIN"), show.names=TRUE, ylab="minutes played")
boxplot(MIN_G, PTS, main="MIN played and PTS scored per game", names=c("MIN_G", "PTS"), ylab="units number")
boxplot(OREB, DREB, REB, AST, main="OREB, DREB, REB, AST per game", names=c("OREB", "DREB", "REB", "AST"), ylab="units number")
boxplot(TOV, STL, BLK, BLKA, PF, PFD, main="TOV, STL, BLK, BLKA, PF, PFD per game", names=c("TOV", "STL", "BLK", "BLKA", "PF", "PFD"), ylab="units number")
boxplot(FG_PCT, FG3_PCT, FT_PCT, TS_PCT, main="FG_PCT, FG3_PCT, FT_PCT, TS_PCT", names=c("FG_PCT", "FG3_PCT", "FT_PCT", "TS_PCT"), ylab="percentage")
boxplot(OFF_RATING, DEF_RATING, main="OFF_RATING, DEF_RATING", names=c("OFF_RATING", "DEF_RATING"), ylab="rating")
boxplot(NET_RATING, main="NET_RATING", names=c("NET_RATING"), show.names=TRUE, ylab="rating")
boxplot(AST_TO, main="AST_TO", names=c("AST_TO"), show.names=TRUE, ylab="units number")
boxplot(PIE, USG_PCT, main="PIE, USG_PCT", names=c("PIE", "USG_PCT"), ylab="percentage")
boxplot(WS, BPM, VORP, main="WS, BPM, VORP", names=c("WS", "BPM", "VORP"), ylab="score")


## Analyze correlations

# covariance and correlation matrices
cov_mat <- round(cov(fd_numeric),2)
cov_mat
cor_mat <- round(cor(fd_numeric),2)
cor_mat

corrplot(cor(fd_numeric), method = 'color')


############
## MODELS ## 
############

# we will start creating a linear model in order to predict the salaries and
# then we'll perform a Subset Selection in order to remove the less
# significant variables.
# After that, we will use Ridge and Lasso regression in order to reduce the
# effect of multicollinearity. Then, we'll compare the performances of the
# models and the results with the actual salaries earned by the players during
# the 2023/2024 season. We will finally do an analysis dividing the players
# based on their position (centers, forwards, guards) fitting a model for each
# of them and analyzing the performances.

# LINEAR REGRESSION MODEL
lm.mod <- lm(Salary~., data=fd_numeric)
summary(lm.mod)

# MSE
lm.mod.pred <- predict(lm.mod)
y <- fd_numeric$Salary
mse.lm.mod <- mean((lm.mod.pred-y)^2)
mse.lm.mod
format(sqrt(mse.lm.mod), scientific = TRUE)

# Residual analysis
par(mfrow = c(2, 2))
plot(lm.mod)
par(mfrow = c(1, 1))

# QQ plot ok, problems on the first plot (linearity residuals vs fitted) and
# on the third plot (eteroschedasticity scale-location)


# trying to transform the response variable (log)
lm.log <- lm(log(Salary)~., data=fd_numeric)
summary(lm.log)

par(mfrow = c(2, 2))
plot(lm.log)
par(mfrow = c(1, 1))
# better plots, better linearity and omoschedasticity

#MSE
lm.log.pred <- predict(lm.log)
mse.lm.log <- mean((exp(lm.log.pred)-y)^2)
mse.lm.log
format(sqrt(mse.lm.log), scientific = TRUE)
# worse performances --> try to find a better transformation


# transform the response variable (sqrt)
lm.sqrt <- lm(sqrt(Salary)~., data = fd_numeric)
summary(lm.sqrt)

par(mfrow = c(2, 2))
plot(lm.sqrt)         # plots ok
par(mfrow = c(1, 1))

lm.sqrt.pred <- predict(lm.sqrt)
mse.lm.sqrt <- mean((lm.sqrt.pred^2 - y)^2)
mse.lm.sqrt
format(sqrt(mse.lm.sqrt), scientific = TRUE)
# better performances w.r.t. the previous two models

# computing performances of the complete model with sqrt of the Salary
# in a test set in order to compare it to the other models
X <- model.matrix(sqrt(Salary)~., data=fd_numeric)
X <- X[,-1]
n <- nrow(X)

set.seed(1)
train <- sample(1:n, n/2)
test  <- setdiff(1:n, train)

lm.sqrt.test <- glmnet(X[train, ], sqrt(y[train]), alpha = 0, lambda = 0)
lm.sqrt.test.pred <- predict(lm.sqrt.test, s = 0, newx = X[test, ], exact = TRUE)
lm.sqrt.test.mse <- mean((lm.sqrt.test.pred^2-y[test])^2)
print(paste("Estimated test MSE = ", format(lm.sqrt.test.mse, scientific = TRUE)))
format(sqrt(lm.sqrt.test.mse), scientific = TRUE)


###############################
# EXHAUSTIVE SUBSET SELECTION
###############################

subset_selection <- function(df) {
  regfit.full <- regsubsets(sqrt(Salary)~., data=df, nvmax=(ncol(df)-1))
  reg.summary <- summary(regfit.full)

  par(mfrow=c(2,2))

  # residual sum of squares
  plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")

  # adjusted-R^2 with its largest value
  plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted Rsq",type="l")
  i <- which.max(reg.summary$adjr2)
  points(i,reg.summary$adjr2[i], col="red",cex=2,pch=20)
  text(i,reg.summary$adjr2[i], i, pos=1)

  # Mallow's Cp with its smallest value
  plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
  i <- which.min(reg.summary$cp)#return the index of the minimum
  points(i,reg.summary$cp[i],col="red",cex=2,pch=20)
  text(i,reg.summary$cp[i], i, pos=3)
  
  covariates = i

  # BIC with its smallest value
  plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
  i <- which.min(reg.summary$bic)
  points(i,reg.summary$bic[i],col="red",cex=2,pch=20)
  text(i,reg.summary$bic[i], i, pos=3)

  par(mfrow = c(1,1))

  selected.model <- reg.summary$which[covariates, ]
  selected.parameters <- names(selected.model[selected.model])[-1] #-1 to lose the intercept
  print(selected.parameters)
  
  selected.formula <- as.formula(paste("sqrt(Salary)~", paste(selected.parameters, collapse = " + ")))
  
  model <- lm(selected.formula, data=df)
  
  return(model)
}

lm.ess <-  subset_selection(fd_numeric)
summary(lm.ess)

# extract the selected parameters
selected.parameters.ess <- attr(terms(lm.ess), "term.labels")

# correlation between dependent variables
corrplot(cor(fd_numeric[c(selected.parameters.ess)]), method = 'color')

# residual analysis
par(mfrow=c(2,2))
plot(lm.ess)
par(mfrow=c(1,1))

# model performances
lm.ess.pred <- predict(lm.ess)
mse.lm.ess <- mean((lm.ess.pred)^2 - y)^2
format(mse.lm.ess, scientific=TRUE)
format(sqrt(mse.lm.ess), scientific=TRUE)

# performances on the test set
selected.formula <- as.formula(paste("sqrt(Salary)~", paste(selected.parameters.ess, collapse = " + ")))
X.ess <- model.matrix(selected.formula, data=fd_numeric)
X.ess <- X.ess[,-1]

lm.ess.test <- glmnet(X.ess[train, ], sqrt(y[train]), alpha = 0, lambda = 0)
lm.ess.test.pred <- predict(lm.ess.test, s = 0, newx = X.ess[test, ], exact = TRUE)
lm.ess.test.mse <- mean((lm.ess.test.pred^2 - y[test])^2)
print(paste("Estimated test MSE = ", format(lm.ess.test.mse, scientific = TRUE)))
print(paste("Square root of the estimated test MSE = ", format(sqrt(lm.ess.test.mse), scientific = TRUE)))


###  function that returns the top_N overpaid and top-N underpaid players tables ###
create_tables <- function(real_values, pred_values, df, N) {
  res <- real_values - pred_values
  res <- as.vector(res)
  
  overpaid_indices <- order(res, decreasing=TRUE)[1:N]
  underpaid_indices <- order(res, decreasing=FALSE)[1:N]
  
  over_diff <- res[overpaid_indices]
  under_diff <- res[underpaid_indices]
  
  over_pred <- pred_values[overpaid_indices]
  under_pred <- pred_values[underpaid_indices]
  
  ## actual salary and player names
  fd_over <- df[overpaid_indices, ][c('Salary')]
  fd_under <- df[underpaid_indices, ][c('Salary')]
  
  overpaid_table <- cbind(fd_over, over_pred, over_diff)
  colnames(overpaid_table) <- c("Salary", "Predicted salary", "Difference")
  underpaid_table <- cbind(fd_under, under_pred, -under_diff)
  colnames(underpaid_table) <- c("Salary", "Predicted salary", "Difference")
  
  return(list(overpaid_table, underpaid_table))
}

lm.ess.tables <- create_tables(y, lm.ess.pred^2, final_dataset, 10)
lm.ess.tables[[1]]
lm.ess.tables[[2]]


#############################
#     RIDGE REGRESSION
#############################

# linear model
summary(lm.sqrt)

### Ten Fold Cross Validation function to select the best lambda ###
ten_fold_cv <- function(X, y, a) {
  print("### 10-FOLD CROSS-VALIDATION ###")
  n <- nrow(X)
  
  set.seed(1)
  train <- sample(1:n, n/2)
  test  <- setdiff(1:n, train)
  
  cv.out <- cv.glmnet(X[train, ], sqrt(y[train]), alpha = a, nfold = 10)
  
  # This plots the cross-validation curve (red dotted line) along with upper and
  # lower standard deviation curves along the lambda sequence (error bars).
  # Two special values along the lambda sequence are indicated by the vertical
  # dotted lines. lambda.min is the value of lambda that gives minimum mean
  # cross-validated error, while lambda.1se is the value of lambda that gives
  # the most regularized model such that the cross-validated error is within one
  # standard error of the minimum.
  plot(cv.out)
  
  ## selecting the lambda that minimizes test MSE
  best_lambda <- cv.out$lambda.min
  print(paste("The best lambda is = ", round(best_lambda)))
  
  # estimated test MSE with bestlambda value
  mod <- glmnet(X[train, ], sqrt(y[train]), alpha = a)
  pred <- predict(mod, s = best_lambda, newx = X[test,])
  mse <- mean((pred^2-y[test])^2)
  print(paste("The estimated test MSE with the best lambda is = ", format(mse, scientific = TRUE)))
  print(paste("Square root of the estimated test MSE with the best lambda is = ", format(sqrt(mse), scientific = TRUE)))
  
  return <- best_lambda
}

best_lambda <- ten_fold_cv(X, y, 0)
# worse performance on the test set

## final model with best lambda on all data
lm.rid <- glmnet(X, sqrt(y), alpha = 0)
coef(lm.rid, s = best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lm.rid, xvar = "lambda", label = TRUE)
abline(v=log(best_lambda), lty = 3, lwd = 2)

# R2

#use fitted best model to make predictions
lm.rid.pred <- predict(lm.rid, s = best_lambda, X)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((lm.rid.pred^2 - y)^2)

#find R-Squared
R2 <- 1 - sse/sst
R2
# R2 better than Exhaustive Subset Selection

# final MSE
mse.lm.rid <- mean((lm.rid.pred^2-y)^2)
mse.lm.rid
format(sqrt(mse.lm.rid), scientific= TRUE)
# mse worse than Exhaustive Subset Selection


### 10 most overpaid and 10 most underpaid players table ###
lm.rid.tables <- create_tables(y, lm.rid.pred^2, final_dataset, 10)
lm.rid.tables[[1]]
lm.rid.tables[[2]]

# Ridge regression shows worse performances compared to the Subset Selection
# in terms of errors, probably the multicollinearity does not influence
# the performance of the Subset Selection



#############################
#     LASSO REGRESSION      #
#############################

best_lambda <- ten_fold_cv(X, y, 1)

# final model with best lambda on all data
lm.las <- glmnet(X, sqrt(y), alpha = 1)
coef(lm.las, s = best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lm.las, xvar = "lambda", label = TRUE)
abline(v = log(best_lambda), lty = 3, lwd = 2)

# use fitted best model to make predictions
lm.las.pred <- predict(lm.las, s = best_lambda, X)

# SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((lm.las.pred^2 - y)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# slightly better R2 than ridge

# final MSE
mse.lm.las <- mean((lm.las.pred^2-y)^2)
mse.lm.las
format(sqrt(mse.lm.las), scientific= TRUE)



### 10 most overpaid and 10 most underpaid players table ###
lm.las.tables <- create_tables(y, lm.las.pred^2, final_dataset, 10)
lm.las.tables[[1]]
lm.las.tables[[2]]


####################################
# ANALYSIS FOR DIFFERENT POSITIONS #
####################################

#########
# ANOVA # 
#########

# function to transform PG and SG into G, and PF and SF into F
recode_pos <- function(x) {
  ifelse(x %in% c("PG", "SG"), "G", 
         ifelse(x %in% c("PF", "SF"), "F", x))
}

final_dataset$Pos <- recode_pos(final_dataset$Pos)

# Is there, on average, a difference between salaries of players
# with different positions?
bartlett.test(Salary ~ Pos, data = final_dataset)

aov.roles <- aov(Salary ~ Pos, data = final_dataset)
summary(aov.roles)


# split the dataset based on position
fd_list <- split(final_dataset, final_dataset$Pos)
fd_center <- fd_list$C
fd_forward <- fd_list$F
fd_guard <- fd_list$G
rm(fd_list)

nrow(fd_center)
nrow(fd_forward)
nrow(fd_guard)

# remove position
fd_center <- fd_center[, numeric_cols]
fd_forward <- fd_forward[, numeric_cols]
fd_guard <- fd_guard[, numeric_cols]


###################
# CENTER POSITION #
###################

# linear model
lm.mod.c <- lm(sqrt(Salary)~., data=fd_center)
summary(lm.mod.c)


### SUBSET SELECTION ###

lm.ess.c = subset_selection(fd_center)
summary(lm.ess.c)

# extract the selected parameters
selected.parameters.ess.c <- attr(terms(lm.ess.c), "term.labels")

# correlation between dependent variables
corrplot(cor(fd_center[c(selected.parameters.ess.c)]), method = 'color')

# residual analysis
par(mfrow=c(2,2))
plot(lm.ess.c)
par(mfrow=c(1,1))

# model performances
y.c <- fd_center$Salary
lm.ess.c.pred <- predict(lm.ess.c)
mse.lm.ess.c <- mean((lm.ess.c.pred)^2 - y.c)^2
format(mse.lm.ess.c, scientific=TRUE)
format(sqrt(mse.lm.ess.c), scientific=TRUE)

# performances on the test set
selected.formula <- as.formula(paste("sqrt(Salary)~", paste(selected.parameters.ess.c, collapse = " + ")))
X.c <- model.matrix(sqrt(Salary)~., data=fd_center)
X.c <- X.c[,-1]
n <- nrow(X.c)

set.seed(1)
train <- sample(1:n, n/2)
test  <- setdiff(1:n, train)

lm.ess.c.test <- glmnet(X.c[train, ], sqrt(y.c[train]), alpha = 0, lambda = 0)
lm.ess.c.test.pred <- predict(lm.ess.c.test, s = 0, newx = X.c[test, ], exact = TRUE)
lm.ess.c.test.mse <- mean((lm.ess.c.test.pred^2 - y.c[test])^2)
print(paste("Estimated test MSE = ", format(lm.ess.c.test.mse, scientific = TRUE)))
format(sqrt(lm.ess.c.test.mse), scientific = TRUE)


### LASSO REGRESSION ###

best_lambda <- ten_fold_cv(X.c, y.c, 1)

# final model with best lambda on all data
lm.las.c <- glmnet(X.c, sqrt(y.c), alpha = 1)
coef(lm.las.c, s=best_lambda)

# Trace plot to visualize how the coefficient estimates changed as
# a result of increasing lambda
plot(lm.las.c, xvar = "lambda", label = TRUE)
abline(v = log(best_lambda), lty = 3, lwd = 2)

# use fitted best model to make predictions
lm.las.c.pred <- predict(lm.las.c, s = best_lambda, X.c)

# SST and SSE
sst <- sum((y.c - mean(y.c))^2)
sse <- sum((lm.las.c.pred^2 - y.c)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# higher R2

# final MSE
mse.lm.las.c <- mean((lm.las.c.pred^2 - y.c)^2)
mse.lm.las.c
# better than before

### 3 most overpaid and 3 most underpaid centers table ###
lm.las.c.tables <- create_tables(y.c, lm.las.c.pred^2, fd_center, 3)
lm.las.c.tables[[1]]
lm.las.c.tables[[2]]


####################
# FORWARD POSITION #
####################

# linear model
lm.mod.f <- lm(sqrt(Salary)~., data=fd_forward)
summary(lm.mod.f)


### SUBSET SELECTION ###

lm.ess.f = subset_selection(fd_forward)
summary(lm.ess.f)

# extract the selected parameters
selected.parameters.ess.f <- attr(terms(lm.ess.f), "term.labels")

# correlation between dependent variables
corrplot(cor(fd_forward[c(selected.parameters.ess.f)]), method = 'color')

# residual analysis
par(mfrow=c(2,2))
plot(lm.ess.f)
par(mfrow=c(1,1))

# model performances
y.f <- fd_forward$Salary
lm.ess.f.pred <- predict(lm.ess.f)
mse.lm.ess.f <- mean((lm.ess.f.pred)^2 - y.f)^2
format(mse.lm.ess.f, scientific=TRUE)
format(sqrt(mse.lm.ess.f), scientific=TRUE)

# performances on the test set
selected.formula <- as.formula(paste("sqrt(Salary)~", paste(selected.parameters.ess.f, collapse = " + ")))
X.f <- model.matrix(sqrt(Salary)~., data=fd_forward)
X.f <- X.f[,-1]
n <- nrow(X.f)

set.seed(1)
train <- sample(1:n, n/2)
test  <- setdiff(1:n, train)

lm.ess.f.test <- glmnet(X.f[train, ], sqrt(y.f[train]), alpha = 0, lambda = 0)
lm.ess.f.test.pred <- predict(lm.ess.f.test, s = 0, newx = X.f[test, ], exact = TRUE)
lm.ess.f.test.mse <- mean((lm.ess.f.test.pred^2 - y.f[test])^2)
print(paste("Estimated test MSE = ", format(lm.ess.f.test.mse, scientific = TRUE)))
format(sqrt(lm.ess.f.test.mse), scientific = TRUE)


### LASSO REGRESSION ###

best_lambda <- ten_fold_cv(X.f, y.f, 1)

# final model with best lambda on all data
lm.las.f <- glmnet(X.f, sqrt(y.f), alpha = 1)
coef(lm.las.f, s = best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lm.las.f, xvar = "lambda", label = TRUE)
abline(v = log(best_lambda), lty = 3, lwd = 2)

# use fitted best model to make predictions
lm.las.f.pred <- predict(lm.las.f, s = best_lambda, X.f)

# SST and SSE
sst <- sum((y.f - mean(y.f))^2)
sse <- sum((lm.las.f.pred^2 - y.f)^2)

# R-Squared
R2 <- 1 - sse/sst
R2

# final MSE
mse.lm.las.f <- mean((lm.las.f.pred^2-y.f)^2)
mse.lm.las.f

### 3 most overpaid and 3 most underpaid forwards table ###
lm.las.f.tables <- create_tables(y.f, lm.las.f.pred^2, fd_forward, 3)
lm.las.f.tables[[1]]
lm.las.f.tables[[2]]


##################
# GUARD POSITION #
##################

# linear model
lm.mod.g <- lm(sqrt(Salary)~., data=fd_guard)
summary(lm.mod.g)


### SUBSET SELECTION ###

lm.ess.g = subset_selection(fd_guard)
summary(lm.ess.g)

# extract the selected parameters
selected.parameters.ess.g <- attr(terms(lm.ess.g), "term.labels")

# correlation between dependent variables
corrplot(cor(fd_guard[c(selected.parameters.ess.g)]), method = 'color')

# residual analysis
par(mfrow=c(2,2))
plot(lm.ess.g)
par(mfrow=c(1,1))

# model performances
y.g <- fd_guard$Salary
lm.ess.g.pred <- predict(lm.ess.g)
mse.lm.ess.g <- mean((lm.ess.g.pred)^2 - y.g)^2
format(mse.lm.ess.g, scientific=TRUE)
format(sqrt(mse.lm.ess.g), scientific=TRUE)

# performances on the test set
selected.formula <- as.formula(paste("sqrt(Salary)~", paste(selected.parameters.ess.g, collapse = " + ")))
X.g <- model.matrix(sqrt(Salary)~., data=fd_guard)
X.g <- X.g[,-1]
n <- nrow(X.g)

set.seed(1)
train <- sample(1:n, n/2)
test  <- setdiff(1:n, train)

lm.ess.g.test <- glmnet(X.g[train, ], sqrt(y.g[train]), alpha = 0, lambda = 0)
lm.ess.g.test.pred <- predict(lm.ess.g.test, s = 0, newx = X.g[test, ], exact = TRUE)
lm.ess.g.test.mse <- mean((lm.ess.g.test.pred^2 - y.g[test])^2)
print(paste("Estimated test MSE = ", format(lm.ess.g.test.mse, scientific = TRUE)))
format(sqrt(lm.ess.g.test.mse), scientific = TRUE)


### LASSO REGRESSION ###

best_lambda <- ten_fold_cv(X.g, y.g, 1)

# final model with best lambda on all data
lm.las.g <- glmnet(X.g, sqrt(y.g), alpha = 1)
coef(lm.las.g, s = best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lm.las.g, xvar = "lambda", label = TRUE)
abline(v = log(best_lambda), lty = 3, lwd = 2)

# use fitted best model to make predictions
lm.las.g.pred <- predict(lm.las.g, s = best_lambda, X.g)

# SST and SSE
sst <- sum((y.g - mean(y.g))^2)
sse <- sum((lm.las.g.pred^2 - y.g)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# worse R2 than Subset Selection

# MSE
mse.lm.las.g <- mean((lm.las.g.pred^2-y.g)^2)
mse.lm.las.g
# worse mse

### 3 most overpaid and 3 most underpaid centers table ###
lm.las.g.tables <- create_tables(y.g, lm.las.g.pred^2, fd_guard, 3)
lm.las.g.tables[[1]]
lm.las.g.tables[[2]]
