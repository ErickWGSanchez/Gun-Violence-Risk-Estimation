#############################
#Erick Guevara
#EDA: Demographic 
#Capstone Project 
#############################
library(tidyverse)
library(ggplot2)
library(maps)
library(corrplot)
library(randomForest)
library(lattice)

#Loading in data and updating variable name
us_states <- map_data('state')
us_states$region <- str_to_title(us_states$region)
demo <- read.csv("C:/Users/justd/Desktop/IPI - PRSA/MainDatasets/health_demo_18_22rev.csv", header=T)
demo <- demo[!demo$county %in% us_states$region, ]
demo <- demo[-1,]
demo <- demo[-68,]
demo$county <- gsub(" County", "", demo$county)



head(demo, 5)
names(demo)




##################################### Descriptive Work ################################
#Add up all the n_killed values of the same county for each year
#Add the column to the Demo dataset named "total_firearm_fatalities"
#Variables are only from shooting death incidents with a gun. no suicides



sum_df2 <- gun_vio %>% group_by(state,county, year) %>%
  summarize(total_firearm_fatal = sum(n_killed))

#Sorting both datasets by county and year
demo <- demo[order(demo$county, demo$year),]
sum_df2 <- sum_df2[order(sum_df2$county, sum_df2$year), ]

sum_df2$county <- gsub("(\\W+).*", "\\1",sum_df2$county)
sum_df2 <- sum_df2 %>% 
  mutate(state_abr = state.abb[match(state, state.name)])

#merging counties that only appear in the demo dataset
nkill_val <- sum_df2[sum_df2$county %in% demo$county,]
demo <- merge(demo, sum_df2, by=c("county", "year", 'state_abr'))


demo_num <- select(demo, -year, -state, -county)

#rereading the dataset 
demo_num <- read.csv("C:/Users/justd/Desktop/IPI - PRSA/MainDatasets/demo_num_v2.csv", header=T)


##################################### Descriptive Visualization #############################

total_firearm <- demo_num$total_firearm_fatal
demo$total_firearm_fatal <- total_firearm

#Total SUM Distribution Across all 5 years 

demo_num_sub <- gun_vio[, c("year", "n_killed")]
total_sum <- aggregate(n_killed ~ year, data=demo_num_sub, FUN=sum)

demo_num_sub2 <- gun_vio[, c("year", "n_injured")]
total_sum2 <- aggregate(n_injured ~ year, data=demo_num_sub2, FUN=sum)

ggplot(total_sum, aes(x=year, y=n_killed, group=year)) + 
  geom_bar(stat='identity') +
  labs(title ='Total Gun Deaths per Year (Not including suicide by Gun)')

ggplot(total_sum2, aes(x=year, y=n_injured, group=year)) + 
  geom_bar(stat='identity') +
  labs(title ='Total Gun Injury per Year (Not including attempted suicide by Gun)')



#Scatter plot matrix on specific variables 


pairs(demo_num_v2[,c(2,4,5,6,7,10,11,12,13,14)])




##################################Correlation Analysis Matrix #############################
#Testing for high collinearity, meaning if two or more predictors are highly correlated with each other
library(car) #package for the VIF function



corrplot(cor(demo_num), 
         method = "number",
         type = "upper",
         tl.cex = 0.6,
         title='Demographic Correlation Matrix')



#Correlation plot shows 4 variables that highly correlated with each other
#I will perform a Variation Inflation Factor to measure extent of multicollinearity in the reg model
#I will remove influential and normalize the linear model
vif_test <- lm(total_firearm_fatal ~. -population -ratio_pop_to_mental_providers. -firearm_fatalities_ratio, data=demo_num)
summary(vif_test)
vif(vif_test)

par(mfrow=c(2,2))
plot(vif_test)

outliers <- boxplot(demo_num)$out
x_clean <- demo_num[!demo_num %in% outliers]

x_lm <- lm(total_firearm_fatal ~. -population -ratio_pop_to_mental_providers. -firearm_fatalities_ratio, data=x_clean)





#The VIF test shows mostly moderate collinearity except for population and homicides_occured varaibles,
#moderate collinearity has a range of 1 < x > 5 and no collinearity of 1 or less
#they're both super high collinearity because theyre greater than the range limit of 5 
#Linear Regression analysis without the to variables 



#Lasso regression will be used to investigate further for each value so there us less bias present
############################## Condition Checklist ###########################################
par(mfrow=c(2,2))
plot(x_lm)


############################## #LASSO Regression #####################################
library(glmnet)

#Turn explanatory and dependent variables into a Matrix
x <- as.matrix(demo_num[,1:13])
y <- demo_num[, 14]
#Cutting dataset in half for testing and training 
nr <- nrow(x)
train_size <- ceiling(nr/2)
lasso_train <- sample(1:nr, train_size)
#Creating training and testing datasets for lasso model

x.train <- x[lasso_train, ]
y.train <- y[lasso_train]

x.test <- x[-lasso_train, ]
y.test <- y[-lasso_train]

#Fitting lasso model with default values of lambda penalties and see regression go to zero from left to right

#Lasso path plot 
lasso_mod <- glmnet(x[lasso_train,],y[lasso_train], alpha=1)
plot(lasso_mod, main='Lasso Path Plot')
par(mar= c(5,4,6,6))


#The plot shows a couple of significant coefficient that seem to increase in magnitude away from zero but is considered low magnitude
#This means those variables are essentially not going to be excluded from the model
#One line has an extreme magnitude which means it has a strong impact on the response var

#To estimate the best tuning parameter I use 10 fold cross validation
######################### 10k - Cross Validation ###############################################################3

cv_out <- cv.glmnet(x[lasso_train, ], y[lasso_train], alpha=1, lower.limits = -Inf)
plot(cv_out, main='Mean Square Error 10k CV Plot')
par(mar= c(5,4,6,6))



cbind(lambda=round(cv_out$lambda, 1),
cvm=signif(cv_out$cvm, 7), 
cvsd=signif(cv_out$cvsd, 5))

#Using this model compromise the convention
#Lambda removed many additional variables, to seek a model with fewer variables 

cvm <- cv_out$cvm
sub1 <- which.min(cvm)
sub1
cvmTest <- cvm[sub1]+
  cv_out$cvsd[sub1]
sub2 <- which(cvm < cvmTest)[1]
sub2
cv_out$lambda[sub2]
cv_out$lambda.1se


#The minimum amount of variables without any bias in the data
lambda_min <- cv_out$lambda.min
coef(cv_out,s=lambda_min)

#The 1se rule can help select the correct model but estimates could suffer large bias 
lambda_1se <- cv_out$lambda.1se
coef(cv_out, s=lambda_1se)

#Making predictions and computing the mean squared error 
lasso_predBest <- predict(lasso_mod, s=lambda_min, newx=x.test)
lasso_pred1st <- predict(lasso_mod, s=lambda_1se, newx=x.test)

mean((lasso_predBest-y.test)^2)
mean((lasso_pred1st-y.test)^2)

#The red dots show the MSE and the standard deviations of the error for each value of the 100 tuning parameters
#by checking the coefficient I will now do a linear regression model with the preferred variables for my new model

lm_clean <- lm(total_firearm_fatal~. -mental_health_providers -ratio_pop_to_mental_providers. -unemployment -homicides_occured, data=demo_num)
plot(lm_clean)
vif(lm_clean)

#Model is now in a range of moderate col linearity which shouldn't present a problem in the analysis

########################## Forward Variable Selection #######################################################
#1. Initializing model
lm_tff<- lm(total_firearm_fatal~.,data=demo_num)
lm_null = lm(total_firearm_fatal~1, data=demo_num)
forward <- step(lm_tff, scope=formula(lm_tff),
     direction = "forward", trace = FALSE)
#2. Validating Variable Selection by residual analysis

plot(lm_tff$fitted.values, lm_tff$residuals,
     xlab = "Fitted Values", ylab="Residuals",
     main= "Residuals vs. Fitted Values")
abline(h=0)

hist(lm_tff$residuals, main='Distribution of Residuals')

#Identifying and Removing Outliers/influential points
boxplot(lm_tff$fitted.values)
bp <- boxplot.stats(lm_tff$fitted.values)

outlier_rm <- lm_tff$fitted.values[-which(lm_tff$fitted.values %in% bp$out)]

lev <- hatvalues(lm_tff)
plot(lev, main='Leverage Points Plot')

meanhat <- mean(lev)
high_lev <- which(lev > 2* meanhat)

demo_num_v2 <- subset(demo_num, !row.names(demo_num) %in% high_lev)

lm_tff_clean <- lm(total_firearm_fatal~.,data=demo_num_v2)

plot(lm_tff$fitted.values, lm_tff$residuals,
     xlab = "Fitted Values", ylab="Residuals",
     main= "Residuals vs. Fitted Values")

lev_clean <- hatvalues(lm_tff)
plot(lev_clean, main='Post Leverage Removal Plot')

##################################Cross validating#########################################
#3. Checking for overfitted or underfitted values by Cross validating data
library(caret)

#Splits the data into training and testing
trainIndex <- createDataPartition(demo_num_v2$total_firearm_fatal, p=0.8, list=FALSE, times=1)
train <- demo_num_v2[trainIndex, ]
test <- demo_num_v2[-trainIndex, ]

#Trains the linear model by using the training dataset
fit <- lm(total_firearm_fatal ~., data=train)

#performs k fold cross validation on training set
ctrl <- trainControl(method = "cv", number = 5)
cvFit <- train(total_firearm_fatal ~., data = train, method='lm', trControl = ctrl)

#Evaluates the performance of the model

pred <- predict(fit, newdata= test)

#Measures the accuracy by showing the mean error difference of the model

RMSE(pred, test$total_firearm_fatal)

################################################################################################################
#Visualize Selected Variables 

windows()
pairs(demo_numeric_2019, pch=".")


ggplot(data=demo_numeric_2019,aes(x= mental_health_providers, y=total_firearm_fatal)) + 
  geom_point(shape = 21, size =2,
             color = "black", fill='gray') +
  geom_smooth(method='lm') + 
  labs(x='Mental Health Providers rate', y='total_firearm_fatal', title='Regression Firearm vs Mental Health Provider')

ggplot(data=demo_numeric_2019,aes(x= homicides_occured, y= total_firearm_fatal)) + 
  geom_point(shape = 21, size =2,
             color = "black", fill='gray') +
  geom_smooth(method='lm') + 
  labs(x='homicides_occured', y='total_firearm_fatal', title='Regression Firearm vs homicides_occured')












