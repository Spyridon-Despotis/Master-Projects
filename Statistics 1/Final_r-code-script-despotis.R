#' ---
#' title: 'Title Bike Sharing Dataset '
#' author: "Spyridon Despotis"


#' 
#' 
#' # 1) You should first perform some descriptive data analysis and visualization. Visualizing the data should give you some insight into certain particularities of this dataset. Pairwise comparisons will help you also learn about the association implied by the data. 
#' 
#' Dataset structure: 

library(stargazer)
library(tidyverse)
library(knitr)
library(psych)
library(reshape)
library("ggcorrplot")
library("ggmosaic")
library(ggplot2)
library(MASS)
library(car)
library(lmtest)
library(nortest)
library(jtools)
library(psych)

df <- read.csv("bike_29.csv",sep=";",dec = ",")

str(df)

#' 
#' Investigating missing values :
#' 
#' 


df1 <- df

#mising values
sapply(df1, function(x) sum(is.na(x)))



#' 
#' Recoding Factors:
#' Factor levels were recoded from numeric to more meaningful descriptive strings. 
#' The "hour" variable was converted from continuous to factor variable with the following levels: mornig, afternoon, evening, night



df1$season <- factor(df1$season
                     ,levels = c(1,2,3,4)
                     ,labels = c("spring", "summer", "fall", "winter"))

df1$workingday <- factor(df1$workingday
                         ,levels = c(0,1)
                         ,labels = c("nonwkday", "wkday"))

df1$weathersit <- factor(df1$weathersit
                         ,levels = c(4,3,2,1)
                         ,labels = c("very bad", "bad", "good", "very good")
                         ,ordered = TRUE)


#Converting hour

df1$hourF <- as.factor(case_when(
  df1$hr >= 7 & df1$hr <= 12 ~ 'Morning',
  df1$hr > 12 & df1$hr <= 17 ~ 'Afternoon',
  df1$hr > 17 & df1$hr <= 22 ~ 'Evening',
  TRUE~ "Night"
))

df2 <- df1 %>% 
  mutate(season = as.factor(season), year = as.factor(yr), month = as.factor(mnth),    
         holiday = as.factor(holiday), weekday = as.factor(weekday), workingday = as.factor(workingday),
         registered = as.numeric(registered), c = as.numeric(casual),
         weather = as.factor(weathersit), hour = as.numeric(hr), count = as.numeric(cnt), humidity = hum) %>% 
  dplyr::select(count, hourF, year, month, workingday, weekday, holiday, weather, temp, humidity, windspeed,season, atemp) 






str(df2)

#' 

#Categorizing data
df_numeric= df2 %>% select_if(is.numeric)

df_factor = df2 %>% select_if(is.factor)

#' 
#' ## Descriptive analysis
#' 
#' ### Visualising the response variable
#' 

#visualising count
p <- ggplot(df2, aes(count, fill = cut(count, 100))) +
  geom_histogram(show.legend = FALSE) +
  theme_minimal() +
  labs(x = "Count", y = "n") +
  ggtitle("Number of Total Rents")
p + scale_fill_discrete(h = c(180, 360), c = 150, l = 80)


#' 
#' 
#' ### Visualising continious variables: histograms
#' 


#visualising continious variables
par(mfrow=c(2,3));
for(k in 1:ncol(df_numeric)){
  hist(df_numeric[,k], main=names(df_numeric)[k])
}

#' 
#' ### Visualising factors: barplots


df3=cbind(df_factor %>% mutate_all(as.character),cnt= df2$count) %>% pivot_longer(-cnt)

df4=df3 %>% dplyr::select(-cnt) %>% group_by(name,value) %>% count()

ggplot(df4) +
  aes(x = value, fill = value, weight = n) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(name), scales = "free")


#' 
#' ### Visualising relationship between response and factor variables: boxplots
#' 


ggplot(df3) +
  aes(x = value, y = cnt,fill = value) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(vars(name), scales = "free")+
  theme(legend.position="none")


#' 
#' ### Visualising relationship between the numeric variables: Correlations
#' 

#correlation numeric
dumcor=cor(df_numeric)
ggcorrplot(dumcor,method = "square", outline.color = "black", type = "lower", lab = TRUE)

#' 



pairs.panels(df_numeric%>% dplyr::select(-count,everything(),count), 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#' 
#' #  2. The main aim is to identify the best model for predicting the number of bike rentals per hour (variable cnt). 
#' ## a) Implement Lasso in order to select the covariates of your model. 
#' 

###Lasso-

require(glmnet)
model1 <- lm(count~.,data=df2 )
X <- model.matrix(model1)[,-1]
lasso <- glmnet(X, df2$count)
# plot(lasso, xvar = "lambda", label = T)

#Use cross validation to find a reasonable value for lambda 
lasso1 <- cv.glmnet(X, df2$count, alpha = 1)



#' 

# lasso1$lambda
lasso1$lambda.min
lasso1$lambda.1se



#' 
#' 

plot(lasso1)


#' 


plot(lasso1$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso1$lambda.min, lasso1$lambda.1se)), lty =2)

#' 



coef(lasso1, s = "lambda.1se")


#' 
#' ## b) Select the appropriate features (after implementing lasso) using stepwise methods in order to select your final model. 
#' Be careful, your model should not be over-parameterized
#' 
#' ### Step-wise with AIC

#STEPWISE- BIC


model0 <- lm(formula = count ~ hourF + year + month + weather + temp + humidity + season, data = df2)

step_m<-step(model0, direction='both',trace=0)

step_m

#' 
#'  
#' 
#' # 3. Check the assumptions of the model and revise your procedure
#' 
#'  ### Model with the final set of predictors
#'  


model1=lm(formula = count ~ hourF + year + temp + humidity + season+weather,     data = df2)

#' 
#' 
#' 
#' ## Check the normality assumption
#' 

qqPlot(model1)

#' 
#' ## Check the Linearity assumption
#' 


residualPlot(model1, type='rstudent')
residualPlots(model1, plot=F, type = "rstudent")

#' 
#' ## Check the homogeneity of variance assumption
#' 


Stud.residuals <- rstudent(model1)
yhat <- fitted(model1)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)



#' 

ncvTest(model1)

#' 


yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)

leveneTest(rstudent(model1)~yhat.quantiles)


#' 


boxplot(rstudent(model1)~yhat.quantiles)


#' 
#' ## Check the independence assumption
#' 

# Independence 
# -------------------
plot(rstudent(model1), type='l')

durbinWatsonTest(model1)

#' 
#' ## Check Multi Collinearity
#' 


#Using VIF 
require(car)
round(vif(model1),1)


#' 
#' ## Final model
#' 

lambda=.31

model3 <-lm(formula =((count^lambda-1)/lambda) ~ hourF +year + poly(temp,3) +poly( humidity,2) + season+weather , data = df2)

#' 
#' # Re- checking assumptions
#' 
#' ### Normality

qqPlot(model3)

#' 
#' 
#' ### Linearity
#' 

residualPlots(model3, plot=F, type = "rstudent")

#' 
#' ### Constant variance
#' 

yhat <- fitted(model3)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)

leveneTest(rstudent(model3)~yhat.quantiles)


boxplot(rstudent(model3)~yhat.quantiles)

#' 
#' ### Independence 


plot(rstudent(model3), type='l')


#' 
#' # 4. Interpret the parameters and the predicting performance of the final model. 
#' 


export_summs(model3,
             error_format = "[{conf.low}, {conf.high}]")

#'  
#' # 5. Use the test dataset to assess the out-of-sample predictive ability and compare the models selected in Q2. Also include the full and the null models in your comparison. 
#' 

# Test dataset

test_dat= read.csv("bike_test.csv",sep=";",dec = ",")

test_dat$season <- factor(test_dat$season
                          ,levels = c(1,2,3,4)
                          ,labels = c("spring", "summer", "fall", "winter"))

test_dat$workingday <- factor(test_dat$workingday
                              ,levels = c(0,1)
                              ,labels = c("nonwkday", "wkday"))

test_dat$weathersit <- factor(test_dat$weathersit
                              ,levels = c(4,3,2,1)
                              ,labels = c("very bad", "bad", "good", "very good")
                              ,ordered = TRUE)


test_dat2 <- test_dat %>% 
  mutate(season = as.factor(season), year = as.factor(yr), month = as.factor(mnth),    
         holiday = as.factor(holiday), weekday = as.factor(weekday), workingday = as.factor(workingday),
         registered = as.numeric(registered), c = as.numeric(casual),
         weather = as.factor(weathersit), hour = as.numeric(hr), count = as.numeric(cnt), humidity = hum) %>% 
  dplyr:: select( count,hour, year, month, workingday, weekday, holiday, weather, temp, humidity, windspeed,season, atemp) 

test_dat2$hourF<- as.factor(case_when(
  test_dat$hr >= 7 & test_dat$hr <= 12 ~ 'Morning',
  test_dat$hr > 12 & test_dat$hr <= 17 ~ 'Afternoon',
  test_dat$hr > 17 & test_dat$hr <= 22 ~ 'Evening',
  TRUE~ "Night"
))

test_dat2$resp=(test_dat2$count^lambda-1)/lambda



#' 
#' 

# RMSE - Final model

# Predict on test: 
pred1 <- predict(model3, newdata = test_dat2, type = "response")

# Compute errors: error
error <- test_dat2$resp - pred1

RMSE_res=data.frame(model=c("Final","Null","Full"),RMSE=c(NA),MAE=NA,MSE=NA)


RMSE_res$RMSE[1] <- sqrt(mean(error^2))

RMSE_res$MSE[1]= mean((error)^2)
RMSE_res$MAE[1] = mean(abs(error))


#' 
#' 

# RMSE - Null model

null_mod=lm((count^lambda - 1)/lambda~1,data=df2)

# Predict on test: 
pred2 <- predict(null_mod, newdata = test_dat2, type = "response")

# Compute errors: error
error2 <- test_dat2$resp - pred2

RMSE_res$RMSE[2] <- sqrt(mean(error2^2))


RMSE_res$MSE[2]= mean((error2)^2)
RMSE_res$MAE[2] = mean(abs(error2))

#' 
#' 

# RMSE - Full model

full_mod=lm((count^lambda - 1)/lambda~.,data=df2)

# Predict on test: 
pred3 <- predict(full_mod, newdata = test_dat2, type = "response")

# Compute errors: error
error3 <- test_dat2$resp - pred3

RMSE_res$RMSE[3] <- sqrt(mean(error3^2))


RMSE_res$MSE[3]= mean((error3)^2)
RMSE_res$MAE[3] = mean(abs(error3))

RMSE_res


# 6--------
#' 
#' # 6. Describe the typical profile of a day for each season (autumn, winter, spring, summer).---------
#' 
#' 
#' ## Typical day by year-hour



dat_typ=df2 %>% group_by(season,hourF,weather,year) %>% summarise(temp=mean(temp),humidity=mean(humidity)) %>% ungroup()



#' 

# Predict on typical dat: 
dat_typ$pred_typ1 <- predict(model3, newdata = dat_typ, type = "response")

# reverse box-cox
invBoxCox <- function(x, lambda)
  if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda)


dat_typ$pred_typ <- invBoxCox(dat_typ$pred_typ1,lambda )

ggplot(dat_typ) +
  aes(x = season, fill = hourF, weight = pred_typ) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(~year+weather,labeller = label_both)


dat_typ2=dat_typ %>% group_by(season,hourF) %>% summarise(mean_cnt=mean(pred_typ))

ggplot(dat_typ2) +
  aes(x = season, fill = hourF, weight = mean_cnt) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal() 


#' 
#' # APPENDIX
#' 
#' ## Box Cox transformation
#' 


model1 <-lm(formula = ( count ~ hourF +year + temp + humidity+ season+weather), data = df2)

boxCox(model1, plotit = TRUE)



#' 
#' 
#' ## Checking linearity assumption of Box Cox Model
#' 

lambda=0.31

model2.1 <-lm(formula =((count^lambda-1)/lambda) ~ hourF +year + temp + humidity + season +weather , data = df2)

residualPlots(model2.1, plot=F, type = "rstudent")


#' 

model2.3 <-lm(formula =((count^lambda-1)/lambda) ~ hourF +year + poly(temp,5) + poly(humidity,5) + season +weather , data = df2)


#' 
#' Model summary


export_summs(model2.3,
             error_format = "[{conf.low}, {conf.high}]")

#' 
#' Check assumptions


library(ggfortify)

autoplot(model2.3)

#'


