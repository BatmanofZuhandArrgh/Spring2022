#evaluation metrics 
errors = c(5, 10, 15, 5)
actuals = c(10, 10, 5, 1)

mape <- errors/actuals
mean(mape)*100

mae_pct <- mean(errors)/mean(actuals)*100
mae_pct

#load libraries
library(fpp3)

#to read in data
setwd("~/Documents/BANA4090/R Scripts")
wm <- readRDS("weekly_walmart_sales.rds")

#to save
#saveRDS(wm, "test_object.rds")

#explore
wm %>% autoplot(Weekly_Sales)

wm %>% 
  group_by(id, Store, Dept) %>% 
  tally()

wm %>% head()

wm_93 <- wm %>% 
  filter(id == '1_93') %>% 
  select(id, Weekly_Sales, IsHoliday, Fuel_Price) %>% 
  mutate(IsHoliday = as.factor(IsHoliday))

#update index to week and year
wm_93 <- wm_93 %>%
  mutate(Week = yearweek(Date)) %>% 
  update_tsibble(index = Week)

head(wm_93)

#scatter plot
wm_93 %>% ggplot(aes(x = Weekly_Sales, y = Fuel_Price)) + geom_point()
cor(wm_93$Weekly_Sales, wm_93$Fuel_Price)

#training/test split
train <- wm_93 %>% filter(Date < '2012-08-01')
test <- wm_93 %>% filter(Date >= '2012-08-01')

#create a mean (benchmark model)
mean_model <- train %>% 
  model(mean = MEAN(Weekly_Sales))

#summary of mean model
report(mean_model)

#residuals of mean model
augment(mean_model)
aug <- augment(mean_model)

aug %>% 
  ggplot(aes(x=Week)) +
  geom_line(aes(y = Weekly_Sales, color = "Data")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  scale_color_manual(values = c(Data = "Black", Fitted = "Blue"))

#forecast into the future with mean model
fc <- mean_model %>% 
  forecast(new_data = test)

fc %>% autoplot(wm_93)

#model all benchmark methods
benchmark <- train %>% 
  model(
    mean = MEAN(Weekly_Sales),
    naive = NAIVE(Weekly_Sales),
    drift = RW(Weekly_Sales ~ drift()),
    lm = TSLM(Weekly_Sales ~ trend())
    )

#when fitting many models use glance to compare
glance(benchmark)

fc <- benchmark %>% 
  forecast(new_data = test)

fc %>% autoplot(test)

accuracy(fc, wm_93)

#linear model
lm <- train %>% 
  model(lm = TSLM(Weekly_Sales ~ trend()))

augment(lm)
aug <- augment(lm)

aug %>% 
  ggplot(aes(x=Week)) +
  geom_line(aes(y = Weekly_Sales, color = "Data")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  scale_color_manual(values = c(Data = "Black", Fitted = "Blue"))

#check if residuals are stationary from trend model
aug %>% features(.innov, unitroot_kpss)
#check if original data is stationary
train %>% features(Weekly_Sales, unitroot_kpss)

#plot of acf of residuals, view of constant variation and distribution histogram
lm %>% gg_tsresiduals()

#difference to convert to stationary, feature engineering (lags)
wm_93_n <- wm_93 %>% 
  select(-c(id, Fuel_Price)) %>% 
  mutate(Diff_Weekly_Sales = difference(Weekly_Sales),
         ws_lag1 = lag(Diff_Weekly_Sales, 1),
         ws_lag2 = lag(Diff_Weekly_Sales, 2),
         ws_lag3 = lag(Diff_Weekly_Sales, 3),
         ws_lag4 = lag(Diff_Weekly_Sales, 4)) %>% 
  filter(!is.na(ws_lag4))

wm_93_n %>% autoplot(Diff_Weekly_Sales)

#high pvalue of kpss implies stationary
wm_93_n %>% features(Diff_Weekly_Sales, unitroot_kpss)

train <- wm_93_n %>% filter(Date < '2012-08-01')
test <- wm_93_n %>% filter(Date >= '2012-08-01')

#train a few linear models and benchmark models
models <- train %>% 
  model(
    lm_holiday = TSLM(Diff_Weekly_Sales ~ IsHoliday),
    lm_t_lag = TSLM(Diff_Weekly_Sales ~ trend() + ws_lag1 + ws_lag2 + ws_lag3 + ws_lag4),
    lm_lag = TSLM(Diff_Weekly_Sales ~ ws_lag1 + ws_lag2 + ws_lag3 + ws_lag4),
    lm_k4_lag = TSLM(Diff_Weekly_Sales ~ fourier(K=4) + ws_lag1 + ws_lag2 + ws_lag3 + ws_lag4),
    lm_k5_lag = TSLM(Diff_Weekly_Sales ~ fourier(K=5) + ws_lag1 + ws_lag2 + ws_lag3 + ws_lag4),
    lm_k6_lag = TSLM(Diff_Weekly_Sales ~ fourier(K=6) + ws_lag1 + ws_lag2 + ws_lag3 + ws_lag4),
    mean = MEAN(Diff_Weekly_Sales),
    naive = NAIVE(Diff_Weekly_Sales),
    drift = RW(Diff_Weekly_Sales ~ drift()),
    lm = TSLM(Diff_Weekly_Sales ~ trend())
  )

glance(models)

fc <- models %>% 
  forecast(new_data = test)

fc %>% autoplot(test, level = NULL)

models %>% accuracy()
fc %>% accuracy(wm_93_n) # test accuracy

#select best model and create a new mable, or model table with only this model
final <- train %>% 
  model(lm_k5_lag = TSLM(Diff_Weekly_Sales ~ fourier(K=5) +
                           ws_lag1 + ws_lag2 + ws_lag3 + ws_lag4))

#view the regression summary
report(final)

#inspect residuals
aug <- augment(final)

aug %>% 
  ggplot(aes(x=Week)) +
  geom_line(aes(y = Diff_Weekly_Sales, color = "Data")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  scale_color_manual(values = c(Data = "Black", Fitted = "Blue"))

#first step is to use gg_tsresiduals to assess residual autocorrelation, distribution and changing properties
final %>% gg_tsresiduals()
#residuals look mostly like white noise
#there is autocorrelation so model could be improved
#residuals look like normal distribution

#residuals are stationary - this is great
aug %>% features(.innov, unitroot_kpss)

#plot residuals against predictors to ensure we modeled correctly
train %>% 
  left_join(residuals(final), by = "Week") %>% 
  pivot_longer(c(ws_lag1:ws_lag4),
               names_to = "regressor", values_to = "x") %>% 
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

#fitted values vs residuals looks like noise, which is great
aug %>% 
  ggplot(aes(x = .fitted, y = .innov)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")

#forecast the test dataset
fc <- final %>% 
  forecast(new_data = test)

#plot the data with the test set and full dataset
fc %>% autoplot(test, level = NULL)
fc %>% autoplot(wm_93_n, level = NULL)

#accuracy on test dataset
fc %>% accuracy(wm_93_n)

#bonus - if data is autocorrelated prediction intervals may not be accurate
#you can use bootstrap sampling on your train residuals to build a prediction interval

fc_bootstrap <- final %>% 
  forecast(new_data = test, bootstrap = TRUE)

fc_bootstrap %>%
  autoplot(test)

