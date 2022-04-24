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

wm_93_n <- wm_93 %>% select(-c(id, Fuel_Price))

#is this data stationary?
wm_93_n %>% autoplot(Weekly_Sales)

#correlogram/ACF plot
wm_93_n %>% ACF(Weekly_Sales) %>% autoplot()
wm_93_n %>% features(Weekly_Sales, unitroot_kpss) #low p-value implies non-stationary

#data is not stationary. We can use R to tell us how many differences we may
#need to take

#seasonal differencing
wm_93_n %>% features(Weekly_Sales, unitroot_nsdiffs) # 2 differences suggested
#but dataset is not long enough for 2 seasonal differences

#another test for seaonal differencing
wm_93_n %>% features(Weekly_Sales, list(unitroot_nsdiffs, feat_stl))
#seasonal strength > 0.64 so should take 1 seasonal difference

#after seasonal differencing check to see is data now stationary?
wm_93_n %>% features(difference(Weekly_Sales,52), unitroot_kpss) #nope

#nonseasonal differencing
wm_93_n %>% features(difference(Weekly_Sales,52), unitroot_ndiffs) # 1 difference


#we will take 1 difference on seasonal based on length of dataset
#and take 1 nonseasonal difference
s_diff_wm <- wm_93_n %>% mutate(s_diff_sales = difference(Weekly_Sales, 52),
                                ns_s_diff_sales = difference(s_diff_sales))

#now data is stationary based on kpss test
s_diff_wm %>% autoplot(ns_s_diff_sales)
s_diff_wm %>% ACF(ns_s_diff_sales) %>% autoplot()
s_diff_wm %>% features(ns_s_diff_sales, unitroot_kpss)

#create an AR(1) model
s_diff_wm_l <- s_diff_wm %>% 
  mutate(Lag_Weekly_Sales = lag(ns_s_diff_sales, 1))

ar1 <- s_diff_wm_l %>% 
  model(ar1 = TSLM(ns_s_diff_sales ~ Lag_Weekly_Sales))

report(ar1)

#equivalent with ARIMA
ar1_arima <- s_diff_wm_l %>% 
  model(ar1 = ARIMA(ns_s_diff_sales ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)))


#ar1 model on original non-stationary wm dataset w/ 1 seasonal and 1 nonseasonal differences
ar1_ma1_arima_original_data <- wm_93_n %>% 
  model(ar1 = ARIMA(Weekly_Sales ~ pdq(1, 1, 0) + PDQ(0, 1, 0)),
        ma1 = ARIMA(Weekly_Sales ~ pdq(0, 1, 1) + PDQ(0, 1, 0)))


#acf and pacf plots for wm weekly sales
#include stationary data to interpret ACF and PACF plots
wm_93_n %>% gg_tsdisplay(difference(Weekly_Sales), plot_type = "partial")

#split train and test
train <- wm_93_n %>% filter(Date < '2012-08-01')
test <- wm_93_n %>% filter(Date >= '2012-08-01')

#try out a few ARIMA models and check for AICc
arima <- train %>% 
  model(AR3 = ARIMA(Weekly_Sales ~ pdq(3,1,0) + PDQ(0,1,0)),
        AR4 = ARIMA(Weekly_Sales ~ pdq(4,1,0) + PDQ(0,1,0)),
        automatic = ARIMA(Weekly_Sales), #fable algorithm
        automatic_exhaustive = ARIMA(Weekly_Sales, stepwise = FALSE), #exhaustive search
        automatic_no_seas_exhaustive = ARIMA(Weekly_Sales ~ PDQ(0, 0, 0), stepwise = FALSE), #exhaustive search no seasonal differences
        automatic_no_seas = ARIMA(Weekly_Sales ~ PDQ(0,0,0)), #fable algorithm no seasonal differencing
        MA = ARIMA(Weekly_Sales ~ pdq(p = 0, d = 1, q = 0:4))) #MA model

#compare aicc
glance(arima)

#review what models were developed
models <- arima 
View(arima)

#compare accuracy on best performing models
final <- train %>% 
  model(full_search = ARIMA(Weekly_Sales, stepwise = FALSE),
        auto_stepwise = ARIMA(Weekly_Sales))

#forecast
fc <- final %>% 
  forecast(new_data = test)

#plot forecast
fc %>% autoplot(test, level = NULL)

#review accuracy to select final model
fc %>% accuracy(wm_93_n) #accuracy on test dataset
